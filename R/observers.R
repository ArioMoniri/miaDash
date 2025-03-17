#' Observers
#' 
#' \code{.create_observers} and \code{.create_launch_observers} define the
#' server to import and build TreeSE objects and track the state of the Build
#' and Launch buttons.
#'
#' @param input The Shiny input object from the server function.
#' @param rObjects A reactive list of values generated in the landing page.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @name create_observers
#' @keywords internal

#' @rdname create_observers
#' @importFrom utils read.csv
#' @importFrom ape read.tree
#' @importFrom S4Vectors DataFrame
#' @importFrom biomformat read_biom
#' @importFrom mia convertFromBIOM importMetaPhlAn
#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment mainExpName 'mainExpName<-'
#' @importFrom SingleCellExperiment altExp altExpNames
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom SingleCellExperiment altExps altExps<-
#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment mainExpName 'mainExpName<-' colTree
#' @importFrom SingleCellExperiment altExp altExpNames reducedDimNames
#' @importFrom SingleCellExperiment altExps altExps<-
#' @importFrom htmltools tags tagList div h4 p hr icon
#' @importFrom shiny showNotification updateCheckboxGroupInput updateSelectInput
#' @importFrom shinyjs simulateClick
#' @importFrom mia taxonomyRanks
#' @importFrom htmltools HTML br tags div tagList
.create_import_observers <- function(input, rObjects) {
  
    # nocov start
    observeEvent(input$import, {
      
        if( input$format == "dataset" ){
      
            rObjects$tse <- isolate(get(input$data))
      
        }else if( input$format == "rds" ){
      
            isolate({
                req(input$file)
                rObjects$tse <- readRDS(input$file$datapath)
            })
      
        }else if( input$format == "raw" ){
      
            isolate({
                req(input$assay)
        
                assay_list <- lapply(input$assay$datapath,
                    function(x) as.matrix(read.csv(x, row.names = 1)))
                
                names(assay_list) <- gsub(".csv", "", input$assay$name)
                
                coldata <- .set_optarg(input$coldata$datapath,
                    alternative = DataFrame(row.names = colnames(assay_list[[1]])),
                    loader = read.csv, row.names = 1)

                rowdata <- .set_optarg(input$rowdata$datapath,
                    loader = read.csv, row.names = 1)
               
                row.tree <- .set_optarg(input$row.tree$datapath,
                    loader = read.tree)
                
                col.tree <- .set_optarg(input$col.tree$datapath,
                    loader = read.tree)
                
                fun_args <- list(assays = assay_list, colData = coldata,
                    rowData = rowdata, rowTree = row.tree, colTree = col.tree)
        
                rObjects$tse <- .update_tse(
                     rObjects$tse, TreeSummarizedExperiment, fun_args
                )
            
            })
      
        }else if( input$format == "foreign" ){
          
            isolate({
                req(input$main.file)
      
                if( input$ftype == "biom" ){

                    biom_object <- read_biom(input$main.file$datapath)
                    
                    fun_args <- list(x = biom_object,
                        removeTaxaPrefixes = input$rm.tax.pref,
                        rankFromPrefix = input$rank.from.pref)

                    rObjects$tse <- .update_tse(
                        rObjects$tse, convertFromBIOM, fun_args
                    )
              
                } else if( input$ftype == "MetaPhlAn" ){
                  
                    coldata <- .set_optarg(input$col.data$datapath,
                        alternative = input$col.data$datapath)
                    
                    treefile <- .set_optarg(input$tree.file$datapath)
                  
                    fun_args <- list(file = input$main.file$datapath,
                        col.data = coldata, tree.file = treefile)
              
                    rObjects$tse <- .update_tse(
                         rObjects$tse, importMetaPhlAn, fun_args
                    ) 
                 
                }
        
            })
            
        }
      
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    # nocov end
  
    invisible(NULL)
}



#' @rdname create_observers
#' @importFrom mia taxonomyRanks
.create_merged_file_observers <- function(input, rObjects) {
    observeEvent(input$merged_file, {
        isolate({
            tryCatch({
                tse <- .process_merged_file(input$merged_file$datapath)
                
                if(input$agglomeration_levels == "All" && 
                   length(taxonomyRanks(tse)) > 0) {
                    levels <- taxonomyRanks(tse)
                } else if(input$agglomeration_levels == "Custom") {
                    levels <- input$custom_levels
                } else {
                    levels <- character(0)
                }
                
                if(length(levels) > 0) {
                    tse <- .create_agglomerated_experiments(tse, levels)
                }
                
                rObjects$tse <- tse
                
                # Show success message
                showNotification(
                    "Merged file successfully loaded",
                    type = "message"
                )
                
            }, error = function(e) {
                .print_message(
                    title = "Error in merged file:",
                    e$message
                )
            })
        })
    })
}


          
#' @rdname create_observers
.create_altexp_observers <- function(input, rObjects) {
    observeEvent(input$add_altexp, {
        isolate({
            req(input$alt_assay)
            req(input$alt_name)
            
            # Create alternative experiment
            alt_assay_list <- lapply(input$alt_assay$datapath,
                function(x) as.matrix(read.csv(x, row.names = 1)))
            names(alt_assay_list) <- gsub(".csv", "", input$alt_assay$name)
            
            alt_coldata <- .set_optarg(input$alt_coldata$datapath,
                alternative = DataFrame(row.names = colnames(alt_assay_list[[1]])),
                loader = read.csv, row.names = 1)
            
            alt_rowdata <- .set_optarg(input$alt_rowdata$datapath,
                loader = read.csv, row.names = 1)
            
            alt_exp <- SummarizedExperiment(
                assays = alt_assay_list,
                colData = alt_coldata,
                rowData = alt_rowdata
            )
            
            # Add to main experiment
            altExp(rObjects$tse, input$alt_name) <- alt_exp
        })
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
}

#' @rdname create_observers
#' @importFrom SummarizedExperiment assay
#' @importFrom mia subsetByPrevalent subsetByRare agglomerateByRank
#'   transformAssay
.create_manipulate_observers <- function(input, rObjects) {
  
    # nocov start
    observeEvent(input$apply, {
      
        if( input$manipulate == "subset" ){
          
            isolate({
                req(input$subassay)
              
                if( input$subkeep == "prevalent" ){
                    subset_fun <- subsetByPrevalent
                } else if( input$subkeep == "rare" ){
                    subset_fun <- subsetByRare
                }
            
                fun_args <- list(x = rObjects$tse, assay.type = input$subassay,
                    prevalence = input$prevalence, detection = input$detection)
                
                # Handle altExp creation if checkbox is selected
                if(input$create_altexp && input$altexp_name != "") {
                    altexp_object <- do.call(subset_fun, fun_args)
                    altExps(rObjects$tse)[[input$altexp_name]] <- altexp_object
                    showNotification(paste("Created alternative experiment:", input$altexp_name), 
                                     type = "message")
                } else {
                    rObjects$tse <- .update_tse(rObjects$tse, subset_fun, fun_args)
                }
            })
          
        } else if( input$manipulate == "agglomerate" ){
          
            isolate({
                
                fun_args <- list(x = rObjects$tse, rank = input$taxrank)
                
                # Handle altExp creation if checkbox is selected
                if(input$create_altexp && input$altexp_name != "") {
                    altexp_object <- do.call(agglomerateByRank, fun_args)
                    altExps(rObjects$tse)[[input$altexp_name]] <- altexp_object
                    showNotification(paste("Created alternative experiment:", input$altexp_name), 
                                     type = "message")
                } else {
                    rObjects$tse <- .update_tse(rObjects$tse, agglomerateByRank, fun_args)
                }
            })
          
        } else if( input$manipulate == "transform" ){

            if( input$trans.method == "clr" && !input$pseudocount &&
                any(assay(rObjects$tse, input$assay.type) <= 0)){
              
                .print_message(
                    "'clr' cannot be used with non-positive data:",
                    "please turn on pseudocount."
                )
              
                return()
            }
          
            isolate({
                req(input$assay.type)
              
                if( input$assay.name != "" ){
                    name <- input$assay.name
                } else {
                    name <- input$trans.method
                }
              
                fun_args <- list(x = rObjects$tse, name = name,
                    method = input$trans.method, assay.type = input$assay.type,
                    MARGIN = input$margin, pseudocount = input$pseudocount)
                
                rObjects$tse <- .update_tse(
                     rObjects$tse, transformAssay, fun_args
                )
                
            })
          
        }
      
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    # nocov end
  
    invisible(NULL)
}


#' @rdname create_observers
#' @importFrom SingleCellExperiment altExp altExpNames 
#' @importFrom TreeSummarizedExperiment mainExpName 'mainExpName<-'
.create_switch_observers <- function(input, rObjects) {
    observeEvent(input$do_switch, {
        isolate({
            req(input$switch_experiment)
            if(input$switch_experiment != mainExpName(rObjects$tse)) {
                # Store the name of the current main experiment
                mainExpName(rObjects$tse) <- input$switch_experiment
                
                # Show notification about the switch
                showNotification(
                    paste0("Switched to experiment: ", input$switch_experiment),
                    type = "message"
                )
            }
        })
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    invisible(NULL)
}


#' @rdname create_observers
#' @importFrom shiny observeEvent isolate req showModal modalDialog removeModal actionButton modalButton
#' @importFrom htmltools tags p div strong
#' @importFrom shinyjs show hide
.create_experiment_state_observers <- function(input, session, rObjects) {
  
    # Check if already initialized and only initialize if needed
    if(!exists("experiment_states", rObjects)) {
        rObjects$experiment_states <- .create_experiment_state_cache()
        rObjects$experiment_history <- .create_history_tracker()
        rObjects$panel_configurations <- .create_panel_config_storage()
        rObjects$transition_preferences <- .create_transition_preferences()
    }
    
    # When an experiment is selected for switching
    observeEvent(input$iSEE_switch_experiment, {
        req(input$iSEE_switch_experiment)
        isolate({
            current_exp <- mainExpName(rObjects$tse)
            if(is.null(current_exp)) current_exp <- "main"
            target_exp <- input$iSEE_switch_experiment
            
            # Only proceed if it's actually a different experiment
            if(current_exp != target_exp) {
                # Verify target experiment exists
                if(target_exp != "main" && !(target_exp %in% altExpNames(rObjects$tse))) {
                    showNotification(
                        paste("Error: Experiment", target_exp, "not found."),
                        type = "error"
                    )
                    return(NULL)
                }
                
                # Check if confirmation is needed
                if(.needs_confirmation(rObjects$transition_preferences, current_exp, target_exp)) {
                    .confirm_experiment_transition(session, rObjects, current_exp, target_exp)
                } else {
                    # No confirmation needed, switch directly
                    .perform_experiment_switch(rObjects, current_exp, target_exp)
                }
            }
        })
    }, ignoreInit = TRUE)
    
    # Observe panel configuration changes
    observeEvent(input$panel_config_changed, {
        req(input$panel_config_changed)
        isolate({
            panel_data <- input$panel_config_changed
            current_exp <- mainExpName(rObjects$tse)
            if(is.null(current_exp)) current_exp <- "main"
            
            .save_panel_config(
                rObjects$panel_configurations, 
                current_exp, 
                panel_data$panel_id, 
                panel_data$config
            )
        })
    }, ignoreInit = TRUE)
    
    # For undo/redo history navigation
    observeEvent(input$undo_experiment_switch, {
        isolate({
            history <- .get_history(rObjects$experiment_history)
            if(length(history) > 0) {
                last_step <- history[[length(history)]]
                .perform_experiment_switch(rObjects, 
                                          last_step$to, 
                                          last_step$from, 
                                          record_history = FALSE)
                # Remove the last step from history
                rObjects$experiment_history(history[-length(history)])
            }
        })
    }, ignoreInit = TRUE)
    
    # Observer for the remember_transitions checkbox
    observeEvent(input$remember_transitions, {
        if (input$remember_transitions) {
            # Show the history panel
            shinyjs::show("experiment_history_panel")
        } else {
            # Hide the history panel
            shinyjs::hide("experiment_history_panel")
        }
    }, ignoreInit = TRUE)
    
    invisible(NULL)
}

# Helper functions for experiment state management

#' @rdname utils
.confirm_experiment_transition <- function(session, rObjects, from_exp, to_exp) {
    # Create a unique ID for the modal inputs to avoid conflicts
    modal_id <- paste0("switch_", from_exp, "_to_", to_exp)
    checkbox_id <- paste0("remember_choice_", modal_id)
    confirm_id <- paste0("confirm_switch_", modal_id)
    
    showModal(modalDialog(
        title = "Switch Experiment?",
        p(paste0("Are you sure you want to switch from '", 
               ifelse(from_exp == "main", "Main Experiment", from_exp),
               "' to '", 
               ifelse(to_exp == "main", "Main Experiment", to_exp), "'?")),
        div(
            tags$strong("Note:"), 
            "Your panel configurations will be preserved where possible."
        ),
        checkboxInput(checkbox_id, "Remember this choice", value = FALSE),
        footer = tagList(
            actionButton(confirm_id, "Switch", class = "btn-primary"),
            modalButton("Cancel")
        )
    ))
    
    # Use a one-time observer with the unique ID
    observeEvent(input[[confirm_id]], {
        isolate({
            # Remember user preference if requested
            if(input[[checkbox_id]]) {
                .save_transition_preference(rObjects$transition_preferences, from_exp, to_exp)
            }
            
            # Perform the actual experiment switch
            .perform_experiment_switch(rObjects, from_exp, to_exp)
            
            removeModal()
        })
    }, once = TRUE)
}

#' @rdname utils
#' @importFrom shiny showNotification
#' @importFrom shinyjs runjs
.perform_experiment_switch <- function(rObjects, from_exp, to_exp, record_history = TRUE) {
    tryCatch({
        # 1. Create snapshot of current state
        if(!is.null(rObjects$panel_configurations)) {
            current_panel_config <- rObjects$panel_configurations()[[from_exp]]
            if(!is.null(current_panel_config)) {
                .create_experiment_snapshot(
                    rObjects$experiment_states, 
                    from_exp, 
                    current_panel_config
                )
            }
        }
        
        # 2. Switch the experiment
        mainExpName(rObjects$tse) <- to_exp
        
        # 3. Record in history if needed
        if(record_history && !is.null(rObjects$experiment_history)) {
            .add_to_history(rObjects$experiment_history, from_exp, to_exp)
        }
        
        # 4. Send update notification to UI
        showNotification(
            paste0("Switched to experiment: ", 
                  ifelse(to_exp == "main", "Main Experiment", to_exp)),
            type = "message"
        )
        
        # 5. Trigger panel reconfiguration in UI
        runjs("if(window.iSEEApp && window.iSEEApp.reconfigurePanels) { window.iSEEApp.reconfigurePanels(); }")
    }, error = function(e) {
        # Handle any errors during the switch
        showNotification(
            paste("Error switching experiment:", e$message),
            type = "error",
            duration = 10
        )
    })
}



                 

                 
#' @rdname create_observers
#' @importFrom stats as.formula
#' @importFrom mia addAlpha runNMDS runRDA getDissimilarity
#' @importFrom TreeSummarizedExperiment rowTree
#' @importFrom scater runMDS runPCA
#' @importFrom vegan vegdist
.create_estimate_observers <- function(input, rObjects) {
  
    # nocov start
    observeEvent(input$compute, {
        
        if( input$estimate == "alpha" ){
          
            if( is.null(input$alpha.index) ){
                .print_message("Please select one or more metrics.")
                return()
            }
        
            isolate({
                req(input$estimate.assay)
              
                if( input$estimate.name != "" ){
                    name <- input$estimate.name
                } else {
                    name <- input$alpha.index
                }
          
                fun_args <- list(x = rObjects$tse, name = name,
                    assay.type = input$estimate.assay, index = input$alpha.index)
                
                rObjects$tse <- .update_tse(rObjects$tse, addAlpha, fun_args)
          
            })
        
        } else if( input$estimate == "beta" ){
          
            if( input$ncomponents > nrow(rObjects$tse) - 1 ){
              
                .print_message(
                    "Please use a number of components smaller than the number",
                    "of features in the assay."
                )
              
                return()
            }
          
            isolate({
                req(input$estimate.assay)
              
                if( input$estimate.name != "" ){
                    name <- input$estimate.name
                } else {
                    name <- input$bmethod
                }
              
                beta_args <- list(x = rObjects$tse, assay.type = input$assay.type,
                    ncomponents = input$ncomponents, name = name)
              
                if( input$beta.index == "unifrac" ){
                  
                    if( is.null(rowTree(rObjects$tse)) ){
                        .print_message("Unifrac cannot be computed without a rowTree.")
                        return()
                    }
                  
                    beta_args <- c(beta_args, FUN = getDissimilarity,
                        tree = list(rowTree(rObjects$tse)),
                        ntop = nrow(rObjects$tse), method = input$beta.index)
                    
                } else if( input$bmethod %in% c("MDS", "NMDS") ){
                  
                    beta_args <- c(beta_args, FUN = vegdist,
                        method = input$beta.index)
                    
                } else if( input$bmethod == "RDA" ){
                  
                    if( input$rda.formula == "" ){
                        .print_message("Please enter a formula.")
                        return()
                    }
                  
                    if( !.check_formula(input$rda.formula, rObjects$tse) ){
                        .print_message("Please make sure all elements in the",
                           "formula match variables of the column data.")
                        return()
                    }
                  
                    beta_args <- c(beta_args,
                        formula = as.formula(input$rda.formula))
                  
                }
                
                beta_fun <- eval(parse(text = paste0("run", input$bmethod)))
                
                rObjects$tse <- .update_tse(rObjects$tse, beta_fun, beta_args)
            })
        
        }
        
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    # nocov end
  
    invisible(NULL)
}



                 
                 
#' @rdname create_observers
#' @param output The Shiny output object from the server function, defaults to NULL.
#' @importFrom SummarizedExperiment assayNames
#' @importFrom mia taxonomyRanks
#' @importFrom rintrojs introjs
.update_observers <- function(input, session, rObjects, output = NULL){
  
    # nocov start
    observe({
        if(isS4(rObjects$tse)) {
            # Get available alternative experiments
            alt_exps <- altExpNames(rObjects$tse)
            current_main <- mainExpName(rObjects$tse)
            if(is.null(current_main)) current_main <- "main"
            
            # Create choices list for experiments
            choices <- c("Main" = "main")
            if(length(alt_exps) > 0) {
                alt_choices <- setNames(alt_exps, paste("Alt:", alt_exps))
                choices <- c(choices, alt_choices)
            }
            
            # Update experiment choice inputs
            updateSelectInput(session, inputId = "experiment_choice",
                choices = choices)
                
            # Update switch experiment dropdown
            updateSelectInput(session, inputId = "switch_experiment",
                choices = setNames(c("main", alt_exps), c("Main", alt_exps)),
                selected = current_main)
            
            # Get current experiment based on mainExpName
            current_exp <- if(current_main != "main" && current_main %in% alt_exps) {
                altExp(rObjects$tse, current_main)
            } else {
                rObjects$tse
            }
            
            # Update assay choices based on current experiment
            updateSelectInput(session, inputId = "subassay",
                choices = assayNames(current_exp))
            
            updateSelectInput(session, inputId = "taxrank",
                choices = taxonomyRanks(current_exp))
              
            updateSelectInput(session, inputId = "assay.type",
                choices = assayNames(current_exp))
              
            updateSelectInput(session, inputId = "estimate.assay",
                choices = assayNames(current_exp))
            
            # Update numeric input based on current experiment dimensions
            updateNumericInput(session, inputId = "ncomponents",
                max = nrow(current_exp) - 1)
        }
                                                       
    })


    # Display experiment metadata for the current experiment
    output$current_experiment_name <- renderText({
        if(isS4(rObjects$tse)) {
            current_main <- mainExpName(rObjects$tse)
            if(is.null(current_main)) current_main <- "main"
            if(current_main == "main") {
                return("Main Experiment")
            } else {
                return(paste("Alternative:", current_main))
            }
        } else {
            return("No Experiment Loaded")
        }
    })
    
    output$current_experiment_meta <- renderUI({
        if(!isS4(rObjects$tse)) {
            return(p("Please import a dataset first."))
        }
        
        current_main <- mainExpName(rObjects$tse)
        if(is.null(current_main)) current_main <- "main"
        
        exp_obj <- if(current_main != "main" && current_main %in% altExpNames(rObjects$tse)) {
            altExp(rObjects$tse, current_main)
        } else {
            rObjects$tse
        }
        
        meta <- list(
            paste0(format(nrow(exp_obj), big.mark=","), " features"),
            paste0(format(ncol(exp_obj), big.mark=","), " samples"),
            paste0(length(assayNames(exp_obj)), " assays")
        )
        
        if(inherits(exp_obj, "TreeSummarizedExperiment")) {
            if(!is.null(rowTree(exp_obj))) {
                meta <- c(meta, "rowTree: available")
            }
            if(!is.null(colTree(exp_obj))) {
                meta <- c(meta, "colTree: available")
            }
        }
        
        if(inherits(exp_obj, "SingleCellExperiment") && 
           length(reducedDimNames(exp_obj)) > 0) {
            meta <- c(meta, paste0(length(reducedDimNames(exp_obj)), 
                                  " reduced dimensions"))
        }
        
        tags$ul(
            style = "padding-left: 15px; margin-bottom: 0;",
            lapply(meta, function(item) tags$li(item))
        )
    })
                
    
    
    # Observer for refreshing experiment list
    observeEvent(input$refresh_experiments, {
        if(isS4(rObjects$tse)) {
            showNotification("Refreshing experiment list...", type = "message")
            
            # Get available alternative experiments
            alt_exps <- altExpNames(rObjects$tse)
            current_main <- mainExpName(rObjects$tse)
            if(is.null(current_main)) current_main <- "main"
            
            # Create choices list for experiments
            choices <- c("Main" = "main")
            if(length(alt_exps) > 0) {
                alt_choices <- setNames(alt_exps, paste("Alt:", alt_exps))
                choices <- c(choices, alt_choices)
            }
            
            # Update experiment choice inputs
            updateSelectInput(session, inputId = "experiment_choice",
                choices = choices)
                
            # Update switch experiment dropdown
            updateSelectInput(session, inputId = "switch_experiment",
                choices = setNames(c("main", alt_exps), c("Main", alt_exps)),
                selected = current_main)
            
            # Update global experiment selector if it exists
            if(!is.null(input$global_experiment_selector)) {
                updateSelectInput(session, inputId = "global_experiment_selector",
                    choices = choices,
                    selected = current_main)
            }
        }
    })
    
    # Observer for global experiment selector
    observeEvent(input$global_experiment_selector, {
        req(input$global_experiment_selector)
        if(isS4(rObjects$tse) && 
           input$global_experiment_selector != mainExpName(rObjects$tse)) {
            # Update the switch experiment selector to match
            updateSelectInput(session, "switch_experiment", 
                             selected = input$global_experiment_selector)
            # Trigger the switch
            simulateClick("do_switch")
        }
    }, ignoreInit = TRUE)
    
    # Observer to sync experiment selection
    observeEvent(input$sync_experiment, {
        if(isS4(rObjects$tse)) {
            current_main <- mainExpName(rObjects$tse)
            if(is.null(current_main)) current_main <- "main"
            
            updateSelectInput(session, "experiment_choice", 
                             selected = current_main)
            
            showNotification(
                paste("Visualization will use:", 
                      ifelse(current_main == "main", "Main Experiment", current_main)),
                type = "message"
            )
        }
    })



      

    # Render experiment details for the switch panel
    output$switch_experiment_details <- renderUI({
        req(input$switch_experiment)
        
        if(!isS4(rObjects$tse)) {
            return(p("Please import a dataset first."))
        }
        
        exp_name <- input$switch_experiment
        exp_obj <- if(exp_name != "main" && exp_name %in% altExpNames(rObjects$tse)) {
            altExp(rObjects$tse, exp_name)
        } else {
            rObjects$tse
        }
        
        divs <- list(
            h4(ifelse(exp_name == "main", "Main Experiment", exp_name), 
               style = "margin-top: 0; color: #337ab7;"),
            div(
                style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
                div(
                    tags$strong("Type:"), 
                    tags$span(class(exp_obj)[1])
                ),
                div(
                    tags$strong("Dimensions:"), 
                    tags$span(paste0(format(nrow(exp_obj), big.mark=","), " × ", 
                                    format(ncol(exp_obj), big.mark=",")))
                )
            )
        )
        
        # Add assays info
        if(length(assayNames(exp_obj)) > 0) {
            divs <- c(divs, list(
                div(
                    tags$strong("Assays:"),
                    tags$span(paste(assayNames(exp_obj), collapse=", "))
                )
            ))
        }
        
        # Add tree info
        if(inherits(exp_obj, "TreeSummarizedExperiment")) {
            tree_status <- c()
            if(!is.null(rowTree(exp_obj))) {
                tree_status <- c(tree_status, "rowTree")
            }
            if(!is.null(colTree(exp_obj))) {
                tree_status <- c(tree_status, "colTree")
            }
            
            if(length(tree_status) > 0) {
                divs <- c(divs, list(
                    div(
                        tags$strong("Trees:"),
                        tags$span(paste(tree_status, collapse=", "))
                    )
                ))
            }
        }
        
        # Add reduced dimension info
        if(inherits(exp_obj, "SingleCellExperiment") && 
           length(reducedDimNames(exp_obj)) > 0) {
            divs <- c(divs, list(
                div(
                    tags$strong("Reduced Dimensions:"),
                    tags$span(paste(reducedDimNames(exp_obj), collapse=", "))
                )
            ))
        }
        
        # Add compatible panels info
        compatible_panels <- .get_compatible_panels_for_experiment(rObjects$tse, exp_name)
        divs <- c(divs, list(
            hr(style = "margin: 10px 0;"),
            p(tags$strong("Compatible Panels:"), style = "margin-bottom: 5px;"),
            tags$ul(
                style = "padding-left: 15px; margin-bottom: 0;",
                lapply(compatible_panels, function(panel) {
                    tags$li(panel)
                })
            )
        ))
        
        do.call(tagList, divs)
    })          

    # Show compatible panels for visualization
    output$compatible_panels_info <- renderUI({
        req(input$experiment_choice)
        
        if(!isS4(rObjects$tse)) {
            return(NULL)
        }
        
        compatible_panels <- .get_compatible_panels_for_experiment(
            rObjects$tse, input$experiment_choice)
        
        if(length(compatible_panels) == 0) {
            return(div(
                class = "alert alert-warning",
                style = "margin-top: 10px; padding: 8px;",
                icon("exclamation-triangle"), 
                "No compatible panels found for this experiment."
            ))
        }
        
        # Filter the panels selection to compatible ones
        updateSelectInput(session, "panels",
            choices = compatible_panels,
            selected = intersect(input$panels, compatible_panels)
        )
        
        # Update checkbox options for alt experiments
        alt_compatible <- c("AbundancePlot", "ComplexHeatmapPlot", "RowDataTable")
        updateCheckboxGroupInput(session, "altexp_panels",
            choices = setNames(alt_compatible, 
                             c("Abundance Plot", "Heatmap", "Data Table")),
            selected = intersect(input$altexp_panels, alt_compatible)
        )
        
        div(
            class = "alert alert-info",
            style = "margin-top: 10px; padding: 8px;",
            p(tags$strong(length(compatible_panels)), " compatible panels found")
        )
    })          
          
          
    
    observeEvent(input$iSEE_INTERNAL_tour_steps, {
      
        introjs(session, options = list(steps = .landing_page_tour))
      
    }, ignoreInit = TRUE)
    # nocov end
    
    invisible(NULL)
}

#' @rdname create_observers
.create_launch_observers <- function(FUN, input, session, rObjects) {
    # nocov start
    observeEvent(input$launch, {
        # Save the experiment choice for iSEE
        experiment_choice <- isolate(input$experiment_choice)
        if(is.null(experiment_choice)) experiment_choice <- "main"
        
        # Ensure experiment states are initialized
        if(!exists("experiment_states", rObjects)) {
            rObjects$experiment_states <- .create_experiment_state_cache()
            rObjects$experiment_history <- .create_history_tracker()
            rObjects$panel_configurations <- .create_panel_config_storage()
            rObjects$transition_preferences <- .create_transition_preferences()
        }
        
        # Take snapshot of initial state
        if(exists("experiment_states", rObjects)) {
            current_panel_config <- isolate(input$panels)
            .create_experiment_snapshot(
                rObjects$experiment_states, 
                experiment_choice, 
                current_panel_config
            )
        }
        
        # Launch iSEE with experiment information
        .launch_isee(
            FUN, 
            input$panels, 
            session, 
            rObjects, 
            input,
            initial_experiment = experiment_choice
        )
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    # nocov end
    invisible(NULL)
}

#' @rdname utils
.process_merged_file <- function(file_path) {
    # Process the merged file and return a TreeSummarizedExperiment object
    tse <- readRDS(file_path)
    return(tse)
}

#' @rdname utils
.create_agglomerated_experiments <- function(tse, levels) {
    for(level in levels) {
        alt_name <- paste0("agglom_", level)
        agglom_tse <- agglomerateByRank(tse, rank = level)
        altExps(tse)[[alt_name]] <- agglom_tse
    }
    return(tse)
}

#' @rdname utils
.get_compatible_panels_for_experiment <- function(tse, exp_name) {
    panels <- c("RowDataTable", "ColumnDataTable", "ReducedDimensionPlot", "ComplexHeatmapPlot")
    if(inherits(tse, "TreeSummarizedExperiment")) {
        panels <- c(panels, "RowTreePlot", "AbundancePlot", "RDAPlot", "AbundanceDensityPlot")
        if(!is.null(colTree(tse))) {
            panels <- c(panels, "ColumnTreePlot")
        }
    }
    return(panels)
}
                                                   
