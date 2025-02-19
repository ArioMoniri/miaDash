#' miaDash
#'
#' miaDash is a web app that provides an interface to build and explore
#' \code{\link[TreeSummarizedExperiment:TreeSummarizedExperiment-constructor]{TreeSummarizedExperiment}}
#' (TreeSE) objects by means of \link[iSEE:iSEE]{iSEE}.
#'
#' @return An \code{\link[iSEE:iSEE]{iSEE}} app with a custom landing page to
#'   build TreeSE objects and explore \link[mia:mia-datasets]{mia datasets}.
#'
#' @examples
#' app <- miaDash()
#'
#' if (interactive()) {
#'   shiny::runApp(app)
#' }
#' 
#' @seealso \link[iSEE:iSEE]{iSEE} \link[mia:mia]{mia}
#'   \link[miaViz:miaViz]{miaViz}
#'
#' @name miaDash

#' @export
#' @rdname miaDash
#' @importFrom iSEE iSEE
#' @importFrom utils packageVersion
#' @importFrom htmltools tags
#' @importFrom SingleCellExperiment altExp altExpNames
miaDash <- function() {
    
    addResourcePath("assets", system.file("assets", package = "miaDash"))
  
    iSEE(
        landingPage = .landing_page,
        appTitle = tags$div(
            paste0("Microbiome Analysis Dashboard - v", packageVersion("miaDash")),
            tags$img(src = "assets/mia_logo.png", height = "40px", style = "margin-left: 10px"),
            style = "cursor: pointer; font-weight: 500",
            onclick = "window.location.href='https://miadash-microbiome.2.rahtiapp.fi/'; window.location.reload(true);"
        )
            
    )

}

#' @importFrom methods is
#' @importFrom shinyjs enable
#' @importFrom iSEE RowDataTable ColumnDataTable ReducedDimensionPlot
#'   ComplexHeatmapPlot
#' @importFrom iSEEtree RowTreePlot AbundancePlot RDAPlot AbundanceDensityPlot
#'   LoadingPlot ColumnTreePlot
#' @importFrom TreeSummarizedExperiment rowLinks colLinks
#' @importFrom mia taxonomyRanks
#' @importFrom SummarizedExperiment rowData colData
#' @importFrom SummarizedExperiment altExp altExp<- altExpNames mainExpName
#' @importFrom SingleCellExperiment reducedDims
.launch_isee <- function(FUN, initial, session, rObjects, input) {
    # nocov start
    tse <- rObjects$tse
    
    # Get the current experiment based on user selection
    current_exp <- if(!is.null(input$experiment_choice) && input$experiment_choice != "main") {
        tryCatch({
            altExp(tse, input$experiment_choice)
        }, error = function(e) {
            # Handle case where the selected experiment doesn't exist
            .print_message(
                title = "Experiment Error:",
                "The selected experiment could not be found. Defaulting to main experiment."
            )
            tse
        })
    } else {
        tse
    }
    
    # Filter panels based on experiment type
    if(!is.null(input$experiment_choice) && input$experiment_choice != "main") {
        # Define which panels are compatible with alternative experiments
        altexp_panels <- c("RowDataTable", "ColumnDataTable", "AbundancePlot", 
                          "AbundanceDensityPlot", "ComplexHeatmapPlot",
                          "ReducedDimensionPlot")
        
        # Only keep compatible panels
        initial <- intersect(initial, altexp_panels)
        
        if(length(initial) == 0) {
            # If no compatible panels remain, add a default one
            initial <- c("AbundancePlot")
            .print_message(
                title = "Panel Compatibility Notice:",
                "The selected panels are not compatible with alternative experiments.",
                "Defaulting to AbundancePlot."
            )
        }
    }
  
    # Convert string panel names to actual panel objects with appropriate configuration
    initial <- lapply(initial, function(x) {
        if(is.character(x)) {
            panel <- eval(parse(text = paste0(x, "()")))
        } else {
            panel <- x
        }
        
        # Configure panel based on experiment type
        if(!is.null(input$experiment_choice) && input$experiment_choice != "main") {
            # Set experiment name for dimension reduction panels
            if(inherits(panel, "DimensionReducedPanel") || 
               inherits(panel, "ReducedDimensionPlot")) {
                # Use try-catch to handle any attribute setting errors
                tryCatch({
                    panel$ExperimentName <- input$experiment_choice
                }, error = function(e) {
                    # Silently continue if attribute can't be set
                })
            }
            
            # Handle visualization settings for alternative experiment panels
            if(inherits(panel, "AbundancePlot") || 
               inherits(panel, "ComplexHeatmapPlot")) {
                tryCatch({
                    panel$ShowFeatureNames <- TRUE
                    if(grepl("^agglomerated_", input$experiment_choice)) {
                        panel$ShowAggregationLevel <- TRUE
                    }
                }, error = function(e) {
                    # Silently continue if attributes can't be set
                })
            }
        }
        
        return(panel)
    })
    
    # Check if panels are compatible with the current experiment
    # Use safer versions of checking functions that handle potential errors
    initial <- .check_panel_safe(current_exp, initial, "RowDataTable", rowData)
    initial <- .check_panel_safe(current_exp, initial, "ColumnDataTable", colData)
    initial <- .check_panel_safe(current_exp, initial, "RowTreePlot", rowLinks)
    initial <- .check_panel_safe(current_exp, initial, "AbundancePlot", taxonomyRanks)
    initial <- .check_panel_safe(current_exp, initial, "ReducedDimensionPlot", reducedDims)
    initial <- .check_panel_safe(current_exp, initial, "LoadingPlot", reducedDims)
    initial <- .check_panel_safe(current_exp, initial, "ColumnTreePlot", colLinks)
  
    # Launch iSEE with the current experiment and validated panels
    FUN(SE = current_exp, INIT = initial)
  
    # Enable iSEE interface buttons
    enable("iSEE_INTERNAL_organize_panels")
    enable("iSEE_INTERNAL_link_graph")
    enable("iSEE_INTERNAL_export_content")
    enable("iSEE_INTERNAL_tracked_code")
    enable("iSEE_INTERNAL_panel_settings")
    enable("iSEE_INTERNAL_open_vignette")
    enable("iSEE_INTERNAL_session_info")
    enable("iSEE_INTERNAL_citation_info") 
  
    invisible(NULL)
    # nocov end
}

# Helper function for safely checking panel compatibility
.check_panel_safe <- function(se, panel_list, panel_class, panel_fun) {
    tryCatch({
        no_keep <- unlist(lapply(panel_list, function(x) inherits(x, panel_class)))
        
        if(any(no_keep)) {
            # Check if the function can be applied to the experiment
            result <- tryCatch({
                func_result <- panel_fun(se)
                is.null(func_result) || isEmpty(func_result)
            }, error = function(e) {
                # If function fails, consider it incompatible
                TRUE
            })
            
            if(result) {
                panel_list <- panel_list[!no_keep]
                warning("no valid ", as.character(substitute(panel_fun)),
                    " fields for ", panel_class, call. = FALSE)
            }
        }
        
        return(panel_list)
    }, error = function(e) {
        # If anything fails, return the original panel list
        warning("Error checking compatibility for ", panel_class, 
                ": ", conditionMessage(e), call. = FALSE)
        return(panel_list)
    })
}

# Helper function to safely get experiment
.get_experiment <- function(tse, experiment_name) {
    if(is.null(experiment_name) || experiment_name == "main") {
        return(tse)
    }
    
    tryCatch({
        # Try to get alternative experiment
        altExp(tse, experiment_name)
    }, error = function(e) {
        # Return main experiment if alternative not found
        .print_message(
            title = "Experiment Not Found:",
            paste("Could not find experiment:", experiment_name),
            "Using the main experiment instead."
        )
        return(tse)
    })
}

# Helper function to filter panels by experiment type
.filter_panels_by_experiment <- function(panels, compatible_panels) {
    if(is.character(panels)) {
        # If panels are character strings
        filtered <- intersect(panels, compatible_panels)
        if(length(filtered) == 0) {
            return(compatible_panels[1])  # return at least one compatible panel
        }
        return(filtered)
    } else {
        # If panels are already objects
        filtered <- panels[sapply(panels, function(p) {
            any(sapply(compatible_panels, function(cp) inherits(p, cp)))
        })]
        if(length(filtered) == 0 && length(compatible_panels) > 0) {
            return(list(eval(parse(text = paste0(compatible_panels[1], "()"))))) 
        }
        return(filtered)
    }
}
