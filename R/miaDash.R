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
#' @importFrom htmltools tags tagList singleton HTML
#' @importFrom SingleCellExperiment altExp altExpNames
#' @importFrom shinyjs runjs enable

miaDash <- function() {
    
    addResourcePath("assets", system.file("assets", package = "miaDash"))
  
    # Enhanced JavaScript for experiment switching
    experiment_js <- "
    window.iSEEApp = window.iSEEApp || {};
    
    // Initialize panel tracking
    window.iSEEApp.panels = {};
    
    // Register panels when they're created
    $(document).on('iSEE:panelCreated', function(event, panelId, panelType) {
        window.iSEEApp.panels[panelId] = {
            type: panelType,
            config: {}
        };
    });
    
    // Track panel configuration changes
    $(document).on('iSEE:panelSettingsChanged', function(event, panelId, settings) {
        if (window.iSEEApp.panels[panelId]) {
            window.iSEEApp.panels[panelId].config = settings;
            Shiny.setInputValue('panel_config_changed', {
                panel_id: panelId,
                config: settings
            });
        }
    });
    
    // Panel reconfiguration for experiment switching
    window.iSEEApp.reconfigurePanels = function(experimentName) {
        var expName = experimentName || $('#iSEE_INTERNAL_experiment_selector').val();
        
        // For each panel in the current view
        for (var panelId in window.iSEEApp.panels) {
            var panelType = window.iSEEApp.panels[panelId].type;
            
            // Request panel-specific reconfiguration from server
            Shiny.setInputValue('reconfigure_panel', {
                panel_id: panelId,
                panel_type: panelType,
                experiment: expName
            });
            
            // Update experiment reference attribute if panel has it
            $('.panel[data-panel-id=\"' + panelId + '\"]')
                .attr('data-experiment', expName);
        }
        
        // Show transition indicator
        $('.experiment-transition-indicator').fadeIn(200).delay(500).fadeOut(200);
    };
    
    // Handle experiment selector changes
    $(document).on('change', '#iSEE_INTERNAL_experiment_selector', function() {
        var selectedExp = $(this).val();
        Shiny.setInputValue('iSEE_switch_experiment', selectedExp);
    });
    
    // Add keyboard shortcuts
    $(document).keydown(function(e) {
        // Alt+E to focus experiment selector
        if (e.altKey && e.keyCode === 69) { // 'E' key
            e.preventDefault();
            $('#iSEE_INTERNAL_experiment_selector').focus();
        }
        
        // Alt+Z for undo experiment switch
        if (e.altKey && e.keyCode === 90) { // 'Z' key
            e.preventDefault();
            $('#undo_experiment_switch').click();
        }
        
        // Alt+Y for redo experiment switch
        if (e.altKey && e.keyCode === 89) { // 'Y' key
            e.preventDefault();
            $('#redo_experiment_switch').click();
        }
    });
    "
    
    # Custom CSS for experiment management UI
    experiment_css <- "
    .experiment-selector-container .selectize-control {
        margin-bottom: 0;
    }
    .experiment-header-row {
        width: 100%;
    }
    .experiment-transition-indicator {
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 3px;
        background-color: #4CAF50;
        display: none;
    }
    .experiment-management-container {
        display: flex;
        align-items: center;
        justify-content: space-between;
        width: 100%;
        margin-bottom: 10px;
    }
    .current-experiment-info {
        display: flex;
        align-items: center;
    }
    .experiment-history-controls .btn-sm {
        padding: 3px 6px;
        font-size: 12px;
    }
    "
    
    # Create the app title
    app_title <- tags$div(
        paste0("Microbiome Analysis Dashboard - v", packageVersion("miaDash")),
        tags$img(src = "assets/mia_logo.png", height = "40px", style = "margin-left: 10px"),
        style = "cursor: pointer; font-weight: 500",
        onclick = "window.location.href='https://miadash-microbiome.2.rahtiapp.fi/'; window.location.reload(true);"
    )
    
    # Create a function that wraps iSEE and injects our custom JS and CSS
    app <- iSEE(
        landingPage = .landing_page,
        appTitle = app_title
    )
    
    # Access the ui function from the app
    original_ui <- app$ui
    
    # Override the ui function to include our custom JS and CSS
    app$ui <- function(request) {
        # Get the original UI
        ui_content <- if (is.function(original_ui)) {
            original_ui(request)
        } else {
            original_ui
        }
        
        # Wrap with our custom elements
        tagList(
            # Add custom CSS
            singleton(tags$head(
                tags$style(experiment_css)
            )),
            # Add custom JavaScript
            singleton(tags$head(
                tags$script(HTML(experiment_js))
            )),
            # Original UI content
            ui_content
        )
    }
    
    return(app)
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
#' @importFrom htmltools tags tagList div hr icon
#' @importFrom shiny selectInput actionButton
.launch_isee <- function(FUN, initial, session, rObjects, input = NULL, initial_experiment = NULL) {
    # nocov start
    tse <- rObjects$tse
    
    # Determine experiment choice, with priority order:
    # 1. Explicit initial_experiment parameter
    # 2. Input$experiment_choice 
    # 3. Default to "main"
    exp_name <- "main"
    if (!is.null(initial_experiment) && initial_experiment != "") {
        exp_name <- initial_experiment
    } else if (!is.null(input) && !is.null(input$experiment_choice) && input$experiment_choice != "") {
        exp_name <- input$experiment_choice
    }
    
    # Get the current experiment based on selection
    current_exp <- if(exp_name != "main") {
        tryCatch({
            altExp(tse, exp_name)
        }, error = function(e) {
            # Handle case where the selected experiment doesn't exist
            .print_message(
                title = "Experiment Error:",
                "The selected experiment could not be found. Defaulting to main experiment."
            )
            exp_name <<- "main"  # Reset exp_name to main
            tse
        })
    } else {
        tse
    }
    
    # Get stored panel configuration if available
    if(exists("panel_configurations", rObjects) && 
       !is.null(rObjects$panel_configurations())) {
        stored_config <- .get_panel_config(
            rObjects$panel_configurations, exp_name, NULL)
        
        if(!is.null(stored_config)) {
            # Use stored panel configuration if available
            initial <- stored_config
        }
    }
    
    # Filter panels based on experiment type
    if(exp_name != "main") {
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
        if(exp_name != "main") {
            # Set experiment name for dimension reduction panels
            if(inherits(panel, "DimensionReducedPanel") || 
               inherits(panel, "ReducedDimensionPlot")) {
                # Use try-catch to handle any attribute setting errors
                tryCatch({
                    panel$ExperimentName <- exp_name
                }, error = function(e) {
                    # Silently continue if attribute can't be set
                })
            }
            
            # Handle visualization settings for alternative experiment panels
            if(inherits(panel, "AbundancePlot") || 
               inherits(panel, "ComplexHeatmapPlot")) {
                tryCatch({
                    panel$ShowFeatureNames <- TRUE
                    if(grepl("^agglomerated_", exp_name)) {
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
    initial <- .check_panel(tse, initial, "RowDataTable", rowData, exp_name)
    initial <- .check_panel(tse, initial, "ColumnDataTable", colData, exp_name)
    initial <- .check_panel(tse, initial, "RowTreePlot", rowLinks, exp_name)
    initial <- .check_panel(tse, initial, "AbundancePlot", taxonomyRanks, exp_name)
    initial <- .check_panel(tse, initial, "ReducedDimensionPlot", reducedDims, exp_name)
    initial <- .check_panel(tse, initial, "LoadingPlot", reducedDims, exp_name)
    initial <- .check_panel(tse, initial, "ColumnTreePlot", colLinks, exp_name)
  
    # Take a snapshot of initial state if state management is active
    if(exists("experiment_states", rObjects)) {
        .create_experiment_snapshot(
            rObjects$experiment_states,
            exp_name,
            initial
        )
    }
  
    # Create experiment selector JavaScript for real-time switching
    experiment_selector_js <- "
    window.iSEEApp = window.iSEEApp || {};
    
    // Initialize panel tracking
    window.iSEEApp.panels = {};
    
    // Register panels when they're created
    $(document).on('iSEE:panelCreated', function(event, panelId, panelType) {
        window.iSEEApp.panels[panelId] = {
            type: panelType,
            config: {}
        };
    });
    
    // Track panel configuration changes
    $(document).on('iSEE:panelSettingsChanged', function(event, panelId, settings) {
        if (window.iSEEApp.panels[panelId]) {
            window.iSEEApp.panels[panelId].config = settings;
            Shiny.setInputValue('panel_config_changed', {
                panel_id: panelId,
                config: settings
            });
        }
    });
    
    // Panel reconfiguration for experiment switching
    window.iSEEApp.reconfigurePanels = function(experimentName) {
        var expName = experimentName || $('#iSEE_INTERNAL_experiment_selector').val();
        
        // For each panel in the current view
        for (var panelId in window.iSEEApp.panels) {
            var panelType = window.iSEEApp.panels[panelId].type;
            
            // Request panel-specific reconfiguration from server
            Shiny.setInputValue('reconfigure_panel', {
                panel_id: panelId,
                panel_type: panelType,
                experiment: expName
            });
            
            // Update experiment reference attribute if panel has it
            $('.panel[data-panel-id=\"' + panelId + '\"]')
                .attr('data-experiment', expName);
        }
        
        // Show transition indicator
        $('.experiment-transition-indicator').fadeIn(200).delay(500).fadeOut(200);
    };
    
    // Handle experiment selector changes
    $(document).on('change', '#iSEE_INTERNAL_experiment_selector', function() {
        var selectedExp = $(this).val();
        Shiny.setInputValue('iSEE_switch_experiment', selectedExp);
    });
    
    // Add keyboard shortcuts
    $(document).keydown(function(e) {
        // Alt+E to focus experiment selector
        if (e.altKey && e.keyCode === 69) { // 'E' key
            e.preventDefault();
            $('#iSEE_INTERNAL_experiment_selector').focus();
        }
        
        // Alt+Z for undo experiment switch
        if (e.altKey && e.keyCode === 90) { // 'Z' key
            e.preventDefault();
            $('#undo_experiment_switch').click();
        }
        
        // Alt+Y for redo experiment switch
        if (e.altKey && e.keyCode === 89) { // 'Y' key
            e.preventDefault();
            $('#redo_experiment_switch').click();
        }
    });
    "
  
    # Launch iSEE with the current experiment and validated panels
    FUN(
        SE = tse,
        INIT = initial
    )
    

  
    shinyjs::runjs(paste0("
    window.iSEEApp = window.iSEEApp || {};
    
    // Initialize panel tracking
    window.iSEEApp.panels = {};
    
    // Register panels when they're created
    $(document).on('iSEE:panelCreated', function(event, panelId, panelType) {
        window.iSEEApp.panels[panelId] = {
            type: panelType,
            config: {}
        };
    });
    
    // Track panel configuration changes
    $(document).on('iSEE:panelSettingsChanged', function(event, panelId, settings) {
        if (window.iSEEApp.panels[panelId]) {
            window.iSEEApp.panels[panelId].config = settings;
            Shiny.setInputValue('panel_config_changed', {
                panel_id: panelId,
                config: settings
            });
        }
    });
    
    // Panel reconfiguration for experiment switching
    window.iSEEApp.reconfigurePanels = function(experimentName) {
        var expName = experimentName || $('#iSEE_INTERNAL_experiment_selector').val();
        
        // For each panel in the current view
        for (var panelId in window.iSEEApp.panels) {
            var panelType = window.iSEEApp.panels[panelId].type;
            
            // Request panel-specific reconfiguration from server
            Shiny.setInputValue('reconfigure_panel', {
                panel_id: panelId,
                panel_type: panelType,
                experiment: expName
            });
            
            // Update experiment reference attribute if panel has it
            $('.panel[data-panel-id=\"' + panelId + '\"]')
                .attr('data-experiment', expName);
        }
        
        // Show transition indicator
        $('.experiment-transition-indicator').fadeIn(200).delay(500).fadeOut(200);
    };
    
    // Handle experiment selector changes
    $(document).on('change', '#iSEE_INTERNAL_experiment_selector', function() {
        var selectedExp = $(this).val();
        Shiny.setInputValue('iSEE_switch_experiment', selectedExp);
    });
    
    // Add keyboard shortcuts
    $(document).keydown(function(e) {
        // Alt+E to focus experiment selector
        if (e.altKey && e.keyCode === 69) { // 'E' key
            e.preventDefault();
            $('#iSEE_INTERNAL_experiment_selector').focus();
        }
        
        // Alt+Z for undo experiment switch
        if (e.altKey && e.keyCode === 90) { // 'Z' key
            e.preventDefault();
            $('#undo_experiment_switch').click();
        }
        
        // Alt+Y for redo experiment switch
        if (e.altKey && e.keyCode === 89) { // 'Y' key
            e.preventDefault();
            $('#redo_experiment_switch').click();
        }
    });
    
    // Add experiment selector UI to the iSEE interface
    // This needs to be done after the iSEE app is loaded
    setTimeout(function() {
        // Create experiment choices
        var all_experiments = {'Main': 'main'};
        ", ifelse(length(altExpNames(tse)) > 0, 
           paste0("var alt_exps = ['", paste(altExpNames(tse), collapse = \"','\"), "'];",
                 "alt_exps.forEach(function(exp) { all_experiments['Alt: ' + exp] = exp; });"), 
           ""), "
        
        // Find the first panel's header
        var firstPanelHeader = $('.panel-heading').first();
        if (firstPanelHeader.length) {
            // Create the experiment selector UI
            var experimentUI = $('<div class=\"experiment-management-container\" style=\"display: flex; align-items: center; justify-content: space-between; width: 100%; margin-bottom: 10px;\"></div>');
            
            // Left: current experiment info
            experimentUI.append('<div class=\"current-experiment-info\"><span class=\"experiment-label\" style=\"margin-right: 5px; font-weight: bold;\">Experiment:</span></div>');
            
            // Center: experiment selector
            var selectorWrapper = $('<div class=\"experiment-selector-wrapper\" style=\"flex-grow: 1; max-width: 300px; margin: 0 10px;\"></div>');
            var selector = $('<select id=\"iSEE_INTERNAL_experiment_selector\" class=\"form-control\" style=\"width: 100%;\"></select>');
            
            // Add options to selector
            Object.keys(all_experiments).forEach(function(label) {
                var value = all_experiments[label];
                var option = $('<option></option>').attr('value', value).text(label);
                if (value === '", exp_name, "') {
                    option.attr('selected', 'selected');
                }
                selector.append(option);
            });
            
            selectorWrapper.append(selector);
            selectorWrapper.append('<div class=\"experiment-transition-indicator\" style=\"position: absolute; top: 0; left: 0; right: 0; height: 3px; background-color: #4CAF50; display: none;\"></div>');
            experimentUI.append(selectorWrapper);
            
            // Right: history controls
            var historyControls = $('<div class=\"experiment-history-controls\" style=\"display: flex;\"></div>');
            historyControls.append('<button id=\"undo_experiment_switch\" class=\"btn btn-sm\" title=\"Undo experiment switch (Alt+Z)\" style=\"margin-right: 5px;\"><i class=\"fa fa-undo\"></i></button>');
            historyControls.append('<button id=\"redo_experiment_switch\" class=\"btn btn-sm\" title=\"Redo experiment switch (Alt+Y)\"><i class=\"fa fa-redo\"></i></button>');
            experimentUI.append(historyControls);
            
            // Create wrapper and insert before the panel title
            var wrapper = $('<div class=\"experiment-header-wrapper\"></div>');
            wrapper.append(experimentUI);
            wrapper.append('<hr style=\"margin: 10px 0;\">');
            
            // Insert at the beginning of the panel heading
            firstPanelHeader.prepend(wrapper);
            
            // Initialize the selector with Selectize
            $('#iSEE_INTERNAL_experiment_selector').selectize({
                dropdownParent: 'body'
            });
            
            // Set up click handlers for history buttons
            $('#undo_experiment_switch').on('click', function() {
                Shiny.setInputValue('undo_experiment_switch', Math.random());
            });
            $('#redo_experiment_switch').on('click', function() {
                Shiny.setInputValue('redo_experiment_switch', Math.random());
            });
        }
    }, 1000); // Wait 1 second for iSEE to fully initialize
    "))
    
    # Enable iSEE interface buttons
    enable("iSEE_INTERNAL_organize_panels")
    enable("iSEE_INTERNAL_link_graph")
    enable("iSEE_INTERNAL_export_content")
    enable("iSEE_INTERNAL_tracked_code")
    enable("iSEE_INTERNAL_panel_settings")
    enable("iSEE_INTERNAL_open_vignette")
    enable("iSEE_INTERNAL_session_info")
    enable("iSEE_INTERNAL_citation_info")

    .handle_isee_experiment_switch(session, input, output, tse)
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
# Replace the current .get_experiment function with:
.get_experiment <- function(tse, experiment_name) {
    if(is.null(experiment_name) || experiment_name == "main") {
        return(list(
            experiment = tse,
            name = "main",
            is_main = TRUE
        ))
    }
    
    result <- tryCatch({
        # Try to get alternative experiment
        alt_exp <- altExp(tse, experiment_name)
        list(
            experiment = alt_exp,
            name = experiment_name,
            is_main = FALSE
        )
    }, error = function(e) {
        # Return main experiment if alternative not found
        .print_message(
            title = "Experiment Not Found:",
            paste("Could not find experiment:", experiment_name),
            "Using the main experiment instead."
        )
        list(
            experiment = tse,
            name = "main",
            is_main = TRUE
        )
    })
    
    return(result)
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


#' @importFrom shinyjs runjs 
#' @importFrom SingleCellExperiment altExp
.handle_isee_experiment_switch <- function(session, input, output, se) {
    # Monitor for experiment switch requests
    observeEvent(input$iSEE_INTERNAL_switch_experiment, {
        req(input$iSEE_INTERNAL_switch_experiment)
        exp_name <- input$iSEE_INTERNAL_switch_experiment
        
        # Get the appropriate experiment
        if (exp_name == "main") {
            current_exp <- se
        } else {
            tryCatch({
                current_exp <- altExp(se, exp_name)
            }, error = function(e) {
                showNotification(
                    paste("Error switching to experiment:", exp_name), 
                    type = "error"
                )
                return(NULL)
            })
        }
        
        if (!is.null(current_exp)) {
            # Update all panels that support alternative experiments
            panel_ids <- grep("^\\.panel\\d+$", names(input), value = TRUE)
            for (panel_id in panel_ids) {
                panel_type <- input[[paste0(panel_id, "Type")]]
                
                # Configure experiment-aware panels
                if (panel_type %in% c("ReducedDimensionPlot", "AbundancePlot", 
                                       "ComplexHeatmapPlot", "RowDataTable")) {
                    panel_name <- gsub("^\\.panel(\\d+)$", "\\1", panel_id)
                    update_script <- sprintf(
                        "if(window.ShinySingleCellApp && 
                         window.ShinySingleCellApp.panels['%s']) {
                            window.ShinySingleCellApp.panels['%s'].requestActiveExperiment('%s');
                         }", 
                        panel_name, panel_name, exp_name
                    )
                    runjs(update_script)
                }
            }
            
            # Show confirmation
            showNotification(
                paste("Switched to experiment:", exp_name),
                type = "message"
            )
        }
    })
}
