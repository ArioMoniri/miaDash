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
            onclick = "window.location='https://miadash-microbiome.2.rahtiapp.fi/'") 
            
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
#' @importFrom SingleCellExperiment reducedDims
.launch_isee <- function(FUN, initial, session, rObjects) {
    # nocov start
    tse <- rObjects$tse
    current_exp <- .get_experiment(tse, input$experiment_choice)
    
    # Filter panels based on experiment type
    if(input$experiment_choice != "main") {
        initial <- .filter_panels_by_experiment(initial, altexp_panels)
    }
  
    initial <- lapply(initial, function(x) {
        panel <- eval(parse(text = paste0(x, "()")))
        if(inherits(panel, "DimensionReducedPanel")) {
            panel$ExperimentName <- input$experiment_choice
        }
        return(panel)
    })
    
    # Check panels for current experiment
    initial <- .check_panel(tse, initial, "RowDataTable", rowData, 
                          input$experiment_choice)
    initial <- .check_panel(tse, initial, "ColumnDataTable", colData, 
                          input$experiment_choice)
    initial <- .check_panel(tse, initial, "RowTreePlot", rowLinks, 
                          input$experiment_choice)
    initial <- .check_panel(tse, initial, "AbundancePlot", taxonomyRanks, 
                          input$experiment_choice)
    initial <- .check_panel(tse, initial, "ReducedDimensionPlot", reducedDims, 
                          input$experiment_choice)
    initial <- .check_panel(tse, initial, "LoadingPlot", reducedDims, 
                          input$experiment_choice)
    initial <- .check_panel(tse, initial, "ColumnTreePlot", colLinks, 
                          input$experiment_choice)
  
    FUN(SE = current_exp, INIT = initial)
  
    enable("iSEE_INTERNAL_organize_panels")  # organize panels
    enable("iSEE_INTERNAL_link_graph")       # link graph
    enable("iSEE_INTERNAL_export_content")   # export content
    enable("iSEE_INTERNAL_tracked_code")     # tracked code
    enable("iSEE_INTERNAL_panel_settings")   # panel settings
    enable("iSEE_INTERNAL_open_vignette")    # open vignette
    enable("iSEE_INTERNAL_session_info")     # session info
    enable("iSEE_INTERNAL_citation_info")    # citation info 
  
    invisible(NULL)
    # nocov end
}
