#' Utilities
#' 
#' Helper functions and constants to support the app functionality.
#'
#' @name utils
#' @keywords internal

#' @rdname utils
.import_datasets <- function(selection) {
  
    mia_datasets <- data(package = "mia")
    mia_datasets <- mia_datasets$results[selection, "Item"]
    data(list = mia_datasets, package = "mia")
    
    return(mia_datasets)
}

#' @rdname utils
.update_tse <- function(tse, fun, fun_args) {

    tse <- tryCatch({withCallingHandlers({
        
        do.call(fun, fun_args)
      
        # nocov start
        }, message = function(m) {
        
            showNotification(conditionMessage(m))
            invokeRestart("muffleMessage")
        
        })}, error = function(e) {
      
            .print_message(e, title = "Unexpected error:")
            return(tse)
          
        })
        # nocov end
  
    return(tse)
}

#' @rdname utils
.print_message <- function(..., title = "Invalid input:") {

    # nocov start
    showModal(modalDialog(
        title = title, ...,
        easyClose = TRUE, footer = NULL
    ))
    # nocov end
}

#' @rdname utils
.set_optarg <- function(item, loader = NULL, alternative = NULL, ...){
  
    if( !(is.null(item) || is.null(loader)) ){
        out <- loader(item, ...)
    } else {
        out <- alternative
    }
  
    return(out)
}

#' @rdname utils
#' @importFrom SummarizedExperiment colData
.check_formula <- function(form, se){
  
    form <- gsub("data ~\\s*", "", form)
    vars <- unlist(strsplit(form, "\\s*[\\+|\\*]\\s*"))
  
    cond <- all(vars %in% names(colData(se)))
    return(cond)
}

#' @rdname utils
#' @importFrom S4Vectors isEmpty
#' @importFrom methods is
#' @importFrom SingleCellExperiment altExp
.check_panel <- function(se, panel_list, panel_class, panel_fun, exp_name = "main") {
    # Get the appropriate experiment
    current_exp <- .get_experiment(se, exp_name)
    
    no_keep <- unlist(lapply(panel_list, function(x) is(x, panel_class)))
  
    if( any(no_keep) && (is.null(panel_fun(current_exp)) || isEmpty(panel_fun(current_exp))) ){
        panel_list <- panel_list[!no_keep]
        warning("no valid ", as.character(substitute(panel_fun)),
            " fields for ", panel_class, call. = FALSE)
    }
  
    return(panel_list)
}


#' @rdname utils
#' @importFrom methods is
#' @importFrom SummarizedExperiment colData rowData assays
.validate_merged_file <- function(tse) {
    # Check class
    if (!is(tse, "TreeSummarizedExperiment")) {
        stop("File must contain a TreeSummarizedExperiment object")
    }
    
    # Check for required components
    if (nrow(tse) == 0 || ncol(tse) == 0) {
        stop("TreeSummarizedExperiment object must contain data")
    }
    
    if (length(assays(tse)) == 0) {
        stop("TreeSummarizedExperiment object must contain at least one assay")
    }
    
    # Check for sample consistency
    sample_names <- colnames(tse)
    if (is.null(sample_names) || any(duplicated(sample_names))) {
        stop("Sample names must be unique and non-null")
    }
    
    # Check alternative experiments if present
    if (length(altExpNames(tse)) > 0) {
        for (alt_name in altExpNames(tse)) {
            alt_exp <- altExp(tse, alt_name)
            if (!identical(colnames(tse), colnames(alt_exp))) {
                stop(sprintf("Alternative experiment '%s' must have the same samples as main experiment", alt_name))
            }
        }
    }
    
    return(TRUE)
}

                             
#' @rdname utils
.validate_altexp <- function(tse, altexp) {
    # Check if samples match
    if(!identical(colnames(tse), colnames(altexp))) {
        stop("Alternative experiment must have same samples as main experiment")
    }
    return(TRUE)
}

#' @rdname utils
.merge_experiments <- function(tse, altexp, name) {
    if(.validate_altexp(tse, altexp)) {
        altExp(tse, name) <- altexp
    }
    return(tse)
}                             

#' @rdname utils
.create_agglomerated_experiments <- function(tse, levels) {
    for(level in levels) {
        alt_exp <- agglomerateByRank(tse, rank = level)
        altExp(tse, paste0("agglomerated_", tolower(level))) <- alt_exp
    }
    return(tse)
}

#' @rdname utils
.process_merged_file <- function(file_path) {
    tse <- readRDS(file_path)
    if(!is(tse, "TreeSummarizedExperiment")) {
        stop("File must contain a TreeSummarizedExperiment object")
    }
    return(tse)
}
                             

#' @rdname utils
#' @importFrom SingleCellExperiment altExp
.get_experiment <- function(se, exp_name) {
    if(exp_name == "main" || is.null(exp_name)) {
        return(se)
    } else {
        return(altExp(se, exp_name))
    }
}

#' @rdname utils
default_panels <- c("RowDataTable", "ColumnDataTable", "RowTreePlot",
    "AbundancePlot", "AbundanceDensityPlot", "ReducedDimensionPlot",
    "ComplexHeatmapPlot")

#' @rdname utils
other_panels <- c("LoadingPlot", "ColumnTreePlot", "RDAPlot", "ColumnDataPlot",
    "RowDataPlot")

#' @rdname utils
altexp_panels <- c("AbundancePlot", "ComplexHeatmapPlot", "RowDataTable")

#' @rdname utils
altexp_compatible_functions <- c(
    "agglomerateByRank",
    "transformAssay", 
    "addAlpha",
    "runPCA",
    "runMDS",
    "runNMDS",
    "runRDA"
)

#' @rdname utils
.filter_panels_by_experiment <- function(panels, allowed_panels) {
    panels[panels %in% allowed_panels]
}
