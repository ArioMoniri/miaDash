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
#' @importFrom SingleCellExperiment altExpNames
#' @importFrom methods is
.detect_all_experiments <- function(se) {
    result <- list(
        main = "Main Experiment",
        alt_exps = list()
    )
    
    if (is(se, "SingleCellExperiment") && length(altExpNames(se)) > 0) {
        alt_exp_names <- altExpNames(se)
        for (exp_name in alt_exp_names) {
            alt_exp <- altExp(se, exp_name)
            result$alt_exps[[exp_name]] <- list(
                name = exp_name,
                type = class(alt_exp)[1],
                dims = dim(alt_exp),
                assays = assayNames(alt_exp),
                features = nrow(alt_exp)
            )
        }
    }
    
    if (is(se, "MultiAssayExperiment")) {
        experiment_list <- experiments(se)
        for (exp_name in names(experiment_list)) {
            exp_obj <- experiment_list[[exp_name]]
            result$alt_exps[[exp_name]] <- list(
                name = exp_name,
                type = class(exp_obj)[1],
                dims = dim(exp_obj),
                assays = if(is(exp_obj, "SummarizedExperiment")) assayNames(exp_obj) else NULL,
                features = nrow(exp_obj)
            )
        }
    }
    
    return(result)
}

#' @rdname utils
#' @importFrom SummarizedExperiment assayNames
#' @importFrom methods is
.get_experiment_metadata <- function(se, exp_name = "main") {
    exp_obj <- .get_experiment(se, exp_name)
    
    metadata <- list(
        name = exp_name,
        type = class(exp_obj)[1],
        dimensions = dim(exp_obj),
        n_features = nrow(exp_obj),
        n_samples = ncol(exp_obj),
        assays = assayNames(exp_obj),
        has_rowTree = !is.null(rowTree(exp_obj)),
        has_colTree = !is.null(colTree(exp_obj)),
        has_reducedDims = if(is(exp_obj, "SingleCellExperiment")) 
                            length(reducedDimNames(exp_obj)) > 0 
                          else FALSE
    )
    
    return(metadata)
}

#' @rdname utils
#' @importFrom methods is
.check_experiment_panel_compatibility <- function(se, exp_name, panel_class) {
    # Define panel compatibility rules
    compatibility_rules <- list(
        "RowTreePlot" = function(exp) !is.null(rowTree(exp)),
        "ColumnTreePlot" = function(exp) !is.null(colTree(exp)),
        "ReducedDimensionPlot" = function(exp) {
            is(exp, "SingleCellExperiment") && length(reducedDimNames(exp)) > 0
        },
        "LoadingPlot" = function(exp) {
            is(exp, "SingleCellExperiment") && length(reducedDimNames(exp)) > 0
        },
        "RDAPlot" = function(exp) {
            is(exp, "SingleCellExperiment") && 
            any(grepl("^RDA", reducedDimNames(exp)))
        },
        "AbundancePlot" = function(exp) {
            is(exp, "TreeSummarizedExperiment") && length(assayNames(exp)) > 0
        },
        "AbundanceDensityPlot" = function(exp) {
            is(exp, "TreeSummarizedExperiment") && length(assayNames(exp)) > 0
        },
        "ComplexHeatmapPlot" = function(exp) length(assayNames(exp)) > 0,
        "RowDataTable" = function(exp) ncol(rowData(exp)) > 0,
        "ColumnDataTable" = function(exp) ncol(colData(exp)) > 0
    )
    
    exp_obj <- .get_experiment(se, exp_name)
    
    if (!panel_class %in% names(compatibility_rules)) {
        return(TRUE)  # If no explicit rule, assume compatible
    }
    
    return(compatibility_rules[[panel_class]](exp_obj))
}

#' @rdname utils
.get_compatible_panels_for_experiment <- function(se, exp_name) {
    all_panels <- c(default_panels, other_panels)
    compatible_panels <- c()
    
    for (panel in all_panels) {
        if (.check_experiment_panel_compatibility(se, exp_name, panel)) {
            compatible_panels <- c(compatible_panels, panel)
        }
    }
    
    return(compatible_panels)
}

#' @rdname utils
.can_apply_operation_to_experiment <- function(operation, exp_name) {
    if (exp_name == "main") {
        return(TRUE)
    }
    
    return(operation %in% altexp_compatible_functions)
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
