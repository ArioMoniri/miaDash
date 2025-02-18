test_that("utils", {
    data("Tengeler2020", package = "mia")
    tse <- Tengeler2020

    idx <- c(1, 3)
    expect_equal(.import_datasets(idx),
        data(package = "mia")$results[idx, "Item"])
    
    expect_no_error(
        tse <- .update_tse(tse, transformAssay,
            list(x = tse, assay.type = "counts", method = "relabundance"))
    )
    
    item <- NULL
    expect_identical(.set_optarg(item, alternative = "alternative"),
       "alternative")
    
    item <- "100"
    expect_identical(.set_optarg(item, loader = as.numeric), 100)
  
    expect_true(.check_formula("data ~ patient_status + cohort", tse))
    expect_false(.check_formula("data ~ wrong_var + sample_name", tse))
    
    panels <- c(RowDataTable(), ReducedDimensionPlot())
    
    expect_warning(
        expect_length(
            .check_panel(tse, panels, "ReducedDimensionPlot", reducedDims), 1
        )
    )
})

test_that("alternative experiment utils", {
    # Create test data
    data("Tengeler2020", package = "mia")
    tse <- Tengeler2020
    
    # Create and add alternative experiment
    alt_exp <- SummarizedExperiment(
        assays = list(counts = matrix(1:12, nrow = 3, ncol = 4)),
        rowData = DataFrame(feature = paste0("feature", 1:3)),
        colData = DataFrame(sample = paste0("sample", 1:4))
    )
    altExp(tse, "test_alt") <- alt_exp

    # Test .get_experiment function
    expect_identical(.get_experiment(tse, "main"), tse)
    expect_identical(.get_experiment(tse, "test_alt"), alt_exp)
    expect_identical(.get_experiment(tse, NULL), tse)
    
    # Test panel filtering
    test_panels <- c("AbundancePlot", "RowTreePlot", "ComplexHeatmapPlot")
    filtered_panels <- .filter_panels_by_experiment(test_panels, altexp_panels)
    expect_true(all(filtered_panels %in% altexp_panels))
})

test_that("panel checking with alternative experiments", {
    data("Tengeler2020", package = "mia")
    tse <- Tengeler2020
    
    # Add alternative experiment
    alt_exp <- SummarizedExperiment(
        assays = list(counts = matrix(1:12, nrow = 3, ncol = 4))
    )
    altExp(tse, "test_alt") <- alt_exp

    panels <- c(RowDataTable(), AbundancePlot())
    
    # Test panel checking for main experiment
    main_panels <- .check_panel(tse, panels, "RowDataTable", rowData, "main")
    expect_true(length(main_panels) > 0)
    
    # Test panel checking for alternative experiment
    alt_panels <- .check_panel(tse, panels, "RowDataTable", rowData, "test_alt")
    expect_true(length(alt_panels) > 0)
})

test_that("compatible functions for alternative experiments", {
    # Test that all altexp_compatible_functions exist
    for(func in altexp_compatible_functions) {
        expect_true(exists(func, mode="function") || 
                   exists(func, mode="character"))
    }
    
    # Test that compatible functions can be applied to alternative experiments
    data("Tengeler2020", package = "mia")
    tse <- Tengeler2020
    alt_exp <- SummarizedExperiment(
        assays = list(counts = matrix(1:12, nrow = 3, ncol = 4))
    )
    altExp(tse, "test_alt") <- alt_exp
    
    # Test transformAssay on alternative experiment
    expect_no_error(
        transformAssay(.get_experiment(tse, "test_alt"), 
                      method = "relabundance", 
                      assay.type = "counts")
    )
})

test_that("merged file validation works", {
    # Create valid test data
    data("Tengeler2020", package = "mia")
    tse <- Tengeler2020
    
    # Should pass validation
    expect_true(.validate_merged_file(tse))
    
    # Test invalid cases
    # Empty TSE
    empty_tse <- TreeSummarizedExperiment()
    expect_error(.validate_merged_file(empty_tse))
    
    # Invalid alternative experiment
    invalid_alt <- SummarizedExperiment(
        assays = list(counts = matrix(1:12, nrow = 3, ncol = 2))
    )
    altExp(tse, "invalid") <- invalid_alt
    expect_error(.validate_merged_file(tse))
})
