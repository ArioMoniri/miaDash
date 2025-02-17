test_that("outputs", {
    output <- new.env()
    rObjects <- new.env()
  
    overview_out <- .render_overview(output, rObjects)
    download_out <- .render_download(output, rObjects)
  
    expect_null(overview_out)
    expect_null(download_out)
    expect_named(output, c("object", "download"))
})

test_that("outputs handle alternative experiments", {
    output <- new.env()
    rObjects <- new.env()
    
    # Create a mock TreeSummarizedExperiment with an alternative experiment
    data("Tengeler2020", package = "mia")
    tse <- Tengeler2020
    alt_exp <- SummarizedExperiment(
        assays = list(counts = matrix(1:12, nrow = 3, ncol = 4))
    )
    altExp(tse, "test_alt") <- alt_exp
    rObjects$tse <- tse
    
    # Test that overview shows alternative experiments
    overview_out <- .render_overview(output, rObjects)
    expect_null(overview_out)
    
    # Test that download includes alternative experiments
    download_out <- .render_download(output, rObjects)
    expect_null(download_out)
    expect_named(output, c("object", "download"))
})
