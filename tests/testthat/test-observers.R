test_that("observers", {
  input <- new.env()
  rObjects <- new.env()
  FUN <- function(SE, INITIAL) invisible(NULL)
  
  import_out <- .create_import_observers(input, rObjects)
  expect_null(import_out)
  
  manipulate_out <- .create_manipulate_observers(input, rObjects)
  expect_null(manipulate_out)
  
  estimate_out <- .create_estimate_observers(input, rObjects)
  expect_null(estimate_out)
  
  update_out <- .update_observers(input, session = NULL, rObjects)
  expect_null(update_out)
  
  launch_out <- .create_launch_observers(FUN, input, session = NULL, rObjects)
  expect_null(launch_out)
})

test_that("alternative experiment observers", {
  input <- new.env()
  rObjects <- new.env()
  session <- new.env()
  
  # Mock the TreeSummarizedExperiment object
  data("Tengeler2020", package = "mia")
  rObjects$tse <- Tengeler2020
  
  # Test altexp observer creation
  altexp_out <- .create_altexp_observers(input, rObjects)
  expect_null(altexp_out)
  
  # Test altexp import
  input$alt_assay <- list(
    datapath = system.file("extdata", "counts.csv", package = "miaDash"),
    name = "counts.csv"
  )
  input$alt_name <- "test_altexp"
  input$add_altexp <- 1
  
  # Test update observers with alternative experiments
  input$experiment_choice <- "main"
  session$output <- list()
  session$updateSelectInput <- function(...) NULL
  
  expect_no_error(
    .update_observers(input, session, rObjects)
  )
})

test_that("observers handle missing inputs gracefully", {
  input <- new.env()
  rObjects <- new.env()
  
  # Test with missing alternative experiment inputs
  input$add_altexp <- 1
  expect_no_error(
    .create_altexp_observers(input, rObjects)
  )
  
  # Test with invalid alternative experiment name
  input$alt_assay <- list(
    datapath = system.file("extdata", "counts.csv", package = "miaDash"),
    name = "counts.csv"
  )
  input$alt_name <- ""
  expect_no_error(
    .create_altexp_observers(input, rObjects)
  )
})

test_that("observers update UI elements correctly", {
  input <- new.env()
  rObjects <- new.env()
  session <- new.env()
  session$output <- list()
  session$updateSelectInput <- function(...) NULL
  
  # Mock TreeSummarizedExperiment with alternative experiment
  data("Tengeler2020", package = "mia")
  tse <- Tengeler2020
  alt_exp <- SummarizedExperiment(
    assays = list(counts = matrix(1:12, nrow = 3, ncol = 4))
  )
  altExp(tse, "test_alt") <- alt_exp
  rObjects$tse <- tse
  
  # Test UI updates
  expect_no_error(
    .update_observers(input, session, rObjects)
  )
})
