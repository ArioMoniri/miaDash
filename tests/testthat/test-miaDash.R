test_that("outputs", {
  expect_no_error(miaDash())
})

test_that("launch_isee handles alternative experiments", {
  # Create a mock TreeSummarizedExperiment with an alternative experiment
  data("Tengeler2020", package = "mia")
  tse <- Tengeler2020
  alt_exp <- SummarizedExperiment(
    assays = list(counts = matrix(1:12, nrow = 3, ncol = 4)),
    rowData = DataFrame(feature = paste0("feature", 1:3)),
    colData = DataFrame(sample = paste0("sample", 1:4))
  )
  altExp(tse, "test_alt") <- alt_exp
  
  # Create mock objects needed for testing
  rObjects <- new.env()
  rObjects$tse <- tse
  input <- new.env()
  input$experiment_choice <- "main"
  input$panels <- c("RowDataTable", "AbundancePlot")
  session <- new.env()
  
  # Test main experiment launch
  expect_no_error(
    .launch_isee(
      FUN = function(SE, INIT) NULL,
      initial = input$panels,
      session = session,
      rObjects = rObjects
    )
  )
  
  # Test alternative experiment launch
  input$experiment_choice <- "test_alt"
  expect_no_error(
    .launch_isee(
      FUN = function(SE, INIT) NULL,
      initial = input$panels,
      session = session,
      rObjects = rObjects
    )
  )
})

test_that("miaDash handles missing alternative experiments gracefully", {
  expect_no_error({
    data("Tengeler2020", package = "mia")
    tse <- Tengeler2020
    rObjects <- new.env()
    rObjects$tse <- tse
    input <- new.env()
    input$experiment_choice <- "nonexistent_alt"
    session <- new.env()
    
    .launch_isee(
      FUN = function(SE, INIT) NULL,
      initial = c("RowDataTable"),
      session = session,
      rObjects = rObjects
    )
  })
})
