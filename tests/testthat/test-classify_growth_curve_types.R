make_classify_input <- function(curve_type = "growing", 
                                n_timepoints = 10,
                                empty = FALSE) {
  
  hours <- seq(0, n_timepoints - 1)
  wells <- c("A01", "A02", "A03")
  
  od_values <- switch(curve_type,
                      "growing"      = seq(0.05, 0.5,  length.out = n_timepoints),
                      "flat_low"     = rep(0.05,        n_timepoints),
                      "OD_drop"      = c(seq(0.3, 0.5, length.out = n_timepoints %/% 2),
                                         seq(0.5, 0.1, length.out = n_timepoints - n_timepoints %/% 2)),
                      "slow_growing" = seq(0.05, 0.12, length.out = n_timepoints),
                      seq(0.05, 0.5, length.out = n_timepoints)  # default
  )
  
  base <- expand.grid(
    Hour       = hours,
    Well       = wells,
    Plate.Type = "PM-M1",
    cell.line  = "H460",
    stringsAsFactors = FALSE
  )
  base$diff_OD_590_740 <- rep(od_values, times = length(wells))
  
  df <- if (empty) base[0, ] else base
  
  list(
    data = df,
    meta = list(
      data_path     = "fake/path",
      sample_column = "Sample",
      variables     = "cell.line"
    )
  )
}

# ── Top-level structure checks ────────────────────────────────────────────────

test_that("classify_growth_curve_types stops when data is not a list", {
  expect_error(
    classify_growth_curve_types("not_a_list"),
    "must be a named list with elements 'data' and 'meta'"
  )
})

test_that("classify_growth_curve_types stops when 'data' element is missing", {
  expect_error(
    classify_growth_curve_types(list(meta = list(variables = "cell.line"))),
    "must be a named list with elements 'data' and 'meta'"
  )
})

test_that("classify_growth_curve_types stops when 'meta' element is missing", {
  expect_error(
    classify_growth_curve_types(list(data = data.frame(x = 1))),
    "must be a named list with elements 'data' and 'meta'"
  )
})

# ── meta structure checks ─────────────────────────────────────────────────────

test_that("classify_growth_curve_types stops when data$meta is not a list", {
  input      <- make_classify_input()
  input$meta <- "not_a_list"
  expect_error(
    classify_growth_curve_types(input),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})

test_that("classify_growth_curve_types stops when data$meta is missing 'variables'", {
  input      <- make_classify_input()
  input$meta <- list(data_path = "fake/path")
  expect_error(
    classify_growth_curve_types(input),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})

# ── data$data checks ──────────────────────────────────────────────────────────

test_that("classify_growth_curve_types stops when data$data is not a data frame", {
  input      <- make_classify_input()
  input$data <- "not_a_data_frame"
  expect_error(
    classify_growth_curve_types(input),
    "'data\\$data' must be a data.frame"
  )
})

test_that("classify_growth_curve_types stops when data$data is empty", {
  input <- make_classify_input(empty = TRUE)
  expect_error(
    classify_growth_curve_types(input),
    "'data\\$data' must contain at least one row"
  )
})

# ── variables checks ──────────────────────────────────────────────────────────

test_that("classify_growth_curve_types stops when variables is empty", {
  input                <- make_classify_input()
  input$meta$variables <- character(0)
  expect_error(
    classify_growth_curve_types(input),
    "'data\\$meta\\$variables' must be a non-empty character vector"
  )
})

test_that("classify_growth_curve_types stops when variables is not character", {
  input                <- make_classify_input()
  input$meta$variables <- 1:3
  expect_error(
    classify_growth_curve_types(input),
    "'data\\$meta\\$variables' must be a non-empty character vector"
  )
})

test_that("classify_growth_curve_types stops when variables is NULL", {
  input                <- make_classify_input()
  input$meta$variables <- NULL
  expect_error(
    classify_growth_curve_types(input),
    "must be a named list containing"
  )
})

# ── Missing column checks ─────────────────────────────────────────────────────

test_that("classify_growth_curve_types stops when variable column is missing", {
  input              <- make_classify_input()
  input$data$cell.line <- NULL
  expect_error(
    classify_growth_curve_types(input),
    "Missing required columns: cell.line"
  )
})

test_that("classify_growth_curve_types stops when Well is missing", {
  input            <- make_classify_input()
  input$data$Well  <- NULL
  expect_error(
    classify_growth_curve_types(input),
    "Missing required columns: Well"
  )
})

test_that("classify_growth_curve_types stops when Hour is missing", {
  input            <- make_classify_input()
  input$data$Hour  <- NULL
  expect_error(
    classify_growth_curve_types(input),
    "Missing required columns: Hour"
  )
})

test_that("classify_growth_curve_types stops when Plate.Type is missing", {
  input                 <- make_classify_input()
  input$data$Plate.Type <- NULL
  expect_error(
    classify_growth_curve_types(input),
    "Missing required columns: Plate.Type"
  )
})

test_that("classify_growth_curve_types stops when diff_OD_590_740 is missing", {
  input                      <- make_classify_input()
  input$data$diff_OD_590_740 <- NULL
  expect_error(
    classify_growth_curve_types(input),
    "Missing required columns: diff_OD_590_740"
  )
})

# ── Numeric parameter checks ──────────────────────────────────────────────────

test_that("classify_growth_curve_types stops when spar is not numeric", {
  input <- make_classify_input()
  expect_error(
    classify_growth_curve_types(input, spar = "high"),
    "'spar' must be a single numeric value between 0 and 1"
  )
})

test_that("classify_growth_curve_types stops when spar is outside 0-1", {
  input <- make_classify_input()
  expect_error(
    classify_growth_curve_types(input, spar = 1.5),
    "'spar' must be a single numeric value between 0 and 1"
  )
})

test_that("classify_growth_curve_types stops when spar is length > 1", {
  input <- make_classify_input()
  expect_error(
    classify_growth_curve_types(input, spar = c(0.5, 0.7)),
    "'spar' must be a single numeric value between 0 and 1"
  )
})

test_that("classify_growth_curve_types stops when drop_threshold is not numeric", {
  input <- make_classify_input()
  expect_error(
    classify_growth_curve_types(input, drop_threshold = "low"),
    "'drop_threshold' must be a single numeric value"
  )
})

test_that("classify_growth_curve_types stops when drop_threshold is length > 1", {
  input <- make_classify_input()
  expect_error(
    classify_growth_curve_types(input, drop_threshold = c(-0.01, -0.02)),
    "'drop_threshold' must be a single numeric value"
  )
})

test_that("classify_growth_curve_types stops when tol is not numeric", {
  input <- make_classify_input()
  expect_error(
    classify_growth_curve_types(input, tol = "zero"),
    "'tol' must be a single non-negative numeric value"
  )
})

test_that("classify_growth_curve_types stops when tol is negative", {
  input <- make_classify_input()
  expect_error(
    classify_growth_curve_types(input, tol = -0.01),
    "'tol' must be a single non-negative numeric value"
  )
})

test_that("classify_growth_curve_types stops when max_od_threshold is not numeric", {
  input <- make_classify_input()
  expect_error(
    classify_growth_curve_types(input, max_od_threshold = "low"),
    "'max_od_threshold' must be a single non-negative numeric value"
  )
})

test_that("classify_growth_curve_types stops when max_od_threshold is negative", {
  input <- make_classify_input()
  expect_error(
    classify_growth_curve_types(input, max_od_threshold = -0.1),
    "'max_od_threshold' must be a single non-negative numeric value"
  )
})

# ── Return structure ──────────────────────────────────────────────────────────

test_that("classify_growth_curve_types returns a list", {
  input  <- make_classify_input()
  result <- classify_growth_curve_types(input)
  expect_type(result, "list")
})

test_that("classify_growth_curve_types adds classification element to output", {
  input  <- make_classify_input()
  result <- classify_growth_curve_types(input)
  expect_true("classification" %in% names(result))
})

test_that("classify_growth_curve_types classification contains derivative_data", {
  input  <- make_classify_input()
  result <- classify_growth_curve_types(input)
  expect_true("derivative_data" %in% names(result$classification))
})

test_that("classify_growth_curve_types classification contains well_summary", {
  input  <- make_classify_input()
  result <- classify_growth_curve_types(input)
  expect_true("well_summary" %in% names(result$classification))
})

test_that("classify_growth_curve_types derivative_data is a data frame", {
  input  <- make_classify_input()
  result <- classify_growth_curve_types(input)
  expect_s3_class(result$classification$derivative_data, "data.frame")
})

test_that("classify_growth_curve_types well_summary is a data frame", {
  input  <- make_classify_input()
  result <- classify_growth_curve_types(input)
  expect_s3_class(result$classification$well_summary, "data.frame")
})

test_that("classify_growth_curve_types preserves original data and meta", {
  input  <- make_classify_input()
  result <- classify_growth_curve_types(input)
  expect_identical(result$data, input$data)
  expect_identical(result$meta, input$meta)
})

# ── well_summary content ──────────────────────────────────────────────────────

test_that("classify_growth_curve_types well_summary contains curve_type column", {
  input  <- make_classify_input()
  result <- classify_growth_curve_types(input)
  expect_true("curve_type" %in% names(result$classification$well_summary))
})

test_that("classify_growth_curve_types well_summary curve_type values are valid", {
  input       <- make_classify_input()
  result      <- classify_growth_curve_types(input)
  valid_types <- c("growing", "flat_low", "OD_drop", "slow_growing", "uncertain")
  expect_true(all(
    result$classification$well_summary$curve_type %in% valid_types
  ))
})

test_that("classify_growth_curve_types well_summary has one row per well", {
  input   <- make_classify_input()
  result  <- classify_growth_curve_types(input)
  n_wells <- length(unique(input$data$Well))
  expect_equal(nrow(result$classification$well_summary), n_wells)
})

test_that("classify_growth_curve_types well_summary contains qc_flag column", {
  input  <- make_classify_input()
  result <- classify_growth_curve_types(input)
  expect_true("qc_flag" %in% names(result$classification$well_summary))
})

# ── Classification correctness ────────────────────────────────────────────────

test_that("classify_growth_curve_types classifies growing curve correctly", {
  input  <- make_classify_input(curve_type = "growing", n_timepoints = 20)
  result <- classify_growth_curve_types(input)
  types  <- unique(result$classification$well_summary$curve_type)
  expect_true(any(types %in% c("growing", "slow_growing")))
})

test_that("classify_growth_curve_types classifies flat_low curve correctly", {
  input  <- make_classify_input(curve_type = "flat_low", n_timepoints = 20)
  result <- classify_growth_curve_types(input)
  types  <- unique(result$classification$well_summary$curve_type)
  expect_true(any(types %in% c("flat_low", "uncertain")))
})

test_that("classify_growth_curve_types classifies OD_drop curve correctly", {
  input  <- make_classify_input(curve_type = "OD_drop", n_timepoints = 20)
  result <- classify_growth_curve_types(input)
  types  <- unique(result$classification$well_summary$curve_type)
  expect_true(any(types %in% c("OD_drop", "uncertain")))
})

test_that("classify_growth_curve_types assigns uncertain when fewer than 4 timepoints", {
  input  <- make_classify_input(n_timepoints = 3)
  result <- classify_growth_curve_types(input)
  expect_true(all(
    result$classification$well_summary$curve_type == "uncertain"
  ))
})

# ── derivative_data content ───────────────────────────────────────────────────

test_that("classify_growth_curve_types derivative_data contains first_derivative", {
  input  <- make_classify_input()
  result <- classify_growth_curve_types(input)
  expect_true("first_derivative" %in% names(result$classification$derivative_data))
})

test_that("classify_growth_curve_types derivative_data contains fitted values", {
  input  <- make_classify_input()
  result <- classify_growth_curve_types(input)
  expect_true("spline" %in% names(result$classification$derivative_data))
})

# ── Single variable edge case ─────────────────────────────────────────────────

test_that("classify_growth_curve_types works with multiple variables", {
  input                <- make_classify_input()
  input$data$treatment <- "drug"
  input$meta$variables <- c("cell.line", "treatment")
  expect_type(classify_growth_curve_types(input), "list")
})

