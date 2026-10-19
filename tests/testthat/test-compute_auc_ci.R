make_auc_input <- function(n_timepoints = 10,
                           empty = FALSE,
                           include_chemical = TRUE) {
  hours <- seq(0, n_timepoints - 1)
  wells <- c("A01", "A02", "A03")

  base <- expand.grid(
    Hour = hours,
    Well = wells,
    Plate.Type = "PM-M1",
    cell.line = "H460",
    PlateId = "plate_001",
    stringsAsFactors = FALSE
  )
  base$diff_OD_590_740 <- seq(0.05, 0.5, length.out = nrow(base))

  if (include_chemical) {
    base$Chemical <- "DrugX"
    base$control <- NA_character_
  }

  df <- if (empty) base[0, ] else base

  well_summary <- data.frame(
    Well = wells,
    Plate.Type = "PM-M1",
    cell.line = "H460",
    curve_type = "growing",
    stringsAsFactors = FALSE
  )

  list(
    data = df,
    meta = list(
      data_path     = "fake/path",
      sample_column = "Sample",
      variables     = "cell.line"
    ),
    classification = list(
      well_summary = well_summary
    )
  )
}

# ── Top-level structure checks ────────────────────────────────────────────────

test_that("compute_auc_ci stops when data is not a list", {
  expect_error(
    compute_auc_ci("not_a_list"),
    "must be a named list with elements 'data', 'meta', and 'classification'"
  )
})

test_that("compute_auc_ci stops when 'data' element is missing", {
  input <- make_auc_input()
  input$data <- NULL
  expect_error(
    compute_auc_ci(input),
    "must be a named list with elements 'data', 'meta', and 'classification'"
  )
})

test_that("compute_auc_ci stops when 'meta' element is missing", {
  input <- make_auc_input()
  input$meta <- NULL
  expect_error(
    compute_auc_ci(input),
    "must be a named list with elements 'data', 'meta', and 'classification'"
  )
})

test_that("compute_auc_ci stops when 'classification' element is missing", {
  input <- make_auc_input()
  input$classification <- NULL
  expect_error(
    compute_auc_ci(input),
    "must be a named list with elements 'data', 'meta', and 'classification'"
  )
})

# ── meta structure checks ─────────────────────────────────────────────────────

test_that("compute_auc_ci stops when data$meta is not a list", {
  input <- make_auc_input()
  input$meta <- "not_a_list"
  expect_error(
    compute_auc_ci(input),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})

test_that("compute_auc_ci stops when data$meta is missing 'variables'", {
  input <- make_auc_input()
  input$meta <- list(data_path = "fake/path")
  expect_error(
    compute_auc_ci(input),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})

# ── classification structure checks ──────────────────────────────────────────

test_that("compute_auc_ci stops when data$classification is not a list", {
  input <- make_auc_input()
  input$classification <- "not_a_list"
  expect_error(
    compute_auc_ci(input),
    "'data\\$classification' must be a named list containing 'well_summary'"
  )
})

test_that("compute_auc_ci stops when well_summary is missing from classification", {
  input <- make_auc_input()
  input$classification$well_summary <- NULL
  expect_error(
    compute_auc_ci(input),
    "'data\\$classification' must be a named list containing 'well_summary'"
  )
})

# ── data$data checks ──────────────────────────────────────────────────────────

test_that("compute_auc_ci stops when data$data is not a data frame", {
  input <- make_auc_input()
  input$data <- "not_a_data_frame"
  expect_error(
    compute_auc_ci(input),
    "'data\\$data' must be a data.frame"
  )
})

test_that("compute_auc_ci stops when data$data is empty", {
  input <- make_auc_input(empty = TRUE)
  expect_error(
    compute_auc_ci(input),
    "'data\\$data' must contain at least one row"
  )
})

# ── variables checks ──────────────────────────────────────────────────────────

test_that("compute_auc_ci stops when variables is empty", {
  input <- make_auc_input()
  input$meta$variables <- character(0)
  expect_error(
    compute_auc_ci(input),
    "'data\\$meta\\$variables' must be a non-empty character vector"
  )
})

test_that("compute_auc_ci stops when variables is NULL", {
  input <- make_auc_input()
  input$meta$variables <- NULL
  expect_error(
    compute_auc_ci(input),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})

test_that("compute_auc_ci stops when variables is not character", {
  input <- make_auc_input()
  input$meta$variables <- 1:3
  expect_error(
    compute_auc_ci(input),
    "'data\\$meta\\$variables' must be a non-empty character vector"
  )
})

# ── Required column checks ────────────────────────────────────────────────────

test_that("compute_auc_ci stops when Well is missing from data$data", {
  input <- make_auc_input()
  input$data$Well <- NULL
  expect_error(
    compute_auc_ci(input),
    "'data\\$data' is missing required columns: Well"
  )
})

test_that("compute_auc_ci stops when Hour is missing from data$data", {
  input <- make_auc_input()
  input$data$Hour <- NULL
  expect_error(
    compute_auc_ci(input),
    "'data\\$data' is missing required columns: Hour"
  )
})

test_that("compute_auc_ci stops when Plate.Type is missing from data$data", {
  input <- make_auc_input()
  input$data$Plate.Type <- NULL
  expect_error(
    compute_auc_ci(input),
    "'data\\$data' is missing required columns: Plate.Type"
  )
})

test_that("compute_auc_ci stops when diff_OD_590_740 is missing from data$data", {
  input <- make_auc_input()
  input$data$diff_OD_590_740 <- NULL
  expect_error(
    compute_auc_ci(input),
    "'data\\$data' is missing required columns: diff_OD_590_740"
  )
})

test_that("compute_auc_ci stops when variable column is missing from data$data", {
  input <- make_auc_input()
  input$data$cell.line <- NULL
  expect_error(
    compute_auc_ci(input),
    "'data\\$data' is missing required columns: cell.line"
  )
})

# ── target_curve_type checks ──────────────────────────────────────────────────

test_that("compute_auc_ci stops when target_curve_type is not character", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, target_curve_type = 1),
    "'target_curve_type' must be a non-empty character vector"
  )
})

test_that("compute_auc_ci stops when target_curve_type is empty", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, target_curve_type = character(0)),
    "'target_curve_type' must be a non-empty character vector"
  )
})

test_that("compute_auc_ci stops when target_curve_type contains invalid value", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, target_curve_type = "invalid_type"),
    "'target_curve_type' contains invalid values: invalid_type"
  )
})

test_that("compute_auc_ci stops when one of multiple target_curve_types is invalid", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, target_curve_type = c("growing", "bad_type")),
    "'target_curve_type' contains invalid values: bad_type"
  )
})

test_that("compute_auc_ci accepts all valid curve types", {
  input <- make_auc_input()
  valid <- c("growing", "OD_drop", "slow_growing", "flat_low")
  # update well_summary to contain all types
  input$classification$well_summary <- data.frame(
    Well = c("A01", "A02", "A03", "A04"),
    Plate.Type = "PM-M1",
    cell.line = "H460",
    curve_type = valid,
    stringsAsFactors = FALSE
  )
  expect_no_error(
    compute_auc_ci(input, target_curve_type = valid, n_boot = 10)
  )
})

# ── n_boot checks ─────────────────────────────────────────────────────────────

test_that("compute_auc_ci stops when n_boot is not numeric", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, n_boot = "many"),
    "'n_boot' must be a single positive integer"
  )
})

test_that("compute_auc_ci stops when n_boot is zero", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, n_boot = 0),
    "'n_boot' must be a single positive integer"
  )
})

test_that("compute_auc_ci stops when n_boot is negative", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, n_boot = -10),
    "'n_boot' must be a single positive integer"
  )
})

test_that("compute_auc_ci stops when n_boot is non-integer numeric", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, n_boot = 10.5),
    "'n_boot' must be a single positive integer"
  )
})

test_that("compute_auc_ci stops when n_boot is length > 1", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, n_boot = c(100, 200)),
    "'n_boot' must be a single positive integer"
  )
})

test_that("compute_auc_ci stops when n_boot is NA", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, n_boot = NA_integer_),
    "'n_boot' must be a single positive integer"
  )
})

# ── spline_df checks ──────────────────────────────────────────────────────────

test_that("compute_auc_ci stops when spline_df is not numeric", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, spline_df = "four"),
    "'spline_df' must be a single positive integer"
  )
})

test_that("compute_auc_ci stops when spline_df is zero", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, spline_df = 0),
    "'spline_df' must be a single positive integer"
  )
})

test_that("compute_auc_ci stops when spline_df is negative", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, spline_df = -1),
    "'spline_df' must be a single positive integer"
  )
})

test_that("compute_auc_ci stops when spline_df is non-integer", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, spline_df = 3.5),
    "'spline_df' must be a single positive integer"
  )
})

test_that("compute_auc_ci stops when spline_df is length > 1", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, spline_df = c(4, 5)),
    "'spline_df' must be a single positive integer"
  )
})

# ── store checks ──────────────────────────────────────────────────────────────

test_that("compute_auc_ci stops when store is not logical", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, store = "yes"),
    "'store' must be a single logical value"
  )
})

test_that("compute_auc_ci stops when store is length > 1", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, store = c(TRUE, FALSE)),
    "'store' must be a single logical value"
  )
})

test_that("compute_auc_ci stops when store is NA", {
  input <- make_auc_input()
  expect_error(
    compute_auc_ci(input, store = NA),
    "'store' must be a single logical value"
  )
})

# ── Return structure ──────────────────────────────────────────────────────────

test_that("compute_auc_ci returns modified data list when store is TRUE", {
  input <- make_auc_input()
  result <- compute_auc_ci(input, n_boot = 10)
  expect_type(result, "list")
  expect_true("auc_results" %in% names(result))
})

test_that("compute_auc_ci returns tibble when store is FALSE", {
  input <- make_auc_input()
  result <- compute_auc_ci(input, n_boot = 10, store = FALSE)
  expect_s3_class(result, "data.frame")
  expect_false("auc_results" %in% names(result))
})

test_that("compute_auc_ci preserves data and meta when store is TRUE", {
  input <- make_auc_input()
  result <- compute_auc_ci(input, n_boot = 10)
  expect_identical(result$data, input$data)
  expect_identical(result$meta, input$meta)
})

# ── auc_results content ───────────────────────────────────────────────────────

test_that("compute_auc_ci auc_results contains required columns", {
  input <- make_auc_input()
  result <- compute_auc_ci(input, n_boot = 10)
  required <- c("AUC", "AUC_low", "AUC_high", "n_points", "Chemical", "control")
  expect_true(all(required %in% names(result$auc_results)))
})

test_that("compute_auc_ci auc_results has one row per well per condition", {
  input <- make_auc_input()
  result <- compute_auc_ci(input, n_boot = 10)
  n_wells <- length(unique(input$data$Well))
  expect_equal(nrow(result$auc_results), n_wells)
})

test_that("compute_auc_ci AUC values are numeric", {
  input <- make_auc_input()
  result <- compute_auc_ci(input, n_boot = 10)
  expect_true(is.numeric(result$auc_results$AUC))
})

test_that("compute_auc_ci AUC_low is always <= AUC_high", {
  input <- make_auc_input()
  result <- compute_auc_ci(input, n_boot = 10)
  valid <- !is.na(result$auc_results$AUC_low) & !is.na(result$auc_results$AUC_high)
  expect_true(all(
    result$auc_results$AUC_low[valid] <= result$auc_results$AUC_high[valid]
  ))
})

test_that("compute_auc_ci returns NA AUC for wells with fewer than 4 timepoints", {
  input <- make_auc_input(n_timepoints = 3)
  result <- compute_auc_ci(input, n_boot = 10)
  expect_true(all(is.na(result$auc_results$AUC)))
})
