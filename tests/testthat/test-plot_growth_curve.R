# ── Helper ────────────────────────────────────────────────────────────────────

make_plot_gc_input <- function(n_timepoints = 10,
                               include_classification = TRUE,
                               empty_data = FALSE) {
  hours <- seq(0, n_timepoints - 1)
  rows <- LETTERS[1:8]
  cols <- sprintf("%02d", 1:12)
  wells <- as.vector(outer(rows, cols, paste0))

  base <- expand.grid(
    Hour = hours,
    Well = wells,
    Plate.Type = "PM-M1",
    cell.line = "H460",
    stringsAsFactors = FALSE
  )
  base$diff_OD_590_740 <- runif(nrow(base), 0.05, 0.5)

  df <- if (empty_data) base[0, ] else base

  well_summary <- data.frame(
    Well = wells,
    Plate.Type = "PM-M1",
    cell.line = "H460",
    curve_type = sample(c("growing", "flat_low", "OD_drop", "slow_growing"),
      length(wells),
      replace = TRUE
    ),
    n_points = n_timepoints,
    qc_flag = FALSE,
    stringsAsFactors = FALSE
  )

  deriv_data <- base
  deriv_data$first_derivative <- runif(nrow(base), -0.01, 0.05)
  deriv_data$fitted <- runif(nrow(base), 0.05, 0.5)

  result <- list(
    data = df,
    meta = list(
      data_path     = "fake/path",
      sample_column = "Sample",
      variables     = "cell.line"
    )
  )

  if (include_classification) {
    result$classification <- list(
      well_summary    = well_summary,
      derivative_data = deriv_data
    )
  }

  result
}

# ── Top-level structure checks ────────────────────────────────────────────────

test_that("plot_growth_curve stops when data is not a list", {
  expect_error(
    plot_growth_curve("not_a_list"),
    "must be a named list with elements 'data', 'meta', and 'classification'"
  )
})

test_that("plot_growth_curve stops when 'data' element is missing", {
  input <- make_plot_gc_input()
  input$data <- NULL
  expect_error(
    plot_growth_curve(input),
    "must be a named list with elements 'data', 'meta', and 'classification'"
  )
})

test_that("plot_growth_curve stops when 'meta' element is missing", {
  input <- make_plot_gc_input()
  input$meta <- NULL
  expect_error(
    plot_growth_curve(input),
    "must be a named list with elements 'data', 'meta', and 'classification'"
  )
})

test_that("plot_growth_curve stops when 'classification' element is missing", {
  input <- make_plot_gc_input(include_classification = FALSE)
  expect_error(
    plot_growth_curve(input),
    "must be a named list with elements 'data', 'meta', and 'classification'"
  )
})

# ── meta structure checks ─────────────────────────────────────────────────────

test_that("plot_growth_curve stops when data$meta is not a list", {
  input <- make_plot_gc_input()
  input$meta <- "not_a_list"
  expect_error(
    plot_growth_curve(input),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})

test_that("plot_growth_curve stops when data$meta is missing 'variables'", {
  input <- make_plot_gc_input()
  input$meta <- list(data_path = "fake/path")
  expect_error(
    plot_growth_curve(input),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})

# ── classification structure checks ──────────────────────────────────────────

test_that("plot_growth_curve stops when data$classification is not a list", {
  input <- make_plot_gc_input()
  input$classification <- "not_a_list"
  expect_error(
    plot_growth_curve(input),
    "'data\\$classification' must be a named list with elements 'well_summary' and 'derivative_data'"
  )
})

test_that("plot_growth_curve stops when well_summary is missing from classification", {
  input <- make_plot_gc_input()
  input$classification$well_summary <- NULL
  expect_error(
    plot_growth_curve(input),
    "'data\\$classification' must be a named list with elements 'well_summary' and 'derivative_data'"
  )
})

test_that("plot_growth_curve stops when derivative_data is missing from classification", {
  input <- make_plot_gc_input()
  input$classification$derivative_data <- NULL
  expect_error(
    plot_growth_curve(input),
    "'data\\$classification' must be a named list with elements 'well_summary' and 'derivative_data'"
  )
})

# ── data$data checks ──────────────────────────────────────────────────────────

test_that("plot_growth_curve stops when data$data is not a data frame", {
  input <- make_plot_gc_input()
  input$data <- "not_a_data_frame"
  expect_error(
    plot_growth_curve(input),
    "'data\\$data' must be a data.frame"
  )
})

test_that("plot_growth_curve stops when data$data is empty", {
  input <- make_plot_gc_input(empty_data = TRUE)
  expect_error(
    plot_growth_curve(input),
    "'data\\$data' must contain at least one row"
  )
})

# ── variables checks ──────────────────────────────────────────────────────────

test_that("plot_growth_curve stops when variables is empty", {
  input <- make_plot_gc_input()
  input$meta$variables <- character(0)
  expect_error(
    plot_growth_curve(input),
    "'data\\$meta\\$variables' must be a non-empty character vector"
  )
})

test_that("plot_growth_curve stops when variables is not character", {
  input <- make_plot_gc_input()
  input$meta$variables <- 1:3
  expect_error(
    plot_growth_curve(input),
    "'data\\$meta\\$variables' must be a non-empty character vector"
  )
})

# ── Required column checks in data$data ──────────────────────────────────────

test_that("plot_growth_curve stops when Well is missing from data$data", {
  input <- make_plot_gc_input()
  input$data$Well <- NULL
  expect_error(
    plot_growth_curve(input),
    "'data\\$data' is missing required columns: Well"
  )
})

test_that("plot_growth_curve stops when Hour is missing from data$data", {
  input <- make_plot_gc_input()
  input$data$Hour <- NULL
  expect_error(
    plot_growth_curve(input),
    "'data\\$data' is missing required columns: Hour"
  )
})

test_that("plot_growth_curve stops when Plate.Type is missing from data$data", {
  input <- make_plot_gc_input()
  input$data$Plate.Type <- NULL
  expect_error(
    plot_growth_curve(input),
    "'data\\$data' is missing required columns: Plate.Type"
  )
})

test_that("plot_growth_curve stops when diff_OD_590_740 is missing from data$data", {
  input <- make_plot_gc_input()
  input$data$diff_OD_590_740 <- NULL
  expect_error(
    plot_growth_curve(input),
    "'data\\$data' is missing required columns: diff_OD_590_740"
  )
})

test_that("plot_growth_curve stops when variable column is missing from data$data", {
  input <- make_plot_gc_input()
  input$data$cell.line <- NULL
  expect_error(
    plot_growth_curve(input),
    "'data\\$data' is missing required columns: cell.line"
  )
})

# ── Required column checks in well_summary ────────────────────────────────────

test_that("plot_growth_curve stops when curve_type is missing from well_summary", {
  input <- make_plot_gc_input()
  input$classification$well_summary$curve_type <- NULL
  expect_error(
    plot_growth_curve(input),
    "'data\\$classification\\$well_summary' is missing required columns: curve_type"
  )
})

test_that("plot_growth_curve stops when Well is missing from well_summary", {
  input <- make_plot_gc_input()
  input$classification$well_summary$Well <- NULL
  expect_error(
    plot_growth_curve(input),
    "'data\\$classification\\$well_summary' is missing required columns: Well"
  )
})

test_that("plot_growth_curve stops when variable column is missing from well_summary", {
  input <- make_plot_gc_input()
  input$classification$well_summary$cell.line <- NULL
  expect_error(
    plot_growth_curve(input),
    "'data\\$classification\\$well_summary' is missing required columns: cell.line"
  )
})

# ── curve_types_to_plot checks ────────────────────────────────────────────────

test_that("plot_growth_curve stops when curve_types_to_plot is not character", {
  input <- make_plot_gc_input()
  expect_error(
    plot_growth_curve(input, curve_types_to_plot = 1:3),
    "'curve_types_to_plot' must be a non-empty character vector or NULL"
  )
})

test_that("plot_growth_curve stops when curve_types_to_plot is empty character", {
  input <- make_plot_gc_input()
  expect_error(
    plot_growth_curve(input, curve_types_to_plot = character(0)),
    "'curve_types_to_plot' must be a non-empty character vector or NULL"
  )
})

test_that("plot_growth_curve warns when curve_types_to_plot contains unknown type", {
  input <- make_plot_gc_input()
  expect_warning(
    plot_growth_curve(input, curve_types_to_plot = c("growing", "unknown_type")),
    "unrecognised curve types: unknown_type"
  )
})

test_that("plot_growth_curve accepts NULL for curve_types_to_plot", {
  input <- make_plot_gc_input()
  expect_no_error(plot_growth_curve(input, curve_types_to_plot = NULL))
})

# ── store_plots checks ────────────────────────────────────────────────────────

test_that("plot_growth_curve stops when store_plots is not logical", {
  input <- make_plot_gc_input()
  expect_error(
    plot_growth_curve(input, store_plots = "yes"),
    "'store_plots' must be a single logical value"
  )
})

test_that("plot_growth_curve stops when store_plots is length > 1", {
  input <- make_plot_gc_input()
  expect_error(
    plot_growth_curve(input, store_plots = c(TRUE, FALSE)),
    "'store_plots' must be a single logical value"
  )
})

# ── Return structure ──────────────────────────────────────────────────────────

test_that("plot_growth_curve returns updated data list when store_plots is TRUE", {
  input <- make_plot_gc_input()
  result <- plot_growth_curve(input, store_plots = TRUE)
  expect_type(result, "list")
  expect_true("growth_curve_plots" %in% names(result))
})

test_that("plot_growth_curve returns plot list when store_plots is FALSE", {
  input <- make_plot_gc_input()
  result <- plot_growth_curve(input, store_plots = FALSE)
  expect_type(result, "list")
  expect_false("growth_curve_plots" %in% names(result))
})

test_that("plot_growth_curve preserves data and meta when store_plots is TRUE", {
  input <- make_plot_gc_input()
  result <- plot_growth_curve(input, store_plots = TRUE)
  expect_identical(result$data, input$data)
  expect_identical(result$meta, input$meta)
})

test_that("plot_growth_curve returns named list of plots", {
  input <- make_plot_gc_input()
  result <- plot_growth_curve(input, store_plots = FALSE)
  expect_false(is.null(names(result)))
})

test_that("plot_growth_curve returns one plot per condition", {
  input <- make_plot_gc_input()
  result <- plot_growth_curve(input, store_plots = FALSE)
  # one condition: cell.line = H460, Plate.Type = PM-M1
  expect_length(result, 1)
})

test_that("plot_growth_curve returns two plots for two conditions", {
  input <- make_plot_gc_input()
  extra_data <- input$data
  extra_ws <- input$classification$well_summary
  extra_dd <- input$classification$derivative_data

  extra_data$cell.line <- "A549"
  extra_ws$cell.line <- "A549"
  extra_dd$cell.line <- "A549"

  input$data <- rbind(input$data, extra_data)
  input$classification$well_summary <- rbind(input$classification$well_summary, extra_ws)
  input$classification$derivative_data <- rbind(input$classification$derivative_data, extra_dd)

  result <- plot_growth_curve(input, store_plots = FALSE)
  expect_length(result, 2)
})

# ── Empty curve subset handling ───────────────────────────────────────────────

test_that("plot_growth_curve handles case where no curves match curve_types_to_plot", {
  input <- make_plot_gc_input()
  # Request a type that is not present
  input$classification$well_summary$curve_type <- "growing"
  result <- expect_no_error(
    plot_growth_curve(input, curve_types_to_plot = "flat_low", store_plots = FALSE)
  )
  expect_type(result, "list")
})

# ── Single variable edge case ─────────────────────────────────────────────────

test_that("plot_growth_curve works with multiple grouping variables", {
  input <- make_plot_gc_input()
  input$data$treatment <- "drug"
  input$meta$variables <- c("cell.line", "treatment")
  input$classification$well_summary$treatment <- "drug"
  input$classification$derivative_data$treatment <- "drug"

  expect_no_error(plot_growth_curve(input, store_plots = FALSE))
})
