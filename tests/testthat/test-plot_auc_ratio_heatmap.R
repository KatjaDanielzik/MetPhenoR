make_heatmap_input <- function(empty      = FALSE,
                               n_plates   = 1,
                               n_xvar     = 2) {
  
  plates    <- paste0("PM-M", seq_len(n_plates))
  xvar_vals <- paste0("condition_", seq_len(n_xvar))
  
  df <- expand.grid(
    Well       = c("A01", "A02", "A03"),
    Plate.Type = plates,
    cell.line  = xvar_vals,
    stringsAsFactors = FALSE
  )
  df$Chemical   <- paste0("Drug", seq_len(nrow(df)))
  df$AUC_ratio  <- runif(nrow(df), 0.5, 2.0)
  df$AUC        <- runif(nrow(df), 0.1, 1.0)
  df$treatment  <- "drug"
  
  list(
    auc_ratio = if (empty) df[0, ] else df,
    meta = list(
      data_path     = "fake/path",
      sample_column = "Sample",
      variables     = c("cell.line", "treatment")
    )
  )
}

# ── Top-level structure checks ────────────────────────────────────────────────

test_that("plot_auc_ratio_heatmap stops when data is not a list", {
  expect_error(
    plot_auc_ratio_heatmap("not_a_list", plate_type = "PM-M1"),
    "must be a named list containing an 'auc_ratio' element"
  )
})

test_that("plot_auc_ratio_heatmap stops when data is a data frame", {
  expect_error(
    plot_auc_ratio_heatmap(data.frame(x = 1), plate_type = "PM-M1"),
    "must be a named list containing an 'auc_ratio' element"
  )
})

test_that("plot_auc_ratio_heatmap stops when auc_ratio element is missing", {
  expect_error(
    plot_auc_ratio_heatmap(list(meta = list(variables = "cell.line")),
                           plate_type = "PM-M1"),
    "must be a named list containing an 'auc_ratio' element"
  )
})

# ── auc_ratio checks ──────────────────────────────────────────────────────────

test_that("plot_auc_ratio_heatmap stops when auc_ratio is not a data frame", {
  input             <- make_heatmap_input()
  input$auc_ratio   <- "not_a_df"
  expect_error(
    plot_auc_ratio_heatmap(input, plate_type = "PM-M1"),
    "'data\\$auc_ratio' must be a data.frame"
  )
})

test_that("plot_auc_ratio_heatmap stops when auc_ratio is empty", {
  input <- make_heatmap_input(empty = TRUE)
  expect_error(
    plot_auc_ratio_heatmap(input, plate_type = "PM-M1"),
    "'data\\$auc_ratio' must contain at least one row"
  )
})

test_that("plot_auc_ratio_heatmap stops when required column is missing", {
  input                    <- make_heatmap_input()
  input$auc_ratio$AUC_ratio <- NULL
  expect_error(
    plot_auc_ratio_heatmap(input, plate_type = "PM-M1"),
    "missing required columns: AUC_ratio"
  )
})

test_that("plot_auc_ratio_heatmap reports all missing columns", {
  input                    <- make_heatmap_input()
  input$auc_ratio$AUC_ratio <- NULL
  input$auc_ratio$Chemical  <- NULL
  expect_error(
    plot_auc_ratio_heatmap(input, plate_type = "PM-M1"),
    "missing required columns"
  )
})

# ── meta and variables checks ─────────────────────────────────────────────────

test_that("plot_auc_ratio_heatmap stops when data$meta is missing", {
  input      <- make_heatmap_input()
  input$meta <- NULL
  expect_error(
    plot_auc_ratio_heatmap(input, plate_type = "PM-M1"),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})

test_that("plot_auc_ratio_heatmap stops when variables is not character", {
  input                <- make_heatmap_input()
  input$meta$variables <- 1:3
  expect_error(
    plot_auc_ratio_heatmap(input, plate_type = "PM-M1"),
    "'data\\$meta\\$variables' must be a non-empty character vector"
  )
})

# ── plate_type checks ─────────────────────────────────────────────────────────

test_that("plot_auc_ratio_heatmap stops when plate_type is missing", {
  input <- make_heatmap_input()
  expect_error(
    plot_auc_ratio_heatmap(input),
    "'plate_type' must be a non-empty character vector"
  )
})

test_that("plot_auc_ratio_heatmap stops when plate_type is NULL", {
  input <- make_heatmap_input()
  expect_error(
    plot_auc_ratio_heatmap(input, plate_type = NULL),
    "'plate_type' must be a non-empty character vector"
  )
})

test_that("plot_auc_ratio_heatmap stops when plate_type is not character", {
  input <- make_heatmap_input()
  expect_error(
    plot_auc_ratio_heatmap(input, plate_type = 1),
    "'plate_type' must be a character vector"
  )
})

test_that("plot_auc_ratio_heatmap warns when plate_type not found in data", {
  input <- make_heatmap_input()
  expect_error(
    plot_auc_ratio_heatmap(input, plate_type = "PM-WRONG"),
    "No data found for the specified 'plate_type'"
  )
})

test_that("plot_auc_ratio_heatmap stops when no rows remain after plate filtering", {
  input <- make_heatmap_input()
  expect_error(
    suppressWarnings(
      plot_auc_ratio_heatmap(input, plate_type = "PM-WRONG")
    ),
    "No data found for the specified 'plate_type' values"
  )
})

# ── x_var checks ──────────────────────────────────────────────────────────────

test_that("plot_auc_ratio_heatmap auto-selects x_var from meta$variables", {
  input  <- make_heatmap_input()
  expect_message(
    plot_auc_ratio_heatmap(input, plate_type = "PM-M1", x_var = NULL),
    "Using 'cell.line' as default 'x_var'"
  )
})

test_that("plot_auc_ratio_heatmap stops when x_var is not character", {
  input <- make_heatmap_input()
  expect_error(
    plot_auc_ratio_heatmap(input, plate_type = "PM-M1", x_var = 1),
    "'x_var' must be a single character string"
  )
})

test_that("plot_auc_ratio_heatmap stops when x_var has length > 1", {
  input <- make_heatmap_input()
  expect_error(
    plot_auc_ratio_heatmap(input, plate_type = "PM-M1",
                           x_var = c("cell.line", "treatment")),
    "'x_var' must be a single character string"
  )
})

test_that("plot_auc_ratio_heatmap stops when x_var not in auc_ratio", {
  input <- make_heatmap_input()
  expect_error(
    plot_auc_ratio_heatmap(input, plate_type = "PM-M1",
                           x_var = "nonexistent"),
    "'x_var' value 'nonexistent' is not a column in 'data\\$auc_ratio'"
  )
})

# ── scale_type checks ─────────────────────────────────────────────────────────

test_that("plot_auc_ratio_heatmap stops when scale_type is invalid", {
  input <- make_heatmap_input()
  expect_error(
    plot_auc_ratio_heatmap(input, plate_type = "PM-M1",
                           x_var = "cell.line",
                           scale_type = "sqrt"),
    "should be one of"
  )
})

test_that("plot_auc_ratio_heatmap accepts 'natural' scale_type", {
  input  <- make_heatmap_input()
  result <- plot_auc_ratio_heatmap(input, plate_type = "PM-M1",
                                   x_var = "cell.line",
                                   scale_type = "natural")
  expect_s3_class(result, "ggplot")
})

test_that("plot_auc_ratio_heatmap accepts 'log10' scale_type", {
  input  <- make_heatmap_input()
  result <- plot_auc_ratio_heatmap(input, plate_type = "PM-M1",
                                   x_var = "cell.line",
                                   scale_type = "log10")
  expect_s3_class(result, "ggplot")
})

# ── Return type and plot structure ────────────────────────────────────────────

test_that("plot_auc_ratio_heatmap returns a ggplot object", {
  input  <- make_heatmap_input()
  result <- plot_auc_ratio_heatmap(input, plate_type = "PM-M1",
                                   x_var = "cell.line")
  expect_s3_class(result, "ggplot")
})

test_that("plot_auc_ratio_heatmap title contains scale_type", {
  input  <- make_heatmap_input()
  result <- plot_auc_ratio_heatmap(input, plate_type = "PM-M1",
                                   x_var = "cell.line",
                                   scale_type = "natural")
  expect_true(grepl("natural", result$labels$title))
})

test_that("plot_auc_ratio_heatmap subtitle contains plate_type", {
  input  <- make_heatmap_input()
  result <- plot_auc_ratio_heatmap(input, plate_type = "PM-M1",
                                   x_var = "cell.line")
  expect_true(grepl("PM-M1", result$labels$subtitle))
})

test_that("plot_auc_ratio_heatmap works with multiple plate types", {
  input  <- make_heatmap_input(n_plates = 2)
  result <- plot_auc_ratio_heatmap(input,
                                   plate_type = c("PM-M1", "PM-M2"),
                                   x_var = "cell.line")
  expect_s3_class(result, "ggplot")
})

test_that("plot_auc_ratio_heatmap warns when x_var has only one unique value", {
  input <- make_heatmap_input(n_xvar = 1)
  expect_warning(
    plot_auc_ratio_heatmap(input, plate_type = "PM-M1",
                           x_var = "cell.line"),
    "only one unique value"
  )
})
