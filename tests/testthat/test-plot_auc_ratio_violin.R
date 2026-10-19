make_violin_input <- function(empty = FALSE,
                              na_auc = FALSE,
                              include_group = TRUE) {
  df <- data.frame(
    Well = paste0("A0", 1:6),
    Plate.Type = "PM-M1",
    cell.line = rep(c("H460", "A549"), 3),
    treatment = "drug",
    pathway = rep(c("Metabolism", "Signalling"), 3),
    Chemical = paste0("Drug", 1:6),
    AUC_ratio = runif(6, 0.5, 2.0),
    AUC = runif(6, 0.1, 1.0),
    stringsAsFactors = FALSE
  )

  if (na_auc) df$AUC_ratio <- NA_real_
  if (!include_group) df$pathway <- NULL

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

test_that("plot_auc_ratio_violin stops when data is not a list", {
  expect_error(
    plot_auc_ratio_violin("not_a_list",
      group_var = "pathway", color_var = "cell.line"
    ),
    "must be a named list containing an 'auc_ratio' element"
  )
})

test_that("plot_auc_ratio_violin stops when data is a data frame", {
  expect_error(
    plot_auc_ratio_violin(data.frame(x = 1),
      group_var = "pathway", color_var = "cell.line"
    ),
    "must be a named list containing an 'auc_ratio' element"
  )
})

test_that("plot_auc_ratio_violin stops when auc_ratio element is missing", {
  expect_error(
    plot_auc_ratio_violin(list(meta = list(variables = "cell.line")),
      group_var = "pathway", color_var = "cell.line"
    ),
    "must be a named list containing an 'auc_ratio' element"
  )
})

# ── auc_ratio checks ──────────────────────────────────────────────────────────

test_that("plot_auc_ratio_violin stops when auc_ratio is not a data frame", {
  input <- make_violin_input()
  input$auc_ratio <- "not_a_df"
  expect_error(
    plot_auc_ratio_violin(input, group_var = "pathway", color_var = "cell.line"),
    "'data\\$auc_ratio' must be a data.frame"
  )
})

test_that("plot_auc_ratio_violin stops when auc_ratio is empty", {
  input <- make_violin_input(empty = TRUE)
  expect_error(
    plot_auc_ratio_violin(input, group_var = "pathway", color_var = "cell.line"),
    "'data\\$auc_ratio' must contain at least one row"
  )
})

test_that("plot_auc_ratio_violin stops when AUC_ratio column is missing", {
  input <- make_violin_input()
  input$auc_ratio$AUC_ratio <- NULL
  expect_error(
    plot_auc_ratio_violin(input, group_var = "pathway", color_var = "cell.line"),
    "missing required column: AUC_ratio"
  )
})

# ── meta and variables checks ─────────────────────────────────────────────────

test_that("plot_auc_ratio_violin stops when data$meta is missing", {
  input <- make_violin_input()
  input$meta <- NULL
  expect_error(
    plot_auc_ratio_violin(input, group_var = "pathway", color_var = "cell.line"),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})


# ── group_var checks ──────────────────────────────────────────────────────────

test_that("plot_auc_ratio_violin stops when group_var is not character", {
  input <- make_violin_input()
  expect_error(
    plot_auc_ratio_violin(input, group_var = 1, color_var = "cell.line"),
    "'group_var' must be a single character string"
  )
})

test_that("plot_auc_ratio_violin stops when group_var has length > 1", {
  input <- make_violin_input()
  expect_error(
    plot_auc_ratio_violin(input,
      group_var = c("pathway", "treatment"),
      color_var = "cell.line"
    ),
    "'group_var' must be a single character string"
  )
})

test_that("plot_auc_ratio_violin stops when group_var not in auc_ratio", {
  input <- make_violin_input()
  expect_error(
    plot_auc_ratio_violin(input,
      group_var = "nonexistent",
      color_var = "cell.line"
    ),
    "'group_var' value 'nonexistent' is not a column"
  )
})

# ── color_var checks ──────────────────────────────────────────────────────────

test_that("plot_auc_ratio_violin auto-selects color_var and emits message", {
  input <- make_violin_input()
  expect_message(
    plot_auc_ratio_violin(input, group_var = "pathway"),
    "Using 'cell.line' as default"
  )
})

test_that("plot_auc_ratio_violin stops when color_var is not character", {
  input <- make_violin_input()
  expect_error(
    plot_auc_ratio_violin(input, group_var = "pathway", color_var = 1),
    "'color_var' must be a single character string"
  )
})

test_that("plot_auc_ratio_violin stops when color_var has length > 1", {
  input <- make_violin_input()
  expect_error(
    plot_auc_ratio_violin(input,
      group_var = "pathway",
      color_var = c("cell.line", "treatment")
    ),
    "'color_var' must be a single character string"
  )
})

test_that("plot_auc_ratio_violin stops when color_var not in auc_ratio", {
  input <- make_violin_input()
  expect_error(
    plot_auc_ratio_violin(input,
      group_var = "pathway",
      color_var = "nonexistent"
    ),
    "'color_var' value 'nonexistent' is not a column"
  )
})

# ── scale_type checks ─────────────────────────────────────────────────────────

test_that("plot_auc_ratio_violin stops when scale_type is invalid", {
  input <- make_violin_input()
  expect_error(
    plot_auc_ratio_violin(input,
      group_var = "pathway",
      color_var = "cell.line",
      scale_type = "sqrt"
    ),
    "should be one of"
  )
})

# ── group_filter checks ───────────────────────────────────────────────────────

test_that("plot_auc_ratio_violin stops when group_filter is not character", {
  input <- make_violin_input()
  expect_error(
    plot_auc_ratio_violin(input,
      group_var = "pathway",
      color_var = "cell.line",
      group_filter = 1
    ),
    "'group_filter' must be a non-empty character vector or NULL"
  )
})

test_that("plot_auc_ratio_violin stops when group_filter is empty character", {
  input <- make_violin_input()
  expect_error(
    plot_auc_ratio_violin(input,
      group_var = "pathway",
      color_var = "cell.line",
      group_filter = character(0)
    ),
    "'group_filter' must be a non-empty character vector or NULL"
  )
})

test_that("plot_auc_ratio_violin warns when group_filter value not found", {
  input <- make_violin_input()
  expect_warning(
    plot_auc_ratio_violin(input,
      group_var = "pathway",
      color_var = "cell.line",
      group_filter = c("Metabolism", "NonExistent")
    ),
    "not found in 'pathway'"
  )
})

test_that("plot_auc_ratio_violin stops when group_filter removes all rows", {
  input <- make_violin_input()
  expect_error(
    suppressWarnings(
      plot_auc_ratio_violin(input,
        group_var = "pathway",
        color_var = "cell.line",
        group_filter = "NonExistent"
      )
    ),
    "No rows remain after applying 'group_filter'"
  )
})

# ── Post-filter AUC validity check ────────────────────────────────────────────

test_that("plot_auc_ratio_violin stops when all AUC_ratio values are NA", {
  input <- make_violin_input(na_auc = TRUE)
  expect_error(
    plot_auc_ratio_violin(input,
      group_var = "pathway",
      color_var = "cell.line"
    ),
    "No valid AUC_ratio values available for plotting"
  )
})

# ── Return type and plot content ──────────────────────────────────────────────

test_that("plot_auc_ratio_violin returns a ggplot object", {
  input <- make_violin_input()
  result <- plot_auc_ratio_violin(input,
    group_var = "pathway",
    color_var = "cell.line"
  )
  expect_s3_class(result, "ggplot")
})

test_that("plot_auc_ratio_violin title contains group_var", {
  input <- make_violin_input()
  result <- plot_auc_ratio_violin(input,
    group_var = "pathway",
    color_var = "cell.line"
  )
  expect_true(grepl("pathway", result$labels$title))
})

test_that("plot_auc_ratio_violin x-label indicates log10 when selected", {
  input <- make_violin_input()
  result <- plot_auc_ratio_violin(input,
    group_var = "pathway",
    color_var = "cell.line",
    scale_type = "log10"
  )
  expect_true(grepl("log10", result$labels$x))
})

test_that("plot_auc_ratio_violin accepts NULL group_filter without error", {
  input <- make_violin_input()
  expect_no_error(
    plot_auc_ratio_violin(input,
      group_var = "pathway",
      color_var = "cell.line",
      group_filter = NULL
    )
  )
})

test_that("plot_auc_ratio_violin correctly filters by group_filter", {
  input <- make_violin_input()
  result <- plot_auc_ratio_violin(input,
    group_var = "pathway",
    color_var = "cell.line",
    group_filter = "Metabolism"
  )
  plot_data <- result$data
  expect_true(all(plot_data$pathway == "Metabolism"))
})
