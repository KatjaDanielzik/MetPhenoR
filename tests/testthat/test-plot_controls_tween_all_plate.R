make_tween_input <- function(empty = FALSE,
                             include_tween = TRUE,
                             include_controls = TRUE) {
  chemicals <- c()
  controls <- c()

  if (include_controls) {
    chemicals <- c(chemicals, "Negative Control", "a-D-Glucose")
    controls <- c(controls, "negative", "positive")
  }
  if (include_tween) {
    chemicals <- c(chemicals, "Tween 20")
    controls <- c(controls, NA_character_)
  }
  if (length(chemicals) == 0) {
    chemicals <- "DrugX"
    controls <- NA_character_
  }

  n <- length(chemicals)

  df <- data.frame(
    Well = paste0("A0", seq_len(n)),
    Plate.Type = "PM-M1",
    cell.line = "H460",
    Chemical = chemicals,
    control = controls,
    AUC = runif(n, 0.1, 1.0),
    AUC_low = runif(n, 0.05, 0.5),
    AUC_high = runif(n, 0.5, 1.5),
    stringsAsFactors = FALSE
  )
  # ensure AUC_low <= AUC_high
  df$AUC_low <- pmin(df$AUC_low, df$AUC_high)
  df$AUC_high <- pmax(df$AUC_low, df$AUC_high)

  auc_results <- if (empty) df[0, ] else df

  list(
    auc_results = auc_results,
    meta = list(
      data_path     = "fake/path",
      sample_column = "Sample",
      variables     = "cell.line"
    )
  )
}

# ── Top-level structure checks ────────────────────────────────────────────────

test_that("plot_controls_tween_all_plate stops when data is not a list", {
  expect_error(
    plot_controls_tween_all_plate("not_a_list"),
    "'data' must be a named list containing 'auc_results'"
  )
})

test_that("plot_controls_tween_all_plate stops when data is a data frame", {
  expect_error(
    plot_controls_tween_all_plate(data.frame(x = 1)),
    "must be a named list containing 'auc_results'"
  )
})

test_that("plot_controls_tween_all_plate stops when auc_results element is missing", {
  expect_error(
    plot_controls_tween_all_plate(list(meta = list(variables = "cell.line"))),
    "must be a named list containing 'auc_results'"
  )
})

# ── variables checks ──────────────────────────────────────────────────────────

test_that("plot_controls_tween_all_plate uses data$meta$variables when variables is NULL", {
  input <- make_tween_input()
  result <- plot_controls_tween_all_plate(input, variables = NULL)
  expect_s3_class(result, "ggplot")
})

test_that("plot_controls_tween_all_plate stops when variables is NULL and meta is missing", {
  input <- make_tween_input()
  input$meta <- NULL
  expect_error(
    plot_controls_tween_all_plate(input, variables = NULL),
    "'variables' must be provided or exist in 'data\\$meta\\$variables'"
  )
})

test_that("plot_controls_tween_all_plate stops when variables is NULL and meta$variables is missing", {
  input <- make_tween_input()
  input$meta$variables <- NULL
  expect_error(
    plot_controls_tween_all_plate(input, variables = NULL),
    "'variables' must be provided or exist in 'data\\$meta\\$variables'"
  )
})

test_that("plot_controls_tween_all_plate stops when variables is not character", {
  input <- make_tween_input()
  expect_error(
    plot_controls_tween_all_plate(input, variables = 1:3),
    "'variables' must be a non-empty character vector"
  )
})

test_that("plot_controls_tween_all_plate stops when variables is empty character", {
  input <- make_tween_input()
  expect_error(
    plot_controls_tween_all_plate(input, variables = character(0)),
    "'variables' must be a non-empty character vector"
  )
})

test_that("plot_controls_tween_all_plate stops when variable not in auc_results", {
  input <- make_tween_input()
  expect_error(
    plot_controls_tween_all_plate(input, variables = "nonexistent_col"),
    "not present in 'data\\$auc_results'"
  )
})

# ── auc_results checks ────────────────────────────────────────────────────────

test_that("plot_controls_tween_all_plate stops when auc_results is not a data frame", {
  input <- make_tween_input()
  input$auc_results <- "not_a_df"
  expect_error(
    plot_controls_tween_all_plate(input),
    "'data\\$auc_results' must be a data.frame"
  )
})

test_that("plot_controls_tween_all_plate stops when auc_results is empty", {
  input <- make_tween_input(empty = TRUE)
  expect_error(
    plot_controls_tween_all_plate(input),
    "'data\\$auc_results' must contain at least one row"
  )
})

test_that("plot_controls_tween_all_plate stops when required column is missing", {
  input <- make_tween_input()
  input$auc_results$AUC <- NULL
  expect_error(
    plot_controls_tween_all_plate(input),
    "'data\\$auc_results' is missing required columns: AUC"
  )
})

test_that("plot_controls_tween_all_plate error names all missing columns", {
  input <- make_tween_input()
  input$auc_results$AUC <- NULL
  input$auc_results$Chemical <- NULL
  expect_error(
    plot_controls_tween_all_plate(input),
    "missing required columns"
  )
})

# ── Post-filter check ─────────────────────────────────────────────────────────

test_that("plot_controls_tween_all_plate stops when no rows remain after filtering", {
  input <- make_tween_input(include_tween = FALSE, include_controls = FALSE)
  expect_error(
    plot_controls_tween_all_plate(input),
    "No data available after filtering"
  )
})

# ── Return type ───────────────────────────────────────────────────────────────

test_that("plot_controls_tween_all_plate returns a ggplot object", {
  input <- make_tween_input()
  result <- plot_controls_tween_all_plate(input)
  expect_s3_class(result, "ggplot")
})

# ── Plot structure ────────────────────────────────────────────────────────────

test_that("plot_controls_tween_all_plate maps AUC to x-axis", {
  input <- make_tween_input()
  result <- plot_controls_tween_all_plate(input)
  expect_equal(rlang::as_label(result$mapping$x), "AUC")
})

test_that("plot_controls_tween_all_plate has correct title", {
  input <- make_tween_input()
  result <- plot_controls_tween_all_plate(input)
  expect_equal(
    result$labels$title,
    "AUC Comparison: Controls vs Tween Across Plates"
  )
})

test_that("plot_controls_tween_all_plate works with user-supplied variables", {
  input <- make_tween_input()
  result <- plot_controls_tween_all_plate(input, variables = "cell.line")
  expect_s3_class(result, "ggplot")
})

test_that("plot_controls_tween_all_plate works with multiple variables", {
  input <- make_tween_input()
  input$auc_results$treatment <- "drug"
  result <- plot_controls_tween_all_plate(
    input,
    variables = c("cell.line", "treatment")
  )
  expect_s3_class(result, "ggplot")
})
