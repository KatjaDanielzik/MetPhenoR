make_per_plate_input <- function(empty = FALSE,
                                 include_controls = TRUE,
                                 include_tween = TRUE,
                                 plate = "PM-M1") {
  chemicals <- character(0)
  controls <- character(0)

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
    Plate.Type = plate,
    cell.line = "H460",
    Chemical = chemicals,
    control = controls,
    AUC = runif(n, 0.2, 0.8),
    AUC_low = runif(n, 0.1, 0.4),
    AUC_high = runif(n, 0.5, 1.2),
    stringsAsFactors = FALSE
  )
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

test_that("plot_controls_tween_per_plate stops when data is not a list", {
  expect_error(
    plot_controls_tween_per_plate("not_a_list", selected_plate = "PM-M1"),
    "must be a named list containing 'auc_results'"
  )
})

test_that("plot_controls_tween_per_plate stops when data is a data frame", {
  expect_error(
    plot_controls_tween_per_plate(data.frame(x = 1), selected_plate = "PM-M1"),
    "must be a named list containing 'auc_results'"
  )
})

test_that("plot_controls_tween_per_plate stops when auc_results is missing", {
  expect_error(
    plot_controls_tween_per_plate(
      list(meta = list(variables = "cell.line")),
      selected_plate = "PM-M1"
    ),
    "must be a named list containing 'auc_results'"
  )
})

# ── auc_results checks ────────────────────────────────────────────────────────

test_that("plot_controls_tween_per_plate stops when auc_results is not a data frame", {
  input <- make_per_plate_input()
  input$auc_results <- "not_a_df"
  expect_error(
    plot_controls_tween_per_plate(input, selected_plate = "PM-M1"),
    "'data\\$auc_results' must be a data.frame"
  )
})

test_that("plot_controls_tween_per_plate stops when auc_results is empty", {
  input <- make_per_plate_input(empty = TRUE)
  expect_error(
    plot_controls_tween_per_plate(input, selected_plate = "PM-M1"),
    "'data\\$auc_results' must contain at least one row"
  )
})

test_that("plot_controls_tween_per_plate stops when required column is missing", {
  input <- make_per_plate_input()
  input$auc_results$AUC <- NULL
  expect_error(
    plot_controls_tween_per_plate(input, selected_plate = "PM-M1"),
    "missing required columns: AUC"
  )
})

test_that("plot_controls_tween_per_plate reports all missing columns", {
  input <- make_per_plate_input()
  input$auc_results$AUC <- NULL
  input$auc_results$Chemical <- NULL
  expect_error(
    plot_controls_tween_per_plate(input, selected_plate = "PM-M1"),
    "missing required columns"
  )
})

# ── variables checks ──────────────────────────────────────────────────────────

test_that("plot_controls_tween_per_plate uses data$meta$variables when variables is NULL", {
  input <- make_per_plate_input()
  result <- plot_controls_tween_per_plate(input, selected_plate = "PM-M1")
  expect_s3_class(result, "ggplot")
})

test_that("plot_controls_tween_per_plate stops when variables is NULL and meta is missing", {
  input <- make_per_plate_input()
  input$meta <- NULL
  expect_error(
    plot_controls_tween_per_plate(input, selected_plate = "PM-M1"),
    "'variables' must be provided or exist in 'data\\$meta\\$variables'"
  )
})

test_that("plot_controls_tween_per_plate stops when variables is not character", {
  input <- make_per_plate_input()
  expect_error(
    plot_controls_tween_per_plate(input,
      selected_plate = "PM-M1",
      variables = 1:3
    ),
    "'variables' must be a non-empty character vector"
  )
})

test_that("plot_controls_tween_per_plate stops when variables is empty", {
  input <- make_per_plate_input()
  expect_error(
    plot_controls_tween_per_plate(input,
      selected_plate = "PM-M1",
      variables = character(0)
    ),
    "'variables' must be a non-empty character vector"
  )
})

test_that("plot_controls_tween_per_plate stops when variable not in auc_results", {
  input <- make_per_plate_input()
  expect_error(
    plot_controls_tween_per_plate(input,
      selected_plate = "PM-M1",
      variables = "nonexistent"
    ),
    "not present in 'data\\$auc_results'"
  )
})

# ── selected_plate checks ─────────────────────────────────────────────────────

test_that("plot_controls_tween_per_plate stops when selected_plate is missing", {
  input <- make_per_plate_input()
  expect_error(
    plot_controls_tween_per_plate(input),
    "'selected_plate' must be provided"
  )
})

test_that("plot_controls_tween_per_plate stops when selected_plate is NULL", {
  input <- make_per_plate_input()
  expect_error(
    plot_controls_tween_per_plate(input, selected_plate = NULL),
    "'selected_plate' must be provided"
  )
})

test_that("plot_controls_tween_per_plate stops when selected_plate is not character", {
  input <- make_per_plate_input()
  expect_error(
    plot_controls_tween_per_plate(input, selected_plate = 1),
    "'selected_plate' must be a single character string"
  )
})

test_that("plot_controls_tween_per_plate stops when selected_plate has length > 1", {
  input <- make_per_plate_input()
  expect_error(
    plot_controls_tween_per_plate(input, selected_plate = c("PM-M1", "PM-M2")),
    "'selected_plate' must be a single character string"
  )
})

# ── tween_labels checks ───────────────────────────────────────────────────────

test_that("plot_controls_tween_per_plate stops when tween_labels is not character", {
  input <- make_per_plate_input()
  expect_error(
    plot_controls_tween_per_plate(input,
      selected_plate = "PM-M1",
      tween_labels = 1:3
    ),
    "'tween_labels' must be a non-empty character vector"
  )
})

test_that("plot_controls_tween_per_plate stops when tween_labels is empty", {
  input <- make_per_plate_input()
  expect_error(
    plot_controls_tween_per_plate(input,
      selected_plate = "PM-M1",
      tween_labels = character(0)
    ),
    "'tween_labels' must be a non-empty character vector"
  )
})

# ── Post-filter check ─────────────────────────────────────────────────────────

test_that("plot_controls_tween_per_plate stops when no rows remain after filtering", {
  input <- make_per_plate_input(include_controls = FALSE, include_tween = FALSE)
  expect_error(
    plot_controls_tween_per_plate(input, selected_plate = "PM-M1"),
    "No data available after filtering"
  )
})

# ── Return type and plot structure ────────────────────────────────────────────

test_that("plot_controls_tween_per_plate returns a ggplot object", {
  input <- make_per_plate_input()
  result <- plot_controls_tween_per_plate(input, selected_plate = "PM-M1")
  expect_s3_class(result, "ggplot")
})

test_that("plot_controls_tween_per_plate maps AUC to x-axis", {
  input <- make_per_plate_input()
  result <- plot_controls_tween_per_plate(input, selected_plate = "PM-M1")
  expect_equal(rlang::as_label(result$mapping$x), "AUC")
})

test_that("plot_controls_tween_per_plate title contains selected_plate", {
  input <- make_per_plate_input()
  result <- plot_controls_tween_per_plate(input, selected_plate = "PM-M1")
  expect_true(grepl("PM-M1", result$labels$title))
})

test_that("plot_controls_tween_per_plate works with user-supplied variables", {
  input <- make_per_plate_input()
  result <- plot_controls_tween_per_plate(input,
    selected_plate = "PM-M1",
    variables = "cell.line"
  )
  expect_s3_class(result, "ggplot")
})

test_that("plot_controls_tween_per_plate works with custom tween_labels", {
  input <- make_per_plate_input()
  input$auc_results$Chemical[input$auc_results$Chemical == "Tween 20"] <- "CustomTween"
  result <- plot_controls_tween_per_plate(input,
    selected_plate = "PM-M1",
    tween_labels = "CustomTween"
  )
  expect_s3_class(result, "ggplot")
})
