# ── Helper ────────────────────────────────────────────────────────────────────

make_auc_plot_input <- function(empty = FALSE,
                                n_variables = 2,
                                plate = "PM-M1") {
  n <- 6
  df <- data.frame(
    Well = paste0("A0", seq_len(n)),
    Plate.Type = plate,
    Chemical = c(
      "Negative Control", "a-D-Glucose",
      "DrugA", "DrugB", "DrugC", "DrugD"
    ),
    control = c(
      "negative", "positive",
      NA, NA, NA, NA
    ),
    AUC = runif(n, 0.1, 1.0),
    AUC_low = runif(n, 0.05, 0.4),
    AUC_high = runif(n, 0.5, 1.2),
    cell.line = "H460",
    treatment = "drug",
    stringsAsFactors = FALSE
  )
  df$AUC_low <- pmin(df$AUC_low, df$AUC_high)
  df$AUC_high <- pmax(df$AUC_low, df$AUC_high)

  vars <- c("cell.line", "treatment")[seq_len(n_variables)]

  list(
    auc_results = if (empty) df[0, ] else df,
    meta = list(
      data_path     = "fake/path",
      sample_column = "Sample",
      variables     = vars
    )
  )
}

# ── Top-level structure checks ────────────────────────────────────────────────

test_that("plot_auc stops when data is not a list", {
  expect_error(
    plot_auc("not_a_list", selected_plate = "PM-M1"),
    "must be a named list containing 'auc_results'"
  )
})

test_that("plot_auc stops when data is a data frame", {
  expect_error(
    plot_auc(data.frame(x = 1), selected_plate = "PM-M1"),
    "must be a named list containing 'auc_results'"
  )
})

test_that("plot_auc stops when auc_results element is missing", {
  expect_error(
    plot_auc(list(meta = list(variables = c("a", "b"))), selected_plate = "PM-M1"),
    "must be a named list containing 'auc_results'"
  )
})

# ── auc_results checks ────────────────────────────────────────────────────────

test_that("plot_auc stops when auc_results is not a data frame", {
  input <- make_auc_plot_input()
  input$auc_results <- "not_a_df"
  expect_error(
    plot_auc(input, selected_plate = "PM-M1"),
    "'data\\$auc_results' must be a data.frame"
  )
})

test_that("plot_auc stops when auc_results is empty", {
  input <- make_auc_plot_input(empty = TRUE)
  expect_error(
    plot_auc(input, selected_plate = "PM-M1"),
    "'data\\$auc_results' must contain at least one row"
  )
})

test_that("plot_auc stops when required column is missing", {
  input <- make_auc_plot_input()
  input$auc_results$AUC <- NULL
  expect_error(
    plot_auc(input, selected_plate = "PM-M1"),
    "missing required columns: AUC"
  )
})

test_that("plot_auc reports all missing columns", {
  input <- make_auc_plot_input()
  input$auc_results$AUC <- NULL
  input$auc_results$Chemical <- NULL
  expect_error(
    plot_auc(input, selected_plate = "PM-M1"),
    "missing required columns"
  )
})

# ── meta and variables checks ─────────────────────────────────────────────────

test_that("plot_auc stops when data$meta is missing", {
  input <- make_auc_plot_input()
  input$meta <- NULL
  expect_error(
    plot_auc(input, selected_plate = "PM-M1"),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})

test_that("plot_auc stops when variables has fewer than 2 elements", {
  input <- make_auc_plot_input(n_variables = 1)
  expect_error(
    plot_auc(input, selected_plate = "PM-M1"),
    "'data\\$meta\\$variables' must contain at least 2 variables"
  )
})

# ── selected_plate checks ─────────────────────────────────────────────────────

test_that("plot_auc stops when selected_plate is missing", {
  input <- make_auc_plot_input()
  expect_error(
    plot_auc(input),
    "'selected_plate' must be provided"
  )
})

test_that("plot_auc stops when selected_plate is NULL", {
  input <- make_auc_plot_input()
  expect_error(
    plot_auc(input, selected_plate = NULL),
    "'selected_plate' must be provided"
  )
})

test_that("plot_auc stops when selected_plate is not character", {
  input <- make_auc_plot_input()
  expect_error(
    plot_auc(input, selected_plate = 1),
    "'selected_plate' must be a single character string"
  )
})

test_that("plot_auc stops when selected_plate has length > 1", {
  input <- make_auc_plot_input()
  expect_error(
    plot_auc(input, selected_plate = c("PM-M1", "PM-M2")),
    "'selected_plate' must be a single character string"
  )
})

test_that("plot_auc warns when selected_plate not found in data", {
  input <- make_auc_plot_input()
  expect_error(
    plot_auc(data = input, selected_plate = "PM-WRONG"),
    "was not found in 'data\\$auc_results\\$Plate.Type'"
  )
})

# ── facet_vars checks ─────────────────────────────────────────────────────────

test_that("plot_auc stops when facet_vars is not character", {
  input <- make_auc_plot_input()
  expect_error(
    plot_auc(input, selected_plate = "PM-M1", facet_vars = 1:2),
    "'facet_vars' must be a non-empty character vector or NULL"
  )
})

test_that("plot_auc stops when facet_vars is empty character", {
  input <- make_auc_plot_input()
  expect_error(
    plot_auc(input, selected_plate = "PM-M1", facet_vars = character(0)),
    "'facet_vars' must be a non-empty character vector or NULL"
  )
})

test_that("plot_auc stops when facet_vars has more than 2 elements", {
  input <- make_auc_plot_input()
  expect_error(
    plot_auc(input,
      selected_plate = "PM-M1",
      facet_vars = c("cell.line", "treatment", "extra")
    ),
    "'facet_vars' must contain at most 2 variables"
  )
})

test_that("plot_auc stops when facet_vars column not in auc_results", {
  input <- make_auc_plot_input()
  expect_error(
    plot_auc(input,
      selected_plate = "PM-M1",
      facet_vars = c("cell.line", "nonexistent")
    ),
    "not present in 'data\\$auc_results'"
  )
})

test_that("plot_auc accepts NULL facet_vars", {
  input <- make_auc_plot_input()
  expect_no_error(
    plot_auc(input, selected_plate = "PM-M1", facet_vars = NULL)
  )
})

# ── Return structure ──────────────────────────────────────────────────────────

test_that("plot_auc returns a list", {
  input <- make_auc_plot_input()
  result <- plot_auc(input, selected_plate = "PM-M1")
  expect_type(result, "list")
})

test_that("plot_auc returns ggplot objects in list", {
  input <- make_auc_plot_input()
  result <- plot_auc(input, selected_plate = "PM-M1")
  expect_true(all(sapply(result, inherits, "ggplot")))
})

test_that("plot_auc plot title contains 'AUC response'", {
  input <- make_auc_plot_input()
  result <- plot_auc(input, selected_plate = "PM-M1")
  expect_equal(result[[1]]$labels$title, "AUC response")
})

test_that("plot_auc subtitle contains selected_plate", {
  input <- make_auc_plot_input()
  result <- plot_auc(input, selected_plate = "PM-M1")
  expect_true(grepl("PM-M1", result[[1]]$labels$subtitle))
})

test_that("plot_auc works with user-supplied facet_vars", {
  input <- make_auc_plot_input()
  result <- plot_auc(input,
    selected_plate = "PM-M1",
    facet_vars = c("cell.line", "treatment")
  )
  expect_true(all(sapply(result, inherits, "ggplot")))
})
