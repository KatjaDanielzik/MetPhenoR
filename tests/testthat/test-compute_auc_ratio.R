make_ratio_input <- function(empty            = FALSE,
                             include_controls = TRUE) {
  
  chemicals <- c("DrugA", "DrugB", "DrugC")
  controls  <- rep(NA_character_, 3)
  
  if (include_controls) {
    chemicals <- c("Negative Control", "No Chemical", chemicals)
    controls  <- c("negative", "negative", rep(NA_character_, 3))
  }
  
  n  <- length(chemicals)
  df <- data.frame(
    Well       = paste0("A0", seq_len(n)),
    Plate.Type = "PM-M1",
    cell.line  = "H460",
    Chemical   = chemicals,
    control    = controls,
    AUC        = runif(n, 0.2, 1.0),
    AUC_low    = runif(n, 0.1, 0.4),
    AUC_high   = runif(n, 0.5, 1.2),
    stringsAsFactors = FALSE
  )
  df$AUC_low  <- pmin(df$AUC_low,  df$AUC_high)
  df$AUC_high <- pmax(df$AUC_low,  df$AUC_high)
  
  list(
    auc_results = if (empty) df[0, ] else df,
    meta = list(
      data_path     = "fake/path",
      sample_column = "Sample",
      variables     = "cell.line"
    )
  )
}

# ── Top-level structure checks ────────────────────────────────────────────────

test_that("compute_auc_ratio stops when data is not a list", {
  expect_error(
    compute_auc_ratio("not_a_list"),
    "must be a named list containing 'auc_results' and 'meta'"
  )
})

test_that("compute_auc_ratio stops when data is a data frame", {
  expect_error(
    compute_auc_ratio(data.frame(x = 1)),
    "must be a named list containing 'auc_results' and 'meta'"
  )
})

test_that("compute_auc_ratio stops when auc_results is missing", {
  expect_error(
    compute_auc_ratio(list(meta = list(variables = "cell.line"))),
    "must be a named list containing 'auc_results' and 'meta'"
  )
})

test_that("compute_auc_ratio stops when meta is missing", {
  input      <- make_ratio_input()
  input$meta <- NULL
  expect_error(
    compute_auc_ratio(input),
    "must be a named list containing 'auc_results' and 'meta'"
  )
})

# ── meta checks ───────────────────────────────────────────────────────────────

test_that("compute_auc_ratio stops when data$meta is not a list", {
  input      <- make_ratio_input()
  input$meta <- "not_a_list"
  expect_error(
    compute_auc_ratio(input),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})

test_that("compute_auc_ratio stops when data$meta$variables is missing", {
  input      <- make_ratio_input()
  input$meta <- list(data_path = "fake")
  expect_error(
    compute_auc_ratio(input),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})

# ── auc_results checks ────────────────────────────────────────────────────────

test_that("compute_auc_ratio stops when auc_results is not a data frame", {
  input             <- make_ratio_input()
  input$auc_results <- "not_a_df"
  expect_error(
    compute_auc_ratio(input),
    "'data\\$auc_results' must be a data.frame"
  )
})

test_that("compute_auc_ratio stops when auc_results is empty", {
  input <- make_ratio_input(empty = TRUE)
  expect_error(
    compute_auc_ratio(input),
    "'data\\$auc_results' must contain at least one row"
  )
})

# ── variables checks ──────────────────────────────────────────────────────────

test_that("compute_auc_ratio stops when variables is empty", {
  input                <- make_ratio_input()
  input$meta$variables <- character(0)
  expect_error(
    compute_auc_ratio(input),
    "'data\\$meta\\$variables' must be a non-empty character vector"
  )
})

# ── Required column checks ────────────────────────────────────────────────────

test_that("compute_auc_ratio stops when AUC column is missing", {
  input                  <- make_ratio_input()
  input$auc_results$AUC  <- NULL
  expect_error(
    compute_auc_ratio(input),
    "missing required columns: AUC"
  )
})

test_that("compute_auc_ratio stops when Plate.Type column is missing", {
  input                       <- make_ratio_input()
  input$auc_results$Plate.Type <- NULL
  expect_error(
    compute_auc_ratio(input),
    "missing required columns: Plate.Type"
  )
})

test_that("compute_auc_ratio stops when Chemical column is missing", {
  input                      <- make_ratio_input()
  input$auc_results$Chemical <- NULL
  expect_error(
    compute_auc_ratio(input),
    "missing required columns: Chemical"
  )
})

test_that("compute_auc_ratio reports all missing columns at once", {
  input                      <- make_ratio_input()
  input$auc_results$AUC      <- NULL
  input$auc_results$Chemical <- NULL
  expect_error(
    compute_auc_ratio(input),
    "missing required columns"
  )
})

# ── control_labels checks ─────────────────────────────────────────────────────

test_that("compute_auc_ratio stops when control_labels is not character", {
  input <- make_ratio_input()
  expect_error(
    compute_auc_ratio(input, control_labels = 1:2),
    "'control_labels' must be a non-empty character vector"
  )
})

test_that("compute_auc_ratio stops when control_labels is empty", {
  input <- make_ratio_input()
  expect_error(
    compute_auc_ratio(input, control_labels = character(0)),
    "'control_labels' must be a non-empty character vector"
  )
})

test_that("compute_auc_ratio warns when control_labels not found in data", {
  input <- make_ratio_input()
  expect_error(
    compute_auc_ratio(input, control_labels = "NonExistentControl"),
    "None of the 'control_labels' values were found"
  )
})

# ── store checks ──────────────────────────────────────────────────────────────

test_that("compute_auc_ratio stops when store is not logical", {
  input <- make_ratio_input()
  expect_error(
    compute_auc_ratio(input, store = "yes"),
    "'store' must be a single logical value"
  )
})

test_that("compute_auc_ratio stops when store is NA", {
  input <- make_ratio_input()
  expect_error(
    compute_auc_ratio(input, store = NA),
    "'store' must be a single logical value"
  )
})

# ── Return structure ──────────────────────────────────────────────────────────

test_that("compute_auc_ratio returns updated list when store is TRUE", {
  input  <- make_ratio_input()
  result <- compute_auc_ratio(input, store = TRUE)
  expect_type(result, "list")
  expect_true("auc_ratio" %in% names(result))
})

test_that("compute_auc_ratio returns data frame when store is FALSE", {
  input  <- make_ratio_input()
  result <- compute_auc_ratio(input, store = FALSE)
  expect_s3_class(result, "data.frame")
  expect_false("auc_ratio" %in% names(result))
})

test_that("compute_auc_ratio preserves data and meta when store is TRUE", {
  input  <- make_ratio_input()
  result <- compute_auc_ratio(input, store = TRUE)
  expect_identical(result$auc_results, input$auc_results)
  expect_identical(result$meta,        input$meta)
})

# ── AUC ratio correctness ─────────────────────────────────────────────────────

test_that("compute_auc_ratio adds AUC_ratio column", {
  input  <- make_ratio_input()
  result <- compute_auc_ratio(input, store = FALSE)
  expect_true("AUC_ratio" %in% names(result))
})

test_that("compute_auc_ratio AUC_ratio is numeric", {
  input  <- make_ratio_input()
  result <- compute_auc_ratio(input, store = FALSE)
  expect_true(is.numeric(result$AUC_ratio))
})

test_that("compute_auc_ratio control wells have AUC_ratio close to 1", {
  input  <- make_ratio_input()
  # Set all control AUC values to identical value for deterministic ratio
  input$auc_results$AUC[input$auc_results$Chemical == "Negative Control"] <- 0.5
  input$auc_results$AUC[input$auc_results$Chemical == "No Chemical"]      <- 0.5
  
  result    <- compute_auc_ratio(input, store = FALSE)
  ctrl_rows <- result[result$Chemical %in% c("Negative Control", "No Chemical"), ]
  expect_true(all(abs(ctrl_rows$AUC_ratio - 1) < 1e-10))
})


test_that("compute_auc_ratio adds control summary columns", {
  input  <- make_ratio_input()
  result <- compute_auc_ratio(input, store = FALSE)
  expect_true("mean_ctrl_AUC" %in% names(result))
})
