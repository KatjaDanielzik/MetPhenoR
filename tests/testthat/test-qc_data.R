
# Helper: minimal valid input matching the structure from read_folder()
make_qc_input <- function(n_replicates = 1, include_duplicate = FALSE) {
  
  hours <- seq(0, 10, by = 2)
  wells <- c("A01", "A02", "A03", "A04", "A05")
  
  base <- expand.grid(
    Hour         = hours,
    Well         = wells,
    Plate.Type   = "PM-M1",
    cell.line    = "H460",
    radiation.dose = "CT",
    time.point   = "0h",
    stringsAsFactors = FALSE
  )
  base$diff_OD_590_740 <- runif(nrow(base))
  base$PlateId <- "plate_001"
  
  df <- base
  
  if (n_replicates == 2) {
    rep2 <- base
    rep2$PlateId <- "plate_002"
    df <- rbind(df, rep2)
  }
  
  if (include_duplicate) {
    df <- rbind(df, base[1, ])
  }
  
  list(
    data = df,
    meta = list(
      data_path     = "fake/path",
      sample_column = "Sample",
      variables     = c("cell.line", "radiation.dose", "time.point")
    )
  )
}

# ── Top-level structure checks ────────────────────────────────────────────────

test_that("qc_data stops when data is not a list", {
  expect_error(
    qc_data("not_a_list"),
    "must be a named list with elements 'data' and 'meta'"
  )
})

test_that("qc_data stops when data is a list but missing 'data' element", {
  expect_error(
    qc_data(list(meta = list(variables = "cell.line"))),
    "must be a named list with elements 'data' and 'meta'"
  )
})

test_that("qc_data stops when data is a list but missing 'meta' element", {
  expect_error(
    qc_data(list(data = data.frame(x = 1))),
    "must be a named list with elements 'data' and 'meta'"
  )
})

# ── meta structure checks ─────────────────────────────────────────────────────

test_that("qc_data stops when data$meta is not a list", {
  input <- make_qc_input()
  input$meta <- "not_a_list"
  expect_error(
    qc_data(input),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})

test_that("qc_data stops when data$meta is missing 'variables'", {
  input <- make_qc_input()
  input$meta <- list(data_path = "fake/path")
  expect_error(
    qc_data(input),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})

# ── data$data checks ──────────────────────────────────────────────────────────

test_that("qc_data stops when data$data is not a data frame", {
  input <- make_qc_input()
  input$data <- "not_a_data_frame"
  expect_error(
    qc_data(input),
    "'data\\$data' must be a data frame"
  )
})

test_that("qc_data stops when data$data is a list instead of data frame", {
  input <- make_qc_input()
  input$data <- list(Well = "A01")
  expect_error(
    qc_data(input),
    "'data\\$data' must be a data frame"
  )
})

# ── variables checks ──────────────────────────────────────────────────────────

test_that("qc_data stops when variables is empty", {
  input <- make_qc_input()
  input$meta$variables <- character(0)
  expect_error(
    qc_data(input),
    "'data\\$meta\\$variables' must be a non-empty character vector"
  )
})

test_that("qc_data stops when variables is not a character vector", {
  input <- make_qc_input()
  input$meta$variables <- 1:3
  expect_error(
    qc_data(input),
    "'data\\$meta\\$variables' must be a non-empty character vector"
  )
})

test_that("qc_data stops when variables is NULL", {
  input <- make_qc_input()
  input$meta$variables <- NULL
  expect_error(
    qc_data(input),
    "must be a named list containing 'variables'"
  )
})

# ── Missing column checks ─────────────────────────────────────────────────────

test_that("qc_data stops when a variable column is missing from data$data", {
  input <- make_qc_input()
  input$data$cell.line <- NULL
  expect_error(
    qc_data(input),
    "Missing required columns: cell.line"
  )
})

test_that("qc_data stops when Well column is missing", {
  input <- make_qc_input()
  input$data$Well <- NULL
  expect_error(
    qc_data(input),
    "Missing required columns: Well"
  )
})

test_that("qc_data stops when Hour column is missing", {
  input <- make_qc_input()
  input$data$Hour <- NULL
  expect_error(
    qc_data(input),
    "Missing required columns: Hour"
  )
})

test_that("qc_data stops when Plate.Type column is missing", {
  input <- make_qc_input()
  input$data$Plate.Type <- NULL
  expect_error(
    qc_data(input),
    "Missing required columns: Plate.Type"
  )
})

test_that("qc_data stops when diff_OD_590_740 column is missing", {
  input <- make_qc_input()
  input$data$diff_OD_590_740 <- NULL
  expect_error(
    qc_data(input),
    "Missing required columns: diff_OD_590_740"
  )
})

test_that("qc_data error lists all missing columns at once", {
  input <- make_qc_input()
  input$data$Well <- NULL
  input$data$Hour <- NULL
  expect_error(
    qc_data(input),
    "Missing required columns:.*Well.*Hour|Missing required columns:.*Hour.*Well"
  )
})

# ── Post-filter check ─────────────────────────────────────────────────────────

test_that("qc_data stops when no rows remain after well filtering", {
  input <- make_qc_input()
  input$data$Well <- "H12"  # replace all wells with one outside A01-A05
  expect_error(
    qc_data(input),
    "No data found for the specified wells"
  )
})

# ── Return type ───────────────────────────────────────────────────────────────

test_that("qc_data returns a ggplot object", {
  input <- make_qc_input()
  result <- qc_data(input)
  expect_s3_class(result, "ggplot")
})

# ── Well filtering ────────────────────────────────────────────────────────────

test_that("qc_data only includes wells A01 to A05 in plot data", {
  input <- make_qc_input()
  # Add a well outside the expected range
  extra_row        <- input$data[1, ]
  extra_row$Well   <- "H12"
  input$data       <- rbind(input$data, extra_row)
  
  result <- qc_data(input)
  expect_false("H12" %in% unique(result$data$Well))
  expect_true(all(unique(result$data$Well) %in% c("A01","A02","A03","A04","A05")))
})

# ── Replicate counts ──────────────────────────────────────────────────────────

test_that("qc_data shows n_entries == 1 for a single non-duplicated replicate", {
  input  <- make_qc_input(n_replicates = 1)
  result <- qc_data(input)
  expect_equal(unique(result$data$n_entries), 1)
})

test_that("qc_data shows n_entries == 2 for two distinct replicates", {
  input  <- make_qc_input(n_replicates = 2)
  result <- qc_data(input)
  expect_equal(unique(result$data$n_entries), 2)
})

# ── Duplicate detection ───────────────────────────────────────────────────────

test_that("qc_data reflects duplicated rows as n_entries > 1", {
  input  <- make_qc_input(include_duplicate = TRUE)
  result <- qc_data(input)
  expect_true(any(result$data$n_entries > 1))
})

test_that("qc_data shows only n_entries == 1 when there are no duplicates", {
  input  <- make_qc_input(include_duplicate = FALSE)
  result <- qc_data(input)
  expect_true(all(result$data$n_entries == 1))
})

# ── Plot structure ────────────────────────────────────────────────────────────

test_that("qc_data plot maps Well to x-axis", {
  input  <- make_qc_input()
  result <- qc_data(input)
  expect_equal(rlang::as_label(result$mapping$x), "Well")
})

test_that("qc_data plot maps n_entries to fill", {
  input  <- make_qc_input()
  result <- qc_data(input)
  expect_equal(rlang::as_label(result$mapping$fill), "n_entries")
})

test_that("qc_data plot has correct title", {
  input  <- make_qc_input()
  result <- qc_data(input)
  expect_equal(result$labels$title, "QC heatmap: Number of observations = 1?")
})

test_that("qc_data plot has correct subtitle", {
  input  <- make_qc_input()
  result <- qc_data(input)
  expect_equal(result$labels$subtitle, "raw_value entries of the first 5 wells")
})

test_that("qc_data plot is faceted by Plate_Type", {
  input  <- make_qc_input()
  result <- qc_data(input)
  facet_vars <- as.character(result$facet$params$facets[[1]])
  expect_true(any(grepl("Plate_Type", facet_vars)))
})

# ── Multiple Plate.Type values ────────────────────────────────────────────────

test_that("qc_data produces two facet panels for two distinct Plate.Type values", {
  input       <- make_qc_input()
  extra        <- input$data
  extra$Plate.Type <- "PM-M2"
  input$data   <- rbind(input$data, extra)
  
  result      <- qc_data(input)
  built       <- ggplot2::ggplot_build(result)
  n_panels    <- length(unique(built$layout$layout$PANEL))
  expect_equal(n_panels, 2)
})

# ── Single variable edge case ─────────────────────────────────────────────────

test_that("qc_data works when variables contains only one element", {
  input                  <- make_qc_input()
  input$meta$variables   <- "cell.line"
  expect_s3_class(qc_data(input), "ggplot")
})
