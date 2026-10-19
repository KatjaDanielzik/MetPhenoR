make_plot_latest_input <- function(n_timepoints = 3, empty = FALSE) {
  hours <- seq(0, n_timepoints - 1)

  # All 96 wells: A01-H12
  rows <- LETTERS[1:8]
  cols <- sprintf("%02d", 1:12)
  wells <- as.vector(outer(rows, cols, paste0))

  base <- expand.grid(
    Hour = hours,
    Well = wells,
    Plate.Type = "PM-M1",
    cell.line = "H460",
    radiation.dose = "CT",
    time.point = "0h",
    stringsAsFactors = FALSE
  )
  base$diff_OD_590_740 <- runif(nrow(base))
  base$PlateId <- "plate_001"

  df <- if (empty) base[0, ] else base

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

test_that("plot_latest_timepoint stops when data is not a list", {
  expect_error(
    plot_latest_timepoint("not_a_list"),
    "must be a named list with elements 'data' and 'meta'"
  )
})

test_that("plot_latest_timepoint stops when 'data' element is missing", {
  expect_error(
    plot_latest_timepoint(list(meta = list(variables = "cell.line"))),
    "must be a named list with elements 'data' and 'meta'"
  )
})

test_that("plot_latest_timepoint stops when 'meta' element is missing", {
  expect_error(
    plot_latest_timepoint(list(data = data.frame(x = 1))),
    "must be a named list with elements 'data' and 'meta'"
  )
})

# ── meta structure checks ─────────────────────────────────────────────────────

test_that("plot_latest_timepoint stops when data$meta is not a list", {
  input <- make_plot_latest_input()
  input$meta <- "not_a_list"
  expect_error(
    plot_latest_timepoint(input),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})

test_that("plot_latest_timepoint stops when data$meta is missing 'variables'", {
  input <- make_plot_latest_input()
  input$meta <- list(data_path = "fake/path")
  expect_error(
    plot_latest_timepoint(input),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})

# ── data$data checks ──────────────────────────────────────────────────────────

test_that("plot_latest_timepoint stops when data$data is not a data frame", {
  input <- make_plot_latest_input()
  input$data <- "not_a_data_frame"
  expect_error(
    plot_latest_timepoint(input),
    "'data\\$data' must be a data.frame"
  )
})

test_that("plot_latest_timepoint stops when data$data is empty", {
  input <- make_plot_latest_input(empty = TRUE)
  expect_error(
    plot_latest_timepoint(input),
    "'data\\$data' must contain at least one row"
  )
})

# ── variables checks ──────────────────────────────────────────────────────────

test_that("plot_latest_timepoint stops when variables is empty", {
  input <- make_plot_latest_input()
  input$meta$variables <- character(0)
  expect_error(
    plot_latest_timepoint(input),
    "'data\\$meta\\$variables' must be a non-empty character vector"
  )
})

test_that("plot_latest_timepoint stops when variables is not character", {
  input <- make_plot_latest_input()
  input$meta$variables <- 1:3
  expect_error(
    plot_latest_timepoint(input),
    "'data\\$meta\\$variables' must be a non-empty character vector"
  )
})

# ── Missing column checks ─────────────────────────────────────────────────────

test_that("plot_latest_timepoint stops when a variable column is missing", {
  input <- make_plot_latest_input()
  input$data$cell.line <- NULL
  expect_error(
    plot_latest_timepoint(input),
    "Missing required columns: cell.line"
  )
})

test_that("plot_latest_timepoint stops when Well column is missing", {
  input <- make_plot_latest_input()
  input$data$Well <- NULL
  expect_error(
    plot_latest_timepoint(input),
    "Missing required columns: Well"
  )
})

test_that("plot_latest_timepoint stops when Hour column is missing", {
  input <- make_plot_latest_input()
  input$data$Hour <- NULL
  expect_error(
    plot_latest_timepoint(input),
    "Missing required columns: Hour"
  )
})

test_that("plot_latest_timepoint stops when Plate.Type column is missing", {
  input <- make_plot_latest_input()
  input$data$Plate.Type <- NULL
  expect_error(
    plot_latest_timepoint(input),
    "Missing required columns: Plate.Type"
  )
})

test_that("plot_latest_timepoint stops when diff_OD_590_740 column is missing", {
  input <- make_plot_latest_input()
  input$data$diff_OD_590_740 <- NULL
  expect_error(
    plot_latest_timepoint(input),
    "Missing required columns: diff_OD_590_740"
  )
})

test_that("plot_latest_timepoint error lists all missing columns at once", {
  input <- make_plot_latest_input()
  input$data$Well <- NULL
  input$data$Hour <- NULL
  expect_error(
    plot_latest_timepoint(input),
    "Missing required columns"
  )
})

# ── Return type ───────────────────────────────────────────────────────────────

test_that("plot_latest_timepoint returns a named list", {
  input <- make_plot_latest_input()
  result <- plot_latest_timepoint(input)
  expect_type(result, "list")
  expect_false(is.null(names(result)))
})

test_that("plot_latest_timepoint returns ggplot objects", {
  input <- make_plot_latest_input()
  result <- plot_latest_timepoint(input)
  expect_true(all(sapply(result, inherits, "ggplot")))
})

# ── One plot per condition ────────────────────────────────────────────────────

test_that("plot_latest_timepoint returns one plot for one condition", {
  input <- make_plot_latest_input()
  result <- plot_latest_timepoint(input)
  expect_length(result, 1)
})

test_that("plot_latest_timepoint returns two plots for two conditions", {
  input <- make_plot_latest_input()
  extra <- input$data
  extra$cell.line <- "A549"
  input$data <- rbind(input$data, extra)

  result <- plot_latest_timepoint(input)
  expect_length(result, 2)
})

test_that("plot_latest_timepoint names match group_id pattern", {
  input <- make_plot_latest_input()
  result <- plot_latest_timepoint(input)
  # group_id is variables + Plate.Type joined by "_"
  expect_true(all(grepl("H460_CT_0h_PM-M1", names(result))))
})

# ── Latest timepoint extraction ───────────────────────────────────────────────

test_that("plot_latest_timepoint only keeps the latest time point per well", {
  input <- make_plot_latest_input(n_timepoints = 3) # hours: 0, 1, 2
  result <- plot_latest_timepoint(input)

  # Extract underlying plot data from first plot
  plot_data <- result[[1]]$data
  expect_true(all(plot_data$Hour == max(plot_data$Hour)))
})

test_that("plot_latest_timepoint has one row per well in plot data", {
  input <- make_plot_latest_input(n_timepoints = 5)
  result <- plot_latest_timepoint(input)

  plot_data <- result[[1]]$data
  n_wells <- length(unique(plot_data$Well))
  expect_equal(nrow(plot_data), n_wells)
})

# ── Well coordinate conversion ────────────────────────────────────────────────

test_that("plot_latest_timepoint creates 'category' column from well row letter", {
  input <- make_plot_latest_input()
  result <- plot_latest_timepoint(input)
  plot_data <- result[[1]]$data

  expect_true("category" %in% names(plot_data))
  expect_true(all(plot_data$category %in% LETTERS[1:8]))
})

test_that("plot_latest_timepoint creates numeric 'number' column from well column", {
  input <- make_plot_latest_input()
  result <- plot_latest_timepoint(input)
  plot_data <- result[[1]]$data

  expect_true("number" %in% names(plot_data))
  expect_true(is.numeric(plot_data$number))
  expect_true(all(plot_data$number %in% 1:12))
})

# ── Single variable edge case ─────────────────────────────────────────────────

test_that("plot_latest_timepoint works when variables has only one element", {
  input <- make_plot_latest_input()
  input$meta$variables <- "cell.line"
  expect_type(plot_latest_timepoint(input), "list")
})
