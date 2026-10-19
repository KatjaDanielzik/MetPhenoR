make_filter_input <- function(empty = FALSE) {
  
  base <- expand.grid(
    Hour           = seq(0, 5),
    Well           = c("A01", "A02"),
    Plate.Type     = c("PM-M1", "PM-M2"),
    cell.line      = "H460",
    radiation.dose = c("0Gy", "4Gy"),
    stringsAsFactors = FALSE
  )
  base$diff_OD_590_740 <- runif(nrow(base))
  
  df <- if (empty) base[0, ] else base
  
  list(
    data = df,
    meta = list(
      data_path     = "fake/path",
      sample_column = "Sample",
      variables     = c("cell.line", "radiation.dose")
    )
  )
}

# ── Top-level structure checks ────────────────────────────────────────────────

test_that("filter_data stops when data is not a list", {
  expect_error(
    filter_data("not_a_list", plate_type = "PM-M1"),
    "must be a named list with elements 'data' and 'meta'"
  )
})

test_that("filter_data stops when 'data' element is missing", {
  expect_error(
    filter_data(list(meta = list(variables = "cell.line")), plate_type = "PM-M1"),
    "must be a named list with elements 'data' and 'meta'"
  )
})

test_that("filter_data stops when 'meta' element is missing", {
  expect_error(
    filter_data(list(data = data.frame(x = 1)), plate_type = "PM-M1"),
    "must be a named list with elements 'data' and 'meta'"
  )
})

# ── meta structure checks ─────────────────────────────────────────────────────

test_that("filter_data stops when data$meta is not a list", {
  input      <- make_filter_input()
  input$meta <- "not_a_list"
  expect_error(
    filter_data(input, plate_type = "PM-M1"),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})

test_that("filter_data stops when data$meta is missing 'variables'", {
  input      <- make_filter_input()
  input$meta <- list(data_path = "fake/path")
  expect_error(
    filter_data(input, plate_type = "PM-M1"),
    "'data\\$meta' must be a named list containing 'variables'"
  )
})

# ── data$data checks ──────────────────────────────────────────────────────────

test_that("filter_data stops when data$data is not a data frame", {
  input      <- make_filter_input()
  input$data <- "not_a_data_frame"
  expect_error(
    filter_data(input, plate_type = "PM-M1"),
    "'data\\$data' must be a data.frame"
  )
})

test_that("filter_data stops when data$data is empty", {
  input <- make_filter_input(empty = TRUE)
  expect_error(
    filter_data(input, plate_type = "PM-M1"),
    "'data\\$data' must contain at least one row"
  )
})

# ── variables checks ──────────────────────────────────────────────────────────

test_that("filter_data stops when variables is empty", {
  input                <- make_filter_input()
  input$meta$variables <- character(0)
  expect_error(
    filter_data(input, plate_type = "PM-M1"),
    "'data\\$meta\\$variables' must be a non-empty character vector"
  )
})

test_that("filter_data stops when variables is not character", {
  input                <- make_filter_input()
  input$meta$variables <- 1:3
  expect_error(
    filter_data(input, plate_type = "PM-M1"),
    "'data\\$meta\\$variables' must be a non-empty character vector"
  )
})

# ── Plate.Type column check ───────────────────────────────────────────────────

test_that("filter_data stops when Plate.Type column is missing from data$data", {
  input                 <- make_filter_input()
  input$data$Plate.Type <- NULL
  expect_error(
    filter_data(input, plate_type = "PM-M1"),
    "'Plate.Type' column not found in data\\$data"
  )
})

# ── plate_type argument checks ────────────────────────────────────────────────

test_that("filter_data stops when plate_type is missing", {
  input <- make_filter_input()
  expect_error(
    filter_data(input),
    "'plate_type' must be a non-empty character vector"
  )
})

test_that("filter_data stops when plate_type is NULL", {
  input <- make_filter_input()
  expect_error(
    filter_data(input, plate_type = NULL),
    "'plate_type' must be a non-empty character vector"
  )
})

test_that("filter_data stops when plate_type is empty character", {
  input <- make_filter_input()
  expect_error(
    filter_data(input, plate_type = character(0)),
    "'plate_type' must be a non-empty character vector"
  )
})

test_that("filter_data stops when plate_type is not character", {
  input <- make_filter_input()
  expect_error(
    filter_data(input, plate_type = 1),
    "'plate_type' must be a character vector"
  )
})

test_that("filter_data warns when plate_type is not found in data", {
  input <- make_filter_input()
  expect_warning(
    filter_data(input, plate_type = "PM-WRONG",
                filters = list(cell.line = "H460")),
    "not found in 'data\\$data\\$Plate.Type'"
  )
})

# ── filters argument checks ───────────────────────────────────────────────────

test_that("filter_data accepts NULL filters without error", {
  input <- make_filter_input()
  expect_no_error(
    filter_data(input, plate_type = "PM-M1", filters = NULL)
  )
})

test_that("filter_data stops when filters is not a list", {
  input <- make_filter_input()
  expect_error(
    filter_data(input, plate_type = "PM-M1", filters = "cell.line=H460"),
    "'filters' must be a named list or NULL"
  )
})

test_that("filter_data stops when filters is an empty list", {
  input <- make_filter_input()
  expect_error(
    filter_data(input, plate_type = "PM-M1", filters = list()),
    "'filters' must contain at least one element when not NULL"
  )
})

test_that("filter_data stops when filters has unnamed elements", {
  input <- make_filter_input()
  expect_error(
    filter_data(input, plate_type = "PM-M1", filters = list("H460")),
    "All elements of 'filters' must be named"
  )
})

test_that("filter_data stops when filters has partially unnamed elements", {
  input <- make_filter_input()
  expect_error(
    filter_data(input, plate_type = "PM-M1",
                filters = list(cell.line = "H460", "0Gy")),
    "All elements of 'filters' must be named"
  )
})

test_that("filter_data stops when filter name is not in variables", {
  input <- make_filter_input()
  expect_error(
    filter_data(input, plate_type = "PM-M1",
                filters = list(nonexistent = "value")),
    "not in 'data\\$meta\\$variables'"
  )
})

test_that("filter_data stops when filter value is not a single character string", {
  input <- make_filter_input()
  expect_error(
    filter_data(input, plate_type = "PM-M1",
                filters = list(cell.line = c("H460", "A549"))),
    "Each element of 'filters' must be a single character string"
  )
})

test_that("filter_data stops when filter value is numeric", {
  input <- make_filter_input()
  expect_error(
    filter_data(input, plate_type = "PM-M1",
                filters = list(cell.line = 1)),
    "Each element of 'filters' must be a single character string"
  )
})

# ── Return structure ──────────────────────────────────────────────────────────

test_that("filter_data returns a list", {
  input  <- make_filter_input()
  result <- filter_data(input, plate_type = "PM-M1",
                        filters = list(cell.line = "H460"))
  expect_type(result, "list")
})

test_that("filter_data returns list with 'data' and 'meta' elements", {
  input  <- make_filter_input()
  result <- filter_data(input, plate_type = "PM-M1",
                        filters = list(cell.line = "H460"))
  expect_true(all(c("data", "meta") %in% names(result)))
})

test_that("filter_data result$data is a data frame", {
  input  <- make_filter_input()
  result <- filter_data(input, plate_type = "PM-M1",
                        filters = list(cell.line = "H460"))
  expect_s3_class(result$data, "data.frame")
})

test_that("filter_data preserves meta unchanged", {
  input  <- make_filter_input()
  result <- filter_data(input, plate_type = "PM-M1",
                        filters = list(cell.line = "H460"))
  expect_identical(result$meta, input$meta)
})

# ── Filtering correctness ─────────────────────────────────────────────────────

test_that("filter_data removes rows matching plate_type and filters", {
  input  <- make_filter_input()
  n_before <- nrow(input$data)
  result <- filter_data(input, plate_type = "PM-M1",
                        filters = list(cell.line = "H460",
                                       radiation.dose = "0Gy"))
  n_after <- nrow(result$data)
  expect_lt(n_after, n_before)
})

test_that("filter_data does not remove rows from other plate types", {
  input  <- make_filter_input()
  result <- filter_data(input, plate_type = "PM-M1",
                        filters = list(cell.line = "H460",
                                       radiation.dose = "0Gy"))
  # PM-M2 rows should be untouched
  expect_true("PM-M2" %in% result$data$Plate.Type)
})

test_that("filter_data with NULL filters removes all rows of plate_type", {
  input  <- make_filter_input()
  result <- filter_data(input, plate_type = "PM-M1", filters = NULL)
  expect_false("PM-M1" %in% result$data$Plate.Type)
})

test_that("filter_data with non-matching filter value removes no rows", {
  input    <- make_filter_input()
  n_before <- nrow(input$data)
  result   <- filter_data(input, plate_type = "PM-M1",
                          filters = list(cell.line = "NONEXISTENT"))
  expect_equal(nrow(result$data), n_before)
})

