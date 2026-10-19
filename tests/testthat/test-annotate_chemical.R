# ── Helpers ───────────────────────────────────────────────────────────────────

make_annotate_input <- function() {
  df <- expand.grid(
    Well = c("A01", "A02", "A03"),
    Plate.Type = c("PM-M1"),
    Hour = c(0, 1, 2),
    stringsAsFactors = FALSE
  )
  df$diff_OD_590_740 <- runif(nrow(df))

  list(
    data = df,
    meta = list(
      data_path     = "fake/path",
      sample_column = "Sample",
      variables     = c("cell.line")
    )
  )
}

make_annotation <- function() {
  data.frame(
    Plate.Type = "PM-M1",
    Well = c("A01", "A02", "A03"),
    Chemical = c("Negative Control", "a-D-Glucose", "DrugX"),
    stringsAsFactors = FALSE
  )
}

# ── Top-level structure checks ────────────────────────────────────────────────

test_that("annotate_chemical stops when data is not a list", {
  expect_error(
    annotate_chemical("not_a_list", make_annotation()),
    "must be a named list with elements 'data' and 'meta'"
  )
})

test_that("annotate_chemical stops when data is missing 'data' element", {
  expect_error(
    annotate_chemical(list(meta = list()), make_annotation()),
    "must be a named list with elements 'data' and 'meta'"
  )
})

test_that("annotate_chemical stops when data is missing 'meta' element", {
  expect_error(
    annotate_chemical(list(data = data.frame(x = 1)), make_annotation()),
    "must be a named list with elements 'data' and 'meta'"
  )
})

# ── data$data checks ──────────────────────────────────────────────────────────

test_that("annotate_chemical stops when data$data is not a data frame", {
  input <- make_annotate_input()
  input$data <- "not_a_data_frame"
  expect_error(
    annotate_chemical(input, make_annotation()),
    "'data\\$data' must be a data.frame"
  )
})

test_that("annotate_chemical stops when data$data is missing Plate.Type", {
  input <- make_annotate_input()
  input$data$Plate.Type <- NULL
  expect_error(
    annotate_chemical(input, make_annotation()),
    "'data\\$data' is missing required columns: Plate.Type"
  )
})

test_that("annotate_chemical stops when data$data is missing Well", {
  input <- make_annotate_input()
  input$data$Well <- NULL
  expect_error(
    annotate_chemical(input, make_annotation()),
    "'data\\$data' is missing required columns: Well"
  )
})

# ── annotation input checks ───────────────────────────────────────────────────

test_that("annotate_chemical stops when annotation is not a data frame or list", {
  input <- make_annotate_input()
  expect_error(
    annotate_chemical(input, annotation = "not_a_df"),
    "'annotation' must be a data.frame or a list of data.frames"
  )
})

test_that("annotate_chemical stops when annotation is a list containing non-data frames", {
  input <- make_annotate_input()
  expect_error(
    annotate_chemical(input, annotation = list(make_annotation(), "not_a_df")),
    "'annotation' must be a data.frame or a list of data.frames"
  )
})

test_that("annotate_chemical stops when annotation is missing Plate.Type", {
  input <- make_annotate_input()
  ann <- make_annotation()
  ann$Plate.Type <- NULL
  expect_error(
    annotate_chemical(input, annotation = ann),
    "missing required columns: Plate.Type"
  )
})

test_that("annotate_chemical stops when annotation is missing Well", {
  input <- make_annotate_input()
  ann <- make_annotation()
  ann$Well <- NULL
  expect_error(
    annotate_chemical(input, annotation = ann),
    "missing required columns: Well"
  )
})

test_that("annotate_chemical stops when annotation is missing Chemical", {
  input <- make_annotate_input()
  ann <- make_annotation()
  ann$Chemical <- NULL
  expect_error(
    annotate_chemical(input, annotation = ann),
    "missing required columns: Chemical"
  )
})

test_that("annotate_chemical identifies which annotation table has missing columns", {
  input <- make_annotate_input()
  ann1 <- make_annotation()
  ann2 <- make_annotation()
  ann2$Chemical <- NULL
  expect_error(
    annotate_chemical(input, annotation = list(ann1, ann2)),
    "Annotation table 2"
  )
})

test_that("annotate_chemical warns when annotation has no rows after deduplication", {
  input <- make_annotate_input()
  ann <- make_annotation()[0, ] # empty data frame, correct columns
  expect_warning(
    annotate_chemical(input, annotation = ann),
    "No annotation data was provided"
  )
})

# ── control_map checks ────────────────────────────────────────────────────────

test_that("annotate_chemical stops when control_map is not a named list", {
  input <- make_annotate_input()
  expect_error(
    annotate_chemical(input, make_annotation(), control_map = c("a", "b")),
    "'control_map' must be a named list"
  )
})

test_that("annotate_chemical stops when control_map is an unnamed list", {
  input <- make_annotate_input()
  expect_error(
    annotate_chemical(input, make_annotation(), control_map = list(c("a"), c("b"))),
    "'control_map' must be a named list"
  )
})

test_that("annotate_chemical stops when control_map values are not character vectors", {
  input <- make_annotate_input()
  expect_error(
    annotate_chemical(input, make_annotation(), control_map = list(negative = 1:3)),
    "All elements of 'control_map' must be character vectors"
  )
})

# ── Return structure ──────────────────────────────────────────────────────────

test_that("annotate_chemical returns a list", {
  input <- make_annotate_input()
  result <- annotate_chemical(input, make_annotation())
  expect_type(result, "list")
})

test_that("annotate_chemical returns list with 'data' and 'meta' elements", {
  input <- make_annotate_input()
  result <- annotate_chemical(input, make_annotation())
  expect_named(result, c("data", "meta"))
})

test_that("annotate_chemical returns a data frame in result$data", {
  input <- make_annotate_input()
  result <- annotate_chemical(input, make_annotation())
  expect_s3_class(result$data, "data.frame")
})

# ── Chemical annotation ───────────────────────────────────────────────────────

test_that("annotate_chemical adds Chemical column to result", {
  input <- make_annotate_input()
  result <- annotate_chemical(input, make_annotation())
  expect_true("Chemical" %in% names(result$data))
})

test_that("annotate_chemical correctly assigns chemical names to wells", {
  input <- make_annotate_input()
  result <- annotate_chemical(input, make_annotation())

  well_a01 <- result$data[result$data$Well == "A01" & result$data$Hour == 0, ]
  expect_equal(well_a01$Chemical, "Negative Control")
})

test_that("annotate_chemical accepts a list of annotation data frames", {
  input <- make_annotate_input()
  ann1 <- make_annotation()[1:2, ]
  ann2 <- make_annotation()[3, ]
  result <- annotate_chemical(input, annotation = list(ann1, ann2))
  expect_true("Chemical" %in% names(result$data))
  expect_false(all(is.na(result$data$Chemical)))
})

test_that("annotate_chemical deduplicates combined annotation rows", {
  input <- make_annotate_input()
  ann <- make_annotation()
  # Pass the same annotation twice
  result <- annotate_chemical(input, annotation = list(ann, ann))
  # Chemical should still be assigned correctly with no duplication artefacts
  expect_equal(
    sum(result$data$Well == "A01" & result$data$Hour == 0),
    1
  )
})

# ── Control classification ────────────────────────────────────────────────────

test_that("annotate_chemical adds control column when control_map is provided", {
  input <- make_annotate_input()
  result <- annotate_chemical(input, make_annotation())
  expect_true("control" %in% names(result$data))
})

test_that("annotate_chemical classifies negative control correctly", {
  input <- make_annotate_input()
  result <- annotate_chemical(input, make_annotation())

  neg <- result$data[result$data$Well == "A01" & result$data$Hour == 0, ]
  expect_equal(neg$control, "negative")
})

test_that("annotate_chemical classifies positive control correctly", {
  input <- make_annotate_input()
  result <- annotate_chemical(input, make_annotation())

  pos <- result$data[result$data$Well == "A02" & result$data$Hour == 0, ]
  expect_equal(pos$control, "positive")
})

test_that("annotate_chemical sets control to NA for non-control chemicals", {
  input <- make_annotate_input()
  result <- annotate_chemical(input, make_annotation())

  drug <- result$data[result$data$Well == "A03" & result$data$Hour == 0, ]
  expect_true(is.na(drug$control))
})

test_that("annotate_chemical does not add control column when control_map is NULL", {
  input <- make_annotate_input()
  result <- annotate_chemical(input, make_annotation(), control_map = NULL)
  expect_false("control" %in% names(result$data))
})

# ── Merge warnings ────────────────────────────────────────────────────────────

test_that("annotate_chemical warns when no wells are matched after merge", {
  input <- make_annotate_input()
  ann <- make_annotation()
  ann$Plate.Type <- "PM-WRONG" # force mismatch
  expect_warning(
    annotate_chemical(input, ann),
    "No chemical annotations were successfully merged"
  )
})

test_that("annotate_chemical warns when only some wells are matched", {
  input <- make_annotate_input()
  ann <- make_annotation()[1, ] # only annotate A01
  expect_warning(
    annotate_chemical(input, ann),
    "out of"
  )
})
