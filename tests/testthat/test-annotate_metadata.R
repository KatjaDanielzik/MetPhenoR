# ── Helpers ───────────────────────────────────────────────────────────────────

make_annotate_meta_input <- function(empty            = FALSE,
                                     na_chemical      = FALSE,
                                     missing_col      = NULL) {
  
  df <- data.frame(
    Well       = c("A01", "A02", "A03"),
    Plate.Type = "PM-M1",
    Chemical   = c("DrugA", "DrugB", "DrugC"),
    AUC        = runif(3, 0.1, 1.0),
    AUC_ratio  = runif(3, 0.5, 2.0),
    stringsAsFactors = FALSE
  )
  
  if (na_chemical) df$Chemical <- NA_character_
  if (!is.null(missing_col)) df[[missing_col]] <- NULL
  
  list(
    auc_ratio = if (empty) df[0, ] else df,
    meta = list(
      data_path     = "fake/path",
      sample_column = "Sample",
      variables     = "cell.line"
    )
  )
}

make_annotation <- function(chemicals = c("DrugA", "DrugB", "DrugC"),
                            extra_col = TRUE) {
  df <- data.frame(
    Chemical = chemicals,
    stringsAsFactors = FALSE
  )
  if (extra_col) df$pathway <- paste0("pathway_", seq_along(chemicals))
  df
}

# ── Top-level structure checks ────────────────────────────────────────────────

test_that("annotate_metadata stops when data is not a list", {
  expect_error(
    annotate_metadata("not_a_list", make_annotation()),
    "must be a named list containing an 'auc_ratio' element"
  )
})

test_that("annotate_metadata stops when data is a data frame", {
  expect_error(
    annotate_metadata(data.frame(x = 1), make_annotation()),
    "must be a named list containing an 'auc_ratio' element"
  )
})

test_that("annotate_metadata stops when auc_ratio element is missing", {
  expect_error(
    annotate_metadata(list(meta = list()), make_annotation()),
    "must be a named list containing an 'auc_ratio' element"
  )
})

# ── auc_ratio checks ──────────────────────────────────────────────────────────

test_that("annotate_metadata stops when auc_ratio is not a data frame", {
  input             <- make_annotate_meta_input()
  input$auc_ratio   <- "not_a_df"
  expect_error(
    annotate_metadata(input, make_annotation()),
    "'data\\$auc_ratio' must be a data.frame"
  )
})

test_that("annotate_metadata stops when auc_ratio is empty", {
  input <- make_annotate_meta_input(empty = TRUE)
  expect_error(
    annotate_metadata(input, make_annotation()),
    "'data\\$auc_ratio' must contain at least one row"
  )
})

test_that("annotate_metadata stops when Chemical is missing from auc_ratio", {
  input <- make_annotate_meta_input(missing_col = "Chemical")
  expect_error(
    annotate_metadata(input, make_annotation()),
    "missing required columns: Chemical"
  )
})

test_that("annotate_metadata stops when Plate.Type is missing from auc_ratio", {
  input <- make_annotate_meta_input(missing_col = "Plate.Type")
  expect_error(
    annotate_metadata(input, make_annotation()),
    "missing required columns: Plate.Type"
  )
})

test_that("annotate_metadata stops when Well is missing from auc_ratio", {
  input <- make_annotate_meta_input(missing_col = "Well")
  expect_error(
    annotate_metadata(input, make_annotation()),
    "missing required columns: Well"
  )
})

# ── annotation type checks ────────────────────────────────────────────────────

test_that("annotate_metadata stops when annotation is not a data frame or list", {
  input <- make_annotate_meta_input()
  expect_error(
    annotate_metadata(input, "not_a_df"),
    "'annotation' must be a data.frame or a list of data.frames"
  )
})

test_that("annotate_metadata stops when annotation list contains non-data frames", {
  input <- make_annotate_meta_input()
  expect_error(
    annotate_metadata(input, list(make_annotation(), "not_a_df")),
    "'annotation' must be a data.frame or a list of data.frames"
  )
})

# ── annotation column checks ──────────────────────────────────────────────────

test_that("annotate_metadata stops when annotation is missing Chemical column", {
  input <- make_annotate_meta_input()
  ann   <- make_annotation()
  ann$Chemical <- NULL
  expect_error(
    annotate_metadata(input, ann),
    "missing required columns: Chemical"
  )
})

test_that("annotate_metadata identifies which annotation table has missing column", {
  input <- make_annotate_meta_input()
  ann1  <- make_annotation()
  ann2  <- make_annotation()
  ann2$Chemical <- NULL
  expect_error(
    annotate_metadata(input, list(ann1, ann2)),
    "Annotation table 2"
  )
})

# ── Warnings ──────────────────────────────────────────────────────────────────

test_that("annotate_metadata warns when annotation is empty after deduplication", {
  input <- make_annotate_meta_input()
  ann   <- make_annotation()[0, ]
  expect_warning(
    annotate_metadata(input, ann),
    "No annotation data was provided"
  )
})

test_that("annotate_metadata warns when Chemical column in auc_ratio is all NA", {
  input <- make_annotate_meta_input(na_chemical = TRUE)
  expect_warning(
    annotate_metadata(input, make_annotation()),
    "Chemical.*column.*no valid values"
  )
})

test_that("annotate_metadata warns when chemicals not found in annotation", {
  input <- make_annotate_meta_input()
  ann   <- make_annotation(chemicals = c("DrugA"))  # DrugB and DrugC missing
  expect_warning(
    annotate_metadata(input, ann),
    "not found in the annotation table"
  )
})

# ── Return structure ──────────────────────────────────────────────────────────

test_that("annotate_metadata returns a list", {
  input  <- make_annotate_meta_input()
  result <- annotate_metadata(input, make_annotation())
  expect_type(result, "list")
})

test_that("annotate_metadata returns updated auc_ratio element", {
  input  <- make_annotate_meta_input()
  result <- annotate_metadata(input, make_annotation())
  expect_true("auc_ratio" %in% names(result))
  expect_s3_class(result$auc_ratio, "data.frame")
})

test_that("annotate_metadata adds annotation columns to auc_ratio", {
  input  <- make_annotate_meta_input()
  result <- annotate_metadata(input, make_annotation(extra_col = TRUE))
  expect_true("pathway" %in% names(result$auc_ratio))
})

test_that("annotate_metadata correctly merges on Chemical", {
  input  <- make_annotate_meta_input()
  ann    <- make_annotation()
  result <- annotate_metadata(input, ann)
  
  expect_equal(
    result$auc_ratio$pathway[result$auc_ratio$Chemical == "DrugA"],
    "pathway_1"
  )
})

test_that("annotate_metadata accepts a list of annotation data frames", {
  input  <- make_annotate_meta_input()
  ann1   <- make_annotation(chemicals = "DrugA")
  ann2   <- make_annotation(chemicals = c("DrugB", "DrugC"))
  result <- suppressWarnings(annotate_metadata(input, list(ann1, ann2)))
  expect_true("pathway" %in% names(result$auc_ratio))
})

test_that("annotate_metadata deduplicates combined annotation rows", {
  input  <- make_annotate_meta_input()
  ann    <- make_annotation()
  # passing the same annotation twice should not duplicate rows
  result <- annotate_metadata(input, list(ann, ann))
  expect_equal(
    nrow(result$auc_ratio),
    nrow(input$auc_ratio)
  )
})

