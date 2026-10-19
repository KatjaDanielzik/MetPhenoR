# Tests ####

test_that("read_folder returns a list with correct structure", {
  # Use test_data() to access files
  test_dir <- file.path(test_path(), "data", "matching")
  non_dir <- file.path(test_path(), "data", "non_matching")
  result <- read_folder(data_path = test_dir, 
                        sample_column = "Sample", 
                        variables = c("cell.line", "radiation.dose", "time.point"))
  expect_type(result, "list")
  expect_named(result, c("data", "meta"))
  expect_s3_class(result$data, "data.frame")
  expect_named(result$meta, c("data_path", "sample_column", "variables"))
  expect_equal(result$meta$sample_column,"Sample")
  expect_equal(result$meta$variables,c("cell.line","radiation.dose","time.point"))
})

test_that("read_folder warns about excluded PlateIds with correct info", {
  test_dir <-file.path(test_path(), "data", "matching")
  non_dir <- file.path(test_path(), "data", "non_matching")
    expect_warning(read_folder(data_path = non_dir , 
                          sample_column = "Sample", 
                          variables = c("cell.line", "radiation.dose", "time.point")),
                 "The following 'sample_column' were found in either 'PlateInfo.csv' or 'RawReads.csv' files but not in both, and were excluded:
  - H460_CT_0Gy
  - H460_CT_4Gy
Proceeding with only the overlapping PlateIds.") 

})

test_that("read_folder gives message when all PlateIds match", {
  test_dir <-file.path(test_path(), "data", "matching")
  expect_message(read_folder(data_path = test_dir, sample_column = "Sample",
                                   variables = c("cell.line", "radiation.dose", "time.point")),
                       "All PlateIds were successfully matched between PlateInfo and RawReads.")
})

test_that("read_folder fails when data_path is invalid", {
  test_dir <-file.path(test_path(), "data", "matching")
  non_dir <- file.path(test_path(), "data", "non_matching")
  expect_error(read_folder(data_path = "/nonexistent/path", sample_column = "Description", variables = c("a", "b")),
               "data_path does not exist or is not a valid directory")
})

test_that("read_folder fails when sample_column is not in PlateInfo", {
  test_dir <-file.path(test_path(), "data", "matching")
  non_dir <- file.path(test_path(), "data", "non_matching")
  expect_error(read_folder(data_path = test_dir, sample_column = "NonExistent", variables = c("a", "b")),
               "'sample_column' must be a column in 'PlateInfo.csv' files")
})

test_that("read_folder fails when variables is not character", {
  test_dir <-file.path(test_path(), "data", "matching")
  non_dir <- file.path(test_path(), "data", "non_matching")
  expect_error(read_folder(data_path = test_dir, sample_column = "Description", variables = 1:3),
               "All elements in variables must be character strings")
})

test_that("read_folder fails when sample_column is not character", {
  test_dir <-file.path(test_path(), "data", "matching")
  non_dir <- file.path(test_path(), "data", "non_matching")
  expect_error(read_folder(data_path = test_dir, sample_column = 1, variables = c("a", "b")),
               "'sample_column' must be a character string")
})

test_that("read_folder correctly separates Description into variables", {
  test_dir <-file.path(test_path(), "data", "matching")
  non_dir <- file.path(test_path(), "data", "non_matching")
  result <- read_folder(data_path = test_dir, sample_column = "Sample", variables = c("cell.line", "radiation.dose", "time.point"))
  
  expect_equal(result$data$cell.line[1], "H460")
  expect_equal(result$data$radiation.dose[1], "CT")
  expect_equal(result$data$time.point[1],"0Gy")
})

test_that("read_folder handles empty variables vector", {
  test_dir <-file.path(test_path(), "data", "matching")
  non_dir <- file.path(test_path(), "data", "non_matching")
  expect_error(read_folder(data_path = test_dir, sample_column = "Sample", variables = character(0)),
               "'variables' must contain at least one element")
})

test_that("read_folder preserves all required columns in output", {
  test_dir <- file.path(test_path(), "data", "matching")
  result <- read_folder(data_path = test_dir, 
                        sample_column = "Sample", 
                        variables = c("cell.line", "radiation.dose", "time.point"))
  
  required_cols <- c(
    "PlateId", "Project.Name", "Project.Code", "Plate.Type", "Sample",
    "Loaded.At", "Incubation.Hours", "Incubation.Temperature.Celsius",
    "Incubated.Offline", "Cycle.Time.Minutes", "Position", "Protocol",
    "Description", "Actual.Temperature.Celsius", "Target.Temperature.Celsius",
    "Read.At", "Hour", "Well", "diff_OD_590_740"
  )
  
  expect_true(all(required_cols %in% names(result$data)))
})

