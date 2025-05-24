test_that("preparate_paths function returns directory names correctly", {


  x <- c("path/to/folder1", "path/to/folder2", "path/to/folder3")
  expected <- c("folder1", "folder2", "folder3")
  expect_equal(preparate_paths(x), expected)


  x <- "path/to/onlyfolder"
  expected <- "onlyfolder"
  expect_equal(preparate_paths(x), expected)

})
