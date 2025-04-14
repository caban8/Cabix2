test_that("extract_labels properly matches labels to variables", {

  dat <- data.frame(
    var1 = haven::labelled(c(1, 2, 3), label = "Variable 1"),
    var2 = haven::labelled(c("a", "b", "c"), label = "Variable 2")
    )
  reference <- tibble::tibble(
    Nazwa = c("var1", "var2"),
    Etykieta = c("Variable 1", "Variable 2")
  )

  # Test equal without warning
  expect_equal(extract_labels(dat), reference )


  # Test warning and equal with warning
  dat$var3 <- 3:1
  reference <- tibble::tibble(
    Nazwa = c("var1", "var2", "var3"),
    Etykieta = c("Variable 1", "Variable 2", "NULL")
  )
  expect_warning(extract_labels(dat), "Not all variables are labelled. The codebook will contain NULLs.")
  expect_equal(
    suppressWarnings(extract_labels(dat)),
    reference
    )




})






test_that("var_labels applies proper labels", {

  dat <- data.frame(
    var1 = haven::labelled(c(1, 2, 3), label = "Variable 1"),
    var2 = haven::labelled(c("a", "b", "c"), label = "Variable 2")
    )
  labs <- c("Zmienna1", "Zmienna2")


  expect_equal(var_labels(dat, var1, var2), c("Variable 1", "Variable 2"))
  expect_equal(var_labels(dat, var1, var2, spss.lab = F), c("var1", "var2"))
  expect_equal(var_labels(dat, var1, var2, labels. = labs), c("Zmienna1", "Zmienna2"))

  expect_error(var_labels(dat, var1, var2, labels. = c("Zmienna1")))
  expect_error(var_labels(dat))
})
