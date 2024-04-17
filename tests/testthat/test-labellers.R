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
