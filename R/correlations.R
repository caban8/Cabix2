single_cor <- function (x, y, method = "auto") {
  nor.x <- shapiro.test(x)
  nor.y <- shapiro.test(y)
  result <- if (method == "pearson") {
    cor.test(x, y, method = "pearson")
  }
  else if (method == "spearman") {
    cor.test(x, y, method = "spearman", exact = F)
  }
  else {
    if (nor.x$p.value > 0.05 & nor.y$p.value > 0.05) {
      cor.test(x, y, method = "pearson")
    }
    else {
      cor.test(x, y, method = "spearman", exact = F)
    }
  }
  stat <- round(result$estimate, 2)
  stat <- formatC(stat, format = "f", digits = 2, decimal.mark = ",")
  p <- result$p.value
  result1 <- if (method == "auto") {
    if (nor.x$p.value > 0.05 & nor.y$p.value > 0.05) {
      paste0(stat, "\U002B3")
    }
    else {
      paste0(stat, "\U002B3\U002B0\U01D52")
    }
  }
  else {
    stat
  }
  result2 <- if (p <= 0.05 & p > 0.01) {
    paste(result1, "*", sep = "")
  }
  else if (p <= 0.01 & p > 0.001) {
    paste(result1, "**", sep = "")
  }
  else if (p <= 0.001) {
    paste(result1, "***", sep = "")
  }
  else {
    result1
  }
  result2
}


#' Compute correlations APA formatted table
#'
#' cor_tab() runs multiple correlations. The coefficients can be either chosen manually
#' or automatically, based on the normality assumption criterion, which, in turn, is
#' verified in the dark with Shapiro-Wilk test.
#'
#' The results are presented in an APA-like table with asteriks signalling statistical
#' significance. If an automatic choice of coefficients was selected, each estimate is
#' additionally accompanied with a superscript representing the chosen correlation
#' coefficient.
#'
#' @param X,Y	a pair of variables' sets to be correlated against one another
#' @param Which coefficient should be used. The default is to auto
#'
#' @returns a data frame
#' @examples
#' cor_tab(mtcars, X = c(mpg, disp, hp), Y = c(drat, wt, qsec))
#' cor_tab(mtcars, X = c(mpg, disp, hp), Y = c(drat, wt, qsec), method = "pearson")
#' cor_tab(mtcars, X = c(mpg, disp, hp), Y = c(drat, wt, qsec), method = "spearman")
#'
#' @export
cor_tab <- function (df, X, Y, method = "auto", spss.lab = T,
                       labels. = c(NULL, NULL)) {
  df1 <- dplyr::select(df, {{X}})
  df2 <- dplyr::select(df, {{Y}})
  labs1 <- var_labels(df1, {{X}}, spss.lab = spss.lab, labels. = labels.[[1]])
  labs2 <- var_labels(df2, {{Y}}, spss.lab = spss.lab, labels. = labels.[[2]])

  korelacje <- data.frame(matrix(nrow = length(df1), ncol = length(df2)))
  for (i in 1:length(df1)) {
    for (j in 1:length(df2)) {
      korelacje[i, j] <- single_cor(df1[[i]], df2[[j]],
                                    method = method)
    }
  }
  korelacje <- cbind(labs1, korelacje)
  names(korelacje) <- c(" ", labs2)
  korelacje
}
