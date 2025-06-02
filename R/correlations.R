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



#' Compute correlations separately by different groups
#'
#' This is a wrapper function for cor_tab() that allows to run multiple correlations
#' for each group separately.
#'
#' @param df A data frame containing the data to be analyzed.
#' @param X,Y A pair of variables' sets to be correlated against one another.
#' @param method The correlation method to be used. Default is "auto", which chooses between Pearson and Spearman based on normality tests.
#' @param spss.lab Logical indicating whether to use SPSS-style variable labels.
#' @param labels. A character vector of length two for custom labels for the X and Y variables.
#' @param grouping The name of the grouping variable in the data frame.
#' This variable is used to split the data into groups for separate correlation analyses.
#'
#' @returns A list of data frames, each containing the correlation results for a specific group.
#'
#' @export
cor_tab_groups <- function(
    df, X, Y, method = "auto", spss.lab = T,
    labels. = c(NULL, NULL), grouping, simplify = TRUE
) {


  if (haven::is.labelled(df[[grouping]])) {
    df[[grouping]] <- haven::as_factor(df[[grouping]])
  } else {
    df[[grouping]] <- as.factor(df[[grouping]])
  }

  groups <- levels(df[[grouping]])
  .l <- vector("list", length(groups))


  for (i in seq_along(groups)) {
    df_group <- df %>%
      dplyr::filter(!!rlang::sym(grouping) == groups[[i]])

    .l[[i]] <- cor_tab(
      df_group, X = {{X}}, Y = {{Y}}, method = method,
      spss.lab = spss.lab, labels. = labels.
    )

  }


  result <- .l %>%
    purrr::set_names(groups) %>%
    cor_simplify(simplify = simplify)




  return(result)


}



# helper cor_tab_groups ---------------------------------------------------



cor_simplify <- function(result, simplify) {



  if (simplify) {result <- result %>%
    purrr::imap(~row_labs(.x, .y, 1)) %>%
    purrr::reduce(dplyr::add_row)}

  return(result)
}




