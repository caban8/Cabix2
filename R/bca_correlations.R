# Calculate rank biserial correlation using bca method
cor_bca <- function(x, g, method = "rank_biserial", R = 1000, na.rm = T) {

  # Create a data.frame for an easier NA control
  df <- data.frame(x = x, g = g)

  # Should remove NAs
  if (na.rm) {df <- tidyr::drop_na(df)}



  # Obtain correlation with bca confidence interval
  result <- rcompanion::wilcoxonRG(g = df$g, x = df$x, ci = T, R = R) %>%
    round_df(lower.ci, upper.ci, rg, digits =  2) %>%
    dplyr::mutate(
      result = paste0(rg, " (", lower.ci, "; ", upper.ci, ")"),
      result = dplyr::if_else(
        (lower.ci < 0 & upper.ci) < 0 | (lower.ci > 0 & upper.ci > 0),
        paste0(result, "*"),
        result
      )
    ) %>%
    dplyr::pull()

  return(result)
}


#' Obtain a rank biserial correlation table
#'
#' @param X ordinal variables
#' @param G grouping binary variable
#' @param R number of boostrap samples
#'
#'
#' @export
rbcor_tab <- function(
    df,
    X,
    G,
    method = "rank_biserial",
    spss.lab = T,
    labels. = c(NULL, NULL),
    R = 1000,
    na.rm = T
    ) {

  # Extract ordinal variables
  df1 <- dplyr::select(df, {{X}})

  # Extract group variables
  df2 <- dplyr::select(df, {{G}})

  # Extract labels for the row variables
  labs1 <- var_labels(df1, {{X}}, spss.lab = spss.lab, labels. = labels.[[1]])

  # Extract labels for the column variables
  labs2 <- var_labels(df2, {{G}}, spss.lab = spss.lab, labels. = labels.[[2]])


  # Extract ordinal variables
  df1 <- purrr::map_df(df1, as.double)

  # Extract group variables
  df2 <- purrr::map_df(df2, as.double)


  # Create a matrix on which the correlation coefficients will be placed
  korelacje <- data.frame(matrix(nrow = length(df1), ncol = length(df2)))

  # Run all correlation analyses
  for (i in 1:length(df1)) {
    for (j in 1:length(df2)) {
      korelacje[i, j] <- cor_bca(x = df1[[i]], g = df2[[j]], method = method, R = R, na.rm = na.rm)
    }
  }

  korelacje <- cbind(labs1, korelacje)
  names(korelacje) <- c(" ", labs2)

  return(korelacje)
}
