
#' Compute basic statistics in a pivot wider version
desc_helper <- function(x, ...) {
  dplyr::select(x, ...)   %>%
    dplyr::summarise(
      dplyr::across(tidyselect::everything(), list(
        s__n = ~n_valid(.),
        s__min = ~min(., na.rm = T),
        s__max = ~max(., na.rm = T),
        s__mean = ~mean(., na.rm = T),
        s__sd = ~sd(., na.rm = T),
        s__median = ~median(., na.rm = T),
        s__skewness = ~skewness(.),
        s__kurtosis = ~kurtosis(.)

      ))
    )}


#'

desc_arrange <- function(df, ...) {

  df %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(ID = dplyr::row_number()) %>%
    dplyr::arrange(ID) %>%
    dplyr::select(-ID)

}


#' Compute n size excluding NAs
n_valid <- function(x) {
  sum(length(x[!is.na(x)]))
}

#' Compute skewness and kurtosis
skewness <- function(x) {
  w=length(x[!is.na(x)]);
  m1=mean(x, na.rm = T);
  m2=sum((x-m1)^2, na.rm = T);
  m3=sum((x-m1)^3, na.rm = T);
  m4=sum((x-m1)^4, na.rm = T);
  s1=sd(x, na.rm = T);
  skew=w*m3/(w-1)/(w-2)/s1^3;
  skew}

kurtosis <- function(z) {
  w=length(z[!is.na(z)]);
  m1=mean(z, na.rm = T);
  m2=sum((z-m1)^2, na.rm = T);
  m3=sum((z-m1)^3, na.rm = T);
  m4=sum((z-m1)^4, na.rm = T);
  s1=sd(z, na.rm = T);
  kurtosis=(w*(w+1)*m4 - 3*m2^2*(w-1)) / ((w-1)*(w-2)*(w-3)*s1^4);
  kurtosis}


postprocess_descriptives <- function(data, stats, index, digits, etykiety) {
  data %>%
    dplyr::mutate(
      dplyr::across(
        .cols = (index + 2):(ncol(stats) + 1),
        ~ round(., digits = digits) %>% formatC(format = "f", digits = digits)
      ),
      p = round(p, 3),
      variable = plyr::mapvalues(variable, from = unique(variable), to = etykiety)
    )
}


#' Compute common statistics and Shapiro-Wilk results
#'
#' `descriptives()` calculates common descriptive statistics with Shapiro-Wilk test results in an APA-like
#' table format.
#'
#'
#' @param df a data frame object
#' @param ... numeric vectors used for computing descriptive statistics
#' @param IV1,IV2 vectors used for grouping of the statistics
#' @param digits integer indicating the number of decimal places to be used in rounding. Default set to 2.
#' Doesn't include the p-value.
#'
#' @returns a data frame containing descriptive statistics and Shapiro-Wilk results
#'
#'
#'
#' @export
descriptives <- function(df, ..., IV1 = NULL, IV2 = NULL, digits = 2) {


  etykiety <- var_labels(df, ...)

  df <- group_by(df, {{IV1}}, {{IV2}})
  index <- ncol(attr(df, "groups"))
  if (length(index) == 0) {index <- 1}




  shapiro <- rstatix::shapiro_test(df, ...)



  stats <- desc_helper(df, ...)
  stats <- pivot_helper(stats, cols = index:ncol(stats))



  results <- dplyr::left_join(stats, shapiro) %>%
    postprocess_descriptives(stats, index, digits, etykiety) %>%
    dplyr::select(variable, tidyselect::everything())

  results <- desc_arrange(results, {{IV1}}, {{IV2}})

  return(results)

}






# -------------------------------------------------------------------------



#' Obtain counts of unique values in each column of a data frame
#'
#' This function calculates the frequency count of unique values in each column of a data frame.
#'
#' @param df A data frame.
#' @param .fun A function to select columns. Default is is.atomic.
#'
#' @return A data frame with variables, counts, percentage, and labels.
#'
#' @examples
#' df <- data.frame(
#'   x = c('a', 'b', 'a', 'c', 'a'),
#'   y = c(1, 2, 1, 3, 1),
#'   z = c('x', 'x', 'y', 'y', 'x')
#' )
#' obtain_counts(df)
#'
#' @import tidyverse
#' @importFrom tibble enframe
#' @importFrom tidyr unnest
#' @importFrom purrr map
#'
#' @export
obtain_counts <- function(df, .fun = is.atomic) {

  df %>%
    select(where(.fun)) %>%
    map(table) %>%
    enframe(name = "variable", value = "n") %>%
    mutate(
      pct = map(n, ~round(prop.table(.x) * 100, 1)),
      label = map(n, names),
      across(c(n, pct), ~map(.x, as.double))
    ) %>%
    tidyr::unnest()

}


