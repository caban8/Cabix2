# Steny
#' Count sten distributions for one or more variables
#'
#' Computes frequency-percentage summaries for sten variables (1-10) and,
#' optionally, appends chi-square goodness-of-fit test statistics.
#'
#' @param .data A data frame containing sten variables.
#' @param X Variables to summarize, supplied with tidy-selection semantics.
#' @param categorization Optional named list defining category mappings for sten
#'   values. When `NULL`, raw sten values are used.
#' @param chisq Logical; if `TRUE` (default), appends chi-square test results
#'   (`statistic` and `p.value`) for each selected variable.
#'
#' @return A tibble with a `value` column (sten/category labels, and optionally
#'   chi-square result labels) plus one column per variable from `X` containing
#'   formatted frequency summaries.
#' @export
count_steny <- function(.data, X, categorization = NULL, chisq = TRUE) {
  out <- .data |>
    count_steny_many(X = {{ X }}, categorization = categorization)

  if (chisq) {
    out <- out |>
      dplyr::mutate(value = as.character(value)) |>
      bind_rows(
        calc_chisqt_many(.data, X = {{ X }}, categorization = categorization)
      )
  }

  return(out)
}


count_steny_many <- function(.data, X, categorization = NULL) {
  .data |>
    dplyr::select({{ X }}) |>
    purrr::imap(count_steny_1, categorization = categorization) |>
    purrr::reduce(dplyr::full_join) |>
    arrange_by_x_values(categorization = categorization) |>
    purrr::modify_at(-1, tidyr::replace_na, "0") |>
    dplyr::arrange(value)
}


arrange_by_x_values <- function(.data, categorization = NULL) {
  if (is.null(categorization)) {
    .data |>
      dplyr::mutate(value = as.numeric(value)) |>
      dplyr::right_join(steny_values_template(), by = "value")
  } else {
    .data |>
      tidyr::complete(value = names(categorization)) |>
      dplyr::mutate(
        value = factor(
          value,
          levels = names(categorization)
        )
      )
  }
}


count_steny_1 <- function(
  x,
  name = NULL,
  categorization = NULL
) {
  assertthat::assert_that(
    all(x %in% 1:10),
    msg = "All sten values must be between 1 and 10"
  )

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (!is.null(categorization)) {
    x <- categorize_steny_1(x)
  }

  calc_count_freqs(x, name)
}


# chi square
calc_chisqt_many <- function(.data, X, categorization = NULL) {
  .data |>
    dplyr::select({{ X }}) |>
    purrr::imap(
      ~ calc_chisqt_1(.x, name = .y, categorization = categorization)
    ) |>
    purrr::reduce(dplyr::full_join)
}

calc_chisqt_1 <- function(x, name = NULL, categorization = NULL) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (!is.null(categorization)) {
    x <- categorize_steny_1(x, categorization = categorization)
  }

  x |>
    calc_chisqt_simple() |>
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = "value",
      values_to = name
    ) |>
    dplyr::mutate(value = replace_with_chi_symbol(value))
}

calc_chisqt_simple <- function(x) {
  table(x) |>
    chisq.test() |>
    broom::tidy() |>
    dplyr::mutate(
      statistic = str_stat(statistic, parameter, c(2, 0)),
      p.value = as.character(round(p.value, 3))
    ) |>
    dplyr::select(statistic, p = p.value)
}


# helpers
replace_with_chi_symbol <- function(x) {
  dplyr::case_when(
    x == "statistic" ~ "\u03C7\u00B2 (df)",
    .default = as.character(x)
  )
}

steny_values_template <- function() {
  tibble::tibble(
    value = 1:10
  )
}

categorize_steny_1 <- function(
  x,
  categorization = list(
    "Niski" = 1:3,
    "Umiarkowany" = 4:7,
    "Wysoki" = 8:10
  )
) {
  assertthat::assert_that(
    all(unlist(categorization) %in% 1:10),
    msg = "All categorization values must be between 1 and 10"
  )

  plyr::mapvalues(
    x,
    from = categorization_enframe(categorization)$value,
    to = categorization_enframe(categorization)$category
  )
}


categorization_enframe <- function(categorization) {
  categorization |>
    tibble::enframe(
      name = "category",
      value = "value"
    ) |>
    tidyr::unnest(cols = c(value))
}


calc_count_freqs <- function(x, name = NULL) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  freq <- table(x)
  pct <- prop.table(freq) * 100

  str_stat(freq, pct, .round = c(0, 1), percent = TRUE, comma = FALSE) |>
    tibble::enframe(name = "value", value = name) |>
    dplyr::mutate(value = names(freq))
}


names_to_numeric <- function(x) {
  as.numeric(names(x))
}


# Validators
val_count_steny <- function(.data) {
  .data |>
    pointblank::col_vals_between(
      columns = value,
      left = 1,
      right = 10,
      na_pass = TRUE
    )
}
