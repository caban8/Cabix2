etl_modify_vars <- function(data) {
  data |>
    factorize_variables() |>
    osi_sub_1() |>
    mutate(wiek = as.double(wiek))
}


# Helpers
osi_sub_1 <- function(data) {
  data |>
    mutate(
      across(starts_with("OSI"), ~ osi_to_numerical(.x))
    )
}

osi_to_numerical <- function(x) {
  x |>
    str_sub(0, 1) |>
    as.numeric()
}
