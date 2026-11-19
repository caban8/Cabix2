etl_engineer <- function(data) {
  data |>
    itemized::compute_scale_new(
      questionnaire = "osi_szp",
      "OSI_SzP",
      rodzaj = "sum"
    ) |>
    itemized::compute_scale_new(
      questionnaire = "osi_zdrowiea",
      "OSI_zdrowieA",
      rodzaj = "sum"
    ) |>
    itemized::compute_scale_new(
      questionnaire = "osi_zdrowieb",
      "OSI_zdrowieB",
      rodzaj = "sum"
    ) |>
    itemized::compute_scale_new(
      questionnaire = "osi_zp",
      "OSI_ZP",
      rodzaj = "sum"
    ) |>
    itemized::compute_scale_new(
      questionnaire = "osi_zszs",
      "OSI_ZszS",
      rodzaj = "sum",
      reverse = FALSE
    )
}
