calc_reliability <- function(data) {
  bind_rows(
    itemized::compute_alpha_new(questionnaire = "osi_szp", "OSI_SzP"),
    itemized::compute_alpha_new(questionnaire = "osi_zdrowiea", "OSI_zdrowieA"),
    itemized::compute_alpha_new(questionnaire = "osi_zdrowieb", "OSI_zdrowieB"),
    itemized::compute_alpha_new(questionnaire = "osi_zp", "OSI_ZP"),
    itemized::compute_alpha_new(
      questionnaire = "osi_zszs",
      "OSI_ZszS",
      reverse = FALSE
    )
  )
}
