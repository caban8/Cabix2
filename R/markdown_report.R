yaml_report <- function() {
  list(
    title = "Wyniki",
    output = list(
      "officedown::rdocx_document" = list(
        tables = list(
          layout = "autofit",
          caption = list(
            pre = "Tabela ",
            sep = ""
          )
        ),
        reference_docx = "materials/wzor4.docx",
        plots = list(
          caption = list(
            pre = "Rycina ",
            sep = ". "
          )
        )
      )
    )
  )
}



create_report_chunk_1 <- function(
    report_data,
    section_col = section,
    section,
    intro,
    flextable,
    caption,
    interpretation,
    caption_indent = "\n\n"
) {

}


create_report_chunks <- function(
    report_data,
    section_col = section,
    sections = list(
      introduction = "",
      descriptives = "Statystyki opisowe",
      verification = "Weryfikacja hipotez",
      summary = "Podsumowanie"
    ),
    intro,
    flextable,
    caption,
    interpretation,
    caption_indent = "\n\n"
) {

}
