create_rmd_report <- function(
    report_data,
    section_col = section,
    intro,
    flextable,
    caption,
    interpretation,
    path,
    caption_indent = "\n\n"
) {


  doc <- new_markdown_doc()

  report_chunks <- create_report_chunks(
    report_data = {{report_data}},
    section_col = {{section_col}},
    intro = {{intro}},
    flextable = {{flextable}},
    caption = {{caption}},
    interpretation = {{interpretation}},
    caption_indent = caption_indent
  )


  doc <- add_markdown(
    doc,
    report_chunks
  )

  doc$yaml <- yaml_report()

  write_markdown(doc, path)

}







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
    expr = NULL,
    heading = NULL,
    caption_indent = "\n\n"
) {




  if (is.null(expr)) {expr <- rlang::expr(
    narrate(
      report = !!rlang::ensym(report_data),
      section_col = !!rlang::ensym(section_col),
      section = !!rlang::ensym(section_col),
      intro = !!rlang::ensym(intro),
      flextable = !!rlang::ensym(flextable),
      caption = !!rlang::ensym(caption),
      interpretation = !!rlang::ensym(interpretation),
      caption_indent = !!caption_indent
    )
  )}



  make_block(
    expr,
    defuse = FALSE,
    heading = heading_switch_if_null(section, heading),
    heading_level = 2
    )


}


create_report_chunks <- function(
    report_data,
    section_col = section,
    intro,
    flextable,
    caption,
    interpretation,
    caption_indent = "\n\n"
) {

  report_data_sym <- rlang::ensym(report_data)
  sections <- report_data |>
    dplyr::pull(!!rlang::ensym(section_col)) |>
    unique()

  exprs <- purrr::map(sections, ~ rlang::expr(
    narrate(
      report = !!report_data_sym,
      section_col = !!rlang::ensym(section_col),
      section = !!.x,
      intro = !!rlang::ensym(intro),
      flextable = !!rlang::ensym(flextable),
      caption = !!rlang::ensym(caption),
      interpretation = !!rlang::ensym(interpretation),
      caption_indent = !!caption_indent
    )
  ))

  purrr::map2(
    exprs,
    sections,
    ~ make_block(
      .x,
      defuse = FALSE,
      heading = heading_switch_if_null(.y),
      heading_level = 2
    )
  ) |>
    unlist(use.names = F)

}



# helpers -----------------------------------------------------------------


heading_switch_if_null <- function(section, heading = NULL) {

  if (is.null(heading)) {
    heading_switch(section)
  } else {
    heading
  }





}

heading_switch <- function(section) {
  switch(
    section,
    introduction = NULL,
    descriptives = "Statystyki opisowe",
    verification = "Weryfikacja hipotez",
    summary = "Podsumowanie",
    section
  )
}

