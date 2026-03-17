
# narrate -----------------------------------------------------------------


narrate <- function(
    report,
    intro,
    flextable,
    caption,
    interpretation,
    caption_indent = "\n\n"
) {

  report <- report |>
    dplyr::mutate(!!ensym(caption) := paste0(caption_indent, {{caption}}))


  narrate_loop(
    report,
    {{ intro }},
    {{ flextable }},
    {{ caption }},
    {{ interpretation }}
  )


}



narrate_loop <- function(report, intro, flextable, caption, interpretation) {

  for (row_i in seq_len(nrow(report))) {
    report |>
      dplyr::slice(row_i) |>
      narrate_1(
        {{ intro }},
        {{ flextable }},
        {{ caption }},
        {{ interpretation }}
      )
  }
}




narrate_1 <- function(
    report,
    intro,
    flextable,
    caption,
    interpretation
) {

  stopifnot(nrow(report) == 1)


  report <- report |>
    dplyr::select({{intro}}, {{flextable}}, {{caption}}, {{interpretation}}) |>
    as.list() |>
    purrr::flatten()


  browser()

  cat(report[[1]])
  cat(space1())
  flex_process_rmd(report[[2]], report[[3]])
  cat(space1())
  cat(report[[4]])
  cat(space2())


}



# helpers -----------------------------------------------------------------



flex_process_rmd <- function(ft, caption) {
  ft |>
    flextable::set_caption(caption) |>
    flextable::flextable_to_rmd()
}



space1 <- function() {
  "\u00A0\n\n" # Spacja na końcu, żeby dodało akapit
}

space2 <- function() {
  "\n\n\u00A0" # Spacja na początku, żeby dodało akapit
}
