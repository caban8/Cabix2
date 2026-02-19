
# reporting 2 -------------------------------------------------------------



#' Generate and print a report of statistical analyses
#' @export
#'
report_print2 <- function(
    data,
    intro = "wprowadzenie",
    flextable = "flex",
    caption = "caption",
    interpretation = "interpretacja",
    caption_indent = "\n\n"
) {


  data[[caption]] <- paste0(caption_indent, data[[caption]])


  report_i2(
    data[[intro]],
    data[[flextable]],
    data[[caption]],
    data[[interpretation]]
    )


}


#' Generates a report composed of flextables with captions and associated interpretations
#'
#' @export
#'
#' @importFrom flextable set_caption flextable_to_rmd
#' @importFrom magrittr %>%
report_i2 <- function(intros, tables, captions, interpretation) {

  for (i in seq_along(tables)) {

    cat(intros[[i]])
    cat("\u00A0\n\n") # Spacja na końcu, żeby dodało akapit
    tables[[i]] %>% flextable::set_caption(captions[[i]]) %>%  flextable::flextable_to_rmd()
    cat("\u00A0\n\n") # Spacja na końcu, żeby dodało akapit
    cat(interpretation[[i]])
    cat("\n\n\u00A0") # Spacja na początku, żeby dodało akapit

  }

}

