library(devtools)

# Interactive testing sessions --------------------------------------------

# Tutaj testuje na bieżąco, to znaczy równolegle wprowadzając zmiany do funkcji i eksplorując, co się dzieje przed finalnym
# produktem
  # Skrót dla load_all ctrl + shift + L

# !!!!
# It would be annoying and error prone to copy and paste the same @param documentation to every function so
# roxygen2 provides @inheritParams which allows you to inherit argument documentation from another package.
# https://r-pkgs.org/man.html#inheriting-arguments
# !!!!





comparison_bg1(
  mtcars,
  DVs = c("mpg", "disp"),
  IV = "am"
)

plot_bar(
  mtcars,
  DVs = c("mpg", "disp"),
  IV = am
)




# -------------------------------------------------------------------------



.


pdf <- pdftools::pdf_text("test_data/Przykładowa ankieta.pdf")

pdf %>%
  pdf_forms_extract_questions()

pdf %>%
  pdf_gforms_extract_q()


pdf %>%
  stringr::str_split("\n\n") %>%
  unlist() %>%
  stringr::str_trim() %>%
  .[. != ""] %>%
  stringr::str_subset("http", negate = T) %>%  # Delete rows with urls
  stringr::str_subset("^\\d{1,2}\\.\\d{1,2}\\.\\d{1,4}", negate = T) %>%  # Delete rows with dates
  stringr::str_subset("Google", negate = T)  # Delete all google comments
  tibble::enframe()

pdf %>%
  tail()
