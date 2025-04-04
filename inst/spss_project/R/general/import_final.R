
# Final import ------------------------------------------------------------

library(tidyverse)

# Import labels codebook
codebook <- read_csv("dane/codebooks/codebook.csv")
view(codebook)

# Import metoda
metoda_path <- "dane/metoda/metoda.xlsx"
metoda_sheets <- readxl::excel_sheets(metoda_path)
metoda <- map(metoda_sheets, readxl::read_excel, path = metoda_path) %>%
  set_names(metoda_sheets)
hipotezy <- metoda$Hipotezy %>%
  Cabix2::extract_hypotheses(hipoteza, przeformułowanie)

# Import the main data
Baza_nazwa <- ""
baza <- haven::read_sav(Baza_nazwa)
baza_fac <- baza %>% mutate(across(where(haven::is.labelled), haven::as_factor))


