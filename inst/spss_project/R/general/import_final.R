
# Final import ------------------------------------------------------------

library(tidyverse)

# Import labels codebook
codebook <- readxl::read_excel(paths$codebook)



# Import metoda
metoda_sheets <- readxl::excel_sheets(paths$metoda)
metoda <- map(metoda_sheets, readxl::read_excel, path = paths$metoda) %>%
  set_names(metoda_sheets)
hipotezy <- metoda$Hipotezy %>%
  Cabix2::extract_hypotheses(hipoteza, przeformułowanie)

# Import the main data
baza <- haven::read_sav(paths$dane_spss)
baza_fac <- baza %>% mutate(across(where(haven::is.labelled), haven::as_factor))


