
# -------------------------------------------------------------------------

library(readxl)
library(itemized)
library(tidyverse)



# Przygotowanie labels ----------------------------------------------------




# Rekodowanie factors -----------------------------------------------------


# Zliczanie skal kwestionariuszy ------------------------------------------


# Dodanie labels ----------------------------------------------------------


## Eksport labels na potrzeby bazy relacyjnej ##
df5 %>%
  names() %>%
  set_names(map(df5, attr, "label")) %>%
  enframe(name = "Etykieta", value = "Nazwa") %>%
  write_csv("Results/codebooks/vars_labels.csv")

# Eksport -----------------------------------------------------------------



Cabix2::spss_write()
