
# Cykl developerski -------------------------------------------------------


library(devtools)

# Ogólny test -------------------------------------------------------------

devtools::document()
devtools::test_coverage() # Ocenia stopień, do jakiego linie kodu w source package są pokryte w ramach testowania
devtools::test()
devtools::load_all()

?compute_var
?conditions_stats

# Ogólny check ------------------------------------------------------------


check()


# Instalacja pakietu ------------------------------------------------------


# Może zrobić dependency na starszych wersjach tidyverse, ze względu na to, że zaczynają się pojawiać jakieś errory?

document()
install()
