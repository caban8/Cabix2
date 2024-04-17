
# Przygotowuję podstawowe informacje --------------------------------------

use_testthat() #To na początku, dzięki temu dodaję strukturę i folder związanę z testowaniem funkcji
use_testthat(edition = 3) # Poprawiam na wersję 3, ze względu na jej jakieś specjalne własciwości

library(devtools)


# Ustalam współpracujące pakiety ------------------------------------------


pakiety <- c("rstatix",
  "dplyr",
  "stringr",
  "tidyr",
  "tibble",
  "haven",
  "tidyselect",
  "broom",
  "lm.beta",
  "plyr",
  "purrr"
)

purrr::walk(pakiety, use_package)


document()


file.edit("DESCRIPTION")
file.edit("NAMESPACE")


# Ustalam licencję --------------------------------------------------------



use_mit_license() # Licencja permissive, open-source. Daje dużą dowolność dla innych do modyfikowania i wykorzystywania kodu
use_proprietary_license(copyright_holder = "Maciej Cabański")



# Dodaję pliki do zignorowania --------------------------------------------


ignore_files <- c("Development", "Raw Functions", "Archiwum")
purrr::walk(ignore_files, use_build_ignore)





# https://r-pkgs.org/man.html#pack-it-in-pack-it-out
# https://raw.githubusercontent.com/rstudio/cheatsheets/main/package-development.pdf


proj_sitrep()
proj_get()


# Skrót dla load_all ctrl + shift + L



# Cytowanie ---------------------------------------------------------------

citation("Cabix2")

citEntry(entry = "Article",
         title = "Summary publishable tables made easy",
         author = as.person("Maciej Cabanski"),
         journal = "Personal Diary",
         year = "Now",
         volume = "none",
         number = "3",
         pages = "1--25",
         url = "maciej.cabanski.pl",

         textVersion = "bla bla bla"
         )



# Dema --------------------------------------------------------------------

demo()
demo(package = "httr")



# Typowy cykl pracy z testowaniem i budowaniem pakietu --------------------


# Edit one or more files below R/.
# document() (if you’ve made any changes that impact help files or NAMESPACE)
# load_all()
# Run some examples interactively.
# test() (or test_active_file())
# check()

stringi::stri_escape_unicode("β") #Sprawdzam, jakie jest oznaczenie unicode na dany nietypowy znak

search()







# Vignettes ---------------------------------------------------------------


browseVignettes()
browseVignettes("car")
browseVignettes("stringr")



# Dodaje readme file dla całego pakietu -----------------------------------


usethis::use_readme_rmd()
# Do readme chciałbym jeszcze z czasem coś dodać


?create_package
create_package("C:/R/Projects/Cabflex2")



