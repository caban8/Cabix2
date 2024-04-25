

# Historia tworzenia wszystkich funkcji -----------------------------------
  # There is also a useful function, devtools::test_active_file(). It infers the target test file from the active file and,
  # similar to how use_r() and use_test() work, it works regardless of whether the active file is a test file or a companion
  # R/*.R  file. You can invoke this via “Run a test file” in the Addins menu. However, for heavy users (like us!), we
  # recommend binding this to a keyboard shortcut; we use Ctrl/Cmd + T.


library(devtools)

# Chi square analysis -----------------------------------------------------

use_r("chi_test")
use_test("chi_test")
test_file("tests/testthat/test-chi_test.R")





# Dodanie wiadomości powitalnej -------------------------------------------



use_r("startup")



# Funkcja do szybkiego i tabelkowego obliczania mean i sd -----------------



use_r("conditions_stats")
use_test("conditions_stats")

# Zaktualizować tak, żeby była kontrola nad wyborem statystyk (median, iqr)




# Porównania wielokrotne --------------------------------------------------


# Sprawdzić, czy dobrze się zgadzają kolumny ze statystykimi
# A następnie zadbać o kompatybilność względem flex table

use_r("multiple_comparisons")
use_test("multiple_comparisons")
testthat::test_file("tests/testthat/test-multiple_comparisons.R")




# Porównania within-group z jednym czynnikiem -----------------------------


use_r("simple_repeated_measures")



# Pomniejsze funkcje do szybckiej modyfikacji zmiennych -------------------
  # Łączenie wartości z dwóch zmiennych w jedną, z nawiasem
  # szybkie zaokrąglanie wszystkich zmiennych numeric w ramach df
  # szybka funkcja do odwracania pozycji

use_r("simple_modifiers")
use_test("simple_modifiers")
test_file("tests/testthat/test-simple_modifiers.R")



# Relative change functions -----------------------------------------------


use_r("relative_change")
use_test("relative_change")


# Labeling functions ------------------------------------------------------

use_r("labellers")
use_test("labellers")



# spss manipulation -------------------------------------------------------


use_r("spss_functions")
use_test("spss_functions")


# Funkcje do zliczania wskaźników -----------------------------------------

use_r("compute")
use_test("compute")


# Zliczanie częstości i procentów - tabelka demograficzna -----------------

use_r("demographics_apa")
use_test("demographics_apa")



# Descriptive statistics table --------------------------------------------
  # Uwzględnić opcję z wartościami alpha

use_r("descriptives")
use_test("descriptives")


# Regression model --------------------------------------------------------

use_r("regressions")

# Correlations ------------------------------------------------------------

use_r("correlations")


# One sample comparisons --------------------------------------------------

use_r("one_sample")
use_test("one_sample")


# Two-way anova -----------------------------------------------------------

use_r("multiple_comparisons_bg2")
use_test("multiple_comparisons_bg2")



# Factor analysis functions -----------------------------------------------

use_r("factor_analysis_functions")
use_test("factor_analysis_functions")


# Mediation and moderation ------------------------------------------------





# Tabelka z unicode dla głównych znaków greckich --------------------------






