# ---- Unit functions ----

factor_zgoda <- function(x) {
  factor(x, levels = c("Tak"))
}

factor_plec <- function(x) {
  factor(x, levels = c("Kobieta", "Mężczyzna"))
}

factor_wyksztalcenie <- function(x) {
  factor(
    x,
    levels = c(
      "Podstawowe",
      "Średnie",
      "Wyższe"
    ),
    ordered = TRUE
  )
}

factor_miejsce_zamieszkania <- function(x) {
  factor(
    x,
    levels = c(
      "Wieś",
      "Miasto do 20 tysięcy",
      "Miasto od 20 do 100 tysięcy mieszkańców",
      "Miasto od 100 do 200 tysięcy mieszkańców",
      "Miasto od 200 do 500 tysięcy mieszkańców",
      "Miasto powyżej 500 tysięcy mieszkańców"
    ),
    ordered = TRUE
  )
}

factor_stan_cywilny <- function(x) {
  factor(
    x,
    levels = c(
      "Panna / Kawaler",
      "Mężatka / Żonaty",
      "W konkubinacie",
      "W separacji",
      "Rozwódka / Rozwodnik",
      "Wdowa / Wdowiec"
    )
  )
}


# ---- General tidyverse-compatible function ----

factorize_variables <- function(data) {
  data %>%
    mutate(
      zgoda = factor_zgoda(zgoda),
      plec = factor_plec(plec),
      wyksztalcenie = factor_wyksztalcenie(wyksztalcenie),
      miejsce_zamieszkania = factor_miejsce_zamieszkania(miejsce_zamieszkania),
      stan_cywilny = factor_stan_cywilny(stan_cywilny)
    )
}
