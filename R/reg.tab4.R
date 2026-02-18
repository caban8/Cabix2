#' @export
reg.tab4 <- function(mod, CI = F, decimal = T) {



  #Przygotowuję etykiety zmiennych


  #Obliczam oszacowania ogólne modelu
  overall <- mod %>%
    broom::glance()  %>%
    select(adj.r.squared, statistic, p.value, df, df.residual) %>%
    mutate(across(c(adj.r.squared, statistic), ~round(., 2))) %>%
    mutate(
      statistic = str_c(statistic, " (", df, ", ", df.residual, ")"),
      p.value = round(p.value, 3)
    ) %>%
    select(statistic, adj.r.squared, p.value) %>%
    rename(p.model = p.value) %>%
    mutate(term = "Ogólne oszacowanie")


  #Dołaczam puste wiersze do oszacowań ogólnych, żeby potem sprawnie połączyć do jednego tibble


  #Obliczam przedziały ufności na unstandardized estimates
  coefficients1 <- mod %>%
    broom::tidy(conf.int = T) %>%
    select(term, conf.low, conf.high)

  #Obliczam pozostałe estimates
  coefficients2 <- mod %>%
    lm.beta() %>%
    broom::tidy() %>%
    select(-statistic)


  #Łącze bazy
  coefficients_all <- coefficients2 %>%
    left_join(coefficients1)  %>%
    mutate(across(-c(term, p.value), ~round(., 2))) %>%
    mutate(
      estimate2 = str_c(estimate, " (", std.error, ")"),
      CI = str_c("[", conf.low, ", ", conf.high, "]")) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      p.value = round(p.value, 3)
    )


  if (CI) {coefficients_all <- coefficients_all %>%
    select(term, estimate, CI, std_estimate, p.value)
  } else {coefficients_all <- coefficients_all %>%
    select(term, estimate2, std_estimate, p.value)}

  coefficients_all2 <- coefficients_all %>%
    rbind(c("Ogólne oszacowanie", rep("", ncol(.) - 1))) %>%
    left_join(overall) %>%
    mutate(
      ID = c(2:nrow(.),1),
      p.model = if_else(p.model < 0.001, "<0.001", as.character(p.model)),
      p.value = if_else(p.value == 0, "<0.001", as.character(p.value))
    ) %>%
    arrange(ID) %>%
    select(-ID)

  if (decimal) {
    coefficients_all2 %>%
      mutate(across(-term, ~str_replace_all(., "[.]", ",")
      ))
  } else {coefficients_all2}



}
