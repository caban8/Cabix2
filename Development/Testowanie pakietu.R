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



# spss drop ---------------------------------------------------------------

library(Bazy)

Bazy::szczescie$stancyw %>%
  spss_drop(values = c(3:7))



# spss labelled -----------------------------------------------------------


X <- sample(1:5, size = 10, T)
unique(X)
spss_labelled(X, labels =  c("Nie", "Raczej Nie", "Środek", "Raczej tak", "Tak"))
spss_labelled(X,
              labels =  c("Nie", "Raczej Nie", "Środek", "Raczej tak", "Tak"),
              var.label = "Etykieta zmiennej")
spss_labelled(X, labs = c("Nie", "Raczej Nie", "Środek", "Raczej tak", "Tak"))

spss_labelled2(iris$Species)
spss_labelled2(iris$Species, zero_start = TRUE)

# -------------------------------------------------------------------------



x <- sample(1:5, 10, TRUE)

reverse(x)
reverse(x, c(1, 3, 5))

dane <- data.frame(x = x)

dane %>%
  dplyr::mutate(y = reverse(x))

nrow(mtcars)
mtcars$mpg %>%
  head_more()

mtcars2 <- tibble::as_tibble(mtcars)
mtcars2 %>%
  head_more()


# One sample comparison ---------------------------------------------------

rstatix::get_summary_stats(mtcars, mpg, hp, type = "mean_sd")
dplyr::summarise(mtcars, across(c(mpg, hp), list(mean, sd))) %>%
  dplyr::mutate(dplyr::across(, ~round(., 10)))
dplyr::summarise(mtcars, across(c(mpg, hp), list(median, iqr = ~IQR(., type = 6)))) %>%
  dplyr::mutate(dplyr::across(, ~round(., 10)))

rstatix::t_test(mtcars, hp ~ 1)
one_sample(mtcars, hp)
rstatix::t_test(mtcars, hp ~ 1) %>% as.data.frame()
rstatix::cohens_d(mtcars, hp ~ 1)
rstatix::t_test(mtcars, mpg ~ 1, mu = 20)
rstatix::cohens_d(mtcars, mpg ~ 1, mu = 20)
rstatix::wilcox_test(mtcars, mpg ~ 1) %>% as.data.frame()
rstatix::wilcox_effsize(mtcars, mpg ~ 1)
one_helper(mtcars, mpg ~ 1, mu = 20)
one_helper(mtcars, mpg ~ 1, mu = 20, test = "wilcoxon")
one_helper(mtcars, mpg ~ 1, mu = 10, test = "wilcoxon")
one_helper(mtcars, mpg ~ 1, mu = 25, test = "wilcoxon")
rstatix::t_test(mtcars, mpg ~ vs)
one_rank(mtcars, mpg, vs)
one_mean(mtcars, mpg, vs)
one_median(mtcars, mpg, vs)
one_stats(mtcars, mpg, vs)
one_stats(mtcars, mpg, vs, type = "median_iqr")
one_stats(mtcars, mpg, vs, type = "mrank_median")
one_stats(mtcars, mpg, vs, type = "mrank_median")
one_stats(Bazy::t_niezalezne, COPE_skala1, COPE_skala2, COPE_skala3, type = "mrank_median")
one_stats(Bazy::t_niezalezne, COPE_skala1, COPE_skala2, COPE_skala3)
one_sample(Bazy::t_niezalezne, COPE_skala1, COPE_skala2, COPE_skala3)
one_sample(Bazy::t_niezalezne, COPE_skala1, COPE_skala2, COPE_skala3, mu = 2.4)
one_sample(Bazy::t_niezalezne, COPE_skala1, COPE_skala2, COPE_skala3, test = "wilcoxon")
one_sample(Bazy::t_niezalezne, COPE_skala1, COPE_skala2, COPE_skala3,
           test = "wilcoxon", type = "median_iqr", mu = 2.5)
one_sample(mtcars, mpg, hp, test = "wilcoxon", type = "median_iqr", mu = 20)
one_stats(mtcars, mpg, hp,  type = "median_iqr")

rstatix::levene_test(mtcars, mpg ~ factor(vs))
rstatix::t_test(mtcars, mpg ~ vs, mu = 0)
rstatix::t_test(mtcars, mpg ~ vs, mu = NULL)
rstatix::t_test(mtcars, mpg ~ 1, mu = NULL)
rstatix::t_test(mtcars, mpg ~ 1, mu = 0)
rstatix::cohens_d(mtcars, mpg ~ 1, mu = 0)
rstatix::t_test(mtcars, mpg ~ 1, mu = 20)



# Testowanie chi-kwadrat many rows ----------------------------------------

cross_freq(mtcars, vs, cyl)
chi_stat(mtcars, vs, cyl)
cross_freq(mtcars, am, cyl)
chi_table(mtcars, am, cyl)
chi_stat(mtcars, am, cyl)

freq_many(mtcars, am, vs)
freq_many(mtcars, am, vs)
freq_many(mtcars, gear, vs)



chi_many(mtcars, vs, am, x = gear, correct = F)
chi_many(mtcars, vs, am, x = gear, yes_row = 1)
chi_many(mtcars, vs, am, x = gear)




# Testowanie chi many rows w wersji 2 -------------------------------------




df <- tibble::tibble(
  x1 = sample(c("Yes", "No"), 30, T),
  x2 = sample(c("Yes" ), 30, T),
  x3 = sample(c("Yes", "No", "Whatever"), 30, T),
  y = sample(c("DDA", "No-DDA"), 30, T)
  )

df2 <- tibble::tibble(
  y = c("2", "2", "2", "1"),
  x1 = c("B", "B", "C", "C")
  )

df2 %>%
  rstatix::freq_table(x1, y) %>%
  tidyr::complete(1, 2, fill = list(n = 0, prop = 0))

df2 %>%
  cross_freq( y, x1)



freq_many(df, x = x1, y = y)
freq_many(df, x = y, y = x1)


chi_many(df, x1, x2, x = y)
chi_many(df, x1, x2, x = y, binary_x = F)

chi_many2(df, x1, x2, x = y)


# Testowanie testów  ------------------------------------------------------


rstatix::wilcox_test(mtcars, mpg ~ vs)
rstatix::wilcox_effsize(mtcars, mpg ~ vs)


# Two-way anova between groups --------------------------------------------


aov2_main(mtcars, mpg, am, vs)
aov2_simple(mtcars, mpg, am, vs)
aov2_des(mtcars, mpg, am, vs)

  # Algorytm do zliczania tabelki APA w ramach two-way anova
aov2_des(mtcars, mpg, IV1 = am)
aov2_des(mtcars, mpg, IV1 = am, IV2 = vs)
aov2_des(iris, Sepal.Length, IV1 = Species)

subgroups <- aov2_des(mtcars, mpg, am, vs)
groups1 <- aov2_des(mtcars, mpg, am)
groups2 <- select_grouped(mtcars, mpg, IV1 = vs) %>%
  mean_sdN(mpg)

subgroups %>%
  dplyr::left_join(groups2) %>%
  tibble::add_row(groups1)



aov2_posthoc(Bazy::Anova_2way_bg1, Attractiveness, IV1 = Alcohol)
aov2_posthoc(Bazy::Anova_2way_bg1, Attractiveness, IV1 = Gender)
aov2_posthoc(Bazy::Anova_2way_bg1, Attractiveness, IV1 = Alcohol, IV2 = Gender)
aov2_posthoc(Bazy::Anova_2way_bg1, Attractiveness, IV2 = Alcohol, IV1 = Gender)
aov2_bg(mtcars, hp, am, vs)
aov2_bg(Bazy::Anova_2way_bg1, Attractiveness, IV1 = Alcohol, IV2 = Gender)

wynik <- aov2_bg(Bazy::Anova_2way_bg2,  postawa, IV2 = malpa, IV1 = nagroda)
wynik$`Descriptive Statistics`[1] %>% tidyr::replace_na(li)

attr(Bazy::Anova_2way_bg2$malpa, which = "label")
attr(Bazy::Anova_2way_bg2$malpa, which = "label", exact = T)
var_labels(Bazy::Anova_2way_bg2, postawa, nagroda)
var_labels(Bazy::Anova_2way_bg2, postawa, nagroda, spss.lab = F)
var_labels(Bazy::Anova_2way_bg2, postawa, nagroda, malpa)
var_labels(Bazy::Anova_2way_bg2, postawa, nagroda, malpa, spss.lab = F)
var_labels(Bazy::Anova_2way_bg2, postawa, nagroda, malpa, labels. = c("raz", "dwa", "trzy"))
var_labels(Bazy::Anova_2way_bg2, postawa, nagroda, malpa, labels. = c("raz", "dwa"))

result <- var_labels(Bazy::Anova_2way_bg2, postawa, nagroda, malpa)
result %in% list(NULL)
list(NULL) %in% result


baza_malpy <- Bazy::Anova_2way_bg2
aov2_posthoc(Bazy::Anova_2way_bg2, postawa, IV1 = malpa, IV2 = nagroda)
aov2_bg(Bazy::Anova_2way_bg1, Attractiveness, IV1 = Gender, IV2 = Alcohol)
aov2_posthoc(Bazy::Anova_2way_bg2, postawa, IV2 = malpa, IV1 = nagroda)
aov2_posthoc(baza_malpy, postawa, IV1 = malpa, IV2 = nagroda)
aov2_posthoc(baza_malpy, postawa, IV2 = malpa, IV1 = nagroda)

aov2_bg(baza_malpy, postawa, IV1 = malpa, IV2 = nagroda)
aov2_bg(baza_malpy, postawa, IV2 = malpa, IV1 = nagroda, spss.lab = F)

unique(Bazy::Anova_2way_bg2$nagroda)
unique(iris$Species)



mean_sdN(mtcars, mpg)
mean_sd(mtcars, mpg, vs)


# Chi square analysis -----------------------------------------------------

ToothGrowth
table(ToothGrowth$supp, ToothGrowth$dose)



table(mtcars$am, mtcars$vs) %>% chisq.test()
rstatix::cramer_v(mtcars$am, mtcars$vs)
table(mtcars$am, mtcars$vs) %>% prop.table(margin = 1)
table(mtcars$am, mtcars$vs) %>% prop.table(margin = 2)
cross_freq(mtcars, am, vs)


chi_table(mtcars, am, vs)
cross_freq(Bazy::szczescie, palenie, miasto)
cross_freq(Bazy::szczescie, palenie, miasto, percents = "cols")
cross_freq(Bazy::szczescie, miasto, palenie)
chi_stat(Bazy::szczescie, miasto, palenie)
chi_table(Bazy::szczescie, miasto, palenie)

chi_table(Bazy::szczescie, miasto, palenie) %>% Cabflex2::flex_chicross()
chi_table(Bazy::szczescie, miasto, palenie, percents = "cols")
dplyr::summarise(Bazy::szczescie, rstatix::chisq_test(alkohol, palenie))
dplyr::summarise(Bazy::szczescie, rstatix::cramer_v(alkohol, palenie))
dplyr::summarise(Bazy::szczescie, rstatix::cramer_v(miasto, palenie))
dplyr::summarise(Bazy::szczescie, rstatix::chisq_test(miasto, palenie))


# value labels ------------------------------------------------------------

value_labels(Bazy::szczescie, miasto, palenie) %>%
  dplyr::select(miasto, palenie, wybór)
value_labels2(Bazy::szczescie, miasto)
value_labels2(iris, Species)


attributes(Bazy::szczescie$miasto)



# Comparisons with 1 bg ---------------------------------------------------

comparison_bg1(mtcars, mpg, IV = am)
comparison_bg1(Bazy::szczescie, wiek, IV = palenie)
conditions_stats(mtcars, mpg, IV = am)


# regresja ----------------------------------------------------------------

model1 <- lm(mpg ~ disp + drat + cyl, data = mtcars)
model2 <- lm(mpg ~ disp + drat + cyl + hp, data = mtcars)
model3 <- lm(mpg ~ disp + drat + cyl + hp + gear + carb, data = mtcars)
reg_mod(model1)


signif_coef(model1, standarize = T)
signif_coef(model1, standarize = F)



all_models <- list(model1, model2, model3)



do.call(anova, all_models) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    F_change = stringr::str_c(round(F, 2), " (", Df, ", ", Res.Df, ")"),
    F_change = paste_p(F_change, `Pr(>F)`)
    ) %>%
  dplyr::pull(F_change) %>%
  stringr::str_replace("NA", "")


rstatix::Anova(model1, model2) %>%
  tibble::as_tibble()

reg_hier(all_models, standarize = T)
reg_hier(all_models, standarize = T, labels = paste("Predyktor", 1:6))



reg_hier(list(model1, model2, model3), standarize = F) %>%
  t() %>%
  tibble::as_tibble(rownames = "term") %>%
  purrr::set_names(nm = c("term", paste0("mod", seq_along())))




reg_tab(model1)

reg_tab(model1, coef.stats = "CI")
model2 <- lm(cyl ~ gear, data = mtcars)
reg_mod(model2)
reg_tab(model2)
reg_tab(model3)


Baza_r <- haven::read_sav("C:/SPSS and statistics/Work/Zlecenia/Justyna Piechowiak/Baza danych - Justyna Piechowiak (2023-01-12).sav")




regresja_plec1 <- lm(SIRI_skala1 ~ TIntS_skala1 * Plec + TIntS_skala2 * Plec, data = Baza_r)
reg_tab(regresja_plec1)



coefficients1 <- broom::tidy(model1, conf.int = T) %>%
  dplyr::select(term, conf.low, conf.high)


coefficients2 <- lm.beta::lm.beta(model1) %>%
  broom::tidy() %>%
  dplyr::select(-statistic)


coef_join(coefficients1, coefficients2)


# Correlations ------------------------------------------------------------


mtcars %>% sapply(shapiro.test)
simple_cor(mtcars$mpg, mtcars$disp)
simple_cor(mtcars$mpg, mtcars$drat)
correlation2(mtcars$mpg, mtcars$disp)
correlation2(mtcars$mpg, mtcars$drat)

wynik1$estimate
wynik2$estimate

mtcars[1]
mtcars[[1]]

dplyr::select(mtcars, c(mpg, disp))

deparse(list(ale, kale))

cor_tab(mtcars, c(mpg, disp), c(cyl, drat))
cor_tab(mtcars, c(mpg, disp), c(cyl, drat), labels. = list(c("a", "b"), c("d", "e")))
Bazy::Korelacje1
cor_tab(Bazy::Korelacje1,
        c(stres_skala1, stres_skala2),
        c(stres_skala3, stres_skala4))
cor_tab(mtcars, c(mpg, disp), c(cyl, drat), method = "pearson")
cor_tab(mtcars, c(mpg, disp), c(cyl, drat), method = "spearman")


mtcars %>%
  tidyr::pivot_longer(c(mpg, disp, drat)) %>%
  dplyr::nest_by(name) %>%
  dplyr::mutate(cors = purrr::map(data, ))

# Testowanie descriptives -------------------------------------------------

pivot_helper()



n_valid(airquality$Day)
n_valid(airquality$Ozone)





rstatix::shapiro_test(mtcars, mpg, disp)

descriptives(mtcars, mpg)

descriptives(mtcars, mpg, disp)
descriptives(airquality, Day, Ozone)


dplyr::grouped_df()

descriptives(mtcars, mpg, disp, IV1 = am)
descriptives(mtcars, mpg, disp, IV1 = am, IV2 = vs)
descriptives(mtcars, mpg, disp, IV1 = am, IV2 = vs) %>%
  dplyr::groups() %>%
  length()

descriptives(Bazy::t_niezalezne, COPE_skala1, COPE_skala2, COPE_skala3)
descriptives(Bazy::t_niezalezne, COPE_skala1, COPE_skala2, COPE_skala3, IV1 = Grupa)
descriptives(Bazy::t_niezalezne, COPE_skala1, COPE_skala2, COPE_skala3, IV1 = Płeć)


descriptives(mtcars, mpg, disp, IV1 = am, IV2 = vs) %>%
  dplyr::group_by(am, vs) %>%
  dplyr::mutate(ID = dplyr::row_number()) %>%
  dplyr::arrange(ID)

descriptives(mtcars, mpg, disp, IV1 = am) %>%
  desc_arrange(am)

descriptives(mtcars, mpg, disp, IV1 = am, IV2 = vs) %>%
  desc_arrange(am, vs)

  # length pivot_longer zgodnie z liczbą IVs


# Testowanie demographics apa ---------------------------------------------


library(rstatix)
library(tidyverse)

mtcars %>%
  select(vs, am, gear) %>%
  map(freq_table)

demographics_apa(mtcars, vs, am)
demographics_apa(Bazy::szczescie, stancyw, wybór, palenie, ćwicz)
demographics_apa(Bazy::szczescie,  wybór, stancyw, palenie, alkohol)




# Testowanei conditions stats ---------------------------------------------


mtcars %>%
  dplyr::group_by(vs) %>%
  rstatix::get_summary_stats(mpg, disp, type = "mean_sd") %>%
  as.data.frame()


dplyr::group_by(mtcars, vs) %>%
  rstatix::get_summary_stats(mpg, disp, type = "median_iqr") %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~formatC(., digits = 4, format = "f"))) %>%
  as.data.frame()

dplyr::group_by(mtcars, vs) %>%
  dplyr::summarise(
    mpg = IQR(mpg, type = 6),
    disp = IQR(disp, type = 6)
  ) %>%
  dplyr::mutate(dplyr::across(-1, ~formatC(., digits = 2, format = "f"))) %>%
  as.data.frame()

median_iqr(mtcars, mpg, disp, IV = vs, digits = 2)
median_iqr(mtcars, mpg, disp, IV = vs, digits = 10)
conditions_stats(df = mtcars, mpg, disp, IV = vs, type = "mrank_median")



select_grouped(mtcars, disp, mpg, IV1 = am)
select_grouped(mtcars, disp, mpg, IV1 = am) %>%
  rstatix::get_summary_stats(type = "mean_sd")
select_grouped(mtcars, disp, mpg, IV1 = am, IV2 = vs) %>%
  rstatix::get_summary_stats(type = "mean_sd")
select_grouped(mtcars, disp, mpg, IV = am) %>%
  dplyr::summarise(dplyr::across(tidyselect::everything(),
                                 list(s__stat1 = ~mean(., na.rm = T), s__stat2 = ~sd(., na.rm = T))))


mean_sd(mtcars, disp, mpg, IV = am)
median_iqr(mtcars, disp, mpg, IV = am)
mrank_median(mtcars, mpg, disp, IV = am)


var_labels(mtcars, disp, cyl, hp, mpg)
var_labels(mtcars, disp, cyl, hp, mpg, IV1 = vs)
var_labels(mtcars, disp, cyl, hp, mpg, IV1 = vs, IV2 = am)

var_labels(Bazy::t_niezalezne, COPE_skala1, COPE_skala2, COPE_skala3, IV = Grupa)
conditions_stats(Bazy::t_niezalezne, COPE_skala1, COPE_skala2, COPE_skala3, IV = Grupa)

comparison_bg1(Bazy::t_niezalezne, COPE_skala1, COPE_skala2, COPE_skala3, IV = Grupa)
comparison_bg1(Bazy::t_niezalezne, COPE_skala1, COPE_skala2, COPE_skala3, IV = Grupa, test = "u_mann")
rstatix::wilcox_test(Bazy::t_niezalezne, COPE_skala1 ~ Grupa)
rstatix::wilcox_test(Bazy::t_niezalezne, COPE_skala1 ~ Grupa, detailed = T)
rstatix::wilcox_test(Bazy::t_niezalezne, COPE_skala1 ~ Grupa, exact = T)
rstatix::wilcox_test(Bazy::t_niezalezne, COPE_skala1 ~ Grupa, exact = F, correct = F)


wilcox_Test(Bazy::t_niezalezne, COPE_skala1 ~ Grupa)
wilcox_Test2(Bazy::t_niezalezne, COPE_skala1 ~ Grupa)
result <- wilcox_Test2(Bazy::t_niezalezne, COPE_skala1 ~ Grupa)
result$p.value
qnorm(result$p.value / 2)

mean_sd(mtcars, disp, mpg, IV = am)
conditions_stats(mtcars, disp, mpg, IV = am)

comparison_bg1(mtcars, disp, mpg, IV = am)
wynik2 <- comparison_bg1(mtcars, disp, mpg, IV = am)
attributes(wynik2)

conditions_stats(mtcars, disp, mpg, IV = am, type = "median_iqr")
conditions_stats(mtcars, disp, mpg, IV = am, type = "mrank_median")
wynik <- conditions_stats(mtcars, disp, mpg, IV = am, type = "mrank_median")
attributes(wynik)

attr(wynik, which = "test") <- "anova"

attributes(wynik)


# Testowanie multiple comparisons with one bg -----------------------------


rstatix::wilcox_effsize(mtcars, mpg ~ vs)
wilcox_Test(mtcars, mpg ~ vs)

?comparison_bg1


comparison_bg1(mtcars, mpg, cyl,  hp, IV = am)
comparison_bg1(mtcars, mpg, cyl,  hp, IV = am, labels. = c("Miles per gallon", "Number of cylinders", "Horsepower"))
comparison_bg1(mtcars, mpg, cyl,  hp, IV = am, test = "u_mann")
comparison_bg1(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, IV = Species, test = "kruskal")
comparison_bg1(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, IV = Species, test = "anova")

  # -------------------------------------------------------------------------



compute_var(mtcars$mpg, mtcars$cyl)
compute_var(mtcars$gear, mtcars$carb)

dane_test <- Bazy::Impulsywnosc %>%
  dplyr::select(tidyselect::starts_with("imp")) %>%
  dplyr::mutate(dplyr::across(, ~as.double(.)))

dane_test2 <- dane_test[-1]

purrr::map_df(dane_test2, is.element, 0) %>%
  rowSums()


dane_test2
dplyr::mutate(dane_test2,
  wskaznik_mean = compute_var(imp_1, imp_2, imp_3, imp_4, imp_5, imp_6),
  wskaznik_sum = compute_var(imp_1, imp_2, imp_3, imp_4, imp_5, imp_6, type = "sum"),
  wskaznik_count = compute_var(imp_1, imp_2, imp_3, imp_4, imp_5, imp_6, type = "count", count = 0)
  )

compute_var(dane_test2, type = "sum") # Śmiesznie, nie wiedziałem, że automatycznie zadziała na całej bazie
compute_var(dane_test2, type = "count", count = 0)
compute_var(dane_test2, type = "ble", count = 0)


