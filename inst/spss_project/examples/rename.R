etl_rename <- function(data) {
  data |>
    set_names(
      "time",
      "zgoda",
      "plec",
      "wiek",
      "wyksztalcenie",
      "miejsce_zamieszkania",
      "stan_cywilny",
      str_c("OSI_SzP", 1:22),
      str_c("OSI_zdrowieA", 1:18),
      str_c("OSI_zdrowieB", 1:12),
      str_c("OSI_ZP", 1:61),
      str_c("OSI_ZszS", 1:28)
    )
}
