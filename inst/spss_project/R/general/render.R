
rmarkdown::render(
  input = "notebooks/04 - Final Analysis.Rmd",
  output_file = paste0(basename(getwd()), " - Wyniki ", Sys.Date(), ".docx"),
  output_dir = "results"
)



