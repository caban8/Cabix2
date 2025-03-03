
# Load SPSS markdown setup ------------------------------------------------



# Set figures and tables centralized options
knitr::opts_chunk$set(
  echo = FALSE,
  comment = NA,
  message = F,
  warning = F,
  fig.width = 6,
  out.width = "70%",
  fig.align = "left",
  ft.align = "left",
  fig.topcaption = FALSE,
  dpi = 300,

  tab.cap.fp_text = officer::fp_text_lite(
    italic = FALSE,
    bold = TRUE,
    font.size = 12,
    font.family = "Times New Roman"),
  fig.cap.fp_text = officer::fp_text_lite(
    italic = FALSE,
    bold = TRUE,
    font.size = 12,
    font.family = "Times New Roman")
)



# Set Polish characters
Sys.setlocale("LC_ALL", "Polish") # Z jakiegoś powodu to jednak nie działa z pakietami stringr itp
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")



# Set comma as the decimal mark
options(OutDec = ",")
set_flextable_defaults(
  font.family = "Times New Roman",  decimal.mark = ","
)




# Libraries ---------------------------------------------------------------



pacman::p_load(
  car,
  onewaytests,
  rstatix,
  tidyverse,
  readxl,
  lm.beta,
  haven
)

# Load libraries I don't want to update
library(flextable)
library(officer)

# Load my own libraries
library(Cabix)
library(Cabix2)
library(Cabflex)
library(Cabflex2)
library(CabStrings)




# ggplot Aesthetics -------------------------------------------------------



# Set centralized plot styling
qual_cols <- c(
  "#4793AF",
  "#FFC470",
  "#DD5746",
  "#8B322C"
)

if (requireNamespace("thematic"))
  thematic::thematic_rmd(
    # bg = "#4793AF",
    # fg = "#FFC470",
    # sequential = ,
    # font = ,
    qualitative = qual_cols
    # accent = "#DD5746"
  )

