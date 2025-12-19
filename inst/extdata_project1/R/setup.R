
# knitr -------------------------------------------------------------------


knitr::opts_chunk$set(
  echo = T,
  fig.width=8,
  # fig.height=4,
  warning = F,
  dpi = 300,
  message = F,
  comment = NA,
  fig.showtext = TRUE
)




# libraries ---------------------------------------------------------------



if(!require('pacman')) {install.packages('pacman')}

# Load all libraries
pacman::p_load(
   tidyverse
  )






# ggplot aesthetics -------------------------------------------------------

colors <- c(
  "#FFB319",
  "#FFE194",
  "#E8F6EF",
  "#B8DFD8"
)




geoms <- c(
  "point", "line", "path", "smooth", "segment", "text", "col",
  "bar", "boxplot", "violin", "area", "tile", "ribbon"
)

for (g in geoms) {
  update_geom_defaults(g, list(colour = colors[1], fill = colors[1]))
}

update_stat_defaults("bin", list(colour = colors[1], fill = colors[1], aes(y = after_stat(count))))


my_ggplot <- function() {
  theme_minimal() +
    theme(
      legend.position = "top",
      strip.text.x = element_text(size = 6),
      # text = element_text(family = "Times New Roman"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(size = 10),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )

}



theme_set(my_ggplot())




# others ------------------------------------------------------------------



# Set up parallel backend
num_cores <- detectCores() - 1
