## code temporaire sur mtcars en attendant la vrai base
library(tidyverse)
tab_EPA_AVC = dplyr::as_tibble(mtcars) %>%
  dplyr::mutate(
    dplyr::across(c("cyl", "vs", "am", "gear", "carb"), ~factor(as.character(.)))
  )


tab_EPA_AVC <- labelled::set_variable_labels(
  tab_EPA_AVC,
  "mpg" = "Consommation (M/G)",
  "cyl" = "Nombre de cylindres",
  # "disp" = "Displacement",
  "hp" = "Nombre de chevaux",
  "drat" = "Rear axle ratio",
  "wt" = "Poids",
  "qsec" = "1/4 mile time",
  "vs" = "Engine type",
  "am" = "Transmission type",
  "gear" = "Nombre de vitesses",
  "carb" = "Nombre de carburateurs")

usethis::use_data(tab_EPA_AVC, overwrite = TRUE)
