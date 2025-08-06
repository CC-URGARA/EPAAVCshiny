#' delaisPlot
#'
#' @description Réalise le plot des délais
#'
#' @param tab Un data.frame ou tibble contenant les délais attendus
#' @importFrom dplyr tibble select all_of everything group_by summarise if_else left_join mutate
#' @importFrom tidyr pivot_longer
#' @importFrom stats quantile
#' @importFrom ggplot2 ggplot aes geom_text geom_segment arrow unit theme_void
#'
#' @return ggplot
#'
#' @noRd
fct_delaisPlot <- function(tab) {
  #Vérif de présence des données nécessaires
  list_delais = c("Del_Symp_Adm", "Del_Symp_Imag", "Del_Symp_TIV", "Del_Symp_NRI",
                  "Del_Adm_Imag", "Del_Adm_TIV", "Del_Adm_NRI", "Del_Imag_TIV",
                  "Del_Imag_NRI")
  if(any(!list_delais %in% names(tab))) {
    del_manquant = setdiff(list_delais, names(tab))
    stop(paste0("Les d\u00e9lais suivants ne sont pas pr\u00e9sents dans tab :", paste0(del_manquant, collapse = ", ")))
  }

  #Création de la grille horizontale du plot
  tab_coord_hgrid <- tibble(
    delais = c("Del_Symp_Adm", "Del_Adm_Imag", "Del_Imag_TIV",
               "Del_Symp_Imag", "Del_Imag_NRI",
               "Del_Symp_TIV",
               "Del_Symp_NRI",
               "Del_Adm_TIV",
               "Del_Adm_NRI"),
    x_min = c(0, 1, 2, 0, 2, 0, 0, 1, 1),
    x_max = c(1, 2, 3, 2, 4, 3, 4, 3, 4)  ,
    y_min = c(rep(1, 3), rep(2, 2), 3, 4, 5, 6),
    y_max = y_min
  )

  #Création de la grille verticale du plot
  tab_coord_vgrid <- tibble(
    x = min(tab_coord_hgrid$x_min):max(tab_coord_hgrid$x_max),
    y = max(tab_coord_hgrid$y_max) + 1,
    delais = c("Sympt\u00f4mes", "Admission", "Imagerie", "Thrombolyse", "Thrombectomie")
  )

  #Calcul des délais
  tab_delais <- tab %>%
    select(all_of(list_delais)) %>%
    pivot_longer(cols = everything(), names_to = "delais", values_to = "val_duree") %>%
    group_by(delais) %>%
    summarise(
      n = sum(!is.na(val_duree)),
      med = round(quantile(val_duree, probs = 0.5, na.rm = T)),
      Q1 = round(quantile(val_duree, probs = 0.25, na.rm = T)),
      Q3 = round(quantile(val_duree, probs = 0.75, na.rm = T)),
      lab = if_else(n < 5, "X",
                    paste0(med, " [", Q1, ";", Q3, "] - n = ", n))
    ) %>%
    left_join(tab_coord_hgrid, by = "delais") %>%
    mutate(#Label au milieu de l'axe x et légèrement au dessus du y
      y = y_min + 0.1,
      x = (x_min + x_max)/2
    )

  #Représentation
  plot = ggplot(tab_delais, aes(x = x, y = y)) +
    geom_text(hjust = 0.5, vjust = 0, aes(label = lab), fontface = "bold", size = 5.5) +#délais
    geom_text(data = tab_coord_vgrid, hjust = 0.5, vjust = 0, size = 7,
              aes(label = delais, x = x, y = y), fontface = "bold") +#label des "moments"
    geom_segment(data = tab_coord_vgrid, aes(x = x, xend = x,#grille verticale
                                             y = 0, yend = y), linetype = 2, color = "grey40") +
    geom_segment(data = tab_coord_hgrid, aes(x = x_min, xend = x_max-0.05,
                                             y = y_min, yend = y_max),#grille horizontale
                 arrow = arrow(length = unit(0.05, "npc")), linewidth = 1) +
    theme_void()

  return(plot)

}




