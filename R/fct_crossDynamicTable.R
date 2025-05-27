#' crossDynamicTable
#'
#' Réalise un tableau croisé dynamique correspondant au plot de crossDynamicPlot selon les variables soumises
#'
#' @param tab Tableau de données source
#' @param x_var char : Variable en axe Y
#' @param y_var char : Variable en axe Y
#' @param group_var char : Variable de regroupement (color/fill selon le type de graphique)
#' @param facet_x char : Variable en facet x
#' @param facet_y char : Variable en facet y
#' @param n_categ int : Nombre de catégorie pour discrétiser group_var, facet_x et facet_y si elles sont continues
#'
#' @importFrom rlang sym
#'
#' @returns tibble
#' @export
#'
#' @examples
#' library(EPAAVCshiny)
#'
crossDynamicTable <- function(tab, x_var, y_var = NULL, group_var = NULL,
                              facet_x = NULL, facet_y = NULL, n_categ = 4){
  #La fonction retourne NULL pour tous les cas autre que 1 continu 1 discrete (boxplot) ou 2 discrete (geom_count)
  # if(!(utils_is.continuous(tab[[x_var]]) & utils_is.discrete(tab[[y_var]]) |
  #      utils_is.discrete(tab[[x_var]]) & utils_is.continuous(tab[[y_var]]) |
  #      utils_is.discrete(tab[[x_var]]) & utils_is.discrete(tab[[y_var]]))) return(NULL)
  #
  # #convertion en symboles pour utilisation dans dplyr
  # x_var_sym <- sym(x_var)
  # y_var_sym <- if (!is.null(y_var)) sym(y_var) else NULL
  # group_var_sym <- if (!is.null(group_var)) sym(group_var) else NULL
  # facet_x_recod <- if (!is.null(facet_x)) sym(facet_x) else NULL
  # facet_y_recod <- if (!is.null(facet_y)) sym(facet_y) else NULL
  #
  # #table croisée
  # cross_tab = tab %>%
  #   group_by(x_var_sym)
  #
  # #Extraction des labels
  # x_lab <- utils_get_label(tab, x_var)
  # y_lab <- if (!is.null(y_var)) utils_get_label(tab, y_var) else "Nombre"#Si pas de var Y, l'axe y est le nombre de X
  # color_lab = utils_get_label(tab, group_var)
  #
  # #Initialisation du plot
  # plot = ggplot(tab)
  #
  # #Détermination du type de plot
  # if(is.null(y_var)){
  #   plot_type = case_when(
  #     utils_is.continuous(tab[[x_var]]) ~ "geom_histogram",
  #     utils_is.discrete(tab[[x_var]]) ~ "geom_bar")
  # } else {
  #   plot_type = case_when(
  #     utils_is.continuous(tab[[x_var]]) & utils_is.continuous(tab[[y_var]]) ~ "geom_point",
  #     utils_is.continuous(tab[[x_var]]) & utils_is.discrete(tab[[y_var]]) ~ "geom_boxplot",
  #     utils_is.discrete(tab[[x_var]]) & utils_is.continuous(tab[[y_var]]) ~ "geom_boxplot",
  #     utils_is.discrete(tab[[x_var]]) & utils_is.discrete(tab[[y_var]]) ~ "geom_count")
  # }
  # if(is.null(plot_type)){stop("Le type de graphique n\'a pas pu \u00eatre d\u00e9termin\u00e9")}
  #
  # #détermination des scales x et y
  # scale_x = utils_get_scale_type(tab[[x_var]], axis = "x")
  # scale_y = if(!is.null(y_var)) utils_get_scale_type(tab[[y_var]], axis = "y") else "scale_y_continuous"#Si pas de y, y est un compte
  #
  # #Ajout de la couche au plot
  # #Gestion à part du cas où plot_type = geom_bar car pas d'argument y
  # if(plot_type %in% c("geom_bar", "geom_histogram")){
  #   plot = plot +
  #     get(plot_type)(aes(x = !!x_var_sym, fill = !!group_var_sym),
  #                    position = position_identity(), alpha = 0.75)
  # } else {
  #   plot = plot +
  #     get(plot_type)(aes(x = !!x_var_sym, y = !!y_var_sym, fill = !!group_var_sym, color = !!group_var_sym),
  #                    position = position_dodge(), alpha = 0.75)
  #
  # }
  #
  # #Ajout du themes et scales
  # plot = plot +
  #   get(scale_x)(name = x_lab) +
  #   get(scale_y)(name = y_lab) +
  #   scale_fill_discrete(name = color_lab) +
  #   scale_color_discrete(name = color_lab) +
  #   facet_grid(facet_formula, labeller = custom_labeller) +
  #   coord_cartesian(xlim = xlim, ylim = ylim) +
  #   theme_pubclean()

  # return(plot)
}
