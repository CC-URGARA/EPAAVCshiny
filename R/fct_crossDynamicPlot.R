#' CrossDynamicPlot
#'
#' Réalise un graphique différent selon les variables soumises
#'
#' @param tab Tableau de données source
#' @param x_var char : Variable en axe Y
#' @param y_var char : Variable en axe Y
#' @param group_var char : Variable de regroupement (color/fill selon le type de graphique)
#' @param facet_x char : Variable en facet x
#' @param facet_y char : Variable en facet y
#' @param includ_x0 booléen : Le 0 doit-il apparaitre sur le graphique sur l'axe x
#' @param includ_y0 booléen : Le 0 doit-il apparaitre sur le graphique sur l'axe y
#' @param n_categ int : Nombre de catégorie pour discrétiser group_var, facet_x et facet_y si elles sont continues
#' @param show_NA Booléen : Afficher ou non les données manquantes sur le graphique
#'
#' @importFrom rlang sym
#' @importFrom stats as.formula
#' @importFrom ggplot2 aes labeller ggplot geom_histogram geom_bar geom_point geom_boxplot geom_count position_identity
#' @importFrom ggplot2 scale_y_continuous scale_x_continuous scale_y_discrete scale_x_discrete position_dodge label_value
#' @importFrom ggplot2 scale_y_date scale_x_date scale_y_datetime scale_x_datetime scale_fill_discrete  scale_color_discrete facet_grid coord_cartesian
#' @importFrom ggpubr theme_pubclean
#' @importFrom labelled var_label "var_label<-"
#' @importFrom scales pretty_breaks
#' @importFrom dplyr between mutate across any_of filter if_all
#'
#'
#' @returns ggplot
#' @export
#'
#' @examples
#' library(EPAAVCshiny)
#' base = dplyr::as_tibble(mtcars) %>%
#'   dplyr::mutate(
#'     dplyr::across(c("cyl", "vs", "am", "gear", "carb"), ~factor(as.character(.)))
#'   )
#'
#'
#' base <- labelled::set_variable_labels(
#'   base,
#'   "mpg" = "Consommation (M/G)",
#'   "cyl" = "Nombre de cylindres",
#'   "disp" = "Displacement",
#'   "hp" = "Nombre de chevaux",
#'   "drat" = "Rear axle ratio",
#'   "wt" = "Poids",
#'   "qsec" = "1/4 mile time",
#'   "vs" = "Engine type",
#'   "am" = "Transmission type",
#'   "gear" = "Nombre de vitesses",
#'   "carb" = "Nombre de carburateurs")
#'
#' fct_cossDynamicPlot(tab = base, x_var = "mpg")
#' fct_cossDynamicPlot(tab = base, x_var = "mpg", y_var = "am")
#' fct_cossDynamicPlot(tab = base, x_var = "am", y_var = "carb")
#' fct_cossDynamicPlot(tab = base, x_var = "mpg", y_var = "drat", group_var = "am")
#' fct_cossDynamicPlot(tab = base, x_var = "am", y_var = "mpg", group_var = "gear")
#' fct_cossDynamicPlot(tab = base, x_var = "am", y_var = "carb", group_var = "vs")
#' fct_cossDynamicPlot(tab = base, x_var = "am", y_var = "carb", facet_x = "vs")
#' fct_cossDynamicPlot(tab = base, x_var = "am", y_var = "carb", facet_y = "vs")
#' fct_cossDynamicPlot(tab = base, x_var = "am", y_var = "carb", facet_x = "am", facet_y = "vs")
#' fct_cossDynamicPlot(tab = base, x_var = "am", y_var = "carb",
#'                     facet_x = "am", facet_y = "wt", n_categ = 3)
#'
#'
fct_cossDynamicPlot <- function(tab, x_var, y_var = NULL, group_var = NULL,
                                facet_x = NULL, facet_y = NULL,
                                includ_x0 = TRUE, includ_y0 = TRUE,
                                n_categ = 6, show_NA = FALSE){
  if(!between(n_categ, 2, 6)) stop("Le nombre de cat\u00e9gories doit \u00eatre entre 2 et 6")
  #Gestion des cas où "" est utilisé au lieu de NULL
  y_var = if(is.null(y_var) || y_var == "") NULL else y_var
  group_var = if(is.null(group_var) || group_var == "") NULL else group_var
  facet_x = if(is.null(facet_x) || facet_x == "") NULL else facet_x
  facet_y = if(is.null(facet_y) || facet_y == "") NULL else facet_y

  #convertion en symboles pour utilisation dans ggplot
  x_var_sym <- sym(x_var)
  y_var_sym <- if (!is.null(y_var)) sym(y_var) else NULL
  group_var_sym <- if (!is.null(group_var)) sym(group_var) else NULL

  #Création de la formule des facet
  facet_x_recod <- if (!is.null(facet_x)) facet_x else "."
  facet_y_recod <- if (!is.null(facet_y)) facet_y else "."
  facet_formula <- as.formula(paste(facet_y_recod, facet_x_recod, sep = "~"))

  lab_facet_x = utils_get_label(tab, facet_x)
  lab_facet_y = utils_get_label(tab, facet_y)

  custom_labeller <- labeller(
    .cols = if (!is.null(facet_x)) function(x) paste(lab_facet_x, "=", x) else label_value,
    .rows = if (!is.null(facet_y)) function(x) paste(lab_facet_y, "=", x) else label_value
  )

  #Extraction des labels
  x_lab <- utils_get_label(tab, x_var)
  y_lab <- if (!is.null(y_var)) utils_get_label(tab, y_var) else "Nombre"#Si pas de var Y, l'axe y est le nombre de X
  color_lab = utils_get_label(tab, group_var)

  #Discrétisation group_var, facet_x, facet_y si les variables sont continues
  old_labels <- var_label(tab)#sauvegarde des labels avant le mutate qui les supprime
  tab <- tab %>%
    mutate(
      across(any_of(c(group_var, facet_x, facet_y)),
             function(x){
               if(utils_is.discrete(x)) return(x)
               if(utils_is.continuous(x)) return(cut(x, n_categ))
               stop(paste0("Le type de la variable ", x, " n\'a pas \u00e9t\u00e9 reconnu"))
             })
    )
  var_label(tab) <- old_labels#réapplication des labels

  #Exclusion ou non des NA
  if (!show_NA) {
    tab <- tab %>% filter(if_all(any_of(c(x_var, y_var, group_var, facet_x, facet_y)), ~ !is.na(.)))
    if(nrow(tab) == 0) stop("Le tableau s\u00e9lectionn\u00e9 ne contient aucunes valeurs")
  }

  #Initialisation du plot
  plot = ggplot(tab)

  #Détermination du type de plot
  if(is.null(y_var)){
    plot_type = case_when(
      utils_is.continuous(tab[[x_var]]) ~ "geom_histogram",
      utils_is.discrete(tab[[x_var]]) ~ "geom_bar")
  } else {
    plot_type = case_when(
      utils_is.continuous(tab[[x_var]]) & utils_is.continuous(tab[[y_var]]) ~ "geom_point",
      utils_is.continuous(tab[[x_var]]) & utils_is.discrete(tab[[y_var]]) ~ "geom_boxplot",
      utils_is.discrete(tab[[x_var]]) & utils_is.continuous(tab[[y_var]]) ~ "geom_boxplot",
      utils_is.discrete(tab[[x_var]]) & utils_is.discrete(tab[[y_var]]) ~ "geom_count")
  }
  if(is.null(plot_type)){stop("Le type de graphique n\'a pas pu \u00eatre d\u00e9termin\u00e9")}

  #détermination des scales x et y
  scale_x = utils_get_scale_type(tab[[x_var]], axis = "x")
  scale_y = if(!is.null(y_var)) utils_get_scale_type(tab[[y_var]], axis = "y") else "scale_y_continuous"#Si pas de y, y est un compte

  #liste d'argmuents des scales x et y
  scale_x_args = if(scale_x == "scale_x_discrete") list(name = x_lab) else list(name = x_lab, breaks = pretty_breaks(n = 10))
  scale_y_args = if(scale_y == "scale_y_discrete") list(name = y_lab) else list(name = y_lab, breaks = pretty_breaks(n = 10))

  #gestion de xlim et ylim
  xlim = if(scale_x == "scale_x_discrete") c(NA, NA) else if(includ_x0) c(0, NA) else c(NA, NA)
  ylim = if(scale_y == "scale_y_discrete") c(NA, NA) else if(includ_y0) c(0, NA) else c(NA, NA)

  #Ajout de la couche au plot
  #Gestion à part du cas où plot_type = geom_bar car pas d'argument y
  if(plot_type %in% c("geom_bar", "geom_histogram")){
    plot = plot +
      get(plot_type)(aes(x = !!x_var_sym, fill = !!group_var_sym),
                     position = position_identity(), alpha = 0.75)
  } else {
    plot = plot +
      get(plot_type)(aes(x = !!x_var_sym, y = !!y_var_sym, fill = !!group_var_sym, color = !!group_var_sym),
                     position = position_dodge(), alpha = 0.75)

  }

  #Ajout du themes et scales
  plot = plot +
    get(scale_x)(scale_x_args) +
    get(scale_y)(scale_y_args) +
    scale_fill_discrete(name = color_lab) +
    scale_color_discrete(name = color_lab) +
    facet_grid(facet_formula, labeller = custom_labeller) +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    theme_pubclean(base_size = 16, base_family = "serif")

  return(plot)
}
