#' fct_crossDynamicTable
#'
#' Réalise un tableau croisé dynamique correspondant au plot de fct_crossDynamicPlot selon les variables soumises
#'
#' @param tab Tableau de données source
#' @param x_var char : Variable en axe Y
#' @param y_var char : Variable en axe Y
#' @param group_var char : Variable de regroupement (color/fill selon le type de graphique)
#' @param facet_x char : Variable en facet x
#' @param facet_y char : Variable en facet y
#' @param n_categ int : Nombre de catégorie pour discrétiser group_var, facet_x et facet_y si elles sont continues
#' @param show_NA Booléen : Afficher ou non les données manquantes sur le graphique
#'
#'
#' @importFrom rlang sym expr
#' @importFrom labelled var_label "var_label<-"
#' @importFrom dplyr between mutate across any_of filter if_all recode group_by count summarise n if_else select arrange
#' @importFrom tidyr pivot_longer
#' @importFrom stats median quantile
#' @importFrom purrr imap
#' @returns tibble
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
#' fct_crossDynamicTable(tab = base, x_var = "mpg")
#' fct_crossDynamicTable(tab = base, x_var = "mpg", y_var = "am")
#' fct_crossDynamicTable(tab = base, x_var = "am", y_var = "carb")
#' fct_crossDynamicTable(tab = base, x_var = "mpg", y_var = "drat", group_var = "am")
#' fct_crossDynamicTable(tab = base, x_var = "am", y_var = "mpg", group_var = "gear")
#' fct_crossDynamicTable(tab = base, x_var = "am", y_var = "carb", group_var = "vs")
#' fct_crossDynamicTable(tab = base, x_var = "am", y_var = "carb", facet_x = "vs")
#' fct_crossDynamicTable(tab = base, x_var = "am", y_var = "carb", facet_y = "vs")
#' fct_crossDynamicTable(tab = base, x_var = "am", y_var = "carb", facet_x = "am", facet_y = "vs")
#' fct_crossDynamicTable(tab = base, x_var = "am", y_var = "carb",
#'                       facet_x = "am", facet_y = "wt", n_categ = 3)
fct_crossDynamicTable <- function(tab, x_var, y_var = NULL, group_var = NULL,
                                  facet_x = NULL, facet_y = NULL, n_categ = 4,
                                  show_NA = FALSE){
  #check nb catégories ok
  if(!between(n_categ, 2, 6)) stop("Le nombre de cat\u00e9gories doit \u00eatre entre 2 et 6")
  #check label ok
  vec_lab_var_select = tab %>%
    select(any_of(c(x_var, y_var, group_var, facet_x, facet_y))) %>%
    lapply(var_label)
  if(any(sapply(vec_lab_var_select, is.null))) stop("Toutes les variables s\u00e9lectionn\u00e9es doivent avoir un label")

  #Gestion des cas où "" est utilisé au lieu de NULL
  y_var = if(is.null(y_var) || y_var == "") NULL else y_var
  group_var = if(is.null(group_var) || group_var == "") NULL else group_var
  facet_x = if(is.null(facet_x) || facet_x == "") NULL else facet_x
  facet_y = if(is.null(facet_y) || facet_y == "") NULL else facet_y

  #Discrétisation group_var, facet_x, facet_y si les variables sont continues
  old_labels <- var_label(tab)#sauvegarde des labels avant le mutate qui les supprime
  tab <- tab %>%
    mutate(
      across(any_of(c(group_var, facet_x, facet_y)),
             function(x){
               if(utils_is.discrete(x)) return(x)
               if(utils_is.continuous(x)) return(cut(x, n_categ, include.lowest = TRUE))
               stop(paste0("Le type de la variable ", x, " n\'a pas \u00e9t\u00e9 reconnu"))
             })
    )
  var_label(tab) <- old_labels#réapplication des labels

  #Exclusion ou non des NA
  if (!show_NA) {
    tab <- tab %>% filter(if_all(any_of(c(x_var, y_var, group_var, facet_x, facet_y)), ~ !is.na(.)))
    if(nrow(tab) == 0) stop("Le tableau s\u00e9lectionn\u00e9 ne contient aucunes valeurs")
  }

  #Initialisation de la liste des variables continues et discretes à traiter
  vec_var_cont = c()
  vec_var_disc = c(group_var, facet_x, facet_y)


  #Détermination du type de tableau et des var continues/discretes
  ##x_var
  if(utils_is.continuous(tab[[x_var]])) {
    tab_type = "quantile"
    vec_var_cont = c(vec_var_cont, x_var)
  } else {
    tab_type = "count"
    vec_var_disc = c(vec_var_disc, x_var)
  }
  ##y_var
  if(!is.null(y_var)){
    if(utils_is.continuous(tab[[y_var]])) {
      tab_type = "quantile"
      vec_var_cont = c(vec_var_cont, y_var)
    } else {
      if(!tab_type %in% "quantile") tab_type = "count"
      vec_var_disc = c(vec_var_disc, y_var)
    }
  }


  #Création du tableau
  # Si des variables continues sont présentes, on les transforme en format long pour le calcul des statistiques

  if(tab_type == "quantile"){
    tab = tab %>%
      pivot_longer(any_of(vec_var_cont), names_to = "Variable", values_to = "Valeur") %>%
      mutate(Variable = recode(Variable, !!!old_labels))#renommage avec labels

  }

  #Regroupement par toutes les var de regroupement
  tab = tab %>%
    group_by(
      across(any_of(vec_var_disc))
    )

  #Calcul du tableau
  if(tab_type == "count"){
    tab_res = tab %>%
      count(name = "N")
  } else if(tab_type == "quantile"){
    tab_res = tab %>%
      summarise(
        N = n(),
        "Renseign\u00e9" = sum(!is.na(Valeur)),
        "M\u00e9diane" = median(Valeur, na.rm = T),
        `Minimum` = min(Valeur, na.rm = T),
        `Q25%` = quantile(Valeur, probs = 0.25, na.rm = TRUE),
        `Q75%` = quantile(Valeur, probs = 0.75, na.rm = TRUE),
        `Maximum` = max(Valeur, na.rm = T)
      )
  }

  #Anonymisation du tableau + application des labels sauvegardés plus tôt
  tab_res = tab_res %>%
    mutate(
      across(any_of(c("N", "Renseign\u00e9")),
             function(x){
               if_else(between(x, 1, 5), "1 \u00e0 5", as.character(x))
             }
      )
    ) %>%
    arrange(across(any_of(c(group_var, facet_x, facet_y, "Variable"))))
  names(tab_res) <- recode(names(tab_res), !!!old_labels)#réapplication des labels

  #formatage en datatable
  tab_res_DT = tab_res %>% DT_theme()

  return(tab_res_DT)
}
