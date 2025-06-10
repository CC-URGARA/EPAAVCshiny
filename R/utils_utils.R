
#' check continuous
#' Vérifie qu'une variable est numéric (double/int) ou temporelle
#'
#' @param var vecteur variable a tester
#'
#' @importFrom lubridate is.POSIXct is.POSIXt is.POSIXlt is.Date
#'
#' @returns Booléen
#'
utils_is.continuous <- function(var){
  res = is.numeric(var) | is.POSIXct(var) | is.POSIXt(var) | is.POSIXlt(var) | is.Date(var)
  return(res)
}

#' check discrete
#'
#' Vérifie qu'une variable est character ou factor
#'
#' @param var vecteur variable a tester
#'
#' @returns Booléen
#'
utils_is.discrete <- function(var){
  res = is.factor(var) | is.character(var)
  return(res)
}

#' Extract label variable
#'
#' Extrait le champ label d'une variable. Retourne une erreur si pas de label
#'
#' @param tab tableau de données dans lequel se trouve la variable
#' @param var Char : Nom de la variable
#'
#' @importFrom labelled get_label_attribute
#' @importFrom rlang sym
#' @importFrom dplyr pull
#'
#' @returns char
#'
utils_get_label = function(tab, var){
  if(is.null(var)) return(NULL)
  label = tab %>% pull(sym(var)) %>% get_label_attribute()
  if(is.null(label)) stop(paste0("Pas de label trouv\u00e9 pour la variable ", var))
  return(label)
}

#' Get scale type
#'
#' Détermine le type de scale à appliquer à un ggplot selon le type d'une variable
#'
#' @param var vecteur variable à tester
#' @param axis axe (x/y) dans lequel la variable sera utilisée
#'
#' @importFrom dplyr case_when
#'
#' @returns char
#'
utils_get_scale_type <- function(var, axis){
  scale = case_when(
    is.numeric(var) ~ paste0("scale_", axis, "_continuous"),
    is.factor(var) ~ paste0("scale_", axis, "_discrete"),
    is.character(var) ~ paste0("scale_", axis, "_discrete"),
    is.POSIXct(var) ~ paste0("scale_", axis, "_datetime"),
    is.Date(var) ~ paste0("scale_", axis, "_date")
  )
  return(scale)
}


#' Add company logo to a plot
#'
#' @description Adds Urg'Ara logo to a plot.
#'
#' @param plot A ggplot object
#' @param height The height of the logo in proportion of the graph (default 0.1)
#' @param width The width of the logo in proportion of the graph (default 0.2)
#'
#' @return a ggplot2 object
plot_add_logo <- function(plot,
                          height = 0.1, width = 0.2){
  #type check
  if(!is.numeric(height) | !is.numeric(width)){
    stop("Height and width must be numerical")} else
      if(!dplyr::between(height, 0, 1) | !dplyr::between(width, 0, 1)){
        stop("Height and width must be between 0 and 1")
      }
  if(!ggplot2::is_ggplot(plot)) stop("plot must be a ggplot object")

  #removing margins of input plot
  plot = plot +
        ggplot2::theme(plot.margin = ggplot2::margin(b = 0))
  #loading of the logo
  logo = system.file("app/www/img/Logo_UrgAra_Long.png", package = "EPAAVCshiny")
  logo_img = magick::image_read(logo)

  #resolution of position
  xy_logo = c(1-width, height/2)
  #y coordinate of the plot (0 if logo at the top, 0+heigth if logo at the bottom)
  y_plot = 0 + height

  #adding logo image to the plot at designated coordinates
  plot_logoed = cowplot::ggdraw() +
    cowplot::draw_plot(plot, x = 0, y = y_plot, width = 1, height = 1-height) +
    cowplot::draw_image(logo_img, x = xy_logo[1], y = xy_logo[2], width = width, height = height, vjust = 0.5)

  return(plot_logoed)
}

