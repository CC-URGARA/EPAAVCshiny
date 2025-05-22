
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
    is.POSIXct(var) ~ paste0("scale_", axis, "_datetime"),
    is.Date(var) ~ paste0("scale_", axis, "_date")
  )
  return(scale)
}

