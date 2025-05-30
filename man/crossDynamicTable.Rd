% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_crossDynamicTable.R
\name{crossDynamicTable}
\alias{crossDynamicTable}
\title{crossDynamicTable}
\usage{
crossDynamicTable(
  tab,
  x_var,
  y_var = NULL,
  group_var = NULL,
  facet_x = NULL,
  facet_y = NULL,
  n_categ = 4
)
}
\arguments{
\item{tab}{Tableau de données source}

\item{x_var}{char : Variable en axe Y}

\item{y_var}{char : Variable en axe Y}

\item{group_var}{char : Variable de regroupement (color/fill selon le type de graphique)}

\item{facet_x}{char : Variable en facet x}

\item{facet_y}{char : Variable en facet y}

\item{n_categ}{int : Nombre de catégorie pour discrétiser group_var, facet_x et facet_y si elles sont continues}
}
\value{
tibble
}
\description{
Réalise un tableau croisé dynamique correspondant au plot de crossDynamicPlot selon les variables soumises
}
\examples{
library(EPAAVCshiny)

}
