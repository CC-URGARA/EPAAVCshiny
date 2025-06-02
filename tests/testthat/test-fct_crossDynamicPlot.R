library(testthat)
library(ggplot2)
library(dplyr)
library(labelled)

# Données de test
df <- tibble(
  var_x = rnorm(100),
  var_y = sample(letters[1:3], 100, replace = TRUE),
  group = sample(c("A", "B"), 100, replace = TRUE),
  facet1 = sample(c("F1", "F2"), 100, replace = TRUE),
  facet2 = sample(c("G1", "G2"), 100, replace = TRUE)
)

# Ajout de labels simulés
var_label(df) <- list(
  var_x = "Variable X",
  var_y = "Variable Y",
  group = "Groupe",
  facet1 = "Facette X",
  facet2 = "Facette Y"
)

test_that("Erreur si n_categ hors bornes", {
  expect_error(fct_cossDynamicPlot(df, "var_x", n_categ = 1), "catégories doit être entre 2 et 6")
  expect_error(fct_cossDynamicPlot(df, "var_x", n_categ = 7), "catégories doit être entre 2 et 6")
})

test_that("Erreur si toutes les valeurs sont NA et show_NA = FALSE", {
  expect_error(fct_cossDynamicPlot(df_na, "varInexist", show_NA = FALSE))
})

test_that("Retourne un objet ggplot", {
  p <- fct_cossDynamicPlot(df, "var_x")
  expect_s3_class(p, "ggplot")
})

test_that("Type de graphique : histogramme si x est continu et y NULL", {
  p <- fct_cossDynamicPlot(df, "var_x")
  expect_true("GeomBar" %in% class(p$layers[[1]]$geom) || "GeomHistogram" %in% class(p$layers[[1]]$geom))
})

test_that("Type de graphique : boxplot si x est discret et y est continu", {
  p <- fct_cossDynamicPlot(tab = df, x_var = "var_x", y_var = "var_y")
  expect_true("GeomBoxplot" %in% class(p$layers[[1]]$geom))
})

test_that("Facettage fonctionne", {
  p <- fct_cossDynamicPlot(df, "var_x", facet_x = "facet1", facet_y = "facet2")
  expect_true("FacetGrid" %in% class(p$facet))
})

