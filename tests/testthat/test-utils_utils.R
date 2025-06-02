library(testthat)
library(ggplot2)
library(dplyr)
library(labelled)

# utils_is.continuous -----------------------------------------------------
test_that("Détecte les types numériques comme continus", {
  expect_true(utils_is.continuous(1:10))
  expect_true(utils_is.continuous(runif(10)))
})

test_that("Détecte les dates comme continues", {
  expect_true(utils_is.continuous(as.Date("2020-01-01") + 0:5))
})

test_that("Détecte les POSIXct comme continues", {
  expect_true(utils_is.continuous(as.POSIXct("2020-01-01 00:00:00") + 0:5))
})

test_that("Détecte les POSIXlt comme continues", {
  expect_true(utils_is.continuous(as.POSIXlt("2020-01-01 00:00:00") + 0:5))
})

test_that("Détecte les POSIXt comme continues (héritage)", {
  x <- as.POSIXct("2020-01-01 00:00:00")
  class(x) <- c("POSIXt", class(x))
  expect_true(utils_is.continuous(x))
})

test_that("Retourne FALSE pour les types non continus", {
  expect_false(utils_is.continuous(letters))
  expect_false(utils_is.continuous(factor(letters)))
  expect_false(utils_is.continuous(as.logical(c(TRUE, FALSE))))
  expect_false(utils_is.continuous(as.character(1:5)))
})



# Utils_is.discrete -------------------------------------------------------

test_that("Détecte les facteurs comme discrets", {
  expect_true(utils_is.discrete(factor(c("A", "B", "C"))))
})

test_that("Détecte les chaînes de caractères comme discrets", {
  expect_true(utils_is.discrete(c("chien", "chat", "oiseau")))
})

test_that("Retourne FALSE pour les types non discrets", {
  expect_false(utils_is.discrete(1:10))                      # numérique
  expect_false(utils_is.discrete(runif(5)))                  # numérique
  expect_false(utils_is.discrete(as.Date("2020-01-01")))     # date
  expect_false(utils_is.discrete(as.POSIXct("2020-01-01")))  # datetime
  expect_false(utils_is.discrete(c(TRUE, FALSE)))            # logique
})

# utils_get_label -------------------------------------------------------

# Données de test
df <- tibble(
  age = 1:5,
  sexe = factor(c("H", "F", "F", "H", "F"))
)

# Ajout de labels
var_label(df) <- list(
  age = "Âge de l'individu",
  sexe = "Sexe biologique"
)

test_that("Retourne NULL si var est NULL", {
  expect_null(utils_get_label(df, NULL))
})

test_that("Retourne le label correct si présent", {
  expect_equal(utils_get_label(df, "age"), "Âge de l'individu")
  expect_equal(utils_get_label(df, "sexe"), "Sexe biologique")
})

test_that("Erreur si aucun label n'est trouvé", {
  df2 <- df
  var_label(df2) <- NULL
  expect_error(utils_get_label(df2, "age"), "Pas de label trouvé pour la variable age")
})



# utils_get_scale_type ----------------------------------------------------

test_that("Retourne scale_x_continuous pour les numériques", {
  expect_equal(utils_get_scale_type(1:10, "x"), "scale_x_continuous")
  expect_equal(utils_get_scale_type(runif(5), "y"), "scale_y_continuous")
})

test_that("Retourne scale_x_discrete pour les facteurs", {
  expect_equal(utils_get_scale_type(factor(c("A", "B")), "x"), "scale_x_discrete")
})

test_that("Retourne scale_y_discrete pour les chaînes de caractères", {
  expect_equal(utils_get_scale_type(c("a", "b", "c"), "y"), "scale_y_discrete")
})

test_that("Retourne scale_x_datetime pour les POSIXct", {
  expect_equal(utils_get_scale_type(as.POSIXct("2020-01-01"), "x"), "scale_x_datetime")
})

test_that("Retourne scale_y_date pour les Date", {
  expect_equal(utils_get_scale_type(as.Date("2020-01-01"), "y"), "scale_y_date")
})

test_that("Retourne NA si type non reconnu", {
  expect_true(is.na(utils_get_scale_type(list(a = 1), "x")))
})

