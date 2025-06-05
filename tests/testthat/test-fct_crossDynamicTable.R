library(testthat)
library(dplyr)

# Exemple de jeu de données
df_test <- tibble(
  sexe = factor(c("H", "F", "F", "H", "F", "H")),
  age = c(25, 30, 45, 50, 35, NA),
  revenu = c(2000, 2500, 3000, 4000, 3500, 2800),
  groupe = c("A", "B", "A", "B", "A", "B"),
  var_sans_label = "8"
)

df_test <- labelled::set_variable_labels(
  df_test,
 "sexe" = "Sexe",
 "age" = "Age",
 "revenu" = "Revenu",
 "groupe" = "Groupe")


test_that("Erreur si n_categ hors bornes", {
  expect_error(fct_fct_crossDynamicTable(df_test, x_var = "age", n_categ = 1))
  expect_error(fct_crossDynamicTable(df_test, x_var = "age", n_categ = 7))
})

test_that("Erreur si variable sans label", {
  expect_error(fct_crossDynamicTable(df_test, x_var = "var_sans_label"))
})

test_that("Fonctionne avec une variable continue seule", {
  res <- fct_crossDynamicTable(df_test, x_var = "age")
  expect_s3_class(res, "datatables")
  expect_s3_class(res, "htmlwidget")
  expect_true(all(c("Médiane", "Minimum", "Maximum") %in% names(res$x$data)))
})

test_that("Fonctionne avec une variable discrète seule", {
  res <- fct_crossDynamicTable(df_test, x_var = "sexe")
  expect_s3_class(res, "datatables")
  expect_s3_class(res, "htmlwidget")
  expect_true("N" %in% names(res$x$data))
})

test_that("Fonctionne avec x et y continus", {
  res <- fct_crossDynamicTable(df_test, x_var = "age", y_var = "revenu")
  expect_s3_class(res, "datatables")
  expect_s3_class(res, "htmlwidget")
  expect_true("Médiane" %in% names(res$x$data))
})

test_that("Fonctionne avec group_var", {
  res <- fct_crossDynamicTable(df_test, x_var = "age", group_var = "groupe")
  expect_s3_class(res, "datatables")
  expect_s3_class(res, "htmlwidget")
  expect_true("Groupe" %in% names(res$x$data))
})

test_that("Anonymisation correcte des petits effectifs", {
  df_small <- df_test[1:5, ]
  res <- fct_crossDynamicTable(df_small, x_var = "sexe")
  expect_true(any(res$x$data$N == "1 à 5"))
})

