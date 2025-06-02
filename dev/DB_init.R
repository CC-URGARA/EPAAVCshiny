# Thu Jul  4 14:42:04 2024 ------------------------------
#DO NOT RUN
stop("DO NOT RUN")
#Initialisation de la base sqlite utilisée par shinymanager
library(shinymanager)
library(RSQLite)
library(DBI)

mydb <- dbConnect(RSQLite::SQLite(), "inst/DB/credentials.sqlite")

# Création des identifiants
credentials <- data.frame(
  user = c("admin", "user"),
  password = c("admin", "user"), # mots de passe en clair
  admin = c(TRUE, FALSE),              # rôle admin ou non
  stringsAsFactors = FALSE,
  profil = c("urgara", "urgara")
)

# Création de la base SQLite avec hashage des mots de passe
create_db(
  credentials_data = credentials,
  sqlite_path = "inst/DB/credentials.sqlite", # chemin vers la base
  # passphrase = "cf authentificator C Claustre"  # clé de chiffrement
)

