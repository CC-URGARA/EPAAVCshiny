# Thu Jul  4 14:42:04 2024 ------------------------------
# Ce script vise à créer la base de données des mdp et des logs
# Ne pas relancer ce script à part pour réinitialiser logs et mdp
stop("DO NOT RUN")
library(DBI)
library(RSQLite)
library(tidyverse)

#Ouverture/création de la base
mydb <- dbConnect(RSQLite::SQLite(), "inst/DB/DB_outilFragilite.sqlite")
dbReadTable(mydb, "logs")

#Création de la table des mdp
dbSendStatement(mydb,
                'CREATE TABLE utilisateurs
                (
                  utilisateur VARCHAR(50) PRIMARY KEY NOT NULL,
                  mdp VARCHAR(50) NOT NULL,
                  entite VARCHAR(255),
                  type_utilisateur VARCHAR(255)
                )')

dbListTables(mydb)
dbReadTable(mydb, "utilisateurs")

dbAppendTable(mydb, "utilisateurs",
              tibble(
                "utilisateur" = c("user1", "user2"),
                "mdp" = c(sodium::password_store("pass1"),
                          sodium::password_store("pass2")),
                "entite" = c("test1", "test2"),
                "type_utilisateur" = c("admin", "user"),
              ))
dbReadTable(mydb, "utilisateurs")

#Création de la table des logs
dbSendStatement(mydb,
                'CREATE TABLE logs
                (
                  utilisateur VARCHAR(50) NOT NULL,
                  action VARCHAR(50) NOT NULL,
                  dttm DATETIME NOT NULL
                )')

dbListTables(mydb)
dbReadTable(mydb, "logs")


#deco

dbDisconnect(mydb)

