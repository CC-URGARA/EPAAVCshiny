#' login_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyauthr loginUI loginServer
mod_login_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyauthr::loginUI(id = ns("login"),
                        title = "Identifiez-vous",
                        user_title = "Nom d\'utilisateur",
                        pass_title = "Mot de passe",
                        login_title = "Se connecter",
                        error_message = "Nom d\'utilisateur ou mot de passe invalide !")
  )
}

#' login_page Server Functions
#'
#' @importFrom golem invoke_js
#' @importFrom RSQLite SQLite dbConnect dbReadTable dbAppendTable
#'
#' @noRd
mod_login_page_server <- function(id, r_global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #Initialisation de credentials
    r_local <- reactiveValues()

    #Appel au "loginServer" pour remplir l'objet credential en cas d'authentification correcte
    observeEvent(eventExpr = r_global$db_con, handlerExpr = {
      r_local$credentials <- shinyauthr::loginServer(
        id = "login",
        data = dbReadTable(conn = r_global$db_con, name = "utilisateurs"),
        user_col = utilisateur,
        pwd_col = mdp,
        sodium_hashed = TRUE
      )})

    #Comportement en cas d'authentification réussie
    observeEvent(eventExpr = r_local$credentials()$user_auth, handlerExpr = {
      req(r_local$credentials()$user_auth)
      #Update du r_global avec les infos d'authentification
      r_global$user_auth = r_local$credentials()$user_auth
      r_global$user_info = r_local$credentials()$info

      #Initialisation des sources de données
      r_global$data = tab_EPA_AVC

      #Apparition/disparition de l'UI
      golem::invoke_js("show", "#hidden_menu")
      golem::invoke_js("show", "#logout_bttn")
      golem::invoke_js("hide", "#login_tab")
      golem::invoke_js("clickon", 'a[data-value=\"accueil\"]')

      #Log de la connexion
      dbAppendTable(r_global$db_con, "logs",
                    data.frame(
                      "utilisateur" = r_local$credentials()$info$utilisateur,
                      "action" = "Connexion",
                      "dttm" = Sys.time()
                    ))
    })
  })
}


## To be copied in the UI
# mod_login_page_ui("login_page_1")

## To be copied in the server
# mod_login_page_server("login_page_1")
