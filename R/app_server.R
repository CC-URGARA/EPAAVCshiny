#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom golem invoke_js
#' @importFrom RSQLite dbConnect dbDisconnect
#' @noRd
app_server <- function(input, output, session) {
  #Authentification
  passphrase <- keyring::key_get(service = "shinymanager", username = "EPAAVCshiny")
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      db = system.file("DB", "credentials.sqlite", package = "EPAAVCshiny"),
      passphrase = passphrase
    )
  )

  # Your application server logic
  # Initialisation du r_global
  r_global = reactiveValues()
  r_global$data <- tab_EPA_AVC

  #Connection à la base de données (gestion des identifiants + logs)
  # r_global$db_con = dbConnect(drv = SQLite(), app_sys("DB/DB_outilFragilite.sqlite"))

  # Serveur logout
  # observeEvent(eventExpr = input$logout_bttn, handlerExpr = {
  #   #Log de le deconnexion
  #   dbAppendTable(r_global$db_con, "logs",
  #                 data.frame(
  #                   "utilisateur" = r_global$user_info$utilisateur,
  #                   "action" = "Deconnexion",
  #                   "dttm" = Sys.time()
  #                 ))
  #
  #   #réinitialisation du r_global
  #   ##Note : Vider r_global en faisant r_global <- reactiveValues()
  #   ##ne transmet pas l'information dans les modules
  #   r_global$user_auth = FALSE
  #   r_global$user_info = NULL
  #
  #   #Apparition/disparition de l'UI
  #   invoke_js("show", "#login_tab")
  #   invoke_js("clickon", 'a[data-value="login_page"]')
  #   invoke_js("hide", "#hidden_menu")
  #   invoke_js("hide", "#logout_bttn")
  #   invoke_js("show", "#login_page-login-panel")
  # })

  #Appel serveurs
  # mod_login_page_server("login_page", r_global = r_global)
  mod_accueil_server("accueil", r_global = r_global)
  mod_structures_server("structures", r_global = r_global)
  mod_pat_delais_server("pat_delais", r_global = r_global)
  mod_pat_parcours_server("pat_parcours", r_global = r_global)
  mod_pat_graph_cust_server("pat_graph_cust", r_global = r_global)

  #A la fermeture de la session
  # onSessionEnded(function() {
  #   #log de la fin de session si l'utilisateur est toujours connecté
  #   utilisateur = isolate(r_global$user_info$utilisateur)
  #   if(!is.null(utilisateur)){
  #     dbAppendTable(isolate(r_global$db_con), "logs",
  #                   data.frame(
  #                     "utilisateur" = utilisateur,
  #                     "action" = "finSession",
  #                     "dttm" = Sys.time()
  #                   ))
  #   }
  #
  #   #déconnection de la bdd
  #   dbDisconnect(conn = isolate(r_global$db_con))
  # })
}
