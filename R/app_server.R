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

  #Appel serveurs
  # mod_login_page_server("login_page", r_global = r_global)
  mod_accueil_server("accueil", r_global = r_global)
  mod_structures_server("structures", r_global = r_global)
  mod_pat_delais_server("pat_delais", r_global = r_global)
  mod_pat_parcours_server("pat_parcours", r_global = r_global)
  mod_pat_graph_cust_server("pat_graph_cust", r_global = r_global)
}
