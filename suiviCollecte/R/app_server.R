#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinymanager
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      data.frame(
        user = c(Sys.getenv("LOGIN_SITE")), # mandatory
        password = c(Sys.getenv("MDP_SITE")), # mandatory
        start = c("2019-04-15"), # optinal (all others)
        expire = c(NA),
        admin = c(FALSE),
        comment = "Simple and secure authentification mechanism
  for single ‘Shiny’ applications.",
        stringsAsFactors = FALSE
      )
    )
  )

  mod_capi_server("capi_1")
  mod_Cawi_server("Cawi_1")
}
