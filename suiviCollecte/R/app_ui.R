#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bs4Dash shinymanager
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4Dash::dashboardPage(
      title = "Suivi collecte EXF",
      fullscreen = TRUE,
      header = bs4Dash::dashboardHeader(
        title = "Suivi collecte EXF",
        status = "olive",
        sidebarIcon = shiny::icon("tree")
      ),
      sidebar = dashboardSidebar(
        status = "olive",
        elevation = 4,
        collapsed = FALSE,
        id = "sidebar",
        bs4Dash::sidebarMenu(
          id = "sidebarmenu",
          bs4Dash::menuItem("Suivi Cawi", tabName = "cawi", icon = icon("globe")),
          bs4Dash::menuItem("Suivi Capi", tabName = "capi", icon = icon("person"))
        )
      ),
      body = dashboardBody(
        bs4Dash::tabItems(
          bs4Dash::tabItem("cawi", mod_Cawi_ui("Cawi_1")),
          bs4Dash::tabItem("capi", mod_capi_ui("capi_1"))
        )
      ),
      footer = bs4Dash::dashboardFooter(
        left = "Equipe EXF",
        right = "2023-2024"
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "testGolemAuth"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
