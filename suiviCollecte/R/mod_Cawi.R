#' Cawi UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import glue
#' @import leaflet
#' @import DT
#' @import dplyr
#' @import janitor
#' @importFrom tidyr pivot_wider pivot_longer unite
#' @importFrom geojsonio topojson_read
#' @import shinyWidgets
#' @importFrom sf st_centroid st_coordinates
#' @import echarts4r
#' @importFrom grDevices col2rgb colors rgb

# source("R/data.R")

mod_Cawi_ui <- function(id){
  ns <- NS(id)

  ### Import des données pour les listes déroulantes
  entreprise <- populate_table("adelorme", "data_collecte_exf/entreprise.parquet")
  gestion <- populate_table("adelorme", "data_collecte_exf/gestion.parquet")
  dossier <- populate_table("adelorme", "data_collecte_exf/dossier.parquet")
  annee_courante <- populate_table("adelorme", "data_collecte_exf/annee_courante.parquet")
  france_regions <- geojsonio::topojson_read(app_sys("regions.topojson"))
  france_departements <- geojsonio::topojson_read(app_sys("departements.topojson"))
  name_regions <- france_regions %>%
    arrange(Name) %>%
    pull(Name)

  nom_gestionnaire <- gestion %>%
    select(NOMSRISE, PRENOMSRISE) %>%
    unique() %>%
    unite( PRENOMSRISE,NOMSRISE,col = libelle, sep = " ") %>%
    filter(!is.na(libelle) & libelle != " ") %>%
    arrange(libelle) %>%
    pull(libelle)

  ###

  r_colors <- rgb(t(col2rgb(colors()) / 255))
  names(r_colors) <- colors()
  tagList(
    fluidRow(
      valueBox(
        value = textOutput(ns('nb_dossier')),
        subtitle = "Questionnaires à collecter",
        color = "info",
        icon = icon("paper-plane")
      ),
      valueBox(
        value = textOutput(ns('taux_collecte')),
        subtitle = "Taux de collecte",
        color = "primary",
        icon = icon("circle-check")
      ),
      valueBox(
        value = textOutput(ns('taux_reponse')),
        subtitle = "Taux de réponse",
        color = "teal",
        icon = icon("thumbs-up")
      )
    ),
    fluidRow(

      bs4Dash::box(title = "Taux de collecte",
                   status = "info",
                   radioGroupButtons(inputId = ns("map_choice"), label = "Représentation par :",
                                choices = c("Département", "Région"),
                                justified = TRUE, selected= "Région"),
                   leafletOutput(ns("map_taux_collecte"))
      ),
      bs4Dash::box(title = "Nombre de questionnaires par région",
                   status = "info",
                   radioGroupButtons(inputId = ns("map_choice_nb_questionnaire"), label = "Représentation par :",
                                     choices = c("Département", "Région"),
                                     justified = TRUE, selected= "Région"),
                   leafletOutput(ns("map_nb_questionnaires"))
      )
    ),
    fluidRow(
      bs4Dash::box(title = "Avancement de la collecte",
                   status = "indigo",

                   height = "600px",
                   sidebar = boxSidebar(
                     id = ns("sidebar_suivi_collecte"),
                     pickerInput(inputId  = ns("choix_region_graph_avancement"),
                                 label = "Filtrer par région:",
                                 choices =  c("France", name_regions),
                                 selected = "France"
                     ),
                     pickerInput(inputId  = ns("choix_gestionnaire_graph_avancement"),
                                 label = "Filtrer par gestionnaire:",
                                 choices =  c("Tous", nom_gestionnaire),
                                 selected = "Tous"
                     ),
                     actionBttn(ns("reset_button"),
                                label = "Réinitialiser",
                                style = "material-flat",
                                color = "success"),
                     actionBttn(ns("update2"),
                                label = "Fermer",
                                style = "material-flat",
                                color = "success")),
                      echarts4rOutput(ns("suivi_collecte")),
                     actionBttn(ns("update"),
                                label = "Filtrer par région ou gestionnaire",
                                style = "bordered",
                                icon = icon("sliders"),
                                color = "success")

        ),
      bs4Dash::box(title = "Collecte par gestionnaire",
                   status = "indigo",
                   DT::dataTableOutput(ns("table_par_gestionnaire")),
                   height = "600px"
      )),
      fluidRow(
        bs4Dash::box(
          title = "Collecte selon le mode de collecte",
          status = "orange",
          height = "500px",
          echarts4rOutput(ns("camenbert_mode_collecte"))

        ),
        bs4Dash::box(
          title = "Suivi par mode de collecte",
          status = "orange",
          height = "500px",
          switchInput(
            inputId = ns("switch_suivi_collecte_cumule"),
            label = "Voir en cumulé",
            labelWidth = "280px"
          ),
          echarts4rOutput(ns("suivi_mode_collecte"))
        )

      )
  )
}
#####################################################################################################
#####################################################################################################
#' Cawi Server Functions
#'
#' @noRd
mod_Cawi_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    france_regions <- geojsonio::topojson_read(app_sys("regions.topojson"))
    france_departements <- geojsonio::topojson_read(app_sys("departements.topojson"))
    entreprise <- populate_table("adelorme", "data_collecte_exf/entreprise.parquet")
    gestion <- populate_table("adelorme", "data_collecte_exf/gestion.parquet")
    dossier <- populate_table("adelorme", "data_collecte_exf/dossier.parquet")
    annee_courante <- populate_table("adelorme", "data_collecte_exf/annee_courante.parquet")
    ### Topojson centroidisés
    suppressWarnings({
      france_regions_centroid <- st_centroid(france_regions)
    })
    france_regions$centroid_longitude <- st_coordinates(france_regions_centroid)[, 1]
    france_regions$centroid_latitude <- st_coordinates(france_regions_centroid)[, 2]
    suppressWarnings({
      france_departements_centroid <- st_centroid(france_departements)
    })
    france_departements$centroid_longitude <- st_coordinates(france_departements_centroid)[, 1]
    france_departements$centroid_latitude <- st_coordinates(france_departements_centroid)[, 2]


    ### Indicateurs
    nombre_dossier <- dossier %>% nrow()
    nombre_dossier_collecte <- dossier %>% filter(ETAT_CONTROLE != 1) %>%  nrow()
    nombre_dossier_repondu <-
      dossier %>%
      left_join(gestion %>%
                  select(Identifiant_dossier, ACCEPT),
                by = "Identifiant_dossier") %>%
      filter(ETAT_CONTROLE != 1 &  ACCEPT == 1) %>%
      nrow()

    # Nombre de dossiers
    output$nb_dossier <- renderText(nombre_dossier)

    # Taux de collecte
    taux_collecte_calcule <- round(100 * nombre_dossier_collecte / nombre_dossier,0)
    output$taux_collecte <- renderText(glue::glue(taux_collecte_calcule," %"))

    # Taux de réponse
    taux_reponse_calcule <- round(100 * nombre_dossier_repondu / nombre_dossier,0)
    output$taux_reponse <- renderText(glue::glue(taux_reponse_calcule," %"))

    ### Entreprises par région
    entreprises_par_regions <- dossier %>%
      left_join(entreprise %>% select(NOM_DOSSIER, HEX_DEPSIEGE_AFFI , HEX_REGSIEGE_AFFI ),
                by = "NOM_DOSSIER")

    calculer_entreprises_par_localisation <- function (df, localisation){
      if(localisation == "Région"){
        col_localisation <- "HEX_REGSIEGE_AFFI"
      }else{
        col_localisation <- "HEX_DEPSIEGE_AFFI"
      }
      df_agrege <- df %>%
        mutate(collecte = ifelse(ETAT_CONTROLE == 1 , "Non collecté", 'Collecté')) %>%
        group_by(collecte, !!sym(col_localisation)) %>%
        count() %>%
        pivot_wider(names_from = collecte, values_from = n) %>%
        filter(!is.na(!!sym(col_localisation))) %>%
        mutate(total = Collecté + `Non collecté`) %>%
        mutate(taux_collecte = Collecté/total)
      return (df_agrege)
    }

    nb_dossier_region <- calculer_entreprises_par_localisation(entreprises_par_regions, "Région")

    nb_dossier_departement <- calculer_entreprises_par_localisation(entreprises_par_regions, "Département")

    # Carte taux de collecte
    pal <- colorNumeric("RdYlGn", NULL)
    data_map <- reactive({
      if (input$map_choice == "Département") {
        data_map <- france_departements %>%
          left_join(nb_dossier_departement, by = c("DEP" = "HEX_DEPSIEGE_AFFI")) %>%
          rename(Name = Nom)
      }
      else{
        data_map <-france_regions %>%
          left_join(nb_dossier_region, by = c("REG" = "HEX_REGSIEGE_AFFI"))
      }
    })
    output$map_taux_collecte <- renderLeaflet({
      data <- data_map()
               leaflet(data) %>%
          addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                       fillColor = ~pal(taux_collecte),
                      label = ~paste0(Name, ": ",
                                      round(100 * taux_collecte,0),
                                      " % / ",Collecté," collectés pour ", total," au total.")) %>%

          addLegend(pal = pal, values = ~round(100*taux_collecte,0), title = "Taux de collecte",
                    opacity = 1, position = "bottomright", na.label= "?",labFormat = labelFormat(suffix=" %"))


    })

    # Carte nb de questionnaires
    data_map_nb_questionnaire <- reactive({
      if (input$map_choice_nb_questionnaire == "Département") {
        data_map_nb_questionnaire <- france_departements %>%
          left_join(nb_dossier_departement, by = c("DEP" = "HEX_DEPSIEGE_AFFI")) %>%
          rename(Name = Nom)
        taille_rond = 300
      }
      else{
        data_map_nb_questionnaire <-france_regions %>%
          left_join(nb_dossier_region, by = c("REG" = "HEX_REGSIEGE_AFFI"))
        taille_rond = 100
      }
      list(df = data_map_nb_questionnaire, taille = taille_rond)
    })

    output$map_nb_questionnaires <- renderLeaflet({
      data_nb_questionnaire <- data_map_nb_questionnaire()$df
      taille_rond <- data_map_nb_questionnaire()$taille
      leaflet(data_nb_questionnaire) %>%
        addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1, color = "white") %>%
        addCircles(
                   lng = ~centroid_longitude,
                   lat = ~centroid_latitude,
                   radius = ~ taille_rond * total,
                   fillOpacity = 0.7,
                   color = "#3d9970",
                   stroke = FALSE,
                   label = ~total
        )
    })

    ####### Suivi de la collecte dans le temps
    # gestion de la barre de filtre
    observeEvent(input$update, {
      updateBoxSidebar("sidebar_suivi_collecte")
    })
    observeEvent(input$update2, {
      updateBoxSidebar("sidebar_suivi_collecte")
    })

    observeEvent(input$reset_button, {
      updateTextInput(session,"choix_region_graph_avancement", value = "France")

      updateTextInput(session, "choix_gestionnaire_graph_avancement", value = "Tous")

    })

    filtre_suivi_collecte <- reactive({
      if (input$choix_region_graph_avancement !=  "France"){
        code_region <- france_regions %>%
          filter(Name == input$choix_region_graph_avancement) %>%
          pull(REG)
        dossier_return <- dossier %>%
          left_join(entreprise %>% select(Identifiant_dossier, HEX_REGSIEGE_AFFI), by = "Identifiant_dossier") %>%
          filter(HEX_REGSIEGE_AFFI == code_region)
      }else{
        dossier_return <- dossier
      }


      if(input$choix_gestionnaire_graph_avancement !=  "Tous"){
        gestionnaire_dossier <- gestion %>%
          select(Identifiant_dossier,NOMSRISE, PRENOMSRISE) %>%
          unite( PRENOMSRISE,NOMSRISE,col = libelle, sep = " ")


        dossier_return <- dossier_return %>%
          left_join(gestionnaire_dossier, by ="Identifiant_dossier" ) %>%
          filter(libelle == input$choix_gestionnaire_graph_avancement)
      }

      return(dossier_return)
    })


     output$suivi_collecte <- renderEcharts4r({
       dossier_filtrer <- filtre_suivi_collecte()
       questionnaire_par_jour <- dossier_filtrer %>%
         select(Identifiant_dossier, DATE_VALIDATION) %>%
         mutate(jour_validation = as.Date(as.POSIXct(DATE_VALIDATION, format = "%Y-%m-%d %H:%M:%OS"))) %>%
         filter(!is.na(jour_validation)) %>%
         count(jour_validation) %>%
         arrange(jour_validation) %>%
         mutate(cum_nb_questionnaire = cumsum(n))

       questionnaire_par_jour %>%
        e_charts(jour_validation, height = "900px") %>%
        e_line(cum_nb_questionnaire, name = "Cumulé") %>%
        e_bar(n, name = "Par jour") %>%
        e_x_axis(type = "time", boundary_gap = 0) %>%
        e_tooltip(formatter = htmlwidgets::JS("function(params) {return params.value[0] + ' : ' + params.value[1];}")) %>%
        e_title("") %>%
        e_y_axis(name = "Questionnaires validés")
    })

    ######### Table nb questionnaires par gestionnaire
     nb_dossier_gestionnaire <- dossier %>%
       select(Identifiant_dossier, ETAT_CONTROLE) %>%
       left_join(gestion %>% select(Identifiant_dossier, NOMSRISE ,PRENOMSRISE  ), by = "Identifiant_dossier") %>%
       left_join(annee_courante %>% select(Identifiant_dossier, MAJC ), by = "Identifiant_dossier") %>%
       mutate(controle = case_when(
         ETAT_CONTROLE!=1 & MAJC %in% c(1,2) ~ "Réponse ok",
         ETAT_CONTROLE!=1 & !MAJC %in% c(1,2) ~ "Collecté",
         TRUE ~ "Non collecté")) %>%
       group_by(NOMSRISE,PRENOMSRISE,controle) %>%
       count() %>%
       pivot_wider(names_from = controle, values_from = n) %>%
       adorn_totals(where="col") %>%
       adorn_percentages("row",... =`Réponse ok`:`Non collecté`) %>%
       adorn_pct_formatting(digits=0,... =`Réponse ok`:`Non collecté`) %>%
       adorn_ns(... = `Réponse ok`:`Non collecté`) %>%
       arrange(NOMSRISE, PRENOMSRISE) %>%
       rename(Nom= NOMSRISE,
              Prénom=PRENOMSRISE)

     nb_dossier_gestionnaire_DT <- nb_dossier_gestionnaire %>%
       datatable(rownames = FALSE,
                 extensions = c("Scroller", "FixedColumns", "Buttons", "Select"),
                 selection = "none",
                 options = list(
                   dom = "Bfrtip",
                   # scroll :
                   scrollY = 400, scrollX = 400, scroller = TRUE,
                   # fixer les colonnes :
                   fixedColumns = list(leftColumns = 1),
                   # selection :
                   select = list(style = 'os', items = 'row'),
                   buttons = c(
                     # enregistrements
                      'csv', 'excel', 'pdf',
                     # selection des elements
                     'selectAll', 'selectNone', 'selectRows'
                   )
                 )
       )


       output$table_par_gestionnaire <-
         DT::renderDataTable(
           nb_dossier_gestionnaire_DT
         )



     ######### Camembert saisie gestionnaire vs saisie internet
     nb_questionnaire_mode_collecte <- dossier %>%
       filter(nchar(DATE_COURRIER)>2 | nchar(DATE_INTERNET)>2) %>%
       mutate(type_collecte = ifelse(nchar(DATE_COURRIER)>2, "Saisie gestionnaire", "Saisie internet")) %>%
       count(type_collecte)
     output$camenbert_mode_collecte <- renderEcharts4r({
       nb_questionnaire_mode_collecte |>
         e_charts(type_collecte) |>
         e_pie(n, radius = c("40%", "70%")) |>
         e_tooltip(formatter = htmlwidgets::JS("function(params) {return params.name + ': ' + params.value;}"))
     })


     ######### Suivi de la collecte par mode de collecte
     questionnaire_par_jour <-dossier %>%
       select(Identifiant_dossier, DATE_COURRIER, DATE_INTERNET) %>%
       mutate(jour_courrier = as.Date(as.POSIXct(DATE_COURRIER, format = "%Y-%m-%d %H:%M:%OS")),
              jour_internet = as.Date(as.POSIXct(DATE_INTERNET, format = "%Y-%m-%d %H:%M:%OS"))) %>%
       filter(!is.na(jour_courrier) | !is.na(jour_internet)) %>%
       mutate(jour = if_else (is.na(jour_courrier), jour_internet, jour_courrier)) %>%
       mutate(type = if_else (is.na(jour_courrier), "Internet", "Gestionnaire")) %>%
       group_by(jour) %>%
       count(type) %>%
       ungroup() %>%
       arrange(jour) %>%
       pivot_wider(names_from = type, values_from = n)



     choix_cumule_suivi_mode_collecte <- reactive({
       if (input$switch_suivi_collecte_cumule ==  TRUE){
         questionnaire_par_jour_cumule <- questionnaire_par_jour %>%
           mutate(across(where(is.numeric), ~ifelse(is.na(.),0,.))) %>%
           mutate(cum_gestionnaire = cumsum(Gestionnaire), cum_internet=cumsum(Internet))
         type = "cumulé"
       }else{
         questionnaire_par_jour_cumule <- questionnaire_par_jour
         type = "indiv"
       }
       list(df = questionnaire_par_jour_cumule, type_graph = type)

     })



     output$suivi_mode_collecte <- renderEcharts4r({
       data_nb_questionnaire <- choix_cumule_suivi_mode_collecte()$df
       type <- choix_cumule_suivi_mode_collecte()$type_graph
       if (type == "indiv"){
         questionnaire_par_jour |>
         e_charts(jour) |>
           e_bar(Gestionnaire, name = "Saisie gestionnaire", stack="stack1") |>
           e_bar(Internet, name = "Saisie Internet", stack="stack1")
       }else{
         data_nb_questionnaire |>
           e_charts(jour) |>
           e_area(cum_gestionnaire, name = "Saisie gestionnaire", stack="stack1") |>
           e_area(cum_internet, name = "Saisie Internet", stack="stack1")
       }

     })


  })
}

## To be copied in the UI
# mod_Cawi_ui("Cawi_1")

## To be copied in the server
# mod_Cawi_server("Cawi_1")
