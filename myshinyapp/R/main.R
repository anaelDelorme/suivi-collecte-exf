
runApp <- function(){
  entreprise <- myshinyapp::populate_table("adelorme", "data_collecte_exf/entreprise.parquet")
  gestion <- myshinyapp::populate_table("adelorme", "data_collecte_exf/gestion.parquet")
  dossier <- myshinyapp::populate_table("adelorme", "data_collecte_exf/dossier.parquet")
  annee_courant <- myshinyapp::populate_table("adelorme", "data_collecte_exf/annee_courant.parquet")
  
  appDir <- system.file("app", package = "myshinyapp")
  shiny::runApp(appDir, display.mode = "normal")
}
