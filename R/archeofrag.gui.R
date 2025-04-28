
archeofrag.gui <- function(){
  shiny::addResourcePath('www', system.file('www', package = 'archeofrag.gui'))
  doParallel::registerDoParallel() #  half of available cores parallel::detectCores()
  # foreach::getDoParWorkers() 
  shinyApp(ui = ui, server = server)
}


 
