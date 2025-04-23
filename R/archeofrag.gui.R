
archeofrag.gui <- function(){
  shiny::addResourcePath('www', system.file('www', package = 'archeofrag.gui'))
  doParallel::registerDoParallel(ceiling(parallel::detectCores() * .66)) #  2/3 of available cores
  # foreach::getDoParWorkers() 
  shinyApp(ui = ui, server = server)
}


 