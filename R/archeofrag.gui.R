
archeofrag.gui <- function(n.cores=NULL){
  shiny::addResourcePath('www', system.file('www', package = 'archeofrag.gui'))
  
  if( ! is.null(n.cores)){
    if(n.cores <= parallel::detectCores()){
      doParallel::registerDoParallel(cl = n.cores) 
    } else{warning(paste(parallel::detectCores(), "cores available, can't use", n.cores, "cores.")) }
  } else{
    doParallel::registerDoParallel() # register half of available cores
  }
  # foreach::getDoParWorkers() 
  shinyApp(ui = ui, server = server)
}
