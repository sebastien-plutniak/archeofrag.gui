.check_weighting_variable <- function(g, var){
  values <- igraph::vertex_attr(g, var)

  if(is.null(values)){
    return(g)
  }
  
  if(var != "-" & ! is.numeric(values)){
    g <- igraph::delete_vertex_attr(g, name = var)
    
    showNotification(paste0("No numerical values in the '", var, "' variable. This variable is not used."),
                     duration = 10, type = "message")
    return(g)
  }
  
  idx <- is.na(values) | values == ""
  if(sum(idx)){ 
    g <- igraph::delete_vertices(g, idx) 
    showNotification(paste0("Incomplete values in '", var, "'. ", as.character(sum(idx)), " fragments removed."),
                     duration = 10, type = "message")
  }
  g
}
