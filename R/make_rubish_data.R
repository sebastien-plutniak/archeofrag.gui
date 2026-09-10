.make_rubish_data <- function(){
    l13 <- archeofrag::frag.simul.process(n.components=24, vertices=70, disturbance=.4, balance = .6)
    igraph::V(l13)[igraph::V(l13)$layer == "2"]$layer <- "3"
    
    l24 <- archeofrag::frag.simul.process(n.components=20, vertices=44, balance=.6, disturbance=0)
    igraph::V(l24)[igraph::V(l24)$layer == "1"]$layer <- "4"
    igraph::V(l24)$name <- paste0(igraph::V(l24)$name, "l24")
    
    l5 <- archeofrag::frag.simul.process(n.components=5, vertices=20)
    igraph::V(l5)$layer  <- "5"
    igraph::V(l5)$name <- paste0(igraph::V(l5)$name, "l5")
    
    l6 <- archeofrag::frag.simul.process(n.components=6, vertices=15)
    igraph::V(l6)$layer  <- "6"
    igraph::V(l6)$name <- paste0(igraph::V(l6)$name, "l6")
    
    # merge
    g <- igraph::disjoint_union(l13, l24, l5, l6)
    igraph::graph_attr(g, "frag_type") <- "cr"
    
    # add connection between 1 and 2
    g <- igraph::add_edges(g, c(rbind(sample(igraph::V(g)[igraph::V(g)$layer == 1], 4, replace = FALSE),
                                      sample(igraph::V(g)[igraph::V(g)$layer == 2], 4, replace = FALSE))))
    
    # add connection between 1 and 3
    g <- igraph::add_edges(g, c(rbind(sample(igraph::V(g)[igraph::V(g)$layer == 1], 6, replace = FALSE),
                                      sample(igraph::V(g)[igraph::V(g)$layer == 3], 6, replace = FALSE))))
    
    # add connection between 2 and 3
    g <- igraph::add_edges(g, c(rbind(sample(igraph::V(g)[igraph::V(g)$layer == 2], 5, replace = TRUE),
                                      sample(igraph::V(g)[igraph::V(g)$layer == 3], 5, replace = TRUE))))
    
    # add connection between 3 and 4
    g <- igraph::add_edges(g, c(rbind(sample(igraph::V(g)[igraph::V(g)$layer == 3], 10, replace = TRUE),
                                      sample(igraph::V(g)[igraph::V(g)$layer == 4], 10, replace = TRUE))))
    
    # add connection between 4 and 5
    g <-  igraph::add_edges(g, c(rbind(sample(igraph::V(g)[igraph::V(g)$layer == 4], 2, replace = TRUE),
                                       sample(igraph::V(g)[igraph::V(g)$layer == 5], 2, replace = TRUE))))
    
    # add connection between 5 and 6
    g <-  igraph::add_edges(g, c(rbind(sample(igraph::V(g)[igraph::V(g)$layer == 5], igraph::gorder(l6) * 2, replace = TRUE),
                                       sample(igraph::V(g)[igraph::V(g)$layer == 6], igraph::gorder(l6) * 2, replace = TRUE ))))
    
    # extract tables and export
    list("connection" = igraph::as_edgelist(g), 
         "fragments" =  data.frame("id" =  igraph::V(g)$name, "layer" =  igraph::V(g)$layer))
  }
