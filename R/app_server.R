server <- function(input, output, session) { 
  .data <- NULL  
  i <- NULL  
  x <- NULL
  value <- NULL
  
  # parallelize box, count n workers
  output$parallelize.box <- renderUI({
    span(`data-toggle` = "tooltip", `data-placement` = "bottom",
         title = "Enabling parallelization using half of the available cores to speed up computations.",
          checkboxInput("parallelize",
                        paste0("Parallelize (",
                               foreach::getDoParWorkers(), " workers)"), value = TRUE)
    )
  })
  
  # rubish generator
  generate.rubish <- function(){
    l13 <- archeofrag::frag.simul.process(n.components=24, vertices=70, disturbance=.4, balance = .6)
    igraph::V(l13)[igraph::V(l13)$layer == 2]$layer <- 3
    
    l24 <- archeofrag::frag.simul.process(n.components=20, vertices=44, balance=.6, disturbance=0)
    igraph::V(l24)[igraph::V(l24)$layer == 1]$layer <- 4
    igraph::V(l24)$name <- paste0(igraph::V(l24)$name, "l24")
    
    l5 <- archeofrag::frag.simul.process(n.components=5, vertices=20)
    igraph::V(l5)$layer  <- 5
    igraph::V(l5)$name <- paste0(igraph::V(l5)$name, "l5")
    
    l6 <- archeofrag::frag.simul.process(n.components=6, vertices=15)
    igraph::V(l6)$layer  <- 6
    igraph::V(l6)$name <- paste0(igraph::V(l6)$name, "l6")
    
    # merge
    g <- igraph::disjoint_union(l13, l24, l5, l6)
    igraph::graph_attr(g, "frag_type") <- "cr"
    
    # add connection between 1 and 2
    g <- igraph::add_edges(g, c(rbind(sample(igraph::V(g)[igraph::V(g)$layer == 1], 4, replace = F),
                                      sample(igraph::V(g)[igraph::V(g)$layer == 2], 4, replace = F))))
    
    # add connection between 1 and 3
    g <- igraph::add_edges(g, c(rbind(sample(igraph::V(g)[igraph::V(g)$layer == 1], 6, replace = F),
                                      sample(igraph::V(g)[igraph::V(g)$layer == 3], 6, replace = F))))
    
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
  
  rubish <- generate.rubish()
  
  output$rubish.text <- renderUI({
    if(input$use_example != "Rubish Site *") return()
    
    HTML("
        <div align=left><h1>Data set presentation: Rubish Site</h1></div>
        <div align=center>
        <div style=width:40%;, align=left>
         <p/>
         Rubish Site is an impressive archaeological location situated at N189 24' 0, W66 6' 0, on the slopes of In Silico Valley (Randomness county). It was extensively excavated from April, 1st, 1969 (2 am) to April, 1st, 1969 (3 am) by Professor Sauvignon & associates. Their efforts led to determining 5+1 stratigraphic units (=6). Intensive post-excavation studies were carried out the next day, despite the difficult conditions faced by the excavation team. Refits were tirelessly researched among fragments of glass bottle material, which excited the archaeologists for looking surprisingly similar to modern bottles they were familiar with.
         </p>
         <p>
         To demonstrate its potential (and save archaeologists' energy this day) the <a href=https://doi.org/10.21105/joss.04335 target=_blank>TSAR</a>  method was automatically applied, revealing the very nature of this unsuspected archaeological record.
         <ul>
          <li> First, the dissimilarity dendrogram revealed the <b>abnormal ordering</b> of Layers 1, 2, and 3.</li>
          <li> Intrigued, the team of experts measured and compared <i>cohesion</i> and <i>admixture</i> values, refering to <a href=10.1016/j.jas.2021.105501 target=_blank>Plutniak 2021</a>, Table 1 to interpret them:
            <ul> 
                <li> Layers 1 and 2 had <b>cohesion</b> values <b>highly</b> different and a <b>low admixture</b> value, suggesting movement of fragments from one certain unit (from Layer 1 to an uncertain Layer 2).</li>
                <li> Layers 1 and 3 showed a rather <b>low</b> difference between <b>cohesion</b> values and a <b>high admixture</b> value, suggesting transport of fragments within a single initial unit (i.e. Layer 1+3), contradicting the previous result.</li>
                <li> Layers 3 and 4 presented a rather <b>low</b> difference between <b>cohesion</b> values and a <b>low admixture</b> value as well, suggesting transport of fragments between two certain units.</li>
                <li> Layers 5 and 6 showed <b>high</b> difference between <b>cohesion</b> values and a <b>high admixture</b> value as well, desperately suggesting general uncertainty about those layers and their formation.</li>
            </ul>
          </li>
          <li> Plunged into confusion by such an enigma, they had to invoke <i>Simulation</i> to help them understand the mysterious formation processes of layers 1-2-3. The answers they received were astonishing: the examination of simulated cohesion values suggested that, most probably,
             <ul>
              <li> Layers 1 and 2 resulted from <b>two</b> independent deposition events </li>
              <li> whereas Layers 2 and 3 resulted from a <b>single</b> deposition event.  </li>
             </ul
          </li>
         </ul>
         Everything became crystal clear and Prof. Sauvignon shouted a loud 'EUREKA!': 
         the so-called Layers 2 and 3 were actually part of the same unit! Their distinction was irrelevant and most probably the result of suspicious post-depositional processes (or even due to Sauvignon's colleague's dubious ability to scrutinize subtle sedimental differences when excavating in hard conditions).
         </p>
         <p>
         And that's how, based on these breathtaking results afterwards reproduced using <i>R programming code</i>,  Prof. Sauvignon famously gave the site its name, known worldwide: Rubish Site.
         </p>
         </div>
         </div>
         ")
  })
  
  
  # DATA INPUT ----
  userNodes <- reactive({
    req(input$inputNodes)
    validate(
      need(input$inputNodes,
           message = "Choose a node file or use the example data.")
    )
    input$inputNodes
  })
  
  userEdges <- reactive({
    validate(
      need(input$inputEdges,
           message = "Choose an edge file or use the example data.")
    )
    input$inputEdges
  })
  
  # datasets list ----
  datasets <- utils::data(package = "archeofrag")$result[, "Item"]
  data.names <- gsub(".*\\((.*)\\)","\\1", datasets)
  data.names <- gsub("([A-Z])"," \\1", data.names)
  data.names <- gsub("^ ", "", data.names)
  names(data.names) <-  gsub("^(.*)\\..*","\\1", datasets)
  
  # dataset selector ----
  output$dataset.selector <- renderUI({
    data.names <- sort(c("-", data.names, "Rubish Site *"))
    selectInput("use_example", "Load example data", 
                choices = c(data.names, use.names=FALSE), selected = "-")
  })
  
  
  graph.data <- reactive({
    req(input$use_example)
    
    if(input$use_example  %in% data.names) {
      idx <- which(data.names == input$use_example)
      eval(parse(text = paste0("edges.df <- archeofrag::",   names(data.names[idx]), ".connection" )))
      eval(parse(text = paste0("objects.df <- archeofrag::", names(data.names[idx]), ".fragments" )))
    } else if(input$use_example == "Rubish Site *"){
      edges.df <- rubish$connection
      objects.df <- rubish$fragments
    } else {
      
      query <- shiny::parseQueryString(session$clientData$url_search)
      
      if ( ! is.null(query[['objects']])) {
        objects.df <- utils::read.csv(url(as.character(query[['objects']])))
      } else{
        objects.df <- utils::read.csv(userNodes()$datapath, header = T, sep=input$sep)
      }
      if ( ! is.null(query[['relations']])) {
        edges.df <- utils::read.csv(url(as.character(query[['relations']])))
      } else{
        edges.df <- utils::read.csv(userEdges()$datapath, header = T, sep=input$sep)
      }
    }
    list("objects.df"=objects.df, "edges.df"=edges.df)
  })
  
  # spatial variable selector ----
  output$variable.selector <- renderUI({
    req(graph.data())
    
    g.data <- graph.data()
    objects.df <- g.data$objects.df
    
    choices.val <- names(objects.df)
    choices.val <- choices.val[ ! tolower(choices.val) == "id"]
    names(choices.val) <- names(choices.val)
    
    default.value <- choices.val[1]
    if(sum("layer" %in% choices.val) > 0 ){
      default.value <- "layer"
    } else if(sum("level" %in% choices.val) > 0 ){
      default.value <- "level"
    }
    
    selectInput("spatial.variable", "Spatial variable",
                selected = default.value,
                choices = choices.val, width= "90%")
  })
  
  graph.data2 <- reactive({ # add spatial variable ----
    req(graph.data, input$spatial.variable)
    
    g.data <- graph.data()
    objects.df <- g.data$objects.df
    
    if( ! input$spatial.variable %in% colnames(objects.df)) return()
    
    objects.df$spatial.variable <- as.character(eval(parse(text = paste0("objects.df$", input$spatial.variable ))))
    
    list("objects.df"=objects.df, "edges.df"=g.data$edges.df)
  })
  
  graph.data3 <- reactive({ # subset data ----
    req(graph.data2, input$subset.variable)
    
    g.data <- graph.data2()
    
    objects.df <- g.data$objects.df
    edges.df <- g.data$edges.df
    
    subset.values <- input$subset.values
    
    if(input$subset.variable == "-") subset.values <- NULL
    
    if( ! is.null(subset.values)){
      expr <- paste0("objects.df[objects.df$", input$subset.variable, " %in% input$subset.values, ]")
      objects.df <- eval(parse(text = expr))
      
      edges.df <- edges.df[edges.df[, 1] %in% objects.df$id, ]
      edges.df <- edges.df[edges.df[, 2] %in% objects.df$id, ]
    }
    
    list("objects.df"=objects.df, "edges.df"=edges.df)
  })
  
  
  # SELECTORS ----
  # ... pair of units selector ----
  output$layers.selector <- renderUI({
    req(graph.list())
    
    g.list <- graph.list()
    
    choices.val <- seq_len(length(g.list))
    names(choices.val) <- names(g.list)
    
    selectInput("units.pair", "Pair of spatial units to study",
                choices = choices.val, width= "90%")
  })
  
  # ... sub-setting variable selector ----
  output$subset.selector <- renderUI({
      req(graph.data())
      
      g.data <- graph.data()
      objects.df <- g.data$objects.df
      
      choices.val <- names(objects.df)
      choices.val <- choices.val[ ! tolower(choices.val) == "id"]
      names(choices.val) <- names(choices.val)
      choices.val <- c("-", choices.val)
      
      selectInput("subset.variable", "Subset by (optional)",
                  choices = choices.val, width= "90%")
    })
  
  output$subset.options <- renderUI({
    req(input$subset.variable)
    
    if(input$subset.variable == "-") return()
    
    g.data <- graph.data()
    objects.df <- g.data$objects.df
    values <- sort(unique(eval(parse(text = paste0("objects.df$", input$subset.variable)))))

    checkboxGroupInput("subset.values", "", choices = values, selected = values) 
  })
  
  
  # ... morpho selector ----
  output$morpho.selector <- renderUI({
    req(graph.data3())
    
    choices.val <- c("-", names(graph.data()$objects.df))
    choices.val <- choices.val[ ! tolower(choices.val) == "id"]
    
    selectInput("morpho.variable", "Morphometry variable",
                choices = choices.val, width= "90%")
  })
  
  # ... x selector ----
  output$x.selector <- renderUI({
    req(graph.data3())
    
    choices.val <- c("-", names(graph.data()$objects.df))
    choices.val <- choices.val[ ! tolower(choices.val) == "id"]
    
    selectInput("x.variable", "X coordinates",
                choices = choices.val, width= "90%")
  })
  
  # ... y selector ----
  output$y.selector <- renderUI({
    req(graph.data3())
    
    choices.val <- c("-", names(graph.data()$objects.df))
    choices.val <- choices.val[ ! tolower(choices.val) == "id"]
    
    selectInput("y.variable", "Y coordinates",
                choices = choices.val, width= "90%")
  })
  
  # ... z selector ----
  output$z.selector <- renderUI({
    req(graph.data3())
    
    choices.val <- c("-", names(graph.data()$objects.df))
    choices.val <- choices.val[ ! tolower(choices.val) == "id"]
    
    selectInput("z.variable", "Z coordinates",
                choices = choices.val, width= "90%")
  })

  # MAKE GRAPH LIST----
  graph.complete <- reactiveVal()
  graph.complete.init <- reactiveVal() # save copy to retrieve it with the 'reset' button

  observe({ 
    req(graph.data3, input$spatial.variable)
    g.data <- graph.data3()
    if(is.null(g.data) | is.null(g.data$objects.df)) return()
    
    try(graph <- archeofrag::make_frag_object(g.data$edges.df, fragments = g.data$objects.df), silent = TRUE)
    if( ! exists("graph")){
      showNotification(geterrmessage(), duration = 10, type = "error")
      return()
    }
    
    graph <- archeofrag::make_cr_graph(graph)
    
    # check if the data is complete for weighting parameter
    check.and.delete.frag <- function(g, var){
      values <- igraph::vertex_attr(g, var)
      idx <- is.na(values) | values == ""
      if(sum(idx)){ 
        g <- igraph::delete_vertices(graph, idx) 
        showNotification(paste0("Incomplete values in '", var, "'. ", as.character(sum(idx)), " fragments removed."),
                         duration = 10, type = "message")
      }
      g
    }
    
    if( ! is.null(input$morpho.variable)){ graph <- check.and.delete.frag(graph, input$morpho.variable)}
    if( ! is.null(input$x.variable)){ graph <- check.and.delete.frag(graph, input$x.variable)}
    if( ! is.null(input$y.variable)){ graph <- check.and.delete.frag(graph, input$y.variable)}
    if( ! is.null(input$z.variable)){ graph <- check.and.delete.frag(graph, input$z.variable)}
    
    graph.complete.init(graph)
    graph.complete(graph)
  })
  
  
  
  graph.list <- reactive({ 
    req(graph.complete)
    if(is.null(graph.complete())) return()
    
    graph <- graph.complete()

    pairs <- utils::combn(sort(unique(igraph::V(graph)$spatial.variable)), 2)
    
    morpho.variable <- input$morpho.variable
    x.variable <- input$x.variable
    y.variable <- input$y.variable
    z.variable <- input$z.variable
    
    extract.pair.graph <- function(x){ # this function includes calls to global variables 
      g <- archeofrag::frag.get.layers.pair(graph, "spatial.variable", pairs[, x], verbose = FALSE)
      if(is.null(g)){ return() }
      if(length(unique(igraph::V(g)$spatial.variable)) != 2){ return() }
      
      archeofrag::frag.edges.weighting(g, "spatial.variable", morphometry = morpho.variable, 
                                       x = x.variable, y = y.variable, z = z.variable, verbose = FALSE)
    }
    
    if(input$parallelize){
      g.list <- foreach::foreach(x = seq_len(ncol(pairs))) %dopar% { extract.pair.graph(x) }
    } else {
      g.list <- lapply(seq_len(ncol(pairs)), function(x) extract.pair.graph(x))
    }
    
    names(g.list) <- sapply(seq_len(ncol(pairs)), function(x)
      paste(pairs[1, x], "/", pairs[2, x]))
    
    g.list[ ! sapply(g.list, is.null)]
  })
  
  graph.selected <- reactive({
    req(graph.list(), input$units.pair)
    
    graph.list <- graph.list()
    
    graph.list[[as.numeric(input$units.pair)]]
  })
  
  
  
  # GET GRAPH PARAMS ----
  input.graph.params <- reactive({ 
    req(graph.selected())
    archeofrag::frag.get.parameters(graph.selected(), "spatial.variable", verbose = FALSE)
  })
  
  output$n.components <- renderUI({
    req(graph.list()) 
    numericInput("n.components", "Initial objects count", value = input.graph.params()$n.components, width = "100%")
  })
  
  output$components.balance <- renderUI({
    req(input.graph.params())
    sliderInput("components.balance", "Estimated initial objects balance",
                min = 0, max= 1, step = .01, 
                value = input.graph.params()$components.balance, width = "100%")
  })
  
  output$balance <- renderUI({
    req(input.graph.params())
    sliderInput("balance", "Estimated fragments balance",  min = 0, max= 1, step = .01,
                value = input.graph.params()$balance, width = "100%")
  })
  
  output$n.final.fragments <- renderUI({
    req(input.graph.params())
    numericInput("n.final.fragments", "Final fragments count", value = input.graph.params()$vertices, width = "100%")
  })
  
  output$disturbance <- renderUI({
    req(input.graph.params())
    sliderInput("disturbance", "Final disturbance", min = 0, max= .5, step = .01, 
                value = input.graph.params()$disturbance, width = "100%")
  })
  
  output$aggreg.factor <- renderUI({
    req(input.graph.params())
    sliderInput("aggreg.factor", "Fragments aggregation", min = 0, max= 1, step = .01, 
                value = input.graph.params()$aggreg.factor, width = "100%")
  })
  
  output$asymmetric <- renderUI({
    req(input.graph.params())
    units.pair <- names(graph.list())[as.numeric(input$units.pair)]
    
    eval(parse(text = paste0(
      "selectInput('asymmetric', 'Unidirectional transport from unit', ",
                  "choices = c('none' = 'none', '",
                  gsub("/", "->", units.pair), "' = '1', '",
                  gsub("^(.*) / (.*)$", "\\2 -> \\1", units.pair), "' = '2'),",
                  "selected = 'none', width = '100%')"
    )))
  })
  
  output$planar <- renderUI({
    req(input.graph.params())
    
    planar <- input.graph.params()$planar
    if(is.na(planar)) { 
      planar <- FALSE 
      showNotification("The RBGL package is not installed: the `planarity` value cannot be determinated and the 'Generate only planar graphs' is set to FALSE", duration = 10, type = "warning")
    }
    
    checkboxInput("planar", "Generate only planar graphs", value = planar)
  })
  
  
  # MEASUREMENT-----
  # data set presentations ----
  output$dataset.presentation <- renderUI({
    if(input$use_example  %in% data.names) {
      graph.data <- graph.data()
      fragments.df <- graph.data$objects.df
      edges.df <- graph.data$edges.df
      HTML(paste0(" <div align=left>
                  <h1>Data set presentation: ", comment(fragments.df)[1], "</h1>",
                  "<ul>",
                    "<li><b>Site</b>: ", comment(fragments.df)[1], "</li>",
                    "<li><b>Period</b>: ", comment(fragments.df)[3], "</li>",
                    "<li><b>Material</b>: ", comment(fragments.df)[2], "</li>",
                    "<li><b>Fragments count</b>: ", nrow(fragments.df), "</li>",
                    "<li><b>Connection count</b>: ", nrow(edges.df), "</li>",
                    "<li><b>Reference</b>: see the 'References' tab</li>",
                  "</ul></div>"
           ))
    }
  })
  
  stats.table <- reactive({    # stats table ----
    req(graph.list, input$morpho.variable)
    g.list <- graph.list()
    
    make.stat.table <- function(g){
      g.stats <- list(balance = NA, components.balance = NA)
      if(igraph::gorder(g) > 6){
        g.stats <- archeofrag::frag.get.parameters(g, layer.attr = "spatial.variable", verbose = FALSE)
      }
      cohesion <- round(archeofrag::frag.layers.cohesion(g, "spatial.variable", verbose = FALSE), 2)
      cohesion.diff <- sort(cohesion)
      cohesion.diff <- round(cohesion.diff[2] - cohesion.diff[1], 2)
      data.frame(
        "Pair of spatial units" = paste(sort(unique(igraph::V(g)$spatial.variable)), collapse=" / "),
        "N. Objects" =  as.integer(igraph::components(g)$no),
        "N. Fragments" = as.integer(igraph::gorder(g)),
        "N. Relations" = as.integer(igraph::gsize(g)),
        "Frag. Balance" = g.stats$balance,
        "Objects Balance" = g.stats$components.balance,
        "Cohesion 1st unit" = cohesion[1],
        "Cohesion 2nd unit" = cohesion[2],
        "Cohesion diff" = cohesion.diff,
        "Admixture" =  round(archeofrag::frag.layers.admixture(g, "spatial.variable", verbose = FALSE), 3)
      )
    }
    
    if(input$parallelize){
      df <- foreach::foreach(g = g.list, .combine = "rbind", .errorhandling = "remove") %dopar% { make.stat.table(g) }
    } else{
      df <- sapply(g.list, function(g) make.stat.table(g))
      df <- data.frame(t(df))
      df[, seq(2, ncol(df))] <- apply(df[, seq(2, ncol(df))], 2, as.numeric)
    }
    colnames(df) <- gsub("\\.", " ", colnames(df))
    df
  })
  
  output$resultsTab <- DT::renderDT({ 
    DT::datatable(stats.table(), rownames = FALSE, escape = FALSE, style = "default", selection = 'none',
                  options = list(dom = 'tp'))
  })
  
  output$download.measurements.tab <- downloadHandler(
    filename = paste0("archeofrag-measurements-", input$spatial.variable, ".csv"),
    content = function(file) {
      write.csv(stats.table(), file, row.names = TRUE)
    }
  )
  
  
  
  dissimilarityTab <- reactive({  # dissimilarity table ----
    req(stats.table(), graph.data3())
    stats.table <- stats.table()
    
    stats.table$unit1 <- gsub("(.*) / .*", "\\1", stats.table[, 1])
    stats.table$unit2 <- gsub("^.* / (.*$)", "\\1", stats.table[, 1])
    
    pairs <- utils::combn(sort(unique(c(stats.table$unit1, stats.table$unit2))), 2)
    pairs <- t(as.data.frame(pairs))
    pairs <- rbind(pairs, pairs[, c(2, 1)])
    
    colnames(pairs) <- c("unit1", "unit2")
    pairs <- merge(pairs,
                   stats.table[, c("Admixture", "unit1", "unit2")], 
                   by = c("unit1", "unit2"), all.x = TRUE)
    
    if(input$normalise.diss){
      pairs$Admixture <- ( pairs$Admixture - min(pairs$Admixture, na.rm = TRUE)) / 
        (max(pairs$Admixture, na.rm = TRUE) - min(pairs$Admixture, na.rm = TRUE))
    }
    
    diss <- stats::reshape(pairs, timevar = "unit1", idvar = "unit2",  v.names = "Admixture", direction = "wide")
    diss <- as.matrix(diss)
    colnames(diss) <- gsub("^Admixture.", "", colnames(diss))
    rownames(diss) <- diss[, 1]
    diss <- diss[, -1]
    diss <- diss[ order(rownames(diss)), order(colnames(diss))]
    diss <- apply(diss, 2,  as.numeric)
    rownames(diss) <- colnames(diss)
    
    diag(diss) <- 1
    diss[is.na(diss)] <- 0
    diss[upper.tri(diss)] <- NA
    1 - diss
  })
  
  
  output$dissimilarityTab <- renderTable({ 
    req(dissimilarityTab())
    dissimilarityTab()
  }, rownames = TRUE, colnames = TRUE, na = "-")
  

  output$download.dissimilarityTab <- downloadHandler(
    filename = paste0("archeofrag-dissimilarity-", input$spatial.variable, ".csv"),
    content = function(file) {
      write.csv(dissimilarityTab(), file, row.names = TRUE)
    }
  )
  
  
  # tanglegram ----
  
clustering.method.names <- c("UPGMA" = "average", "WPGMA" = "mcquitty", "Single linkage" = "single", "Complete linkage" = "complete",  Ward = "ward.D2")
  
  
  # dissimilarityTab <- reactive({
  #   req(admixTab())
  #   # diss.tab <- 1 - admixTab()
  #   diss.tab <- admixTab()
  #   # diss.tab[is.na(diss.tab)] <- 1
  #   diss.tab
  # })
  
  
  expected.dendrogram <- reactive({
    req(dissimilarityTab())
    expected.dissimilarityTab <- dissimilarityTab()
    expected.dissimilarityTab[ lower.tri(expected.dissimilarityTab, diag = FALSE) ] <- 1
    diag(expected.dissimilarityTab) <- 0
    
    expected.dissimilarityTab <- stats::as.dist(expected.dissimilarityTab)
    
    stats::as.dendrogram(stats::hclust(expected.dissimilarityTab, method = "average"))
  })
  
  
  observed.dendrograms <- reactive({
    req(dissimilarityTab())
    
    observed.dissimilarityTab <- dissimilarityTab()
    observed.dissimilarityTab <- stats::as.dist(observed.dissimilarityTab)
    
    # compute clusterings and dendrograms: 
    dendrograms.list <- lapply(clustering.method.names, function(method)  
      stats::as.dendrogram(stats::hclust(observed.dissimilarityTab, method = method)))
  
    names(dendrograms.list) <- clustering.method.names
    dendrograms.list
  })
  
    
  selected.clustering.method.name <- reactive({
    names(clustering.method.names)[clustering.method.names == input$clustmethod]
  })
  

  clustering.stats.df <- reactive({
    req(observed.dendrograms(), expected.dendrogram(), dissimilarityTab())
    
    observed.dendrograms <- observed.dendrograms()
    expected.dendrogram <-  expected.dendrogram()
    
    # compute entanglement:
    entanglement.values <- sapply(seq_len(length(clustering.method.names)), function(x) 
      dendextend::entanglement(sort(observed.dendrograms[[x]]), sort(expected.dendrogram)))
    entanglement.values <- round(entanglement.values, 2)

    # turn dissimilarity tab into distance tab
    observed.dissimilarityTab <- dissimilarityTab()
    observed.dissimilarityTab <- stats::as.dist(observed.dissimilarityTab)
    
    # compute cophenetic correlations:
    cophenetic.cor.values <- sapply(seq_len(length(clustering.method.names)), function(x) 
      # stats::cor(observed.dissimilarityTab, stats::cophenetic(observed.dendrograms[[x]])))
    stats::cor(observed.dissimilarityTab, stats::cophenetic(stats::as.hclust(observed.dendrograms[[x]]))))
    cophenetic.cor.values <- round(cophenetic.cor.values, 2)
    
    # sapply(seq_len(length(clustering.method.names)), function(x) 
    #   dendextend::cor_cophenetic(expected.dendrogram, observed.dendrograms[[x]]))
    
    # compute Baker's Gamma  correlations:
    bakers.cor.values <- sapply(seq_len(length(clustering.method.names)), function(x) 
      dendextend::cor_bakers_gamma(expected.dendrogram, observed.dendrograms[[x]]))
    bakers.cor.values <- round(bakers.cor.values, 2)
    
    # output table
    stats.df <- data.frame("Method" = names(clustering.method.names),
                           "Cophenetic correlation" = cophenetic.cor.values,
                           "Entanglement" =  entanglement.values, 
                           "Baker's Gamma correlation" = bakers.cor.values,
                           check.names = FALSE)
    stats.df[order(stats.df$Entanglement, decreasing = FALSE), ]
  })
  
  output$clustering.stats <- renderDT({
    
    tab <- clustering.stats.df()
    
    sketch <-  "<table class='display'>
                  <thead>
                    <tr>
                      <th rowspan=2> Method</th>
                      <th>Observed tree</th>
                      <th colspan=2> Observed and expected trees</th>
                    </tr>
                    <tr>
                      <th>Cophenetic correlation</th>
                      <th>Entanglement</th>
                      <th>Baker's Gamma correlation</th>
                    </tr>
                  </thead>
                </table>"
    
    
    
    DT::datatable(tab, container = sketch, rownames = FALSE, style = "default", selection = 'none', options = list(dom = 't'))
  })
    
    
  output$admix.clustering.selector <- renderUI({
    clustering.method.options <- clustering.method.names
    selectInput("clustmethod", "Clustering method", choices = clustering.method.options)
  })  
  
  
  selected.tanglegrams <- reactive({
    req(observed.dendrograms(), expected.dendrogram(), input$clustmethod)

    observed.dendrograms <- observed.dendrograms()
    expected.dendrogram <- expected.dendrogram()
    
    list(observed.dendrograms[ names(observed.dendrograms) == input$clustmethod ][[1]],
         expected.dendrogram)
  })
  
  
  output$tanglegram.plot <- renderPlot({
    req(selected.tanglegrams())
    
    clustering.stats.df <- clustering.stats.df()
    idx <- clustering.stats.df[, 1] == names(clustering.method.names)[which(clustering.method.names == input$clustmethod)]
    
    dendextend::tanglegram(selected.tanglegrams()[[1]], selected.tanglegrams()[[2]],
                           sort = TRUE,
                           main = toupper(input$spatial.variable),
                           margin_bottom = 11, margin_inner = 12,
                           lab.cex = 1.4, cex_main = 1.6, cex_sub = 1,
                           main_left = "Observed", main_right = "Expected",
                           highlight_branches_lwd = FALSE, highlight_distinct_edges = FALSE)
    mtext(paste0("\n\n\n",
                 "Dissimilarity: 1 - admixture",
                 "\n",
                 "Clustering method: ", selected.clustering.method.name(),
                 "\n",
                 "Cophenetic correlation: ", clustering.stats.df[idx, ]$"Cophenetic correlation",
                 "\n",
                 "Entanglement value: ", clustering.stats.df[idx, ]$Entanglement,
                 "\n",
                 "Baker correlation: ", clustering.stats.df[idx, ]$"Baker's Gamma correlation"
    ), side =  1)
  })
  
  
  output$tanglegram.download <- downloadHandler(
    filename = paste0("archeofrag-dissimilarity-tanglegram-", input$spatial.variable, "-",  selected.clustering.method.name(), ".svg"),
    content = function(file) {
      clustering.stats.df <- clustering.stats.df()
      idx <- clustering.stats.df[, 1] == names(clustering.method.names)[which(clustering.method.names == input$clustmethod)]
      grDevices::svg(file,  width = 10)
        dendextend::tanglegram(selected.tanglegrams()[[1]], selected.tanglegrams()[[2]],
                               sort = TRUE,
                               main = toupper(input$spatial.variable),
                               margin_bottom = 11, margin_inner = 12,
                               lab.cex = 1.4, cex_main = 1.6, cex_sub = 1,
                               main_left = "Observed", main_right = "Expected",
                               highlight_branches_lwd = FALSE, highlight_distinct_edges = FALSE)
        mtext(paste0("\n\n\n",
                     "Dissimilarity: 1 - admixture",
                     "\n",
                     "Clustering method: ", selected.clustering.method.name(),
                     "\n",
                     "Cophenetic correlation: ", clustering.stats.df[idx, ]$"Cophenetic correlation",
                     "\n",
                     "Entanglement value: ", clustering.stats.df[idx, ]$Entanglement,
                     "\n",
                     "Baker correlation: ", clustering.stats.df[idx, ]$"Baker's Gamma correlation"
        ), side =  1)
      grDevices::dev.off()
    }
  )
  
  output$tanglegram.download.button <- renderUI({
    downloadButton("tanglegram.download", "Download as SVG")
  })
  
  # SIMULATION ####
  
  # Function to execute the simulation and retrieve some measures:
  exec.simulation  <- function(initial.layers, n.components, vertices,
                               balance, components.balance, disturbance,
                               aggreg.factor, planar, asymmetric.transport.from,
                               edge.loss, vertice.loss){
    
    g <- archeofrag::frag.simul.process(initial.layers, n.components, vertices, edges = Inf,
                                        balance, components.balance, disturbance,
                                        aggreg.factor, planar, asymmetric.transport.from)
    
    if(edge.loss != 0){
      g <- archeofrag::frag.observer.failure(g, likelihood = edge.loss / 100, remove.vertices=TRUE)[[1]]
    }
    if(vertice.loss != 0){
      n.frag.to.remove <- round((vertice.loss / 100) * igraph::gorder(g), 0)
      g <- archeofrag::frag.graph.reduce(g, n.frag.to.remove = n.frag.to.remove, conserve.objects.nr=FALSE)
    }
    
    data.frame(
      "admixture" = round(archeofrag::frag.layers.admixture(g, "layer", verbose = FALSE), 3),
      "cohes" = rbind(archeofrag::frag.layers.cohesion(g, "layer", verbose = FALSE)),
      "n.objects" = as.integer(igraph::components(g)$no),
      "e.obs" = igraph::gsize(g),
      "v.obs" = igraph::gorder(g),
      "balance.obs" = table(igraph::V(g)$layer)[1] / igraph::gorder(g),
      "weights.sum" = sum(igraph::E(g)$weight),
      "weights.median" = stats::median(igraph::E(g)$weight),
      "weights.sd" = stats::mad(igraph::E(g)$weight)
    )    
  }
  
  
  hypotheses <- eventReactive(input$goButton, { # run simulation ####
    
    req(input$replications)
    req(graph.selected())
    graph <- graph.selected()
    
    start.time <- Sys.time()  # save start time
    
    if(input$replications < 30 | input$replications > 1000) {
      showNotification("The number of replication must be in [30, 1000].", type="warning")
      return(NULL)
    }
    asymmetric <- input$asymmetric
    if(asymmetric == "none") asymmetric <- 0
    
    params <- list("n.components" = input$n.components,
                   "n.final.fragments" = input$n.final.fragments,  
                   "balance" = input$balance,
                   "components.balance" = input$components.balance,
                   "disturbance" = input$disturbance,
                   "aggreg.factor" = input$aggreg.factor,
                   "planar" = input$planar,
                   "edge.loss" = input$edge.loss,
                   "vertice.loss" = input$vertice.loss)
    
    if( ! is.na(input$seed)){
       doRNG::registerDoRNG(input$seed) # set seed if required
    }
    
    if(input$parallelize){
      hypothese1.res <- foreach::foreach(i = seq_len(input$replications), .combine = "rbind",
                                         .errorhandling = "remove"
                                         ) %dopar%{
                                           exec.simulation(initial.layers = 1,
                                                           n.components = params$n.components,
                                                           vertices = params$n.final.fragments,  
                                                           balance = params$balance,
                                                           components.balance = params$components.balance,
                                                           disturbance = params$disturbance,
                                                           aggreg.factor = params$aggreg.factor,
                                                           planar = params$planar,
                                                           asymmetric.transport.from = asymmetric,
                                                           edge.loss = params$edge.loss,
                                                           vertice.loss = params$vertice.loss)
                                         }
      
      hypothese2.res <- foreach::foreach(i = seq_len(input$replications), .combine = "rbind",
                                         .errorhandling = "remove") %dopar%{
                                           exec.simulation(initial.layers = 2,
                                                           n.components = params$n.components,
                                                           vertices = params$n.final.fragments,  
                                                           balance = params$balance,
                                                           components.balance = params$components.balance,
                                                           disturbance = params$disturbance,
                                                           aggreg.factor = params$aggreg.factor,
                                                           planar = params$planar,
                                                           asymmetric.transport.from = asymmetric,
                                                           edge.loss = params$edge.loss,
                                                           vertice.loss = params$vertice.loss)
                                         }
      
    } else {
      hypothese1.res <- foreach::foreach(i = seq_len(input$replications),  .combine = "rbind",
                                         .errorhandling = "remove") %do%{
                                           exec.simulation(initial.layers = 1,
                                                           n.components = params$n.components,
                                                           vertices = params$n.final.fragments,  
                                                           balance = params$balance,
                                                           components.balance = params$components.balance,
                                                           disturbance = params$disturbance,
                                                           aggreg.factor = params$aggreg.factor,
                                                           planar = params$planar,
                                                           asymmetric.transport.from = asymmetric,
                                                           edge.loss = params$edge.loss,
                                                           vertice.loss = params$vertice.loss)
                                         }
      
      hypothese2.res <- foreach::foreach(i = seq_len(input$replications), .combine = "rbind",
                                         .errorhandling = "remove") %do%{
                                           exec.simulation(initial.layers = 2,
                                                           n.components = params$n.components,
                                                           vertices = params$n.final.fragments,  
                                                           balance = params$balance,
                                                           components.balance = params$components.balance,
                                                           disturbance = params$disturbance,
                                                           aggreg.factor = params$aggreg.factor,
                                                           planar = params$planar,
                                                           asymmetric.transport.from = asymmetric,
                                                           edge.loss = params$edge.loss,
                                                           vertice.loss = params$vertice.loss)
                                         }
    } # end else
    
    if(is.null(hypothese1.res) | is.null(hypothese2.res)){
      showNotification("No solution found for those parameters.", duration = 12)
      return(NULL)
    }
    
    if(nrow(hypothese1.res[complete.cases(hypothese1.res),]) < 31 | 
       nrow(hypothese2.res[complete.cases(hypothese2.res),]) < 31){
      showNotification("Excessive information loss. For each hypothesis, less than 30 graphs with valid cohesion values generated. Increase the number of replications or decrease information loss parameters.", duration = 12)
      return(NULL)
    }
    
    hypothese1.res$hypothesis <- "1"
    hypothese2.res$hypothesis <- "2"
    
    hypotheses.df <- rbind(hypothese1.res, hypothese2.res)
    exec.time <- Sys.time() - start.time
    
    comment(hypotheses.df) <- paste(round(as.numeric(exec.time), 0), units(exec.time))
    hypotheses.df
  })
  
  output$simul.graph.nr <- renderUI({
    req(hypotheses)
    hypotheses.df <- hypotheses()
    if(is.null(hypotheses.df)) return()
    hypotheses.df <- hypotheses.df[complete.cases(hypotheses.df),]
    HTML("<b>", paste( round(nrow(hypotheses.df)/ 2, 0), "</b> graphs with valid cohesion values generated in <b>",
                       comment(hypotheses.df), "</b>"))
  })
  
  summary.tab <- eventReactive(input$goButton, {# summary table  ----
    req(hypotheses)
    
    hypotheses.df <- hypotheses()
    if(is.null(hypotheses.df)) return()
    
    colnames(hypotheses.df) <- c("admixture", "cohesion1", "cohesion2", "n.objects", "edges", "fragments", "balance", "weightsum", "weights.median", "weights.sd", "hypothesis")
    hypotheses.df <- hypotheses.df[, c("admixture", "cohesion1", "cohesion2", "edges", "balance", "weightsum", "hypothesis")]
    
    summary.df <- archeofrag::frag.simul.summarise(graph.selected(), 
                                                   layer.attr = "spatial.variable", 
                                                   res.h1 = hypotheses.df[hypotheses.df$hypothesis == "1", -ncol(hypotheses.df)], 
                                                   res.h2 = hypotheses.df[hypotheses.df$hypothesis == "2", -ncol(hypotheses.df)], 
                                                   cohesion1.attr = "cohesion1", cohesion2.attr = "cohesion2", 
                                                   admixture.attr = "admixture", 
                                                   verbose = FALSE)
    colnames(summary.df)  <- c("H1 != H2?", "p.value", "Obs. value/H1", "Obs. value/H2")
    summary.df
  })
  
  output$summary.tab <- renderTable({summary.tab()}, rownames = TRUE)
  
  
  
  # .. plot cohesion ####
  test.simul.cohesion.plot <- eventReactive(input$goButton, {   
    req(hypotheses)
    
    hypotheses.df <- hypotheses()
    if(is.null(hypotheses.df)) return()
    
    obs.graph <- graph.selected()
    
    hypotheses.df2 <- stats::reshape(hypotheses.df, dir = "long",
                                     varying = c("cohes.cohesion1", "cohes.cohesion2"),
                                     v.names = "cohesion", timevar="Layer")
    
    units.pair <- names(graph.list())[as.numeric(input$units.pair)]
    
    hypotheses.df2$Layer <- factor(hypotheses.df2$Layer, 
                                   labels = c(gsub("^(.*) /.*", "\\1", units.pair), gsub("^.*/ (.*)", "\\1", units.pair)))
    
    hypotheses.df2$hypothesis <- factor(hypotheses.df2$hypothesis, levels = c("1", "2"),
                                        labels=c("Hypothesis 1", "Hypothesis 2"))
    
    cohes.values <- archeofrag::frag.layers.cohesion(obs.graph, "spatial.variable", verbose = FALSE)
    
    ggplot2::ggplot(hypotheses.df2, ggplot2::aes(x = .data[["cohesion"]], fill = .data[["Layer"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3) +
      ggplot2::geom_boxplot(outlier.shape = 21) +
      ggplot2::geom_vline(xintercept = cohes.values[1],  color = "#BBDF27FF") +
      ggplot2::geom_vline(xintercept = cohes.values[2], color = "#440154FF") +
      ggplot2::facet_wrap(~hypothesis, ncol=1,   scales = "free_y") +
      ggplot2::scale_fill_manual("spatial unit", values = c("#BBDF27FF", "#440154FF")) +
      ggplot2::scale_x_continuous("Cohesion", limits=c(0,1)) + ggplot2::ggtitle("Cohesion by spatial unit") +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 13))
  })
  output$test.simul.cohesion.plot <- renderPlot({test.simul.cohesion.plot()})
  
  output$cohesion.plot.download <- downloadHandler(
    filename = paste0("archeofrag-cohesion-", input$spatial.variable, "-",
                      gsub(" / ", "-", names(graph.list())[as.numeric(input$units.pair)]), ".svg"),
    content = function(file) {
      ggplot2::ggsave(file, plot = test.simul.cohesion.plot(), device = "svg", width=10, height=5, pointsize = 14)
    }
  )
  
  output$cohesion.plot.download.button <- renderUI({
    if(is.null(test.simul.cohesion.plot())) return()
    downloadButton("cohesion.plot.download", "as SVG") 
  })
  
  
  
  # .. plot admixture ####
  test.simul.admixture.plot <- eventReactive(input$goButton, {   
    req(hypotheses)
    hypotheses.df <- hypotheses()
    if(is.null(hypotheses.df)) return()
    obs.graph <- graph.selected() 
    
    ggplot2::ggplot(hypotheses.df, ggplot2::aes(x = .data[["admixture"]], fill= .data[["hypothesis"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3) +
      ggplot2::scale_fill_grey(start = .4, end = .9) +
      ggplot2::geom_vline(xintercept = round(archeofrag::frag.layers.admixture(obs.graph,
                                                                               "spatial.variable", verbose=FALSE), 3)) +
      ggplot2::xlab("Admixture") + ggplot2::ggtitle("Admixture")
  })
  
  output$test.simul.admixture.plot <- renderPlot({ test.simul.admixture.plot()  })
  
  output$admixture.plot.download <- downloadHandler(
    filename = paste0("archeofrag-admixture-", input$spatial.variable, "-",
                      gsub(" / ", "-", names(graph.list())[as.numeric(input$units.pair)]), ".svg"),
    content = function(file) {
      ggplot2::ggsave(file, plot = test.simul.admixture.plot(), device = "svg", width=10, height=3, pointsize = 14)
    }
  )
  
  output$admixture.plot.download.button <- renderUI({
    if(is.null(test.simul.admixture.plot())) return()
    downloadButton("admixture.plot.download", "as SVG") 
  })
  
  # .. plot edge count ####
  test.simul.edges.plot <- eventReactive(input$goButton, {   
    req(hypotheses)
    hypotheses.df <- hypotheses()
    if(is.null(hypotheses.df)) return()
    obs.graph <- graph.selected()
    
    ggplot2::ggplot(hypotheses.df, ggplot2::aes(x= .data[["e.obs"]], fill = .data[["hypothesis"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3) +
      ggplot2::scale_fill_grey(start = .4, end = .9) +
      ggplot2::geom_vline(xintercept = igraph::gsize(obs.graph))  + 
      ggplot2::xlab("Relationships count") + ggplot2::ggtitle("Relationships count")
  })
  
  output$test.simul.edges.plot <- renderPlot({test.simul.edges.plot()})
  
  output$edges.plot.download <- downloadHandler(
    filename = paste0("archeofrag-relation-count-", input$spatial.variable, "-",
                      gsub(" / ", "-", names(graph.list())[as.numeric(input$units.pair)]), ".svg"),
    content = function(file) {
      ggplot2::ggsave(file, plot = test.simul.edges.plot(), device = "svg", width=10, height=3, pointsize = 14)
    }
  )
  
  output$edges.plot.download.button <- renderUI({
    if(is.null(test.simul.edges.plot())) return()
    downloadButton("edges.plot.download", "as SVG") 
  })
  
  
  # .. plot object count ####
  test.simul.objects.plot <- eventReactive(input$goButton, {   
    req(hypotheses)
    hypotheses.df <- hypotheses()
    if(is.null(hypotheses.df)) return()
    if(length(unique(hypotheses.df$n.objects)) < 2) return()
    obs.graph <- graph.selected()
    
    ggplot2::ggplot(hypotheses.df, ggplot2::aes(x= .data[["n.objects"]], fill = .data[["hypothesis"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3) +
      ggplot2::scale_fill_grey(start = .4, end = .9) +
      ggplot2::geom_vline(xintercept = igraph::components(obs.graph)$no)  + 
      ggplot2::scale_x_continuous("Object count", breaks = function(x) unique(round(pretty(x), 0)) ) +
      ggplot2::ggtitle("Object count")
  })
  
  output$test.simul.objects.plot <- renderPlot({test.simul.objects.plot()})
  
  output$objects.plot.download <- downloadHandler(
    filename = paste0("archeofrag-object-count-", input$spatial.variable, "-",
                      gsub(" / ", "-", names(graph.list())[as.numeric(input$units.pair)]), ".svg"),
    content = function(file) {
      ggplot2::ggsave(file, plot = test.simul.objects.plot(), device = "svg", width=10, height=3, pointsize = 14)
    }
  )
  
  
  output$test.simul.objects.block <- renderUI({
    if(is.null(test.simul.objects.plot())) return()
  
    HTML(paste0(
      h2("Object count"),
      column(10, align="center",
             HTML("<div style=width:40%;, align=left><p>
                   The number of objects (i.e. sets of connected fragments). This value is equal to the 'initial objects count', unless one of the 'Information loss' parameters is not null.
                  </p></div>")
      ),
    fluidRow(column(10,
                    imageOutput("test.simul.objects.plot", height = "200px", width= "100%")),
             column(1, 
                    downloadButton("objects.plot.download", "as SVG"),
                    style="padding-top:80px;"))
    ))
  })
  
  
  
  # .. plot fragments count ####
  test.simul.frag.plot <- eventReactive(input$goButton, {   
    req(hypotheses)
    hypotheses.df <- hypotheses()
    if(is.null(hypotheses.df)) return()
    
    if(length(unique(hypotheses.df$v.obs)) < 2) return()
    obs.graph <- graph.selected()
    
    ggplot2::ggplot(hypotheses.df, ggplot2::aes(x= .data[["v.obs"]], fill = .data[["hypothesis"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3) +
      ggplot2::scale_fill_grey(start = .4, end = .9) +
      ggplot2::geom_vline(xintercept = igraph::gorder(obs.graph))  + 
      ggplot2::scale_x_continuous("Fragment count", breaks = function(x) unique(round(pretty(x), 0)) ) +
      ggplot2::ggtitle("Fragment count")
  })
  
  output$test.simul.frag.plot <- renderPlot({test.simul.frag.plot()})
  
  output$frag.plot.download <- downloadHandler(
    filename = paste0("archeofrag-fragment-count-", input$spatial.variable, "-",
                      gsub(" / ", "-", names(graph.list())[as.numeric(input$units.pair)]), ".svg"),
    content = function(file) {
      ggplot2::ggsave(file, plot = test.simul.frag.plot(), device = "svg", width=10, height=3, pointsize = 14)
    }
  )
  
  
  output$test.simul.frag.block <- renderUI({
    if(is.null(test.simul.frag.plot())) return()
    
    HTML(paste0(
      h2("Fragments count"),
      column(10, align="center",
             HTML("<div style=width:40%;, align=left><p>
                   The number of fragments. This value is equal to the 'Final fragments count', unless one of the 'Information loss' parameters is not null.
                  </p></div>")
      ),
      fluidRow(column(10,
                      imageOutput("test.simul.frag.plot", height = "200px", width= "100%")),
               column(1, 
                      downloadButton("frag.plot.download", "as SVG"),
                      style="padding-top:80px;"))
    ))
  })
  
  # .. plot weights ####
  test.simul.weights.plot <- eventReactive(input$goButton, { 
    req(hypotheses)
    hypotheses.df <- hypotheses()
    if(is.null(hypotheses.df)) return()
    obs.graph <- graph.selected()
    
    w.sum.df <- cbind(hypotheses.df[, c("hypothesis", "weights.sum")], var = "Sum")
    w.sd.df <- cbind(hypotheses.df[, c("hypothesis", "weights.sd")], var = "Median absolute deviation")
    w.median.df <- cbind(hypotheses.df[, c("hypothesis", "weights.median")], var = "Median")
    colnames(w.sum.df)[2] <- "value"
    colnames(w.sd.df)[2] <- "value"
    colnames(w.median.df)[2] <- "value"
    weights.df <- rbind(w.sum.df, w.sd.df, w.median.df)
    
    vlines <-  data.frame("var" = c("Median absolute deviation", "Median", "Sum"),
                          "value" = c(stats::mad(igraph::E(obs.graph)$weight), 
                                      stats::median(igraph::E(obs.graph)$weight),
                                      sum(igraph::E(obs.graph)$weight)) )
    
    ggplot2::ggplot(weights.df, ggplot2::aes(x = .data[["value"]],  fill = .data[["hypothesis"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3) +
      ggplot2::scale_fill_grey(start = .4, end = .9)  +
      ggplot2::geom_vline(data = vlines,  ggplot2::aes(xintercept = value)) + 
      ggplot2::facet_wrap(~var, ncol=1, scales = "free") +
      ggplot2::xlab("Connection strength") + ggplot2::ggtitle("Connection strength") +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 13))
  })
  
  output$test.simul.weights.plot <-  renderPlot({test.simul.weights.plot()})
  
  output$weights.plot.download <- downloadHandler(
    filename = paste0("archeofrag-weights-", input$spatial.variable, "-",
                      gsub(" / ", "-", names(graph.list())[as.numeric(input$units.pair)]), ".svg"),
    content = function(file) {
      ggplot2::ggsave(file, plot = test.simul.weights.plot(), device = "svg", width=10, height=3, pointsize = 14)
    }
  )
  
  output$weights.plot.download.button <- renderUI({
    if(is.null(test.simul.weights.plot())) return()
    downloadButton("weights.plot.download", "as SVG") 
  })
  
  # .. plot frag. balance ####
  test.simul.balance.plot <-  eventReactive(input$goButton, {  
    req(hypotheses())
    hypotheses.df <- hypotheses()
    if(is.null(hypotheses.df)) return()
    obs.graph <- graph.selected()
    
    ggplot2::ggplot(hypotheses.df, ggplot2::aes(x= .data[["balance.obs"]], fill = .data[["hypothesis"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3, bw=.01)  +
      ggplot2::geom_vline(xintercept = input.graph.params()$balance) +
      ggplot2::scale_x_continuous("Fragments balance") + #, breaks = seq(.2, .4, .02)) + #) +
      ggplot2::scale_fill_grey(start = .4, end = .9) + ggplot2::ggtitle("Fragments balance")
  })
  output$test.simul.balance.plot <- renderPlot({test.simul.balance.plot()})
  
  output$balance.plot.download <- downloadHandler(
    filename = paste0("archeofrag-fragment-balance-", input$spatial.variable, "-",
                      gsub(" / ", "-", names(graph.list())[as.numeric(input$units.pair)]), ".svg"),
    content = function(file) {
      ggplot2::ggsave(file, plot = test.simul.balance.plot(), device = "svg", width=10, height=3, pointsize = 14)
    }
  )
  
  output$balance.plot.download.button <- renderUI({
    if(is.null(test.simul.balance.plot())) return()
    downloadButton("balance.plot.download", "as SVG") 
  })
  
  
  # SPATIAL UNITS OPTIMISATION ####
  output$optimisation.sp.ui <- renderUI({
    graph <- graph.complete()
    
    if(is.null(graph)) return()
    
    spatial.units <- sort(unique(igraph::V(graph)$spatial.variable))
    checkboxGroupInput("optimisation.sp", "Spatial units",
                       choices = spatial.units,
                       selected = spatial.units[seq_len(6)],
                       inline = TRUE
                       )
  })
  
  
  # ... spatial unit checkboxes grid  ----
  optimisation.sp.merge <- reactive({
  req(graph.complete, graph.list)
  graph <- graph.complete()
  
  if(is.null(graph)) return()
  
  spatial.units <- sort(unique(igraph::V(graph)$spatial.variable))
  spatial.units2 <- expand.grid(spatial.units, spatial.units)
  
  merge.su.pairs <- apply(spatial.units2, 1, function(su){
    as.character(checkboxInput(inputId = paste0("merge.", su[1], ".", su[2]), label="", width="10px") )
  })
  
  df <- matrix(merge.su.pairs, ncol = length(spatial.units))
  df <- as.data.frame(df)
  rownames(df) <- spatial.units
  colnames(df) <- spatial.units
  
  df[upper.tri(df, diag = TRUE)] <- ""
  
  df[ -1, -ncol(df)]
  })
  
  
  output$optimisation.sp.merge.ui <- renderDT({
    req(optimisation.sp.merge)
    df <- optimisation.sp.merge()
    
    if(length(df) == 1){
      df <- data.frame(Message = "No additional merge possible. Click on 'reset' to explore other options.")
    }
    
    DT::datatable(df,
                  escape = FALSE,  selection = 'none', filter = "none",
                  options = list(dom = 't', ordering = FALSE, paging = FALSE,
                                 preDrawCallback = DT::JS("function() {Shiny.unbindAll(this.api().table().node()); }"), 
                                 drawCallback = DT::JS("function() {Shiny.bindAll(this.api().table().node()); } ")
                  ))
  })
  
  # ... units to merge ----
  optimisation.table <- eventReactive(input$optimisationButton, {
    req(graph.complete, input$optimisation.sp)
    graph <- graph.complete()
    
    start.time <- Sys.time()  # save start time
    
    spatial.units <- unique(igraph::V(graph)$spatial.variable)
    spatial.units <- spatial.units[spatial.units %in% input$optimisation.sp]
    
    if(length(spatial.units) == 2){
      showNotification("There are only 2 spatial units. No possible merge.", duration = 10, type = "warning")
      return()
    } 
    
    if(length(spatial.units) > 8){
      showNotification("Select no more than 8 spatial units to combine.", duration = 10, type = "warning")
      return()
    }
    if(length(spatial.units) > 5){
      showNotification("Computation has started... Please wait... It can take several minutes.", duration = 20, type ="message")
    } 
    
    pairs  <- .heap_permutation(spatial.units)
    
    # 1. list all combinations:
    # eval(parse(text = paste0("pairs <- expand.grid(", paste0(rep("spatial.units, ", length(spatial.units)), collapse = ""),
    #                           "stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)")))
    # pairs <- as.matrix(pairs)
    # 
    # # 2. keep only the combinations including all spatial.units
    # items <- apply(pairs, 1, function(x)  length(unique(x)))  # this step is a little slow
    # 
    # items.nr <- length(spatial.units)
    # pairs <- pairs[ items == items.nr, ]

    # filter duplicated, considering that within a pair the order of the spatial units does not matter:
    n.pairs <- floor(length(spatial.units) / 2)
    n.pairs <- matrix(seq_len(2 * n.pairs), ncol = 2, byrow = TRUE)
    
    # for each pair of columns, create a tag combining the two labels
    for(row in seq_len(nrow(n.pairs))){
      pairs <- cbind(pairs, apply(pairs, 1, function(x, cols = c(n.pairs[row, ]))  paste0(sort(x[ cols ]), collapse = "")))
    }
    pairs <- pairs[ ! duplicated(pairs[, seq(length(spatial.units) + 1, ncol(pairs))]), ] 
    
    # clean, remove tags columns
    pairs <- pairs[, seq_len(length(spatial.units))]
    
    # 3.  create a reference table with recoded spatial units
    recoded.spatial.units <- pairs
    
    # merge some or all possible pairs of spatial units:
    if(input$parallelize){
      recoded.spatial.units <- foreach::foreach(i = seq(0, nrow(n.pairs) -1), .combine = "rbind", .errorhandling = "remove") %dopar% {
        df <- recoded.spatial.units
        for(row in seq_len(nrow(n.pairs) - i) ){
          df[, n.pairs[row, ]] <- apply(df[, n.pairs[row, ]], 1, function(x) paste0(sort(unlist(x)), collapse = " + "))
        }
        df
      }
    } else {
      recoded.spatial.units <- lapply(seq(0, nrow(n.pairs) -1), function(i){
        df <- recoded.spatial.units
        for(row in seq_len(nrow(n.pairs) - i) ){
          df[, n.pairs[row, ]] <- apply(df[, n.pairs[row, ]], 1, function(x) paste0(sort(unlist(x)), collapse = " + "))
        }
        df
      })
      recoded.spatial.units <- do.call("rbind", recoded.spatial.units)
    }
    
    # duplicate the rows of reference table to get the same row numbers than the recoded table:
    eval(parse(text =  paste0("pairs <- rbind(", paste0(rep("pairs, ", nrow(n.pairs) - 1), collapse = ""), "pairs)")))
    
    # add a line for no merging at all:
    pairs <- rbind(spatial.units, pairs, deparse.level = 0)
    recoded.spatial.units <- rbind(spatial.units, recoded.spatial.units, deparse.level = 0)
    
    # Function which, for each combination of spatial units 
    frag.get.cohesion.dispersion <- function(g, raw.spatial.units.row, recoded.spatial.units.row){
      # 1) recodes spatial units:
      additional.sp.units <- unique(igraph::V(g)$spatial.variable) # determine which are the non selected spatial units
      additional.sp.units <- additional.sp.units[ ! additional.sp.units %in% raw.spatial.units.row]
      
      igraph::V(g)$sp.u.aggregated <- as.character(factor(igraph::V(g)$spatial.variable,
                                              levels = c(raw.spatial.units.row, additional.sp.units),
                                              labels = c(recoded.spatial.units.row, additional.sp.units)))
      # 2) computes edges weights
      g <- archeofrag::frag.edges.weighting(g, "sp.u.aggregated", verbose = FALSE)
      # 3) summarises the difference between cohesion values:
      cohesion.res <- NA
      cohesion.res <- archeofrag::frag.layers.cohesion(graph = g, layer.attr = "sp.u.aggregated", verbose = FALSE)
      cohesion.diff <- apply(cohesion.res, 1, function(x)  sort.int(x)[2] - sort.int(x)[1] )
      admix.res <- apply(cohesion.res, 1, function(x) 1 - sum(x))

      c("Cohesion difference median" = stats::median(cohesion.diff, na.rm = TRUE),
        "MAD" = stats::mad(cohesion.diff, na.rm = TRUE),
        "Admixture median"  = stats::median(admix.res, na.rm = TRUE),
        "MAD." = stats::mad(admix.res, na.rm = TRUE))
    }
    
    # ... run computation ----
    # (Note that it is by far the slowest step of the workflow and it should be improved
    if(input$parallelize){
      cohes.diff.res <- foreach::foreach(i = seq_len(nrow(pairs)), .combine = "rbind", .errorhandling = "pass") %dopar%{
        frag.get.cohesion.dispersion(graph, 
                                     raw.spatial.units.row = pairs[i, ],
                                     recoded.spatial.units.row = recoded.spatial.units[i, ])
      }
    } else{
      cohes.diff.res <- sapply(seq_len(nrow(pairs)), function(i){
        frag.get.cohesion.dispersion(graph,
                                     raw.spatial.units.row = pairs[i, ],
                                     recoded.spatial.units.row = recoded.spatial.units[i, ])
      })
      cohes.diff.res <- t(cohes.diff.res)
    }
    recoded.spatial.units <- apply(recoded.spatial.units, 1, function(x) {x[which(duplicated(x))] <- "" ; x}, simplify = FALSE) # remove duplicated merged spatial units labels
    recoded.spatial.units <- do.call("rbind", recoded.spatial.units)
    recoded.spatial.units <- data.frame(recoded.spatial.units)
    recoded.spatial.units <- cbind(recoded.spatial.units, cohes.diff.res)
    
    # remove duplicates:
    idx <- apply(recoded.spatial.units[, seq_len(ncol(pairs))], 1, function(x)  paste0(sort.int(x), collapse = ""))
    recoded.spatial.units <- recoded.spatial.units[ ! duplicated(idx), ]
    
    recoded.spatial.units[recoded.spatial.units == ""] <- NA
    
    # sort the contents of the lines:
    idx <- seq_len(ncol(pairs))
    recoded.spatial.units[, idx] <- t(apply(recoded.spatial.units[, idx], 1,
                   function(x) sort(unlist(x), na.last = TRUE)))
    
    # remove empty columns
    idx <- apply(recoded.spatial.units, 2, function(x) ! all(is.na(x)))
    recoded.spatial.units <- recoded.spatial.units[, idx ]
    
    colnames(recoded.spatial.units) <- gsub("Var", "Sp. unit ", colnames(recoded.spatial.units))
    rownames(recoded.spatial.units) <- NULL
    # order the result by cohesion difference median value:
    recoded.spatial.units$"Cohesion difference median" <- round(recoded.spatial.units$"Cohesion difference median", 3)
    recoded.spatial.units$MAD <- round(recoded.spatial.units$MAD, 3)
    recoded.spatial.units$"Admixture median" <- round(recoded.spatial.units$"Admixture median", 3)
    recoded.spatial.units$MAD. <- round(recoded.spatial.units$MAD., 3)
    
    exec.time <- Sys.time() - start.time
    exec.time <- paste(round(as.numeric(exec.time), 0), units(exec.time))
    
    idx <- order(recoded.spatial.units$"Cohesion difference median",
                 recoded.spatial.units$"Admixture median", recoded.spatial.units[, 1])
    
    list(recoded.spatial.units[idx, ], exec.time)
    })  
  
  
  # ... merge stats table ----
  output$optimisationTab <- DT::renderDT({ 
    tab <- optimisation.table()[[1]]
    
    if(is.null(tab)) return()
    
    sketch <-  paste0("<table class='display'>
  <thead>
    <tr>",
       paste0(sapply(seq_len(ncol(tab) - 4),
         function(x)  paste0('<th rowspan=2>Sp. unit ', x, '</th>' , collapse = "")), collapse = ""),
      "<th colspan=2>Cohesion differences</th>
      <th colspan=2>Admixture values</th>
    </tr>
    <tr>
      <th>Median</th>
      <th>MAD</th>
      <th>Median</th>
      <th>MAD</th>
    </tr>
  </thead>
</table>", collapse="")
    
    DT::datatable(tab, rownames = FALSE, container = sketch, escape = FALSE, style = "default", selection = 'none',
                  options = list(dom = 'tp'))
    })
  
  
  
    output$optimisationText <- renderText({
      req(optimisation.table, graph.selected)
      graph <- graph.complete()
      optim.results <- optimisation.table()
      
      spatial.units <- unique(igraph::V(graph)$spatial.variable)
      spatial.units <- spatial.units[spatial.units %in% input$optimisation.sp]
      
      if(is.null(optim.results[[1]])) return()
      
      cohesion.res <- frag.layers.cohesion(graph, layer.attr = "spatial.variable", verbose = FALSE)
      cohesion.res <- apply(cohesion.res, 1, function(x)  sort.int(x)[2] - sort.int(x)[1] )
      median.res <- round(stats::median(cohesion.res, na.rm = TRUE), 3)
      
      if(median.res <= min(optim.results[[1]]$"Cohesion difference median")){
        comments.str <- "none of the merging solutions returned a lower median value."
      } else{
        comments.str <- "some merging solutions returned lower median values."
      }
      
      paste0("<b>Computation results:</b> ", nrow(optim.results[[1]]),
             " merging solutions  from ", length(spatial.units), " spatial units, computed in ", optim.results[[2]], "<br>",
            "<b>Median of the cohesion differences without merging:</b> ",
            median.res, 
            " +/- ", round(stats::mad(cohesion.res, na.rm = TRUE), 3), "<br>",
            "<b>Comment:</b> ", comments.str
            )
  })
  
    
  # ... merge and udpate graph.complete ----
    observeEvent(input$mergeButton, { 
      req(optimisation.sp.merge, graph.selected)
      graph.init <- graph.complete()    # save a copy
      graph.to.update <- graph.complete()
      
      units.to.merge <- optimisation.sp.merge()
      units.to.merge <- expand.grid(rownames(units.to.merge), colnames(units.to.merge))
      
      idx <- apply(units.to.merge, 1, function(su){
        eval(parse(text = paste0("isTRUE(input$merge.", su[1], ".", su[2], ")" )))
      })
      
      units.to.merge <- units.to.merge[idx, ]
      
      g <- igraph::graph_from_data_frame(units.to.merge)
      igraph::V(g)$membership <- igraph::components(g)$membership
      
      for(cluster in unique(igraph::V(g)$membership)){
        selected.units <- sort(igraph::V(g)[igraph::V(g)$membership == cluster]$name)
        igraph::V(graph.to.update)[ igraph::V(graph.to.update)$spatial.variable %in% selected.units]$spatial.variable <- paste0(selected.units, collapse = "+")
      }
      
      if(length(unique(igraph::V(graph.to.update)$spatial.variable)) == 1){
        showNotification("Merging results in only one spatial units. Change settings.",
                         duration = 10, type = "warning")
        graph.complete(graph.init)
      } else {
        graph.complete(graph.to.update)
      }
    })
    
    
    observeEvent(input$resetMergeButton, {
      graph.complete(graph.complete.init())
    })
  
  
  # VISUALISATION ####
  output$visualisation.title <- renderText({
    units.pair <- names(graph.list())[as.numeric(input$units.pair)]
    req(graph.selected())
    g <- graph.selected()
    
    paste0("Fragmentation graph for spatial units <b>", units.pair, 
           "</b> from the <b>",  input$spatial.variable, 
           "</b> variable. 
            <ul>
              <li>lines: connection relationships (n=", igraph::gsize(g), ")</li> 
              <li>nodes: fragments (n=", igraph::gorder(g), ")</li> 
              <li>colors: spatial units associated with the fragments (<b><font color=YellowGreen>green</font></b> for <b>",  
           gsub("^(.*)/.*", "\\1", units.pair), 
           "</b>, <b><font color=purple>purple</font></b> for <b>", gsub("^.*/(.*)", "\\1", units.pair), 
           "</b>)</li></ul>
           Note that the node positions are only determined by the graph drawing method and do not reflect the archaeological location of the fragments in the site.
           ")
  })
  
  frag.graph.viz <- reactive({   
    req(graph.selected())
    g <- graph.selected()
    igraph::E(g)$weight <- 1 # temporary workaround to deal with unexpected 'negative' weights from frag.edge.weight
    g
  })
  
  output$frag.graph.viz.plot <- renderPlot({ 
    archeofrag::frag.graph.plot(frag.graph.viz(), layer.attr = "spatial.variable") 
  })
  
  
  output$frag.graph.viz.download <- downloadHandler(
    filename = paste0("archeofrag-frag-graph-", input$spatial.variable, "-",
                      gsub(" / ", "-", names(graph.list())[as.numeric(input$units.pair)]), ".svg"),
    content = function(file) {
      grDevices::svg(file)
      archeofrag::frag.graph.plot(frag.graph.viz(), layer.attr = "spatial.variable") 
      grDevices::dev.off()
    }
  )
  
  output$frag.graph.viz.download.button <- renderUI({
    if(is.null(frag.graph.viz())) return()
    downloadButton("frag.graph.viz.download", "Download as SVG")
  })
  
  
  
  # R CODE ----
  
  r.code <- reactive({
    req(input$n.components)
    
    asymmetric <- input$asymmetric
    if(asymmetric == "none"){
      asymmetric.str <- ""
    } else{
      asymmetric.str <- paste0("                                     asymmetric.transport.from = ", asymmetric, ",<br>")
    }
    
    mode <- "%par%"
    if(input$parallelize) mode <- "%dopar%"
    
    generate.run.code <- function(n.layers, edge.loss, vertice.loss){
      
      connection.to.remove.str <- ""
      if(edge.loss > 0){
        connection.to.remove.str <- paste0("              g <- archeofrag::frag.observer.failure(g, likelihood = ",
             edge.loss, " / 100,<br>",
             "                                                     remove.vertices = TRUE)[[1]]<br>")
      }
      
      frag.to.remove.str <- ""
      if(vertice.loss > 0){
        frag.to.remove.str <- paste0("              n.frag.to.remove <- round(",
                                 input$vertice.loss, " / 100) * igraph::gorder(g), 0)<br>",
             "              g <- archeofrag::frag.graph.reduce(g, n.frag.to.remove = n.frag.to.remove,<br>",
             "                                                    conserve.objects.nr = FALSE)<br>")
      }
      
      paste0("<pre>",
             "h", n.layers, " <- foreach(i=1:", input$replications, ", .combine = 'rbind', .errorhandling = 'remove') ", mode," {<br><br>",
             "             g <- frag.simul.process(initial.layers = ", n.layers, ",\n",
             "                                     n.components = ", input$n.components, ",<br>",
             "                                     vertices = ", input$n.final.fragments, ",<br>",  
             "                                     balance = ", input$balance, ",<br>",
             "                                     components.balance = ", input$components.balance, ",<br>",
             "                                     disturbance = ", input$disturbance, ",<br>",
             "                                     aggreg.factor = ", input$aggreg.factor, ",<br>",
                                                   asymmetric.str,
             "                                     planar = ", input$planar, ")<br>",
             connection.to.remove.str,
             frag.to.remove.str,
             "              data.frame(<br>",
             "                 'admixture'       = round(frag.layers.admixture(g, 'layer'), 3),<br>",
             "                 'cohesion'        = rbind(frag.layers.cohesion(g, 'layer')),<br>",
             "                 'relations'       = igraph::gsize(g),<br>",
             "                 'balance'         = table(igraph::V(g)$layer)[1] / igraph::gorder(g),<br>",
             "                 'weights.sum'     = sum(igraph::E(g)$weight),<br>",
             "                 'weights.median'  = stats::median(igraph::E(g)$weight),<br>",
             "                 'weights.mad'     = stats::mad(igraph::E(g)$weight)<br>",
             "              )<br>",
             "       }", 
             "</pre>")
    }
    
    sessioninfo.str <- toLatex(utils::sessionInfo(), locale = FALSE)
    sessioninfo.str <- paste0(sessioninfo.str[- c(1, length(sessioninfo.str))], collapse = "<br>")
    sessioninfo.str <- paste0("<pre>", sessioninfo.str, "</pre>")
    sessioninfo.str <- gsub("\\\\item ", "", sessioninfo.str)
    sessioninfo.str <- gsub("\\\\verb", "", sessioninfo.str)
    sessioninfo.str <- gsub("[~|]", " ", sessioninfo.str)
    sessioninfo.str <- gsub(" <br>; \\\\quad\\\\", ",", sessioninfo.str)
    
    parallel.string <- ""
    if(input$parallelize) parallel.string <- "library(doParallel)<br>registerDoParallel()<br>"
    
    paste0("<pre>library(archeofrag) <br>library(igraph) <br>library(foreach)<br>", parallel.string, "</pre>",
           generate.run.code(1, edge.loss = input$edge.loss, vertice.loss = input$vertice.loss), 
           "<br>", 
           generate.run.code(2, edge.loss = input$edge.loss, vertice.loss = input$vertice.loss),
           "<br><h2>Session info</h2>",
           sessioninfo.str
           )
  }) # end reactive
  
  output$r.code <- reactive({r.code()})
  
  observeEvent(input$r.code.copy.button, {
    Rcode.plaintext <- r.code()
    
    Rcode.plaintext <- gsub("<br>", "\n", Rcode.plaintext)
    Rcode.plaintext <- gsub("</?pre>", "", Rcode.plaintext)
    session$sendCustomMessage("txt", Rcode.plaintext)
  })
  
  
  # openMOLE  ----
  # .. UI elements  ----
  
  output$OM.objectsNumber.min.ui <- renderUI({
    numericInput("OM.objectsNumber.min", "minimum", min = 1, step = 1, 
                 value = round(input.graph.params()$n.components / 2, 0))
  })
  
  output$OM.objectsNumber.max.ui <- renderUI({
    numericInput("OM.objectsNumber.max", "maximum", min = 1, step = 1,
                 value =  input.graph.params()$n.components * 10)
  })
  
  output$OM.fragmentsNumber.min.ui <- renderUI({
    numericInput("OM.fragmentsNumber.min", "minimum", min = 1, step = 1, 
                 value =  input.graph.params()$vertices)
  })
  
  output$OM.fragmentsNumber.max.ui <- renderUI({
    numericInput("OM.fragmentsNumber.max", "maximum", min = 1, step = 1,
                 value =  input.graph.params()$vertices * 100)
  })
  
  output$OM.FinalfragmentsCount.sens.ui <- renderUI({
    sliderInput("OM.fragmentsCountOut.sens", 
                paste0("Final fragments count (obs. value: ", input.graph.params()$vertices, ") +/- (%)"), 
                width="100%",
                value = 0, min = 0, max = 50, step = 1)
  })
  
  output$OM.fragmentsBalance.val.ui <- renderUI({
    bal <- input.graph.params()$balance
    sliderInput("OM.fragmentsBalance.val", 
                paste0("Fragments balance (obs. value: ", input.graph.params()$balance, ")"),
                min = 0.01, max=0.99, step = 0.01, 
                value = c(bal - .1, bal + .1))
  })
  
  output$OM.objectsBalance.val.ui <- renderUI({
    comp.bal <- input.graph.params()$components.balance
    sliderInput("OM.objectsBalance.val", 
                paste0("Initial objects balance (obs. value: ", input.graph.params()$components.balance, ")"),
                min = 0.01, max = 0.99, step = 0.01, 
                value = c(comp.bal - .1, comp.bal + .1))
  })
  
  output$OM.disturbance.val.ui <- renderUI({
    disturbance <- input.graph.params()$disturbance
    disturbance.max <- disturbance + .1
    disturbance.min <- disturbance - .1
    if(disturbance.min <= 0){disturbance.min <- 0.01}
    
    sliderInput("OM.disturbance.val",
                paste0("Disturbance (obs. value: ", input.graph.params()$disturbance, ")"), 
                min = 0, max = 1, step = 0.01, 
                value = c(disturbance.min, disturbance.max))
  })
  
  output$OM.aggregFactor.val.ui <- renderUI({
    agreg <- input.graph.params()$aggreg.factor

    sliderInput("OM.aggregFactor.val", 
                paste0("Fragments aggregation (obs. value: ", input.graph.params()$aggreg.factor, ")"),
                min = 0, max = 1, step = 0.01, value = c(0, 0))
  })
  
  output$OM.asymmetric.selection <- renderUI({
    req(input.graph.params())
    units.pair <- names(graph.list())[as.numeric(input$units.pair)]
    
    from1to2  <- gsub("/", "->", units.pair)       
    from2to1 <- gsub("^(.*) / (.*)$", "\\2 -> \\1", units.pair)
                
    eval(parse(text = paste0(
      "selectInput('OM.asymmetric.val', 'Unidirectional transport from unit', ",
      "choices = c('none' = 'none',",
      "'", from1to2, "' = '1',", 
      "'", from2to1, "' = '2',",
      "'", from2to1, ", ", from2to1, "' = '1, 2',",
      "'", "none, ", from2to1, ", ", from2to1, "' = 'none, 1, 2'",
      "),",
      "selected = '", "none, ", from2to1, ", ", from2to1,"', width = '100%')"
    )))
  })
  
  
  # .. code ----
  openMOLE.code <- reactive({
    req(input$OM.asymmetric.val)
    

    # .. origin variables ----
    OM.layerNumber.str <- ""
    OM.objectsNumber.str <- ""
    OM.fragmentsNumber.str <- ""
    OM.fragmentsBalance.str <- ""
    OM.objectsBalance.str <- ""
    OM.disturbance.str <- ""
    OM.aggregFactor.str <- ""
    OM.preserveObjectsNumber.str <- ""
    OM.preserveFragmentsBalance.str <- ""
    OM.preserveInterUnitsConnection.str <- ""
    OM.planarGraphsOnly.str <- ""
    OM.asymmetric.str <- ""
    
    OM.layerNumber.str <- paste0("    layerNumber in Seq(", input$OM.layerNumber.val, "),<br>")
    OM.objectsNumber.str <- paste0("    objectsNumber in (", format(input$OM.objectsNumber.min, nsmall = 2), " to ",
                                   format(input$OM.objectsNumber.max, nsmall = 2), "),<br>")
    OM.fragmentsNumber.str <- paste0("    fragmentsNumber in (", format(input$OM.fragmentsNumber.min, nsmall = 2), " to ",
                                     format(input$OM.fragmentsNumber.max, nsmall = 2), "),<br>")
    
    # final fragment count (min / max)
    final.frag.count.adjust <- round(input.graph.params()$vertices * input$OM.fragmentsCountOut.sens / 100, 0)
    finalFragCountMin <- input.graph.params()$vertices - final.frag.count.adjust
    finalFragCountMax <- input.graph.params()$vertices + final.frag.count.adjust
    
    OM.finalFragmentsNumberMinOut.str <- paste0('  finalFragmentsNumberMin := ', finalFragCountMin, ',<br>') 
    
    OM.finalFragmentsNumberMaxOut.str <- ""
    OM.finalFragmentsNumberMax.map.str <- ""
    OM.finalFragmentsNumberMax.init.str <- ""
    
    if(finalFragCountMin != finalFragCountMax){
      OM.finalFragmentsNumberMaxOut.str <- paste0('  finalFragmentsNumberMax := ', finalFragCountMax, ',<br>') 
      OM.finalFragmentsNumberMax.map.str <- '  inputs += finalFragmentsNumberMax.mapped,<br>'
      OM.finalFragmentsNumberMax.init.str <- 'val finalFragmentsNumberMax = Val[Int]<br>'
    }
    
    # preserve object number
    preserveObjectsNumber.str <- unlist(strsplit(input$OM.preserveObjectsNumber.val, split = ", "))
    
    if(length(preserveObjectsNumber.str) > 1){
      OM.preserveObjectsNumber.str <- "    preserveObjectsNumber in TrueFalse,<br>"
    }
    
    # preserve fragments balance
    preserveFragmentsBalance.str <- unlist(strsplit(input$OM.preserveFragmentsBalance.val, split = ", "))
    
    if(length(preserveFragmentsBalance.str) > 1){
      OM.preserveFragmentsBalance.str <- "    preserveFragmentsBalance in TrueFalse,<br>"
    }    
    
    # preserve inter units connection
    preserveInterUnitsConnection.str <- unlist(strsplit(input$OM.preserveInterUnitsConnection.val, split = ", "))
    
    if(length(preserveInterUnitsConnection.str) > 1){
      OM.preserveInterUnitsConnection.str <- "    preserveInterUnitsConnection in TrueFalse,<br>"
    }    
    
    # planarity
    planarGraphOnly.str <- unlist(strsplit(input$OM.planarGraphsOnly.val, split = ", "))
    
    if(length(planarGraphOnly.str) > 1){
      OM.planarGraphsOnly.str <- "    planarGraphsOnly in TrueFalse,<br>"
    }
    
    # asymmetric transport
    asymmetric.val <- gsub("none", "0", input$OM.asymmetric.val)
    asymmetric.str <- as.integer(unlist(strsplit(asymmetric.val, split = ", ")))
    
    if(length(asymmetric.str) > 1){
      OM.asymmetric.str <- paste0("    asymmetricTransport in Seq(", paste(asymmetric.str, collapse = ", "), "),<br>")
    }
  
    # For each variable, 4 strings are defined and used only if the variable is selected as an objective:
    # .init.str: variable declaration
    # .map.str: OM variable mapping
    # .R.str: R code
    # .obj.str: opoen mole code to declare the variable as an objective
    OM.relationCountOut.obj.str  <- ""
    OM.objectCountOut.obj.str  <- ""
    OM.disturbanceOut.obj.str  <- ""
    OM.objectsBalanceOut.obj.str  <- ""
    OM.fragBalanceOut.obj.str  <- ""
    OM.asymmetricOut.obj.str  <- ""
    OM.aggregFactorOut.obj.str  <- ""
    # OM.weightsumOut.obj.str  <- ""
    OM.cohesion1Out.obj.str  <- ""
    OM.cohesion2Out.obj.str  <- ""
    OM.admixtureOut.obj.str  <- ""
    
    OM.relationCountOut.map.str  <- ""
    OM.objectCountOut.map.str  <- ""
    OM.disturbanceOut.map.str  <- ""
    OM.objectsBalanceOut.map.str  <- ""
    OM.fragBalanceOut.map.str  <- ""
    OM.aggregFactorOut.map.str  <- ""
    # OM.weightsumOut.map.str  <- ""
    OM.cohesion.map.str  <- ""
    OM.cohesion1Out.map.str  <- ""
    OM.cohesion2Out.map.str  <- ""
    OM.admixtureOut.map.str  <- ""
    OM.finalFragmentCountOut.map.str <- ""
    
    OM.relationCountOut.R.init.str  <- ""
    OM.objectCountOut.R.init.str  <- ""
    OM.disturbanceOut.R.init.str  <- ""
    OM.objectsBalanceOut.R.init.str  <- ""
    OM.fragBalanceOut.R.init.str  <- ""
    OM.aggregFactorOut.R.init.str  <- ""
    # OM.weightsumOut.R.init.str  <- ""
    OM.cohesion.R.init.str  <- ""
    OM.cohesion1Out.R.init.str  <- ""
    OM.cohesion2Out.R.init.str  <- ""
    OM.admixtureOut.R.init.str  <- ""
    OM.finalFragmentCountOut.R.init.str <- ""
    
    OM.relationCountOut.R.str  <- ""
    OM.objectCountOut.R.str  <- ""
    OM.disturbanceOut.R.str  <- ""
    OM.objectsBalanceOut.R.str  <- ""
    OM.fragBalanceOut.R.str  <- ""
    OM.aggregFactorOut.R.str  <- ""
    # OM.weightsumOut.R.str  <- ""
    OM.cohesion.R.str  <- ""
    OM.cohesion1Out.R.str  <- ""
    OM.cohesion2Out.R.str  <- ""
    OM.admixtureOut.R.str  <- ""
    OM.finalFragmentCountOut.R.str <- ""
    
    OM.relationCountOut.init.str <- ""
    OM.objectCountOut.init.str <- ""
    OM.disturbanceOut.init.str <- ""
    OM.objectsBalanceOut.init.str <- ""
    OM.fragBalanceOut.init.str <- ""
    OM.aggregFactorOut.init.str <- ""
    # OM.weightsumOut.init.str  <- ""
    OM.cohesion1Out.init.str <- ""
    OM.cohesion2Out.init.str <- ""
    OM.admixtureOut.init.str <- ""
    OM.finalFragmentCountOut.init.str <- ""
    
    obs.admix <-  round(archeofrag::frag.layers.admixture(graph.selected(), "spatial.variable", verbose = FALSE), 2)
    obs.cohesion <- round(archeofrag::frag.layers.cohesion(graph.selected(), "spatial.variable", verbose = FALSE), 2)
    
    if(input$OM.relationCountOut){ 
      OM.relationCountOut.init.str <- 'val relationCountOut = Val[Int]<br>' 
      OM.relationCountOut.map.str  <- '  outputs += relationCountOut.mapped,<br>'
      OM.relationCountOut.R.init.str <- "            relationCountOut <- -1<br>"
      OM.relationCountOut.R.str <- "                relationCountOut <- frag.params$edges<br>"
      
      OM.relationCountOut.sens <- 0.1
      if(input$OM.relationCountOut.sens != 0){
        OM.relationCountOut.sens <- round(input.graph.params()$edges * input$OM.relationCountOut.sens / 100, 0)
      }
      OM.relationCountOut.obj.str <- paste0("    relationCountOut evaluate \"relationCountOut.map(x => math.abs(x - ",
                                          input.graph.params()$edges, ")).max\" under ", OM.relationCountOut.sens, ",<br>")
    }
   
    if(input$OM.objectCountOut){
      OM.objectCountOut.init.str <- 'val objectCountOut = Val[Int]<br>' 
      OM.objectCountOut.map.str  <- '  outputs += objectCountOut.mapped,<br>'
      OM.objectCountOut.R.init.str <- '            objectCountOut   <- -1<br>'
      OM.objectCountOut.R.str <- '                objectCountOut   <- frag.params$n.components<br>'
      
      OM.objectCountOut.sens <- 0.1
      if(input$OM.objectCountOut.sens != 0){
        OM.objectCountOut.sens <- round(input.graph.params()$n.components * input$OM.objectCountOut.sens / 100, 0)
      }
      OM.objectCountOut.obj.str <- paste0("    objectCountOut evaluate \"objectCountOut.map(x => math.abs(x - ",
                                          input.graph.params()$n.components, ")).max\" under ", OM.objectCountOut.sens, ",<br>")
    }
    
    if(input$OM.disturbanceOut){
      OM.disturbanceOut.init.str <- 'val disturbanceOut = Val[Double]<br>' 
      OM.disturbanceOut.map.str  <- '  outputs += disturbanceOut.mapped,<br>'
      OM.disturbanceOut.R.init.str <- '            disturbanceOut   <- -1.0<br>'
      OM.disturbanceOut.R.str <- '                disturbanceOut   <- frag.params$disturbance<br>'
      
      OM.disturbanceOut.sens <- 0.001
      if(input$OM.disturbanceOut.sens != 0){OM.disturbanceOut.sens <- input$OM.disturbanceOut.sens}
      OM.disturbanceOut.obj.str <- paste0("    disturbanceOut evaluate \"disturbanceOut.map(x => math.abs(x - ",
                                            input.graph.params()$disturbance, ")).max\" under ", OM.disturbanceOut.sens, ",<br>")
    }
    
    if(input$OM.objectsBalanceOut){
      OM.objectsBalanceOut.init.str <- 'val objectsBalanceOut = Val[Double]<br>' 
      OM.objectsBalanceOut.map.str  <- '  outputs += objectsBalanceOut.mapped,<br>'
      OM.objectsBalanceOut.R.init.str <- '            objectsBalanceOut <- -1.0<br>'
      OM.objectsBalanceOut.R.str <- '                objectsBalanceOut <- frag.params$components.balance<br>'
      
      OM.objectsBalanceOut.sens <- 0.001
      if(input$OM.objectsBalanceOut.sens != 0){OM.objectsBalanceOut.sens <- input$OM.objectsBalanceOut.sens}
      OM.objectsBalanceOut.obj.str <- paste0("    objectsBalanceOut evaluate \"objectsBalanceOut.map(x => math.abs(x - ",
                                          input.graph.params()$components.balance, ")).max\" under ", OM.objectsBalanceOut.sens, ",<br>")
    }
    
    if(input$OM.fragBalanceOut){
      OM.fragBalanceOut.init.str <- 'val fragBalanceOut = Val[Double]<br>' 
      OM.fragBalanceOut.map.str  <- '  outputs += fragBalanceOut.mapped,<br>'
      OM.fragBalanceOut.R.init.str <- '            fragBalanceOut   <- -1.0<br>'
      OM.fragBalanceOut.R.str <- '                fragBalanceOut   <- frag.params$balance<br>'
      
      OM.fragBalanceOut.sens <- 0.001
      if(input$OM.fragBalanceOut.sens != 0){OM.fragBalanceOut.sens <- input$OM.fragBalanceOut.sens}
      OM.fragBalanceOut.obj.str <- paste0("    fragBalanceOut evaluate \"fragBalanceOut.map(x => math.abs(x - ",
                                          input.graph.params()$balance, ")).max\" under ", OM.fragBalanceOut.sens, ",<br>")
    }
    
    if(input$OM.aggregFactorOut){
      OM.aggregFactorOut.init.str <- 'val aggregationOut = Val[Double]<br>' 
      OM.aggregFactorOut.map.str  <- '  outputs += aggregationOut.mapped,<br>'
      OM.aggregFactorOut.R.init.str <- '            aggregationOut   <- -1.0<br>'
      OM.aggregFactorOut.R.str <- '                aggregationOut   <- frag.params$aggreg.factor<br>'
        
      OM.aggregFactorOut.sens <- 0.001
      if(input$OM.aggregFactorOut.sens != 0){OM.aggregFactorOut.sens <- input$OM.aggregFactorOut.sens}
      OM.aggregFactorOut.obj.str <- paste0("    aggregationOut evaluate \"aggregationOut.map(x => math.abs(x - ",
                                      obs.admix, ")).max\" under ", OM.aggregFactorOut.sens, ",<br>")
    }
    # if(input$OM.weightsumOut) OM.weightsumOut.str <- paste0("weightsumOut delta", input$TODO, "under", OM.weightsumOut.sens, ",<br>")
    
    if(input$OM.cohesion1Out | input$OM.cohesion2Out) {
      OM.cohesion.R.str <- '                cohesion.results <- frag.layers.cohesion(g, \'layer\')<br><br>'
    }
    
    if(input$OM.cohesion1Out) {
      
      OM.cohesion1Out.sens <- 0.001
      if(input$OM.cohesion1Out.sens != 0){OM.cohesion1Out.sens <- input$OM.cohesion1Out.sens}
      
      OM.cohesion1Out.init.str <- 'val cohesion1Out = Val[Double]<br>' 
      OM.cohesion1Out.map.str  <- '  outputs += cohesion1Out.mapped,<br>'
      OM.cohesion1Out.R.init.str <- '            cohesion1Out     <- -1.0<br>'
      OM.cohesion1Out.R.str <- '                cohesion1Out     <- cohesion.results[1]<br>'
      OM.cohesion1Out.obj.str <- paste0("    cohesion1Out evaluate \"cohesion1Out.map(x => math.abs(x - ",
                                        obs.cohesion[1], ")).max\" under ", OM.cohesion1Out.sens, ",<br>")
    }
    
    if(input$OM.cohesion2Out){
      
      OM.cohesion2Out.sens <- 0.001
      if(input$OM.cohesion2Out.sens != 0){OM.cohesion2Out.sens <- input$OM.cohesion2Out.sens}
      
      OM.cohesion2Out.init.str <- 'val cohesion2Out = Val[Double]<br>' 
      OM.cohesion2Out.map.str  <- '  outputs += cohesion2Out.mapped,<br>'
      OM.cohesion2Out.R.init.str <- '            cohesion2Out     <- -1.0<br>'
      OM.cohesion2Out.R.str <- '                cohesion2Out     <- cohesion.results[2]<br>'
      OM.cohesion2Out.obj.str <- paste0("    cohesion2Out evaluate \"cohesion2Out.map(x => math.abs(x - ",
                                        obs.cohesion[2], ")).max\" under ",  OM.cohesion2Out.sens, ",<br>")
    }
    
    if(input$OM.admixtureOut){
      
      OM.admixtureOut.sens <- 0.001
      if(input$OM.admixtureOut.sens != 0){OM.admixtureOut.sens <- input$OM.admixtureOut.sens}
      
      OM.admixtureOut.init.str <- 'val admixtureOut = Val[Double]<br>'
      OM.admixtureOut.map.str  <- '  outputs += admixtureOut.mapped,<br>'
      OM.admixtureOut.R.init.str <- '            admixtureOut     <- -1.0<br>'
      
      OM.admixtureOut.R.str <- '                admixtureOut     <- frag.layers.admixture(g, \'layer\')<br>'
      if(input$OM.cohesion1Out | input$OM.cohesion2Out){
        OM.admixtureOut.R.str <- '                admixtureOut     <- 1 - sum(cohesion.results)<br>'
      }
      
      OM.admixtureOut.obj.str <- paste0("    admixtureOut evaluate \"admixtureOut.map(x => math.abs(x - ",
                                        obs.admix, ")).max\" under ", OM.admixtureOut.sens, ",<br>")
    }
# browser()
    if(input$OM.fragmentsCountOut.sens > 0){
      OM.finalFragmentCountOut.init.str <-   'val finalFragmentCountOut = Val[Int]<br>' 
      OM.finalFragmentCountOut.map.str  <-   '  outputs += finalFragmentCountOut.mapped,<br>'
      OM.finalFragmentCountOut.R.init.str <- '            finalFragmentCountOut <- -1.0<br>'
      OM.finalFragmentCountOut.R.str <-      '                finalFragmentCountOut <- frag.params$vertices <br>'
    }
    
    get.param.str <- ""
    if(input$OM.relationCountOut | input$OM.objectCountOut | input$OM.disturbanceOut | input$OM.objectsBalanceOut | input$OM.fragBalanceOut | input$OM.aggregFactorOut){
      get.param.str <- '                frag.params <- frag.get.parameters(g, \'layer\')<br>'
    }
    
    # Default initialisation values for ranges: by default, the value read on the studied graph. However, if the values selected by the user are equal, replace the default value by this selected value
    fragmentsBalance.default <- input.graph.params()$balance
    if(input$OM.fragmentsBalance.val[1] == input$OM.fragmentsBalance.val[2]){
      fragmentsBalance.default <- input$OM.fragmentsBalance.val[1]
    }
    
    objectsBalance.default <- input.graph.params()$components.balance 
    if(input$OM.objectsBalance.val[1] == input$OM.objectsBalance.val[2]){
      objectsBalance.default <-   input$OM.objectsBalance.val[1]
    }
    
    disturbance.default <- input.graph.params()$disturbance 
    if(input$OM.disturbance.val[1] == input$OM.disturbance.val[2]){
      disturbance.default <-   input$OM.disturbance.val[1]
    }
    
    aggregFactor.default <- input.graph.params()$aggreg.factor 
    if(input$OM.aggregFactor.val[1] == input$OM.aggregFactor.val[2]){
      aggregFactor.default <-  input$OM.aggregFactor.val[1]
    }
    
    
    # check whether ranges of value are available for origins variables
    if(input$OM.fragmentsBalance.val[1] != input$OM.fragmentsBalance.val[2]){
      OM.fragmentsBalance.str <- paste0("    fragmentsBalance in (", input$OM.fragmentsBalance.val[1], " to ",
                                        input$OM.fragmentsBalance.val[2], "),<br>")
    }
    
    if(input$OM.objectsBalance.val[1] != input$OM.objectsBalance.val[2]){
      OM.objectsBalance.str <- paste0("    objectsBalance in (", input$OM.objectsBalance.val[1], " to ",
                                         input$OM.objectsBalance.val[2], "),<br>")
    }
    
    if(input$OM.disturbance.val[1] != input$OM.disturbance.val[2]){ 
      OM.disturbance.str <- paste0("    disturbance in (", input$OM.disturbance.val[1], " to ",
                                   input$OM.disturbance.val[2], "),<br>")
    }
    
    if(input$OM.aggregFactor.val[1] != input$OM.aggregFactor.val[2]){ 
      OM.aggregFactor.str <- paste0("    aggregation in (", input$OM.aggregFactor.val[1], " to ",
                                    input$OM.aggregFactor.val[2], "),<br>")
    }
    
    # nfrag string, determining the number of fragments to remove
    frag.reduce.str <- paste0(
    '                g <- frag.graph.reduce(graph = g,<br>',
    '                                       n.frag.to.remove = length(g) - finalFragmentsNumberMin,<br>')
                              
    if(finalFragCountMin != finalFragCountMax){
      frag.reduce.str <- paste0(
        '                n.frag <- igraph::gorder(g) - sample(seq.int(finalFragmentsNumberMin, finalFragmentsNumberMax), 1)<br>',
        '                g <- frag.graph.reduce(graph = g,<br>',
        '                                       n.frag.to.remove = n.frag,<br>')
    }
    
    frag.reduce.str <- paste0(frag.reduce.str,
    '                                       conserve.objects.nr = preserveObjectsNumber,<br>',
    '                                       conserve.fragments.balance = preserveFragmentsBalance,<br>',
    '                                       conserve.inter.units.connection = preserveInterUnitsConnection)<br>')
    
    # .. settings ----
    OM.islands.str <- ""
    if(input$OM.islands > 0){
      OM.islands.str <- paste0(" by Island(", input$OM.islands, " minutes) ")
    }
    
    # .. model ----
    om.code <- paste0('<pre>',
           '// Input values<br>',
           'val layerNumber = Val[Int]<br>',
           'val objectsNumber = Val[Int]<br>',
           'val fragmentsNumber = Val[Int]<br>',
           'val finalFragmentsNumberMin = Val[Int]<br>',
           OM.finalFragmentsNumberMax.init.str,
           'val objectsBalance = Val[Double]<br>',
           'val fragmentsBalance = Val[Double]<br>',
           'val disturbance = Val[Double]<br>',
           'val aggregation = Val[Double]<br>',
           'val asymmetricTransport = Val[Int]<br>',
           'val planarGraphsOnly = Val[Boolean]<br>',
           'val preserveObjectsNumber = Val[Boolean]<br>',
           'val preserveFragmentsBalance = Val[Boolean]<br>',
           'val preserveInterUnitsConnection = Val[Boolean]<br>',
           'val mySeed = Val[Int]<br>',
           '<br>',
           '// Output values<br>',
           OM.cohesion1Out.init.str,
           OM.cohesion2Out.init.str,
           OM.admixtureOut.init.str,
           OM.relationCountOut.init.str,
           OM.objectCountOut.init.str,
           OM.objectsBalanceOut.init.str,
           OM.fragBalanceOut.init.str,
           OM.disturbanceOut.init.str,
           OM.aggregFactorOut.init.str,
           OM.finalFragmentCountOut.init.str,
           '<br>',
           'val local = LocalEnvironment(', input$OM.parallelize, ')  // Number of cores to use. Adjust as needed<br>',
           '<br>',
           'val archeofrag =  RTask(<br>',
           '//  containerSystem = SingularityFlatImage(), // optionnal <br>',
           '  script = """<br>',
           '            library(archeofrag)<br>',
           '            # Declare default values:<br>',
           OM.cohesion1Out.R.init.str,
           OM.cohesion2Out.R.init.str,
           OM.admixtureOut.R.init.str,
           OM.relationCountOut.R.init.str,
           OM.objectCountOut.R.init.str,
           OM.objectsBalanceOut.R.init.str,
           OM.disturbanceOut.R.init.str,
           OM.fragBalanceOut.R.init.str,
           OM.aggregFactorOut.R.init.str,
           OM.finalFragmentCountOut.R.init.str,
           '            try({<br>',
           '                set.seed(mySeed)<br>',
           '                # Generate fragmentation graph:<br>',
           '                g <- frag.simul.process(initial.layers = layerNumber,<br>',
           '                                        n.components = objectsNumber,<br>',
           '                                        vertices = fragmentsNumber,<br>',
           '                                        edges = Inf,<br>',
           '                                        balance = fragmentsBalance,<br>',
           '                                        components.balance = objectsBalance,<br>',
           '                                        disturbance = disturbance,<br>',
           '                                        aggreg.factor = aggregation,<br>',
           '                                        asymmetric.transport.from = asymmetricTransport,<br>',
           '                                        planar = planarGraphsOnly <br>',
           '                                        )<br>',
           '                # Randomly delete fragments:<br>',
           frag.reduce.str,
           
           '                # compute edge weights:<br>',
           '                g <- frag.edges.weighting(g, \'layer\')<br><br>',
           '                # Measure values:<br>',
           get.param.str,
           OM.cohesion.R.str,
           OM.cohesion1Out.R.str,
           OM.cohesion2Out.R.str,
           OM.admixtureOut.R.str,
           OM.relationCountOut.R.str,
           OM.objectCountOut.R.str,
           OM.objectsBalanceOut.R.str,
           OM.disturbanceOut.R.str,
           OM.fragBalanceOut.R.str,
           OM.aggregFactorOut.R.str,
           OM.finalFragmentCountOut.R.str,
           '            }, silent = FALSE)<br>',
           '            """,<br>',
           '  install = Seq(<br>',
           '    """R --slave -e \'install.packages("BiocManager") ; library("BiocManager") ; BiocManager::install("RBGL")\' """,<br>',
           '    """R --slave -e \'install.packages("remotes", dependencies = TRUE)\' """,<br>',
           '    """R --slave -e \'library(remotes); remotes::install_github("sebastien-plutniak/archeofrag", force=TRUE)\' """<br>',
           '  )<br>',
           ') set (<br>',
           '  inputs += mySeed.mapped,<br>',
           '  inputs += objectsNumber.mapped,<br>',
           '  inputs += fragmentsNumber.mapped,<br>',
           '  inputs += finalFragmentsNumberMin.mapped,<br>',
           OM.finalFragmentsNumberMax.map.str,
           '  inputs += preserveObjectsNumber.mapped,<br>',
           '  inputs += preserveFragmentsBalance.mapped,<br>',
           '  inputs += preserveInterUnitsConnection.mapped,<br>',
           '  inputs += fragmentsBalance.mapped,<br>',
           '  inputs += objectsBalance.mapped,<br>',
           '  inputs += disturbance.mapped,<br>',
           '  inputs += layerNumber.mapped,<br>',
           '  inputs += aggregation.mapped,<br>',
           '  inputs += planarGraphsOnly.mapped,<br>',
           '  inputs += asymmetricTransport.mapped,<br>',
           OM.cohesion1Out.map.str,
           OM.cohesion2Out.map.str,
           OM.admixtureOut.map.str,
           OM.relationCountOut.map.str,
           OM.objectCountOut.map.str,
           OM.objectsBalanceOut.map.str,
           OM.disturbanceOut.map.str,
           OM.fragBalanceOut.map.str,
           OM.aggregFactorOut.map.str,
           OM.finalFragmentCountOut.map.str,
           # OM.weightsumOut.map.str,
           '  // Default values, taken from the studied graph:<br>',
           '  mySeed := 1,<br>',
           '  layerNumber := 1,<br>',
           '  objectsNumber := ', input.graph.params()$n.components, ',<br>',
           '  fragmentsNumber := ', input.graph.params()$vertices, ',<br>', 
           OM.finalFragmentsNumberMinOut.str, 
           OM.finalFragmentsNumberMaxOut.str, 
           '  preserveObjectsNumber := ', preserveObjectsNumber.str[1], ',<br>',
           '  preserveFragmentsBalance := ', preserveFragmentsBalance.str[1], ',<br>',
           '  preserveInterUnitsConnection := ', preserveInterUnitsConnection.str[1], ',<br>',
           '  aggregation := ', aggregFactor.default, ',<br>', 
           '  objectsBalance := ', objectsBalance.default, ',<br>', 
           '  fragmentsBalance := ', fragmentsBalance.default, ',<br>', 
           '  disturbance := ', disturbance.default, ',<br>', 
           '  planarGraphsOnly := ', planarGraphOnly.str[1], ',<br>',
           '  asymmetricTransport := ', asymmetric.str[1], '<br>',
           ')<br>',
           '<br>',
           '<br>',
           '// model settings<br>',
           'HDOSEEvolution(<br>',
           '  evaluation = archeofrag,<br>',
           '  parallelism = ', input$OM.parallelize, ',                  //  nr of workers for parallelization. Adjust as needed<br>',
           '  termination = ', input$OM.replications, ',                  //  nr of executions. Adjust as needed<br>',
           '  origin = Seq(<br>',   # .... origin ----
           OM.layerNumber.str,
           OM.objectsNumber.str,
           OM.fragmentsNumber.str,
           OM.fragmentsBalance.str,
           OM.objectsBalance.str,
           OM.disturbance.str,
           OM.aggregFactor.str,
           OM.asymmetric.str,
           OM.preserveObjectsNumber.str,
           OM.preserveFragmentsBalance.str,
           OM.preserveInterUnitsConnection.str,
           OM.planarGraphsOnly.str,
           '  ),<br>',
           '  objective = Seq(<br>', # .... objective ----
           OM.cohesion1Out.obj.str,
           OM.cohesion2Out.obj.str,
           OM.admixtureOut.obj.str,
           OM.relationCountOut.obj.str,
           OM.objectCountOut.obj.str,
           OM.disturbanceOut.obj.str,
           OM.aggregFactorOut.obj.str,
           OM.objectsBalanceOut.obj.str,
           OM.fragBalanceOut.obj.str,
           # OM.weightsumOut.str,
           '  ),<br>',
           '  stochastic = Stochastic(seed = mySeed)<br>',
           ') ', OM.islands.str,
           'hook (workDirectory / "hdose-results", frequency = 100) on local // adjust execution machine',
           "</pre>")
    
    gsub("\\(0 ", "\\(0.0 ", om.code)   # format 0 values as double for openMOLE
  }) # end reactive

  output$openMOLE.code <- reactive({openMOLE.code()})
    
  observeEvent(input$OMcode.copy.button, {
    OMCode.plaintext <- openMOLE.code()
    
    OMCode.plaintext <- gsub("<br>", "\n", OMCode.plaintext)
    OMCode.plaintext <- gsub("</?pre>", "", OMCode.plaintext)
    
    session$sendCustomMessage("txt", OMCode.plaintext)
  })
  
} # end server
