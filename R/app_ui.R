
css <- '
.tooltip {
  pointer-events: none;
}
.tooltip > .tooltip-inner {
  pointer-events: none;
  background-color: #FFFFFF;
  color: #000000;
  border: 1px solid black;
  padding: 5px;
  font-size: 13px;
  text-align: left;
  max-width: 300px; 
}
.tooltip > .arrow::before {
  border-right-color: #73AD21;
}
'

js <- "
$(function () {
  $('[data-toggle=tooltip]').tooltip()
})
"


paste.js <- "
Shiny.addCustomMessageHandler('txt', function (txt) {
    navigator.clipboard.writeText(txt);
});
"


ui <- shinyUI(fluidPage(  # UI ----
                          theme = shinythemes::shinytheme("cosmo"),  # slate  flatly
                          tags$head(
                            tags$style(HTML(css)),
                            tags$script(HTML(js)),
                            tags$script(HTML(paste.js))
                          ),
                          sidebarLayout( # side bar ----
                            sidebarPanel(
                              h3(div(HTML("<a href=https://github.com/sebastien-plutniak/archeofrag.gui title='Go to the archeofrag.gui page' target=_blank>archeofrag.gui</a> v",
                                          as.character(utils::packageVersion("archeofrag.gui"))
                                          ))),
                              div(HTML("using <a href=https://github.com/sebastien-plutniak/archeofrag title='Go to the archeofrag page' target=_blank>archeofrag</a> v",  as.character(utils::packageVersion("archeofrag")) )),
                              h3("Computation"),
                              uiOutput("parallelize.box"),
                              h3("Input data"),
                              uiOutput("dataset.selector"),
                              fileInput('inputEdges', 'Relationships (CSV file):',
                                        width="70%",
                                        accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                              fileInput('inputNodes', 'Fragments (CSV file):',
                                        width="70%",
                                        accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                              radioButtons(inputId = 'sep', label = 'Separator:', 
                                           choices = c("," =',' , ";"=';'
                                                       ,"tab"='\t'), inline=T, selected = ','),
                              
                              h3("Variable selection"),
                              uiOutput("subset.selector"),
                              uiOutput("subset.options"),
                              uiOutput("variable.selector"),
                              uiOutput("layers.selector"),
                              width=2), # end sidebarpanel
                            
                            mainPanel(
                              tabsetPanel(id="tabs", 
                                          tabPanel("Introduction", # INTRODUCTION ----  
                                                   column(11, align="center",
                                                          tags$div(
                                                            HTML("<div style=width:40%;, align=left>
                <h1><i>archeofrag.gui</i></h1>
                <br>
                <img width='100%' src=www/general-idea.png><br><br>
                <p>
                 <i>archeofrag.gui</i> aids in assessing the <b>distinctions</b> between observed <b>archaeological spatial units</b> (e.g. layers) and their <b>formation process</b> by examining the refitting relationships between fragments of objects contained in these units.
                This application complements and draws upon the <i><a href=https://cran.r-project.org/web/packages/archeofrag/index.html target=_blank>archeofrag</a></i> R package, which implements the TSAR method (Topological Study of Archaeological Refitting). The TSAR method covers not just the quantity of refitting relationships, but also their corresponding distribution and <b>topology</b>. 
                </p>
                <p>
                It relies on strong principles about archeological data:
                  <ul>
                    <li> <b>Physical refits only give evidence of common origin</b>: fragments are considered as parts of the same original object only if they physically refit to each other (relationships based on other factors such as motif style, estimation of chemical composition, etc. are excluded).</li>
                    <li> <b>The spatial location of isolated objects is uncertain</b>: the association between an object and the spatial unit where it was found is a weak relationship, due to the many factors which might have moved it since its deposition. For this reason, the analysis focuses on fragments with at least one refitting relationship, that provides minimal support about their inclusion in a spatial unit. </li>
                  </ul>
                  These principles reduce the quantity of relevant and usable data, but ensure analytical robustness in return.
                </p>
                <h3>Input Data</h3>
                <p>
                  Use the menu on the left to upload your 'relations' and 'fragments' data as CSV files. 
                  <ul>
                    <li>The <b>relationships</b> table must have a row for each refitting relationship and two columns containing the identifiers of each pair of refitting fragments, respectively;</li>
                    <li>The <b>fragments</b> table must have a row for each fragment and, at least, a column for fragments' unique identifiers and a column for the spatial units they belong to. Optionally, columns with morphometric data (e.g. length, surface) and with the fragments' X, Y, Z coordinates  can be used.</li>
                  </ul>
                  Alternatively, load one of the real-world example datasets (or explore Rubish Site data).
                </p>
                <h3>Variable selection</h3>
                  <p>Use the menu on the left to select:
                     <ul>
                      <li>the <b>spatial variable</b>  to consider (i.e. the spatial unit containing the fragments),
                      <li>the <b>pair of spatial units</b> to consider: this selection determines the plot generated in the 'Visualisation' tab and the simulation presets in the 'Simulation' tab.</li>
                      <li>Optionally, a variable can be used to subset the dataset and specify certain values.</li>
                    </ul>
                  </p>
                <h3>Measurements</h3>
                <p>In this tab, statistics are reported for all pairs of spatial units for the selected 'Spatial variable': number of fragments and refitting relationships, etc. The <b>cohesion</b> and <b>admixture</b> values are calculated using the TSAR method. Tables and figures facilitate the exploration of the results. Using admixture values helps  detecting anomaly in the spatial distribution of refitting connection.</p>
                <h3>Spatial units optimisation</h3>
                <p> The spatial units defined in the input dataset might need critical revision. This tab allows
                <ul>
                  <li> 1. for the <b>fast exploration</b> of multiple options to merge those spatial units,</li>
                  <li> 2. <b>modifying</b> the dataset by merging a selection of spatial units (which are then available from <i>archeofrag.gui</i>'s functionalities).</li>
                </ul>  
                  </p>
                <h3>Comparison with simulated data</h3>
                <p>This tab presents functions to investigate the formation process of the selected pair of spatial units. 
                Simulation is used to compare it to similar artificial data while controlling differences in some parameters.
                Here, the chosen pair of spatial units is compared to simulated data generated under two formation hypotheses:
                <ul>
                    <li>H1, the archaeological material studied comes from a <b>single deposition event</b>.</li>
                    <li>H2, the material was deposited during <b>two deposition events</b>.</li>
                </ul>
                 Using the <i>archeofrag</i> R package, other hypotheses can be tested by adjusting the simulator parameters.
                </p>
                <br>
                </div>"))
                                                   ) # end column
                                          ), #end tabPanel
                                          
                                          tabPanel("Measurements", # MEASUREMENTS ----
                                                   fluidRow(
                                                   column(10, align="center",
                                                   uiOutput("dataset.presentation"),
                                                   uiOutput("rubish.text"),
                                                   ) # end column
                                                   ), #end fluidrow
                                                   HTML("<div align=left><h1>Weighting options</h1></div>"), # .. weighting options----
                                                   fluidRow(
                                                     column(2, uiOutput("morpho.selector")),
                                                     column(2, uiOutput("x.selector")),
                                                     column(2, uiOutput("y.selector")),
                                                     column(2, uiOutput("z.selector"))
                                                   ), #end fluidrow
                                                   fluidRow(
                                                   column(10, align="center",
                                                   HTML(
                                                  "<div style=width:40%;, align=left>
                                                   <p>Use the <b>morphometry</b> variable to include any sort of morpho-metrical information about the objects in the computation (e.g. length, surface, volume, weight). Use at least two <b>coordinates</b> to include physical distances between the object found places in the computation (whatever the unit: metre, centimetre, inch, etc.). See   <a href=https://doi.org/10.4324/9781003350026-1 target=_blank>Plutniak <i>et al.</i> 2023</a> for details.</p>
                                                   <p>Note that these weighting options are <b>not</b> supported by the simulation function, which computes cohesion values from the topology of the connection relationships only.</p>
                                                  </div>"
                                                   ), #end HTML
                                                   ) #end columns
                                                   ), # end fluidrow
                                                   fluidRow(
                                                   h1("Statistics by pair of spatial units"),
                                                   column(12, align="center",
                                                   DT::DTOutput("resultsTab",  width="90%"), 
                                                   ), # end column
                                                   column(10, align="left",
                                                   HTML("
                                                        <ul>
                                                          <li><b>Fragments balance</b>: considering only the fragments with connection relationships within their spatial unit, the proportion of fragments in the spatial unit whose label comes first alphanumerically.</li>   
                                                          <li><b>Objects balance</b>: considering only the fragments with connection relationships within their spatial unit, the proportion of objects (i.e. sets of refitted fragments) in the spatial unit whose label comes first alphanumerically.</li>
                                                          <li><b>Cohesion</b>: for a pair of spatial units, the measure of the consistency of each unit, how it is 'self-adherent' to itself (see <a href=https://doi.org/10.1016/j.jas.2021.105501 target=_blank>Plutniak 2021</a>).</li>
                                                          <li><b>Cohesion diff.(erence)</b>: for a pair of spatial units, highest cohesion value - lowest cohesion value. (See the 'Spatial units optimisation' tab for details.) </li>
                                                        </ul>
                                                        "),
                                                   ), # end column
                                                   column(10, align="center",
                                                          downloadButton("download.measurements.tab", "Download table"),
                                                          ) #end column
                                                   ), #end fluirow
                                                  fluidRow(
                                                  h1("Ranking of spatial units by cohesion"), # .. ranking ----
                                                  column(10, align="center",
                                                         HTML("<div  style=width:40%;, align=left> 
                                                              Considering the series of spatial units pairs, the units are ranked based on the number of pairs where they have the higher cohesion value. Higher count suggest relatively better defined and reliable units in the series:
                                                              </div>"),
                                                         br(),
                                                         tableOutput("unit.ranks")
                                                   ), #end column
                                                   ), #end fluirow
                                                   fluidRow(
                                                   h1("Anomaly detection in the spatial unit series"), # .. dissimilarity ----
                                                   column(10, align="center",
                                                   HTML("<div  style=width:40%;, align=left> 
                                                   <h2>Method</h2>
                                                   <p> Let us assume that:
                                                   <ul>
                                                    <li>A spatial unit is expected to be more related to those near it  (i.e. having higher admixture values). In the case of stratigraphic layers, for example, a layer is expected to have more refitting connection with the layers located directly above and below it.</li>
                                                    <li>The alphanumerical labels of the spatial units in the dataset reflect their relative location (e.g. the labels follow the stratigraphic order).</li>
                                                    </ul>
                                                   Then, admixture can be used to study a series of related spatial units and detect anomaly in the spatial distribution of refitting connection.
                                                   </p>
                                                   <p>
                                                   Dissimilarity between two spatial units A and B is calculated as <i>1 - admixture(A, B)</i>. The higher the dissimilarity value, the more likely it is that these two archaeological units correspond to different depositional events.
                                                    Computing a hierarchical clustering on this dissimilarity matrix and constraining the resulting dendrogram's branches to be ordered alphanumerically should reveal anomalies when, despite this ordering constraint, the expected proximity relationships are not observed in the results (see <a href=https://doi.org/10.4324/9781003350026-1 target=_blank>Plutniak <i>et al.</i> 2023</a>).
                                                  </p>
                                                  <h2>Instructions</h2>
                                                  <p>
                                                    <ul>
                                                      <li>Observe the dissimilarity matrix below.</li>
                                                      <li>Optionally, tick the box to <b>normalize</b> the values (by <a href=https://en.wikipedia.org/wiki/Feature_scaling target=_blank>feature scaling</a>).</li>
                                                      <li>Select a <a href=https://en.wikipedia.org/wiki/Hierarchical_clustering#Common_Linkage_Criteria target=_blank>clustering method</a>:
                                                        <ul>
                                                          <li>UPGMA, average linkage (Unweighted Pair Group Method with Arithmetic Mean)</li>
                                                          <li>WPGMA, weighted average linkage</li>
                                                          <li>Single linkage </li>
                                                          <li>Complete linkage </li>
                                                          <li>Ward, using Ward's clustering <a href=https://doi.org/10.1007/s00357-014-9161-z target=_blank>criterion</a>.</li>
                                                        </ul>
                                                      </li>
                                                      <li> The resulting <a href=https://cran.r-project.org/web/packages/dendextend/vignettes/dendextend.html#tanglegram target=_blank>tanglegram</a> includes:
                                                        <ul>
                                                          <li>on the left, the clustering result obtained from <b>observed</b> refitting data,</li> 
                                                          <li>on the right, the clustering result obtained from the <b>expected</b> ordering of the spatial units (assuming no perturbation and an equal number of refits for all spatial units),</li>
                                                          <li> in the middle of the figure, the lines connect the location of the spatial units in the two clustering results, highlighting differences.</li>
                                                      </ul>
                                                          The difference between these results is evaluated using
                                                        <ul>
                                                         <li>
                                                        "),
                                                               span(`data-toggle` = "tooltip", `data-placement` = "left", title = " The cophenetic distance between two observations that have been clustered is defined to be the intergroup dissimilarity at which the two observations are first combined into a single cluster. A dendrogram is an appropriate summary of some data if the correlation between the original distances and the cophenetic distances is high. Click for more information.",
                                                          HTML("<a href=https://en.wikipedia.org/wiki/Cophenetic_correlation target=_blank>Cophenetic correlation</a>")
                                                               ), #end span
                                                          HTML(": a quality measure for the dendrogram about observed refitting data (correlation between the observed dissimilarity and the dendrogram cophenetic distances). Values range between -1 to 1, with near 0 values meaning low quality.</li>
                                                          <li>"),
                                                   span(`data-toggle` = "tooltip", `data-placement` = "left", title = "Entanglement measures how well the labels of two dendrograms are aligned, from 0 (fully aligned labels) to 1 (fully mismatched labels). It is computed by numbering the labels (1 to the total number of labels) of each dendrogram, and then computing the L-norm distance between these two vectors. Click for more information.",
                                                          HTML("<a href=https://cran.r-project.org/web/packages/dendextend/vignettes/dendextend.html#tanglegram target=_blank>Entanglement</a>")
                                                        ),#end span 
                                                          HTML(": Values range between 1 (full entanglement) and 0 (no entanglement).</li>
                                                          <li>"),
                                                               span(`data-toggle` = "tooltip", `data-placement` = "left", title = " Baker's Gamma index is defined as the rank correlation between the stages at which pairs of objects combine in each of the two trees.",
                                                                    HTML("<a href=https://search.r-project.org/CRAN/refmans/dendextend/html/cor_bakers_gamma.html target=_blank>Baker's Gamma</a>")
                                                               ), #end span
                                                               HTML(": a measure of similarity between the expected and the observed dendrograms. Values range between -1 to 1, with near 0 values meaning that the two dendrograms. are not statistically similar. 
                                                          </li>
                                                          </ul>
                                                          </li>
                                                    </ul>
                                                  </p>
                                                  <h2>Results</h2>
                                                  </div> "),
                                                   h3("Dissimilarity matrix"),
                                                   checkboxInput("normalise.diss", "Normalise", value = FALSE),
                                                   tableOutput("dissimilarityTab"),
                                                   downloadButton("download.dissimilarityTab", "Download table"),
                                                   h3("Clustering"),
                                                   DT::DTOutput("clustering.stats"), 
                                                   br(),
                                                   uiOutput("admix.clustering.selector"),
                                                   br(),
                                                   imageOutput("tanglegram.plot",  width= "100%", height = "600px"),
                                                   br(),
                                                   uiOutput("tanglegram.download.button"),
                                                   br(), br()
                                                   ) # end column
                                                   ) #end fluidrow
                                          ), #end tabPanel
                                          tabPanel("Visualisation", # VISUALISATION ----
                                                   fluidRow(
                                                     h1("Fragmentation graph"),
                                                     column(10, align="center",
                                                       br(),
                                                       HTML("<div  style=width:50%;, align=left>"),
                                                       uiOutput("visualisation.title"),
                                                       HTML("</div>"),
                                                       br(),
                                                       uiOutput("frag.graph.viz.download.button"),
                                                       imageOutput("frag.graph.viz.plot", height = "800px", width= "100%")
                                                     ) #end column
                                                   ) # end fluidrow
                                          ), #end tabPanel
                                          tabPanel("Spatial units optimisation", # SPATIAL UNITS OPTIMISATION ----
                                                   fluidRow(
                                                     h1("Explore spatial units merging"),
                                                     column(10, align="center",
                                                            HTML("<div  style=width:40%;, align=left>"),
                                                            HTML("
                                                            <h2>Presentation</h2>
                                                            <p>
                                                                 It happens that archaeological spatial units (e.g.  stratigraphic layers) should be merged for analysis. But which ones? <i>archeofrag.gui</i> helps you determine merging solutions that generate more balanced spatial units series. Here 'balanced' means that there is as much archaeological information about every spatial unit to support their recognition as archaeologically significant.</p>
                                                                 <p> An ideal situation, where equal information is known about a series of distinct spatial units would result in spatial units with cohesion values = 0.5 and admixture = 0.  (Here, 'information' means information about the number of fragments and the distribution of their refitting relationships.)  In such a case, for every pair of units, the <b>difference</b> between the two cohesion values would be 0. Consequently, looking for spatial divisions minimising the difference between pairs of cohesion values informs us about the archaeologically relevant merging of spatial units. 
                                                                 </p>
                                                                 <p>
                                                                 It 1) determines the series of possible spatial units merging, 2) generates the corresponding fragmentation graphs, 3) computes the cohesion value of each unit for all possible pairs of spatial units (as in the 'Measurements' tab), 4) summarises these values by measuring their median and <a href=https://en.wikipedia.org/wiki/Median_absolute_deviation target=_blank>median absolute deviation</a>.</p>
                                                                 <h2>Instructions</h2>
                                                                   <ul>
                                                                      <li>Select the spatial units to consider for possible merging. The maximum number of units is <b>limited to 8</b> (for more than 6 units, and depending on  graph size and the available computational power, the computation might take several minutes). Note that the merging of all selected spatial units are considered, regardless of their relative position (adjacent or not) in the archaeological space.</li>
                                                                      <li>  In the <b>Results</b> section, merged spatial units are indicated by the <b>'+' symbol</b>. Results are decreasingly ordered according to the median value of the differences between cohesion values: the lower the median, the more balanced the archaeological information about the series of spatial units. In addition, the median of the admixture values is also reported: the higher the value, the more mixed the spatial units. Use the dynamic table to explore the combinations and find out which optimal merging solution fits best with archaeological interpretation.</li>
                                                                  <li> In the <b>Merge units</b> section, the dataset can be edited to actually merge the selected spatial units. The resulting spatial units are then available from all <i>archeofrag.gui</i>'s functions. Reducing the number of spatial units is a way around to the 8 spatial units limit. </li>
                                                                   </ul>
                                                                 </p>
                                                                 
 
                                                                 </div>")
                                                     ), # end column,
                                                     column(10, align="left",
                                                            br(), br(), 
                                                            uiOutput("optimisation.sp.ui"),
                                                            HTML("Select up to 8 spatial units and run the computation:"),
                                                            br(), br(),  
                                                            actionButton("optimisationButton", "Run computation"), 
                                                            br(),  
                                                            h1("Results"),
                                                            uiOutput("optimisationText"),
                                                            br(),
                                                            ), #end column
                                                     column(10, align="center",
                                                            DT::DTOutput("optimisationTab",  width="90%"),
                                                     ),
                                                     column(10, align="left",
                                                            HTML("
                                                        <ul>
                                                          <li><b>Sp. unit</b>: spatial units. Merged spatial  units are associated with a '+' symbol.</li>                               
                                                          <li><b>Cohesion difference</b>: for a pair of spatial units, highest cohesion value - lowest cohesion value.</li>
                                                          <li><b>MAD</b>: <a href=https://en.wikipedia.org/wiki/Median_absolute_deviation target=_blank>median absolute deviation</a>.</li>
                                                          <li><b>SD</b>: standard deviation.</li>
                                                        </ul>
                                                        "),
                                                     ) #end column
                                                   ), # end fluidrow
                                                   fluidRow( # merge units -----
                                                     h1(" Merge units"),
                                                     column(12, align = "center",
                                                            actionButton("mergeButton",
                                                                         "Merge selected units"), 
                                                            actionButton("resetMergeButton", "Reset"), 
                                                           tags$style("table.dataTable {width:auto}"),
                                                           DT::DTOutput("optimisation.sp.merge.ui", width="80%"),
                                                           br(), br()
                                                   ) # end column
                                                   )#end fluidrow
                                          ), #end tabPanel
                                          tabPanel("Simulations", # SIMULATIONS ---- 
                                                   tabsetPanel(id="simul", 
                                                   tabPanel("Introduction", # Introduction ----  
                                                   fluidRow(
                                                  h1("Introduction to the simulation of site and assemblage formation"),
                                                   column(10,  align = "center",
                                                            HTML("
                                                            <div align=center>
                                                            <img width='40%' src=www/timeline-simulation.png><br>
                                                            </div>
                                                            <br>
                                                            <div  style=width:40%;,  align=left>
                                                            <p>
                                                            <h2>Time aspects</h2>
                                                            Any archaeological investigation regards three temporal components:
                                                              <ul>
                                                                <li><b>Deposition event</b>: the <i>point</i> in time when unaltered material objects were abandoned.</li>
                                                                <li><b>Alteration phase</b>: the <i>period</i> of time during which those material objects were fragmented and moved.</li>
                                                                <li><b>Excavation event</b>: the <i>point</i> in time when those altered material objects are observed in space.</li>
                                                              </ul>
                                                              Note that components in this model are theoretical and an analytical simplification (e.g. defining  <i>deposition</i> as an event and not a phase). 
                                                              </p>
                                                              <h2>Material objects</h2>
                                                              <p>
                                                              In the archaeological study process, archaeologists use excavation data from a location to reconstruct and learn about past state(s) of this location.  In practice:
                                                              <ol>
                                                                <li> When excavating (at <sub>t0</sub>), <b>sets of fragmented objects</b> associated with spatial units are observed.</li>
                                                                <li> Studying <b>refitting relationships</b> between fragments at the lab, a theoretical state of the assemblage is reconstructed, corresponding to a moment in the <b>alteration phase</b> (t<sub>-1</sub>).</li>
                                                                <li>Because fragments are often missing,  <b>assumptions</b>  are made about the assemblage's possible completion state at an earlier moment, the <b>deposition event</b> (t<sub>-2</sub>). </b></li>
                                                              </ol>
                                                            </p>
                                                            <h2>Simulations</h2>
                                                            <p>
                                                              Simulation can be used to study the formation process between two points in time:
                                                              <ul>
                                                                <li>From the <b>reconstructed point in the alteration process</b> to the <b>excavation event (t<sub>-1</sub> to t<sub>0</sub>)</b>: this simulation covers only a part of the formation process but is only grounded  on archaeological observation and does not require any additional assumption.</li>
                                                                <li>From the <b>deposition event</b> to the <b>excavation event (t<sub>-2</sub> to t<sub>-1</sub>)</b>: this simulation covers the entire formation process but requires assumptions about the initial state of the assemblage at the deposition event.</li>
                                                              </ul>
                                                            </p>
                                                            <h2>Testing formation scenarios</h2>
                    <p>
                    Hypotheses about two aspects of formation processes are of particular interest and can be studied by generating series of fragmentation graphs to compare: the <b>number of deposition events</b> and the <b>direction of fragments transport</b> between the first and second spatial units considered. 
                    <p>
                    Combining these parameters allows to explore and test 6 formation scenarios (A to F):
                   <table><thead>
                    <tr>
                      <th>Deposition events &emsp;&emsp;</th>
                      <th colspan=3> Transport direction</th>
                    </tr></thead>
                    <tbody>
                      <tr>
                        <td></td>
                        <td> 1 &#8660; 2</td>
                        <td> 1 &#8658; 2</td>
                        <td> 1 &#8656; 2</td>
                      </tr>
                      <tr>
                        <td>One (H1)</td>
                        <td>A</td>
                        <td>C</td>
                        <td>E</td>
                      </tr>
                      <tr>
                        <td>Two (H2)</td>
                        <td>B</td>
                        <td>D</td>
                        <td>F</td>
                      </tr>
                    </tbody>
                    </table>
                    </p>
                    </p>
                    <h3>1. Number of deposition events</h3>
                    <p>
                    Fragmentation graphs can be generated for two hypotheses regarding the number of deposition events involved in the formation of the considered pair of spatial units:
                    <ol type='1'>
                     <li> The objects were buried during  <b>one deposition event</b> forming a single spatial unit, were subsequently fragmented and moved, and were discovered in two spatial units according to the archaeologists;</li>
                     <li> The objects were buried during <b>two deposition events</b> forming two spatial units, were subsequently fragmented and moved, and were discovered in two spatial units according to the archaeologists.</li>
                    </ol>
                    </p>
                    <h3>2. Direction of fragments transport</h3>
                    <p> The <i>Unidirectional transport from unit...</i> parameter makes it possible to constrain, or let free, the direction of fragments transport between the two spatial units under study. 
                    </p>
                    <hr>
                    <p>
                      For more details about the formation model implemented in the <i>archeofrag</i> simulator see <a href=https://doi.org/10.1016/j.jas.2021.105501 target=_blank>Plutniak 2021</a>, Fig. 7 in particular.
                    </p>
                    
                                                            <br><br>
                                                            </div>")
                                                            ) #end column
                                                   ) #end fluirow
                                                   ), # end tabpanel
                                                   tabPanel("Alteration -> Excavation", # Alteration > Excavation----  
                                                            fluidRow(
                                                              h1("From (a moment in) the Alteration phase to the Excavation event"),
                                                              column(10,  align = "center",
                                                                     HTML(
                   "<div style=width:40%;, align=left>
                    <h2>Presentation</h2>
                    <p>
                      This tool enables simulating the formation process of two spatial units. Its advantages includes:
                      <ul>
                        <li>Robustness: the parameters are based on observed evidence, no assumptions are required.</li>
                        <li>Fast computation: it can be run on a personal computer.</li>
                      </ul>
                      However, it simulates what might have happen during the period of time spaning from a moment (undetermined) in the 'alteration phase' to the excavation event. (In other words, it does not cover not the assemblage's entire timespan from the deposition event to the excavation event.)
                    </p>
                    <h2>Instructions</h2>
                    <p>
                      <ul>
                      <li>Select the pair of spatial units to compare in the sidebar menu.</li>
                      <li>The parameters of the simulation are automatically filled with the values measured on the graph corresponding to the two spatial units chosen (number of objects, fragments balance, <a href=https://en.wikipedia.org/wiki/Planar_graph target=_blank>planarity</a>, etc.). However, those parameters can be edited to test other hypotheses. The final number of refitting relationships is not constrained.</li>
                      <li>Optionally, set an amount of 'Information loss' to simulate the non-observation of connection relationships or fragments, respectively.</li>
                      <li> Set the number of simulated graphs to generate for each hypothesis, and click on the 'Run' button.</li>
                      </ul>
                    </p>
                    <h2>Results</h2>
                    <p>
                    The table below summarises the results for some parameters, indicating:
                   <ul>
                     <li>whether the simulated values for H1 and H2 are significantly different (<a href=https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test target=_blank>Wilcoxon test</a>, 'H1 != H2?' and 'p.value' columns), and 
                     <li>whether the observed value is lower / within / higher than  the interquartile range of values simulated for H1 and H2, respectively ('Obs. value/H1' and 'Obs. value/H2' columns).</li>
                     </ul>
                      Charts are generated to compare parameter values measured on the empirical fragmentation graph and on the artificial graphs: 
                      <ul>
                      <li>The value observed on the empirical graph is represented by a vertical bar, </li>
                      <li>The distributions of values for each hypothesis are represented by dark (H1) and light (H2) grey shades, respectively (except for cohesion).</li>
                      </ul>
                      </p>
                    </p>
                 </div>") 
                                                   ) # end column
                                                   ), # end fluidrow
                                                  fluidRow(column(5, 
                                                                  h1("Model parameters set up"),
                                                                  h2("Initial state"))
                                                           ),
                                                  fluidRow( # .. parameters ----
                                                     column(2, 
                                                        span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                              title = "Initial number of objects to create.",
                                                            uiOutput("n.components")
                                                        ) #end span
                                                     ), #end column
                                                     column(3, 
                                                         span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                               title = "Estimated initial proportion of objects in the first (alphanumerically) spatial unit.",
                                                            uiOutput("components.balance")
                                                         ) #end span
                                                     ) #end column
                                                  ), #end fluidrow
                                                  fluidRow(column(5, h2("Formation process"))),
                                                  fluidRow(
                                                    column(2, 
                                                      span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                        title = "Whether generating or not only planar graphs. Activating this option makes the computation slower.",
                                                           uiOutput("planar")
                                                      ) #end span
                                                    ), #end column
                                                    column(3, 
                                                      span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                                title = "Estimated proportion of fragments to generate in the first (alphanumerically) spatial unit, regardless of disturbance.",
                                                           uiOutput("balance")
                                                      ) #end span
                                                    ), #end column
                                                    column(3, 
                                                      span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                                title = "Determine the probability of selecting largest sets of fragments (i.e. objects) when simulating the fragmentation process. Higher the value, more unequal the distribution of fragments between the objects.",
                                                           uiOutput("aggreg.factor")
                                                      ) #end span
                                                    ), #end column
                                                    column(2, 
                                                      span(`data-toggle` = "tooltip", `data-placement` = "top",
                                                            title = "Applying disturbance only to fragments from a specific spatial unit.",
                                                           uiOutput("asymmetric")
                                                      ) #end span
                                                    ) #end column
                                                  ), #end fluidrow
                                                  fluidRow(
                                                    column(5, h2("Final state")),
                                                    column(5, h2("Information loss")),
                                                  ),
                                                  fluidRow(
                                                    column(2,
                                                       span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                            title = "Final number of fragments.",
                                                           uiOutput("n.final.fragments")
                                                       ) #end span
                                                    ), #end column
                                                    column(3,
                                                        span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                        title = "Final proportion of fragments moved from a spatial unit to the other.",
                                                           uiOutput("disturbance")
                                                        ) #end span
                                                    ), #end column
                                                  # ), #end fluidrow
                                                  # fluidRow(
                                                    column(3,
                                                           span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                                title = "Proportion of relationships to remove from the final fragmentation graph, to reproduce any relevant reason not to archaeologically observe them. Isolated fragments are also removed.",
                                                                sliderInput("edge.loss", "Connection loss (%)",
                                                                            min = 0, max = 100, step = 1, 
                                                                            value = 0, width = "100%")
                                                           ) #end span
                                                    ), #end column  
                                                    column(3,
                                                           span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                                title = "Proportion of fragments to remove from the final fragmentation graph, to reproduce any relevant reason  not to archaeologically observe them.",
                                                                sliderInput("vertice.loss", "Fragments loss (%)",
                                                                            min = 0, max= 100, step = 1, 
                                                                            value = 0, width = "100%")
                                                           ) #end span
                                                    ), #end column
                                                  ), #end fluidrow
                                                  fluidRow(
                                                      h1("Computation set up"),
                                                      column(1, 
                                                             span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                                  title = "[30;1000].",
                                                             numericInput("replications", "Replications",
                                                                          60, min=30, max=1000, width = "100%"),
                                                            ) #end span
                                                      ), #end column
                                                      column(1,
                                                        span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                           title = "If non null, seed for pseudorandom number generation. Executions with the same seed return the same results.",
                                                           numericInput("seed", "Seed", value=NULL, min=1, max=50, width = "100%"),
                                                        ), #end span
                                                      ), #end column
                                                      # column(2, 
                                                      #        uiOutput("parallelize.box"),
                                                      #        style="padding:27px;"),
                                                      column(1, actionButton("goButton", "Run"), style="padding:27px;")
                                                  ), #end fluidrow
                                                  fluidRow( # .. plots----
                                                      h1("Results"),
                                                      uiOutput("simul.graph.nr"),
                                                      textOutput("n.objects"),
                                                        column(10, align="center",
                                                          tableOutput("summary.tab"))
                                                  ), # end fluidrow 
                                                  fluidRow(
                                                    h2("Cohesion by spatial unit"),
                                                    column(10, align="center",
                                                           HTML("<div style=width:40%;, align=left><p>
                                                                Comparing cohesion values is the main purpose of the TSAR method. Because cohesion is a complex measurement combining multiple aspects, it is likely to reveal differences between compared hypotheses useful for archaeological interpretation. For each hypothesis (top and bottom part of the chart), compare the cohesion values observed for each spatial unit on the empirical graph (purple and yellow vertical bars) and the simulated values (purple and yellow density curves and boxplots).
                                                                </p></div>")
                                                           )),
                                                  fluidRow(column(10,
                                                                  imageOutput("test.simul.cohesion.plot", height = "400px", width= "100%")),
                                                           column(1, uiOutput("cohesion.plot.download.button"),
                                                                  style="padding-top:180px;")
                                                           
                                                  ), # end fluidrow
                                                  fluidRow(
                                                    h2("Admixture"),
                                                    column(10, align="center",
                                                           HTML("<div style=width:40%;, align=left><p>
                                                                The admixture value summarises a pair of cohesion values. Less informative than the cohesion values, it is nevertheless simpler and convenient to examine. In this chart, the grey and light grey density curves corresponds to the admixture values generated for 1 and 2 initial spatial units, respectively.
                                                                </p></div>")
                                                    )),
                                                  fluidRow(column(10,
                                                                  imageOutput("test.simul.admixture.plot", height = "200px", width= "100%")),
                                                           column(1, uiOutput("admixture.plot.download.button"),
                                                                  style="padding-top:80px;")
                                                  ), #end fluidrow
                                                  fluidRow(
                                                    h2("Fragments balance"),
                                                    column(10, align="center",
                                                           HTML("<div style=width:40%;, align=left><p>
                                                                Fragments balance, here, is a descriptive statistic simply defined as the proportion of fragments included in the first spatial unit, whatever their initial spatial unit (see code in 'R code' tab). Note that this definition differs from that of 'estimated' fragments balance value presented in the 'Measurements' tab  and used to set up the simulation. The rationale behind this is to test whether the empirical balance can be obtained in simulated results from a guessed estimated initial balance.
                                                                </p></div>")
                                                    )),
                                                  fluidRow(column(10,
                                                          imageOutput("test.simul.balance.plot", height = "200px", width= "100%")),
                                                          column(1, uiOutput("balance.plot.download.button"),
                                                                 style="padding-top:80px;")
                                                  ), #end fluidrow
                                                  fluidRow(
                                                    h2("Relationships count"),
                                                    column(10, align="center",
                                                           HTML("<div style=width:40%;, align=left><p>
                                                                The number of connection relationships (edges of the graph) is not constrained in this use of the simulator. Consequently, the edge count variability can be used to compare the empirical and simulated fragmentation graphs. Note that this variable is not essential in the TSAR method.
                                                                </p></div>")
                                                    )),
                                                  fluidRow(column(10,
                                                           imageOutput("test.simul.edges.plot", height = "200px", width= "100%")),
                                                           column(1, uiOutput("edges.plot.download.button"),
                                                                  style="padding-top:80px;")
                                                  ), #end fluidrow
                                                  fluidRow(
                                                    uiOutput("test.simul.frag.block")
                                                  ), #end fluidrow
                                                  fluidRow(
                                                    uiOutput("test.simul.objects.block")
                                                  ), #end fluidrow
                                                  fluidRow(
                                                    h2("Connection strength"),
                                                    column(10, align="center",
                                                           HTML("<div style=width:40%;, align=left><p>
                                                                In the TSAR method, attributing values to the  connection relationships ('edge weighting'), to represent their 'strength', is a crucial step before computing cohesion. In this regard, three statistics are calculated on the edge weights of the simulated graphs to complement the exploration of the simulated results: their median, <a href=https://en.wikipedia.org/wiki/Median_absolute_deviation target=_blank>median absolute deviation</a>, and sum. When comparing results generated about different hypotheses, the distribution of these statistics can help distinguish between graphs mostly made of 'weak' or 'strong' connection relationships.
                                                                </p></div>")
                                                    )),
                                                  fluidRow(column(10,
                                                          imageOutput("test.simul.weights.plot", height = "600px", width= "100%")),
                                                          column(1,  br(), br(), br(),
                                                                 uiOutput("weights.plot.download.button"),
                                                                                style="padding-top:230px;")
                                                   ), #end fluidrow
                                                  ), # end 'execution' tabPanel
                                                  tabPanel("R code", # R code ----
                                                           fluidRow(
                                                                  h1("R code for the Alteration -> Excavation simulation"),
                                                           column(10, align="center",
                                                                  HTML("
                                                                  <div  style=width:40%;,  align=left>
                                                                  The following R code runs the 'Alteration to Excavation' simulation with the current settings and returns a series of values for the two hypotheses about the initial number of spatial units:
                                                          <ul>
                                                            <li>admixture</li>
                                                            <li>cohesion value for the spatial units 1 and 2</li> 
                                                            <li>number of refitting relationships</li>
                                                            <li>fragments balance</li>
                                                            <li>summary statstics about relationship weights (sum, median, and median absolute deviation)</li>
                                                                       </div>"),
                                                                  br(),
                                                                  HTML(paste("<div style=width:80%;, align=left>",
                                                                             "<br><div style=\"font-family:Courier; font-size: small; width:100%;\", align=left>",
                                                                             actionButton("r.code.copy.button", "Copy code to clipboard"),
                                                                             br(),
                                                                             br(),
                                                                             htmlOutput("r.code"),
                                                                             "</div>",
                                                                             "</div>"
                                                                  ) # end paste
                                                                  )  # end HTML
                                                           ) # end column
                                                           ) #end fluidrow
                                                  ), #end 'R code' tabPanel
                                                  tabPanel("Deposition -> Excavation", # Deposition > Excavation----
                                                           fluidRow( 
                                                                     h1("From the Deposition event to the Excavation event"),
                                                                     column(10, 
                                                                            HTML("<h2>Introduction to <i>openMOLE</i>'s HD Origin Space Exploration method</h2>")),
                                                                     column(10, align="center",
                                                                     HTML("
                                                                          <div  style=width:40%;, align=left>
                                                                          <h3>The problem</h3>
                                                                          <p>
                                                                            Simulating a formation process from the <i>Deposition event</i> to the <i>Excavation event</i> requires making assumptions about the non-observed part of the archaeological information: i.e., the information not observed due, for example, to the partial excavation of the site,  the transport of material objects to other places, information loss, etc. Estimating missing information raises difficult issues because the range of possibilities is extensive,  leading to <a href=https://en.wikipedia.org/wiki/Combinatorial_explosion target=_blank>combinatorial explosions</a>. How many objects did this site originally included? How many fragments of this vessel are missing and not observed? </p>
                                                                            <h3>Origin Space Exploration</h3>
                                                                            <p>
                                                                            Model exploration methods address those cases. In particular, the <a href=https://openmole.org/HDOSE.html target=_blank>High Dimension Origin Space Exploration</a> method (HDOSE) enables determining the possible combinations of a model's initial parameters, overcoming combinatorial explosions. Conducting an HDOSE analysis requires defining:
                                                                            <ol>
                                                                            <li> <b>Origin values</b>: the ranges of possible initial values for each parameter of the model. </li>
                                                                            <li> <b>Objective values</b>: the values corresponding to an observed state of a model (e.g. the values describing the state of the model at t<sub>0</sub>).</li>
                                                                            </ol>
                                                                            The HDOSE procedure returns the combinations of origin values that best generate the observed state (at t<sub>0</sub>) and, consequently, the most probable initial state(s) at t<sub>-2</sub>. Note that this approach requires to define the virtual total number of fragments and simulate the loss of part of it.
                                                                            </p>
                                                                            <p>
                                                                            The HDOSE method is implemented in the <i><a href=https://openmole.org  target=_blank>openMOLE</a></i> software. 
                                                                            </p>
                                                                            <h3>Instructions</h3>
                                                                            <p>
                                                                            <i>archeofrag.gui</i> does not include the HDOSE method. However, it allows to set-up and generate the (Scala and R) code embedding the <i>archeofrag</i> formation model into the <i>openMOLE</i> framework, making this workflow smoother to use.
                                                                            </p>
                                                                            <ol>
                                                                              <li> Define the range of <b>origin values</b> to explore for each variable. Note that some values are automatically filed using the parameters of the selected pair of spatial units.</li>
                                                                              <li> Select the <b>objective variables</b>: the HDOSE procedure will return the combinations of variables that generate the values of those variables. Optionally, a tolerance percentage can be set for each value to allow approximation.</li>
                                                                              <li>Execute the generated code in <i>openMOLE</i>.</li>
                                                                            </ol>
                                                                          </div>
                                                                          "),
                                                                     ), #end column
                                                           ), #end fluid row
                                                           fluidRow(
                                                                     column(10, # .. exploration var. ----
                                                                     h2("Origin variables: ranges of values to explore"), 
                                                                     h3("Initial state"),
                                                           ), #end  column
                                                  ), #end fluid row
                                                                     fluidRow(
                                                                       column(2, selectInput("OM.layerNumber.val", "Initial number of spatial units", choices = c("1, 2", "1", "2")),
                                                                       h4("Initial objects count:"),
                                                                              ),
                                                                     ),
                                                                     fluidRow(
                                                                       column(1, 
                                                                              span(`data-toggle` = "tooltip", `data-placement` = "top", title = "Minimal value for the range of values to explore about the number of initially non-fragmented objects to generate. By default: 50% of observed objects count.",
                                                                              uiOutput("OM.objectsNumber.min.ui")
                                                                              ) #end span
                                                                              ),
                                                                       column(1, 
                                                                              span(`data-toggle` = "tooltip", `data-placement` = "top", title = "Maximal value for the range of values to explore about the number of initially non-fragmented objects to generate. By default: objects count * 10.",
                                                                              uiOutput("OM.objectsNumber.max.ui")
                                                                              ) #end span
                                                                              ),
                                                                       column(3,
                                                                              span(`data-toggle` = "tooltip", `data-placement` = "top", title = "Range of values to explore regarding the proportion of objects in the first (alphanumerically) spatial unit, regardless of disturbance. By default: observed value +/- 0.1. Only applies if the initial number of spatial units = 2.",
                                                                                   uiOutput("OM.objectsBalance.val.ui")
                                                                                   ) #end span
                                                                             ), #end column
                                                                     ), #end fluidrow
                                                  fluidRow(column(10, h3("Formation process"))),
                                                  fluidRow(column(2, h4("Total fragments count")),
                                                           column(10, h4("Fragments deletion"))
                                                  ), #end fluidrow
                                                                     fluidRow(
                                                                       column(1, 
                                                                              span(`data-toggle` = "tooltip", `data-placement` = "top", title = "Minimal value for the range of values to explore regarding the number of fragments to generate in total (including those not archaeologically observed). By default: observed fragment count.",
                                                                                   uiOutput("OM.fragmentsNumber.min.ui")
                                                                                ) #end span
                                                                               ), #end column
                                                                       column(1, 
                                                                              span(`data-toggle` = "tooltip", `data-placement` = "top", title = "Maximal value for the range of values to explore regarding the number of fragments to generate in total (including those not archaeologically observed). By default: observed fragment count * 100.",
                                                                              uiOutput("OM.fragmentsNumber.max.ui")
                                                                              ) #end span
                                                                              ),      
                                                                       column(2, 
                                                                              span(`data-toggle` = "tooltip", `data-placement` = "top",
                                                                                   title = "Whether or not to try to preserve the fragments balance (i.e. proportion of fragments in each spatial units) when removing fragments to reach the targeted final fragment count. By default: active.",
                                                                                   selectInput("OM.preserveFragmentsBalance.val", "Preserve fragments balance", choices = c("true", "false", "true, false"), selected = "true", width = "100%")
                                                                              ) # end span
                                                                       ), #end column                                                                     
                                                                       column(2,
                                                                       span(`data-toggle` = "tooltip", `data-placement` = "top",
                                                                            title = "Whether or not to try to preserve the object count (i.e. sets of connected fragments) when removing fragments to reach the targeted final fragment count.",
                                                                            selectInput("OM.preserveObjectsNumber.val", "Preserve objects number", choices = c("true", "false", "true, false"), selected = "false", width = "100%")
                                                                       ) # end span
                                                                     ), #end column
                                                                      column(2,
                                                                             span(`data-toggle` = "tooltip", `data-placement` = "top",
                                                                                  title = "Whether or not to try to preserve the proportion of connection relationships between spatial units when removing fragments to reach the targeted final fragment count.",
                                                                                  selectInput("OM.preserveInterUnitsConnection.val", "Preserve inter-units connection", choices = c("true", "false", "true, false"), selected = "true", width = "100%")
                                                                             ) # end span
                                                                      ), #end column
                                                                    ), #end fluidrow
                                                                    fluidRow(column(4, h4("Graph topology"))),
                                                                    fluidRow(
                                                                       column(2, 
                                                                              selectInput("OM.planarGraphsOnly.val", "Generate only planar graphs", choices = c("true", "false", "true, false"), selected = "true, false")),
                                                                       column(3, 
                                                                              span(`data-toggle` = "tooltip", `data-placement` = "top", title = "Range of values to explore for aggregation. Higher values increase the likelihood that the biggest sets of fragments are selected when adding fragments or connections during the creation of the graph. By default: 0, assuming similar fragmentation patterns for all objects.",
                                                                              uiOutput("OM.aggregFactor.val.ui")
                                                                              ) #end span
                                                                              ) # end column
                                                                     ), #end fluidrow
                                                                    fluidRow(column(4, h4("Inter-units perturbation"))),
                                                                    fluidRow(
                                                                       column(2, 
                                                                              span(`data-toggle` = "tooltip", `data-placement` = "top", title = "Parameters to explore for the direction of transport between spatial units.",
                                                                              uiOutput("OM.asymmetric.selection")
                                                                              ) #end span
                                                                       ), # end column
                                                                       column(3, 
                                                                              span(`data-toggle` = "tooltip", `data-placement` = "top", title = "Range of values to explore regarding the proportion of fragments in the first (alphanumerically) spatial unit, regardless of disturbance. By default: observed value +/- 0.1.",
                                                                              uiOutput("OM.fragmentsBalance.val.ui")
                                                                              ) #end span
                                                                              ), #end column
                                                                      column(3,  
                                                                             span(`data-toggle` = "tooltip", `data-placement` = "top", title = "Range of values to explore for disturbance, i.e. the final proportion of fragments moved from a spatial unit to another. Highest admixture are generated for disturbance=0.5. By default: observed value +/- 0.1.",
                                                                             uiOutput("OM.disturbance.val.ui")
                                                                              ) #end span
                                                                            ), #en column
                                                                     ), #end fluidrow
                                                  fluidRow(column(10, h3("Final state"))),
                                                  fluidRow(column(3, uiOutput("OM.FinalfragmentsCount.sens.ui"))), 
                                                  fluidRow(column(10, 
                                                                  h2("Objective variables: archaeologically observed values")), # .. objective var. ----
                                                           column(10, align="center",
                                                                  HTML("
                                                                     <div  style=width:40%;, align=left>
                                                                    To define a variable as an objective tick its box. By default, the algorithm will search solutions that fit to the exact values. However, to accept surrounding values, use the corresponding slider to define a tolerance percentage.
                                                                     </div>")
                                                           ), #end column
                                                  ), #end fluidrow
                                                  fluidRow(column(10, h3("Relevance of spatial units"))),
                                                  fluidRow(
                                                    column(2, checkboxInput("OM.cohesion1Out", "Cohesion spatial unit 1", value = TRUE), style="padding-top:35px;"),
                                                    column(2, sliderInput("OM.cohesion1Out.sens", "+/- tolerance", value = 0.05, min = 0, max = 0.25, step = 0.01)),
                                                     column(2, checkboxInput("OM.cohesion2Out", "Cohesion spatial unit 2", value = FALSE), style="padding-top:35px;"),
                                                     column(2, sliderInput("OM.cohesion2Out.sens", "+/- tolerance", value = 0, min = 0, max = 0.25, step = 0.01)),
                                                     column(1, checkboxInput("OM.admixtureOut", "Admixture", value = TRUE), style="padding-top:35px;"),
                                                     column(2, sliderInput("OM.admixtureOut.sens", "+/- tolerance", value = 0, min = 0, max = 0.25, step = 0.001))
                                                     ),
                                                   fluidRow(column(10, h3("Entities count"))),
                                                   fluidRow(
                                                     column(2, checkboxInput("OM.relationCountOut", "Connection count"), style="padding-top:35px;"),
                                                     column(2, sliderInput("OM.relationCountOut.sens", "+/- tolerance (%)", value = 0, min = 0, max = 50, step = 1)),
                                                     column(1, checkboxInput("OM.objectCountOut", "Object count", value = TRUE), style="padding-top:35px;"),
                                                    column(2, 
                                                           span(`data-toggle` = "tooltip", `data-placement` = "top", title = "Using the final object count as a target value is a good way to get results similar to the archaeological observations. However, because constraining too much the value reduces drastically the accepted results in the HDOSE method, defining a loose constraint more likely generates useful results.",
                                                           sliderInput("OM.objectCountOut.sens", "+/- tolerance (%)", value = 15, min = 0, max = 50, step = 1)),
                                                   )#end span
                                                     ),
                                                  fluidRow(column(10, h3("Alteration processes"))),
                                                   fluidRow(
                                                    column(2, checkboxInput("OM.disturbanceOut", "Disturbance"), style="padding-top:35px;"),
                                                    column(2, sliderInput("OM.disturbanceOut.sens", "+/- tolerance", value = 0, min = 0, max = 0.25, step = 0.01)),
                                                    column(2, checkboxInput("OM.aggregFactorOut", "Fragments aggregation"), style="padding-top:35px;"),
                                                    column(2, sliderInput("OM.aggregFactorOut.sens", "+/- tolerance", value = 0, min = 0, max = 0.25, step = 0.01))
                                                    ), #end fluidrow
                                                  fluidRow(column(10, h3("Distribution of the material in the two spatial units"))),
                                                   fluidRow(
                                                     column(2, checkboxInput("OM.objectsBalanceOut", "Objects balance"), style="padding-top:35px;"),
                                                     column(2, sliderInput("OM.objectsBalanceOut.sens", "+/- tolerance", value = 0, min = 0, max = 0.25, step = 0.01)),
                                                     column(2, checkboxInput("OM.fragBalanceOut", "Fragments balance"), style="padding-top:35px;"),
                                                     column(2, sliderInput("OM.fragBalanceOut.sens", "+/- tolerance", value = 0, min = 0, max = 0.25, step = 0.01))
                                                   ),
                                                  # fluidRow(column(2, checkboxInput("OM.weightsumOut", "Relation weights sum")),
                                                  #                 column(3, sliderInput("OM.weightsumOut.sens", "+/- tolerance (%)", value = 0, min = 0, max = 50, step = 1))),                                                  
                                                  fluidRow(column(10, 
                                                                  h2("Computation set up")), # .. settings ----
                                                           # column(10, align="center",
                                                                  # HTML("
                                                                  #    <div  style=width:40%;, align=left>
                                                                  #   To include a variable in the objective variables  tick its box. By default, the algorithm will look for the exact value. However, to also admit the surrounding  values, use the corresponding slider to set up a tolerance (in percentage).
                                                                  #    </div>
                                                                  #    <br><br>")
                                                           # ), #end column
                                                  ), #end fluidrow
                                                  fluidRow(column(2, numericInput("OM.replications", "Replications", min = 30, value = 30, step = 1, width = "100%")),
                                                           
                                                           column(2, 
                                                                  numericInput("OM.parallelize", "Parallelize on n cores", min = 1,
                                                                               value =  foreach::getDoParWorkers(),
                                                                               step = 1, width = "100%")
                                                                  ), #end column
                                                           column(2, sliderInput("OM.islands", "Group executions by n minutes", min = 0, max=15, value = 0, step = 1, width = "100%"))
                                                  ), # end fluidrow 
                                                  br(), br(),
                                                actionButton("OMcode.copy.button", "Copy code to clipboard"),
                                                HTML(paste("<div style=width:80%;, align=left>",
                                                                      "<br><div style=\"font-family:Courier; font-size: small; width:100%;\", align=left>",
                                                                      htmlOutput("openMOLE.code"),
                                                                      "</div>",
                                                                      "</div>"
                                                           ) # end paste
                                                           )  # end HTML
                                                           
                                                   ), #end 'OM code' tabPanel
                                                   ), # end tabsetPanel
                                          ), # end tabPanel
                                          tabPanel("References", # REFERENCES ----  
                                                   column(10, align="center",
                                                          tags$div(
                                                            HTML("<div style=width:40%;, align=left>
                <h2>About <i>archeofrag</i></h2>
                <p>
                To cite <i>archeofrag</i> or <i>archeofrag.gui</i>, please use <b>Plutniak 2022a</b>. 
                <ul>
                  <li><b>Plutniak, S. 2022a</b>. 'Archeofrag: an R package for Refitting and Spatial Analysis in Archaeology', <i>Journal of Open Source Software</i>, 7 (75), p. 4335. doi: <a href=https://doi.org/10.21105/joss.04335 target=_blank>10.21105/joss.04335</a>.</li>
                  <li><b>Plutniak, S. 2022b</b>. '<a href=https://rzine.gitpages.huma-num.fr/site/ressources/20220811_archeofrag_joss/ target=_blank>Archeofrag: un package R pour les remontages et l'analyse spatiale en archeologie</a>', <i>Rzine</i>.</li>
                </ul>
                The open source programming code of this software is available on the <a target=_blank, href=https://cran.r-project.org/package=archeofrag>CRAN</a> and on <a target=_blank, href=https://github.com/sebastien-plutniak/archeofrag/>github</a>.
                </p>
                <h2>About the TSAR method</h2>
                <p>
                <ul>
                  <li><b>Plutniak, S. 2021</b>. '<a href=https://hal.archives-ouvertes.fr/hal-03419952 target=_blank>The Strength of Parthood Ties. Modelling Spatial Units and Fragmented Objects with the TSAR Method - Topological Study of Archaeological Refitting</a>', <i>Journal of Archaeological Science</i>, 136, p. 105501. doi: <a href=https://doi.org/10.1016/j.jas.2021.105501 target=_blank>10.1016/j.jas.2021.105501</a>.</li>
                  <li><b>Plutniak, S. 2022c</b>. '<a href=http://www.prehistoire.org/offres/doc_inline_src/515/0-BSPF_2022_1_2e_partie_Correspondance_PLUTNIAK.pdf target=_blank>L'analyse topologique des remontages archeologiques : la methode TSAR et le package R archeofrag</a>', <i>BSPF</i>, 119 (1), p. 110-113.</li>
                  <li><b>Plutniak, S., J. Caro, C. Manen 2023</b>. '<a href=https://hal.science/hal-04355706 target=_blank>Four Problems for Archaeological Fragmentation Studies. Discussion and Application to the Tai Cave's Neolithic Pottery Material (France)</a>', in A. Sorman, A. Noterman, M. Fjellstrom (eds.) <i>Broken Bodies, Places and Objects. New Perspectives on Fragmentation in Archaeology</i>, London: Routledge, p. 124-142. doi: <a href=https://doi.org/10.4324/9781003350026-1 target=_blank>10.4324/9781003350026-11</a>.</li>
                </ul>
                </p>
                <h2>Datasets</h2> 
                <ul>
                  <li><b>Bout des Vergnes</b>:  Ihuel, E. (dir.),  M. Baillet, A. Barbeyron, M. Brenet, H. Camus, E. Claud, N. Mercier, A. Michel, F. Sellami. 2020. <i>Le Bout des Vergnes, Bergerac (Dordogne, Nouvelle-Aquitaine), Contournement ouest de Bergerac, RD 709</i>, Excavation report, Perigueux. </li>
                  <li><b>Chauzeys</b>: Chadelle J.-P. (dir.),  M. Baillet, A. Barbeyron, M. Brenet, H. Camus, E. Claud, F. Jude, S. Kreutzer, A. Michel,  N. Mercier, M. Rabanit, S. Save, F. Sellami, A. Vaughan-Williams. 2021. <i>Chauzeys, Saint-Medard-de-Mussidan (Dordogne, Nouvelle-Aquitaine)</i>, Excavation report, Perigueux. </li>
                  <li><b>Cuzoul</b>:  Gardeur M. 2025. 'Bone refits from the Cuzoul de Gramat Mesolithic layers (archaeological site, France)', <i>Zenodo</i>, doi: <a href=https://doi.org/10.5281/zenodo.14975910 target=_blank>10.5281/zenodo.14975910</a>.</li>
                  <li><b>Eaton</b>: Engelbrecht W. 2014. 'Madison Point Refits', <i>tDAR</i>, doi: <a href=https://doi.org/10.6067/xcv8t43v1j target=_blank>10.6067/xcv8t43v1j</a>. See also Plutniak S. 2025. 'Reprocessing script for William Engelbrecht's 'Madison Point Refits' dataset (including generated tables and figures)'. <i>Zenodo</i>. doi: <a href=https://doi.org/10.5281/zenodo.15091301 target=_blank>10.5281/zenodo.15091301</a>.</li>  
                  <li><b>Font-Juvenal</b>: Caro J. 2024. 'Font-Juvenal_Refiting', <i>Zenodo</i>, doi:  <a href=https://doi.org/10.5281/zenodo.14515444 target=_blank>10.5281/zenodo.14515444</a>.</li>  
                  <li><b>Fumane</b>: Falcucci A. 2025. 'Refitting the context: accepted paper b (v0.1.3)', <i>Zenodo</i>, doi: <a href=https://doi.org/10.5281/zenodo.15017627   target=_blank>10.5281/zenodo.15017627</a>.</li> 
                  <li><b>Geelbek</b>: Conard N. J.,  A. W. Kandel,  S. Plutniak. 2025. 'Refitting archaeological objects from the Geelbek Dunes Middle Stone Age site (South Africa)', <i>Zenodo</i>, doi: <a href=https://doi.org/10.5281/zenodo.15803288 target=_blank>10.5281/zenodo.15803288</a>.</li>
                  <li><b>Grande Rivoire 1st Meso</b>: Derbord L., A. Angelin. 2025. 'Mesolithic artefact refitting data from La Grande Rivoire (Sassenage, Isere)', <i>Zenodo</i>, doi: <a href=https://doi.org/10.5281/zenodo.15289796 target=_blank>10.5281/zenodo.15289796</a>.</li>
                  <li><b>Grande Rivoire 2nd Meso</b>: Derbord L., A. Angelin. 2025. 'Mesolithic artefact refitting data from La Grande Rivoire (Sassenage, Isere)', <i>Zenodo</i>, doi: <a href=https://doi.org/10.5281/zenodo.15289796 target=_blank>10.5281/zenodo.15289796</a>.</li>
                  <li><b>Grotte 16</b>: Dancette C., E. Discamps, S. Plutniak. 2025. 'Bone refits from the Grotte XVI Pleistocene Faunal Assemblage (Cénac-et-Saint-Julien, France)', <i>Zenodo</i>, doi: <a href=https://doi.org/10.5281/zenodo.15655628 target=_blank>10.5281/zenodo.15655628</a>.</li>
                  <li><b>Liang Abu</b>: Plutniak S. 2021. 'Refitting Pottery Fragments from the Liang Abu Rockshelter, Borneo', <i>Zenodo</i>, doi: <a href=https://doi.org/10.5281/zenodo.4719577 target=_blank>10.5281/zenodo.4719577</a>.</li>
                  <li><b>St Cesaire 1987</b>: Morin E., S. Plutniak. 2025. 'Middle and Upper Palaeolithic Bone Refitting data from La Roche a Pierrot site (Saint-Cesaire, France), Excavations 1976-1987', <i>Zenodo</i>, doi: <a href=https://doi.org/10.5281/zenodo.15638561 target=_blank>10.5281/zenodo.15638561</a>.</li>
                  <li><b>St Cesaire 2024</b>: Morin E., S. Plutniak. 2025. 'Middle and Upper Palaeolithic Bone Refitting data from La Roche a Pierrot site (Saint-Cesaire, France), Excavations 2013-2024', <i>Zenodo</i>, doi: <a href=https://doi.org/10.5281/zenodo.15638691 target=_blank>10.5281/zenodo.15638691</a>.</li>
                  <li><b>Tai Cave and Tai South</b>:  Caro J., Plutniak S. 2022. 'Refitting and Matching Neolithic Pottery Fragments from the Tai site, France', <i>Zenodo</i>, doi: <a href=https://doi.org/10.5281/zenodo.7408706 target=_blank>10.5281/zenodo.7408706</a>.</li>
                </ul>
                <br>
                
                                                                 </div>"))
                                                   ) # end column
                                          ), #end tabPanel                                          
                                          ), # end  tabsetPanel
                              width=10) # end mainPanel
                          ) #sidebarLayout
) #end fluidPage
) #end  shinyUI
