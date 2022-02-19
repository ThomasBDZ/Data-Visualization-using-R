
library(markdown)
library(ggplot2)


shinyUI(navbarPage(title = "My title",
             tabPanel("Guide",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("plotType", "Plot type",
                                       c("Scatter"="p", "Line"="l")
                          )
                        ),
                        mainPanel(
                          plotOutput("plot")
                        )
                      )
             ),
             tabPanel("Données",
                fluidRow(
                  column(3, wellPanel(
                    fluidRow(
                      tags$h4("Charger les données d'exemple"),
                      actionButton(inputId = "loadExData", label = "Chargement"),
                      htmlOutput("description"),
                      tags$hr(),
                      tags$h4("Charger ses propres données"),
                      checkboxInput(inputId = "showsettings", value = FALSE, label = "Options de chargement"),
                      conditionalPanel(
                        condition = "input.showsettings == true",
                        checkboxInput("csvSettings", "Options du format CSV", FALSE),
                        conditionalPanel(
                          condition = "input.csvSettings == true",
                          selectInput("encodtab", "Codage des charactères", choices = c(UTF8 = "UTF-8", Latin1 = "latin1"), selected = "UTF-8", multiple = FALSE, width = "50%"),
                          radioButtons("sepcol", "Separateur de colonnes",
                                       c(Virgule = ",",
                                         Point_virgule = ";",
                                         Tabulation = "\t"),
                                       ","),
                          radioButtons("sepdec", "Separateur décimal",
                                       c(Point = ".",
                                         Virgule = ","),
                                       "."),
                          radioButtons("quote", "Guillemets",
                                       c(None = "",
                                         "Double Quote" = '"',
                                         "Single Quote" = "'"),
                                       '"')),
                        fileInput("fileInput", "Charger le tableau", accept = "text/csv", multiple = FALSE),
                        selectInput("idtab",
                                    "Variable identifiant", 
                                    choices = "",
                                    selected = "", 
                                    multiple = FALSE,
                                    selectize = TRUE, width = "50%"),
                        tags$hr(),
                        # Charger le shape
                        fileInput("shapeInput", "Charger le fond de carte", accept = c("application/zip", "application/x-gzip", ".zip")),
                        selectInput("idshape",
                                    "Variable identifiant", 
                                    choices = "",
                                    selected = "", 
                                    multiple = FALSE,
                                    selectize = TRUE, width = "50%")
                      ),
                      tags$hr(),
                      tags$h4("Récupérer le tableau"),
                      radioButtons("csvtype", "Options du CSV", c("Norme anglo (virgule et point)" = "anglo",
                                                                  "Norme franco (point-virgule et virgule)" = "franco")),
                      downloadButton("downloaddata", "Télécharger")
                    )
                  )),
                  
                  column(9, wellPanel(
                    tags$h4(HTML("Filtrer les données")),
                    textInput("filterrow", label = 'Écrire un test conditionnel (ex. : BUREAU == "75116_1626" ou TXABS > 12)', width = "50%"),
                    fluidRow(column(3,
                                    actionButton("addfilter", label = "Appliquer le filtre")),
                             column(3,
                                    actionButton("delfilter", label = "Supprimer le filtre")))
                  )),
                  column(9,
                         div(dataTableOutput("contentstable"), style = "overflow-x: auto;")
                  )
                )
             ),
             navbarMenu("Analyses",
                        tabPanel("Analyse uni-variable",
                                 
                                 # Univarié ----
                                 
                                 tabPanel("Univarié",
                                          fluidRow(
                                            column(3, wellPanel(
                                              tags$h4("Choisir la variable à explorer"),
                                              selectInput(inputId = "uniquanti", 
                                                          label = "Choisir une variable quanti", 
                                                          choices = "", 
                                                          selected = "", 
                                                          multiple = FALSE, 
                                                          selectize = TRUE),
                                              selectInput(inputId = "uniquali", 
                                                          label = "Choisir une variable quali", 
                                                          choices = "", 
                                                          selected = "", 
                                                          multiple = FALSE, 
                                                          selectize = TRUE),
                                              checkboxInput("uniset", "Personnaliser l'histogramme"),
                                              conditionalPanel(condition = "input.uniset == true",
                                                               sliderInput(inputId = "nbins", 
                                                                           label = "Nombre de classes", 
                                                                           min = 0, 
                                                                           max = 30, 
                                                                           value = 10,
                                                                           step = 1),
                                                               checkboxInput("drawsummary", 
                                                                             label = "Tracer les résumés (Q1, Q2, Q3, Moyenne)", 
                                                                             value = FALSE)
                                              ),
                                              tags$br(),
                                              tags$h4("Récupérer le graphique"),
                                              numericInput(inputId = "widthuni", label = "Width (cm)", value = 20, min = 1, max = 30),
                                              numericInput(inputId = "heightuni", label = "Height (cm)", value = 15, min = 1, max = 30),
                                              downloadButton("downloaduniplot", "Télécharger")
                                            )),
                                            column(5, 
                                                   tags$h4("Résumé graphique"),
                                                   plotOutput("uniplot")),
                                            column(4, 
                                                   tags$h4("Résumé numérique"),
                                                   htmlOutput("unisummary"),
                                                   tableOutput("unitab"))
                                            
                                          )
                                 )
                        ),
                        tabPanel("Analyse bi-variable",
                                 # Bivarié ----
                                 
                                 tabPanel("Bivarié",
                                          tabsetPanel(
                                            tabPanel("CONTINGENCE (quali-quali)",
                                                     fluidRow(
                                                       column(3, wellPanel(
                                                         tags$h4("Choisir les variables"),
                                                         selectInput(inputId = "qualidep", 
                                                                     label = "Choisir la variable à expliquer", 
                                                                     choices = "", 
                                                                     selected = "", 
                                                                     multiple = FALSE, 
                                                                     selectize = TRUE),
                                                         selectInput(inputId = "qualiindep", 
                                                                     label = "Choisir la variable explicative", 
                                                                     choices = "", 
                                                                     selected = "", 
                                                                     multiple = FALSE, 
                                                                     selectize = TRUE),
                                                         radioButtons(inputId = "contcont", label = "Contenu du tableau",
                                                                      c("Effectifs observés" = "obsfreq",
                                                                        "Pourcentages en ligne" = "rowpct",
                                                                        "Effectifs espérés" = "expfreq",
                                                                        "Résidus bruts" = "rawresid",
                                                                        "Résidus standardisés" = "stdresid"),
                                                                      selected = "obsfreq"),
                                                         tags$br(),
                                                         tags$h4("Récupérer le graphique"),
                                                         numericInput(inputId = "widthmosaic", label = "Width (cm)", value = 20, min = 1, max = 30),
                                                         numericInput(inputId = "heightmosaic", label = "Height (cm)", value = 15, min = 1, max = 30),
                                                         downloadButton("downloadmosaicplot", "Télécharger")
                                                       )),
                                                       column(5,
                                                              tags$h4("Résumé graphique"),
                                                              plotOutput("mosaic")
                                                       ),
                                                       column(4, 
                                                              tags$h4("Résumé numérique"),
                                                              tags$h5(HTML("<strong>Mesures locales</strong>")),
                                                              tableOutput("contingtab"),
                                                              tags$h5(HTML("<strong>Mesures globales</strong>")),
                                                              htmlOutput("contingtext")
                                                       )
                                                     )
                                            ),
                                            
                                            
                                            tabPanel("RÉGRESSION (quanti-quanti)",
                                                     fluidRow(
                                                       column(3, wellPanel(
                                                         tags$h4("Choisir les variables"),
                                                         selectInput(inputId = "quantidep", 
                                                                     label = "Choisir la variable à expliquer", 
                                                                     choices = "", 
                                                                     selected = "", 
                                                                     multiple = FALSE, 
                                                                     selectize = TRUE),
                                                         selectInput(inputId = "quantiindep", 
                                                                     label = "Choisir la variable explicative", 
                                                                     choices = "", 
                                                                     selected = "", 
                                                                     multiple = FALSE, 
                                                                     selectize = TRUE),
                                                         checkboxInput("reg1save", "Enregistrer les résidus"),
                                                         conditionalPanel(condition = "input.reg1save == true",
                                                                          textInput(inputId = "reg1prefix", label = "Préfixe", value = ""),
                                                                          actionButton(inputId = "addreg1resid", label = "Ajouter les résidus")),
                                                         tags$br(),
                                                         tags$h4("Récupérer le graphique"),
                                                         numericInput(inputId = "widthreg1", label = "Width (cm)", value = 20, min = 1, max = 30),
                                                         numericInput(inputId = "heightreg1", label = "Height (cm)", value = 15, min = 1, max = 30),
                                                         downloadButton("downloadreg1", "Télécharger")
                                                       )),
                                                       column(5, 
                                                              tags$h4("Résumé graphique"),
                                                              plotOutput("scatterplot")),
                                                       column(4,
                                                              tags$h4("Résumé numérique"),
                                                              tableOutput("coefreg"))
                                                     )
                                            ),
                                            
                                            tabPanel("ANOVA (quali-quanti)",
                                                     fluidRow(
                                                       column(3, wellPanel(
                                                         tags$h4("Choisir les variables"),
                                                         selectInput(inputId = "quanlidep", 
                                                                     label = "Choisir la variable à expliquer (quanti)", 
                                                                     choices = "", 
                                                                     selected = "", 
                                                                     multiple = FALSE, 
                                                                     selectize = TRUE),
                                                         selectInput(inputId = "quanliindep", 
                                                                     label = "Choisir la variable explicative (quali)", 
                                                                     choices = "", 
                                                                     selected = "", 
                                                                     multiple = FALSE, 
                                                                     selectize = TRUE),
                                                         checkboxInput(inputId = "bpjitter", label = "Surimposer les points", value = FALSE),
                                                         checkboxInput("aov1save", "Enregistrer les résidus"),
                                                         conditionalPanel(condition = "input.aov1save == true",
                                                                          textInput(inputId = "aov1prefix", label = "Préfixe", value = ""),
                                                                          actionButton(inputId = "addaov1resid", label = "Ajouter les résidus")),
                                                         tags$br(),
                                                         tags$h4("Récupérer le graphique"),
                                                         numericInput(inputId = "widthanova1", label = "Width (cm)", value = 20, min = 1, max = 30),
                                                         numericInput(inputId = "heightanova1", label = "Height (cm)", value = 15, min = 1, max = 30),
                                                         downloadButton("downloadanova", "Télécharger")
                                                       )),
                                                       column(5, 
                                                              tags$h4("Résumé graphique"),
                                                              tags$h5("Écarts à la moyenne"),
                                                              plotOutput("aovplot"),
                                                              tags$h5("Boîtes à moustaches"),
                                                              plotOutput("boxes")),
                                                       column(4,
                                                              tags$h4("Résumé numérique"),
                                                              tableOutput("coefanova"),
                                                              tableOutput("tabanova")
                                                       )
                                                     )
                                            )
                                          )
                                 )
                        ),
                        tabPanel("Analyse multi-variable",
                                 DT::dataTableOutput("table")
                        )
             ), 
             tabPanel("Cartographie",
                      verbatimTextOutput("summary")
             ),
             inverse = T
  ))
  
  