
library(markdown)
library(ggplot2)


shinyUI(navbarPage(title = "My title",
             tabPanel("Guide",
                      fluidRow(
                        column(10, includeMarkdown("Guide.md")))
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
                    textInput("filterrow", label = 'Écrire un test conditionnel (ex. : day == "fri" ou temp > 60)', width = "50%"),
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
                        
                        # Multivarié ----
                        
                        tabPanel("Analyse multi-variable",
                                 tabsetPanel(
                                   tabPanel("RÉGRESSION",
                                            fluidRow(
                                              column(3, wellPanel(
                                                tags$h4("Choisir les variables"),
                                                selectInput(inputId = "regmultdep", 
                                                            label = "Choisir une variable à expliquer (quanti)", 
                                                            choices = "", 
                                                            selected = "", 
                                                            multiple = FALSE, 
                                                            selectize = TRUE),
                                                selectInput(inputId = "regmultindep", 
                                                            label = "Choisir plusieurs variables explicatives (quanti)", 
                                                            choices = "", 
                                                            selected = "", 
                                                            multiple = TRUE, 
                                                            selectize = TRUE),
                        
                                              )),
                                              column(5,
                                                     tags$h4("Matrice de corrélation"),
                                                     div(tableOutput("matcor"), style = "overflow-x: auto;")
                                              ),
                                              column(4, 
                                                     tags$h4("Résumé numérique du modèle"),
                                                     div(tableOutput("coefregmult"), style = "overflow-x: auto;")
                                              )
                                            )
                                   ),
                                   
                                   tabPanel("ANALYSE FACTORIELLE",
                                            fluidRow(
                                              column(3, wellPanel(
                                                tags$h4("Choisir les variables"),
                                                selectInput(inputId = "factovar", 
                                                            label = "Choisir plusieurs variables quantitatives", 
                                                            choices = "", 
                                                            selected = "", 
                                                            multiple = TRUE, 
                                                            selectize = TRUE),
                                                actionButton("buttonpca", "Calculer l'ACP"),
                                                selectInput("xaxis", label = "Axe des abscisses (x)", choices = 1:4, selected = 1, multiple = FALSE, selectize = TRUE),
                                                selectInput("yaxis", label = "Axe des ordonnées (y)", choices = 1:4, selected = 2, multiple = FALSE, selectize = TRUE))
                                              ),
                                              column(9,
                                                     tags$h4("Matrice de corrélation"),
                                                     div(tableOutput("facmatcor"), style = "overflow-x: auto;"))
                                            ),
                                            fluidRow(
                                              column(3, wellPanel(
                                                tags$h4("Récupérer le graphique"),
                                                numericInput(inputId = "widthpca", label = "Width (cm)", value = 20, min = 1, max = 30),
                                                numericInput(inputId = "heightpca", label = "Height (cm)", value = 25, min = 1, max = 30),
                                                downloadButton("downloadpca", "Télécharger"))),
                                              column(4,
                                                     tags$h4("Cercle des corrélations"),
                                                     plotOutput("corcircle")),
                                              column(5,
                                                     tags$h4("Décomposition de l'inertie"),
                                                     plotOutput("compinert")
                                              )
                                            ),
                                            fluidRow(
                                              column(3, wellPanel(checkboxInput("labelindiv", "Etiqueter les individus", value = FALSE),
                                                                  checkboxInput("facsave", "Enregistrer les coordonnées"),
                                                                  conditionalPanel(condition = "input.facsave == true",
                                                                                   textInput(inputId = "facprefix", label = "Préfixe", value = ""),
                                                                                   actionButton(inputId = "addfaccoord", label = "Ajouter les coordonnées factorielles"))
                                              )),
                                              column(9,
                                                     tags$h4("Coordonnées des individus"),
                                                     plotOutput("indivpca"))
                                            ),
                                            fluidRow(
                                              column(3, wellPanel()),
                                              column(4,
                                                     tags$h4("Contribution des variables (somme = 1000)"),
                                                     tableOutput("contribvar")),
                                              column(5,
                                                     tags$h4("Contribution des individus (somme = 1000)"),
                                                     dataTableOutput("contribind")
                                              )
                                            )
                                   ),
                                   tabPanel("CLASSIFICATION",
                                            fluidRow(
                                              column(3, wellPanel(
                                                tags$h4("Choisir les variables"),
                                                selectInput(inputId = "cahvar", 
                                                            label = "Choisir plusieurs variables quantitatives", 
                                                            choices = "", 
                                                            selected = "", 
                                                            multiple = TRUE, 
                                                            selectize = TRUE),
                                                checkboxInput("cahstandardize", label = "Standardiser les variables", value = FALSE),
                                                selectInput("cahmethod", label = "Choisir un critère d'aggrégation", choices = c("Minimum" = "single",
                                                                                                                                 "Maximum" = "complete",
                                                                                                                                 "Moyenne" = "average",
                                                                                                                                 "Ward" = "ward"), 
                                                            selected = "average", multiple = FALSE, selectize = TRUE),
                                                actionButton(inputId = "buttoncah", label = "Calculer la CAH"),
                                                tags$hr(),
                                                sliderInput("cahnclass", label = "Choisir le nombre de classes", min = 2, max = 12, step = 1, value = 4),
                                                checkboxInput("cahsave", "Enregistrer les classes"),
                                                conditionalPanel(condition = "input.cahsave == true",
                                                                 textInput(inputId = "cahprefix", label = "Préfixe", value = ""),
                                                                 actionButton(inputId = "addcahclass", label = "Ajouter les classes"))
                                              )),
                                              column(5,
                                                     tags$h4("Dendrogramme"),
                                                     plotOutput("cahdendro")),
                                              column(4, 
                                                     tags$h4("Niveaux"),
                                                     plotOutput("cahheight"))),
                                            fluidRow(
                                              column(3, wellPanel(
                                                tags$h4("Récupérer le graphique"),
                                                numericInput(inputId = "widthclus", label = "Width (cm)", value = 20, min = 1, max = 30),
                                                numericInput(inputId = "heightclus", label = "Height (cm)", value = 30, min = 1, max = 30),
                                                downloadButton("downloadclus", "Télécharger")
                                              )),
                                              column(9, 
                                                     tags$h4("Profil des observations"),
                                                     plotOutput("cahprofile")))
                                   )
                                 )
                        ),
             ),
             inverse = T
  )
  )
  
  