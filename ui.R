shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Deepak Nangamuthu Chanthiramathi"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ),
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             
             tabsetPanel(
               tabPanel("NULL Model",
                        br(),
                        fluidRow(
                          column(width = 4),
                          column(width = 1, 
                                 actionButton(inputId = "NullGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "NullGo", title = "This will train or retrain your model")
                                 )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "NullMetrics"),
                        hr(),
                        verbatimTextOutput(outputId = "NullRecipe")
               ),
               tabPanel("GLMnet Model",
                        verbatimTextOutput(outputId = "GlmnetModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "GlmnetPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "GlmnetPreprocess", 
                                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "GlmnetGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "GlmnetGo", title = "This will train or retrain your model")
                                 )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "GlmnetMetrics"),
                        hr(),
                        plotOutput(outputId = "GlmnetModelPlots"),
                        verbatimTextOutput(outputId = "GlmnetRecipe"),
                        verbatimTextOutput(outputId = "GlmnetModelSummary2")
               ),
               tabPanel("PLS Model",
                        verbatimTextOutput(outputId = "PlsModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "PlsPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "PlsPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "PlsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "PlsMetrics"),
                        hr(),
                        plotOutput(outputId = "PlsModelPlots"),
                        verbatimTextOutput(outputId = "PlsRecipe"),
                        verbatimTextOutput(outputId = "PlsModelSummary2")
               ),
               tabPanel("Rpart Model",
                        verbatimTextOutput(outputId = "RpartModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "RpartPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "RpartPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "RpartGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RpartGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "RpartMetrics"),
                        hr(),
                        plotOutput(outputId = "RpartModelPlots"),
                        plotOutput(outputId = "RpartModelTree"),
                        verbatimTextOutput(outputId = "RpartRecipe")
               ),
               tabPanel("Bay.Reg.Neural Network",
                        verbatimTextOutput(outputId = "BRNNModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "BRNNPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "BRNNPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "BRNNGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "BRNNGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "BRNNMetrics"),
                        hr(),
                        plotOutput(outputId = "BRNNModelPlots"),
                        verbatimTextOutput(outputId = "BRNNRecipe"),
                        verbatimTextOutput(outputId = "BRNNModelSummary2")
               ),
               tabPanel("SVM Poly Model",
                        verbatimTextOutput(outputId = "svmPolyModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "svmPolyPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "svmPolyPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "svmPolyGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "svmPolyGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "svmPolyMetrics"),
                        hr(),
                        plotOutput(outputId = "svmPolyPlots"),
                        verbatimTextOutput(outputId = "svmPolyRecipe"),
                        verbatimTextOutput(outputId = "svmPolySummary2")
               ),
               tabPanel("Gauss Poly Model",
                        verbatimTextOutput(outputId = "gaussprPolyModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "gaussprPolyPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "gaussprPolyPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "gaussprPolyGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gaussprPolyGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "gaussprPolyMetrics"),
                        hr(),
                        plotOutput(outputId = "gaussprPolyPlots"),
                        verbatimTextOutput(outputId = "gaussprPolyRecipe"),
                        verbatimTextOutput(outputId = "gaussprPolySummary2")
               ),
               tabPanel("Linear with Stepwise Model",
                        verbatimTextOutput(outputId = "lmStepAICModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "lmStepAICPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "lmStepAICPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "lmStepAICGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "lmStepAICGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "lmStepAICMetrics"),
                        hr(),
                        plotOutput(outputId = "lmStepAICPlots"),
                        verbatimTextOutput(outputId = "lmStepAICRecipe"),
                        verbatimTextOutput(outputId = "lmStepAICSummary2")
               ),
               tabPanel("Cubist Model",
                        verbatimTextOutput(outputId = "CubistModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "CubistPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "CubistPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "CubistGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "CubistGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "CubistMetrics"),
                        hr(),
                        plotOutput(outputId = "CubistModelPlots"),
                        verbatimTextOutput(outputId = "CubistRecipe"),
                        verbatimTextOutput(outputId = "CubistModelSummary2")
               ),
               tabPanel("Bagged MARS",
                        verbatimTextOutput(outputId = "BaggedMARSModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "BaggedMARSPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "BaggedMARSPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "BaggedMARSGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "BaggedMARSGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "BaggedMARSMetrics"),
                        hr(),
                        plotOutput(outputId = "BaggedMARSModelPlots"),
                        verbatimTextOutput(outputId = "BaggedMARSRecipe"),
                        verbatimTextOutput(outputId = "BaggedMARSModelSummary2")
               ),
               tabPanel("Model Tree",
                        verbatimTextOutput(outputId = "ModelTreeModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "ModelTreePreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "ModelTreePreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "ModelTreeGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "ModelTreeGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "ModelTreeMetrics"),
                        hr(),
                        plotOutput(outputId = "ModelTreeModelPlots"),
                        verbatimTextOutput(outputId = "ModelTreeRecipe"),
                        verbatimTextOutput(outputId = "ModelTreeModelSummary2")
               ),
               tabPanel("Ridge with Variable Selection",
                        verbatimTextOutput(outputId = "RidgeModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "RidgePreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "RidgePreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "RidgeGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RidgeGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "RidgeMetrics"),
                        hr(),
                        plotOutput(outputId = "RidgeModelPlots"),
                        verbatimTextOutput(outputId = "RidgeRecipe"),
                        verbatimTextOutput(outputId = "RidgeModelSummary2")
               )


               
               
######################################################### maintenance point ####################################################
               
             )
             ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             plotOutput(outputId = "TestPlot")
    )
  )
))
