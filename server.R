shinyServer(function(input, output, session) {
  
  models <- reactiveValues()  # this is a collection of the models
  
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  # load the previously trained models - Note: you can delete files in the SavedModels directory
  for (rdsfile in list.files(path = "SavedModels", pattern = "\\.rds")) {
    name <- gsub(rdsfile, pattern = "\\.rds$", replacement = "")
    rdsfile <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, rdsfile)
    showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
    models[[name]] <- readRDS(file = rdsfile)
  }

  ############################################################################## 
  getData <- reactive({
    read.csv(file = "Ass3Data.csv", row.names = "ID")
  })
  
  ############################################################################## 
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Y"]
    n <- 25
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "random",
                 index = caret::createResample(y = y, times = n), savePredictions = "final")
  })
  
  ############################################################################## 
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  ############################################################################## 
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  ############################################################################## 
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  ############################################################################## 
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  ############################################################################## 
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  ############################################################################## 
  getSplit <- reactive({
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  ############################################################################## 
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  ############################################################################## 
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE)
  })
  
  ############################################################################## 
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  ############################################################################## 
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  ############################################################################## 
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  

  ############################################################ NULL ########################################################
  
  
  
  
  ##############################################################################  
  getNullRecipe <- reactive({
    recipe <- recipes::recipe(Y ~ ., data = getTrainData())
  })
  
  ##############################################################################  
  observeEvent(
    input$NullGo,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 0)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ##############################################################################  
  output$NullMetrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$NullRecipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  
############################################################ GLMNET ########################################################
  
  
  
  
  ##############################################################################  
  getGlmnetRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$GlmnetPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$GlmnetGo,
    {
      library(glmnet)
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ############################################################################## 
  output$GlmnetModelSummary0 <- renderText({
    description("glmnet")
  })

  ##############################################################################  
  output$GlmnetMetrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$GlmnetModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })
  
  ############################################################################## 
  output$GlmnetRecipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  ############################################################################## 
  output$GlmnetModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })


  
  
############################################################ PLS ########################################################
  
  
    
  
  ##############################################################################  
  getPlsRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$PlsPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$PlsGo,
    {
      library(pls)
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$PlsModelSummary0 <- renderText({
    description("pls")
  })

  ##############################################################################  
  output$PlsMetrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$PlsModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  ############################################################################## 
  output$RpartRecipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  ############################################################################## 
  output$PlsModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  

  
############################################################ RPART ########################################################
  
  
    
  
  ##############################################################################  
  getRpartRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RpartPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$RpartGo,
    {
      library(rpart)
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "rpart")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ############################################################################## 
  output$RpartModelSummary0 <- renderText({
    description("rpart")
  })
  
  ##############################################################################  
  output$RpartMetrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$RpartRecipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  ############################################################################## 
  output$RpartModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  ############################################################################## 
  output$RpartModelTree <- renderPlot({
    library(rpart.plot)
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel)
  })     
  

  
  
  
 
   
  
  ############################################################ Bayesian Regularized Neural Networks ########################################################
  
  
  
  
  ##############################################################################  
  getBRNNRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$BRNNPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$BRNNGo,
    {
      library(brnn)
      method <- "brnn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getBRNNRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$BRNNModelSummary0 <- renderText({
    description("brnn")
  })
  
  ##############################################################################  
  output$BRNNMetrics <- renderTable({
    req(models$brnn)
    models$brnn$results[ which.min(models$brnn$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$BRNNModelPlots <- renderPlot({
    req(models$brnn)
    plot(models$brnn)
  })     
  
  ############################################################################## 
  output$BRNNRecipe <- renderPrint({
    req(models$brnn)
    models$brnn$recipe
  })  
  
  ############################################################################## 
  output$BRNNModelSummary2 <- renderPrint({
    req(models$brnn)
    summary(models$brnn$finalModel)
  })
  
  
  ############################################################ Support Vector Machines with Polynomial Kernel ########################################################
  
  
  
  
  ##############################################################################  
  getsvmPolyRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$svmPolyPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$svmPolyGo,
    {
      library(kernlab)
      method <- "svmPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getsvmPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$svmPolyModelSummary0 <- renderText({
    description("svmPoly")
  })
  
  ##############################################################################  
  output$svmPolyMetrics <- renderTable({
    req(models$svmPoly)
    models$svmPoly$results[ which.min(models$svmPoly$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$svmPolyPlots <- renderPlot({
    req(models$svmPoly)
    plot(models$svmPoly)
  })     
  
  ############################################################################## 
  output$svmPolyRecipe <- renderPrint({
    req(models$svmPoly)
    models$svmPoly$recipe
  })  
  
  ############################################################################## 
  output$svmPolySummary2 <- renderPrint({
    req(models$svmPoly)
    summary(models$svmPoly$finalModel)
  })
  
  
  ############################################################ Gaussian Process with Polynomial Kernel ########################################################
  
  
  
  
  ##############################################################################  
  getgaussprPolyRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$gaussprPolyPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$gaussprPolyGo,
    {
      library(kernlab)
      method <- "gaussprPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getgaussprPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$gaussprPolyModelSummary0 <- renderText({
    description("gaussprPoly")
  })
  
  ##############################################################################  
  output$gaussprPolyMetrics <- renderTable({
    req(models$gaussprPoly)
    models$gaussprPoly$results[ which.min(models$gaussprPoly$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$gaussprPolyPlots <- renderPlot({
    req(models$gaussprPoly)
    plot(models$gaussprPoly)
  })     
  
  ############################################################################## 
  output$gaussprPolyRecipe <- renderPrint({
    req(models$gaussprPoly)
    models$gaussprPoly$recipe
  })  
  
  ############################################################################## 
  output$gaussprPolySummary2 <- renderPrint({
    req(models$gaussprPoly)
    summary(models$gaussprPoly$finalModel)
  })

  
  ############################################################ Linear Regression with Stepwise Selection ########################################################
  
  
  
  
  ##############################################################################  
  getlmStepAICRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$lmStepAICPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$lmStepAICGo,
    {
      library(MASS)
      method <- "lmStepAIC"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getlmStepAICRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$lmStepAICModelSummary0 <- renderText({
    description("lmStepAIC")
  })
  
  ##############################################################################  
  output$lmStepAICMetrics <- renderTable({
    req(models$lmStepAIC)
    models$lmStepAIC$results[ which.min(models$lmStepAIC$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$lmStepAICPlots <- renderPlot({
    req(models$lmStepAIC)
    plot(models$lmStepAIC)
  })     
  
  ############################################################################## 
  output$lmStepAICRecipe <- renderPrint({
    req(models$lmStepAIC)
    models$lmStepAIC$recipe
  })
  
  ############################################################################## 
  output$lmStepAICSummary2 <- renderPrint({
    req(models$lmStepAIC)
    summary(models$lmStepAIC$finalModel)
  })  
  
 
  ############################################################ Cubist ########################################################
  
  
  
  
  ##############################################################################  
  getCubistRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$CubistPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$CubistGo,
    {
      library(Cubist)
      method <- "cubist"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getCubistRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$CubistModelSummary0 <- renderText({
    description("cubist")
  })
  
  ##############################################################################  
  output$CubistMetrics <- renderTable({
    req(models$cubist)
    models$cubist$results[ which.min(models$cubist$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$CubistModelPlots <- renderPlot({
    req(models$cubist)
    plot(models$cubist)
  })
  
  ############################################################################## 
  output$CubistRecipe <- renderPrint({
    req(models$cubist)
    models$cubist$recipe
  })  
  
  ############################################################################## 
  output$CubistModelSummary2 <- renderPrint({
    req(models$cubist)
    print(models$cubist)
  })
  

  ############################################################ Bagged MARS ########################################################
  
  
  
  
  ##############################################################################  
  getBaggedMARSRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$BaggedMARSPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$BaggedMARSGo,
    {
      library(earth)
      method <- "bagEarth"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getBaggedMARSRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$BaggedMARSModelSummary0 <- renderText({
    description("bagEarth")
  })
  
  ##############################################################################  
  output$BaggedMARSMetrics <- renderTable({
    req(models$bagEarth)
    models$bagEarth$results[ which.min(models$bagEarth$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$BaggedMARSModelPlots <- renderPlot({
    req(models$bagEarth)
    plot(models$bagEarth)
  })
  
  ############################################################################## 
  output$BaggedMARSRecipe <- renderPrint({
    req(models$bagEarth)
    models$bagEarth$recipe
  })  
  
  ############################################################################## 
  output$BaggedMARSModelSummary2 <- renderPrint({
    req(models$bagEarth)
    
    print(models$bagEarth)
  })
 
  
  
  
  ############################################################ Model Tree ########################################################
  
  
  
  
  ##############################################################################  
  getModelTreeRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$ModelTreePreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$ModelTreeGo,
    {
      library(RWeka)
      method <- "M5"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getModelTreeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$ModelTreeModelSummary0 <- renderText({
    description("M5")
  })
  
  ##############################################################################  
  output$ModelTreeMetrics <- renderTable({
    req(models$M5)
    models$M5$results[ which.min(models$M5$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$ModelTreeModelPlots <- renderPlot({
    req(models$M5)
    plot(models$M5)
  })
  
  ############################################################################## 
  output$ModelTreeRecipe <- renderPrint({
    req(models$M5)
    models$M5$recipe
  })  
  
  ############################################################################## 
  output$ModelTreeModelSummary2 <- renderPrint({
    req(models$M5)
    
    print(models$M5)
  })   
  
  
  
  
  ############################################################ Ridge Regression with Variable Selection ########################################################
  
  
  
  
  ##############################################################################  
  getRidgeRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RidgePreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$RidgeGo,
    {
      library(foba)
      method <- "foba"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getRidgeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$RidgeModelSummary0 <- renderText({
    description("foba")
  })
  
  ##############################################################################  
  output$RidgeMetrics <- renderTable({
    req(models$foba)
    models$foba$results[ which.min(models$foba$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$RidgeModelPlots <- renderPlot({
    req(models$foba)
    plot(models$foba)
  })
  
  ############################################################################## 
  output$RidgeRecipe <- renderPrint({
    req(models$foba)
    models$foba$recipe
  })  
  
  ############################################################################## 
  output$RidgeModelSummary2 <- renderPrint({
    req(models$foba)
    
    print(models$foba)
  })    
    
######################################################### maintenance point ####################################################
  
          
  
  
  
  
  
  
  
#####################################################################################################################  
  
  
    
  
  
  
  ############################################################################## 
  getResamples <- reactive({
    results <- caret::resamples(reactiveValuesToList(models))
    NullModel <- "null"
    
    #scale metrics using null model. Tough code to follow -sorry
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models, selected = "brnn" )
    results
  })
  
  
  ############################################################################## 
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  
  
  ############################################################################## 
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  ############################################################################## 
  getTestResults <- reactive({
    test <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = test)
    d <- data.frame(test$Y, predictions)
    colnames(d) <- c("obs", "pred")
    d
  })
  
  ############################################################################## 
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  ############################################################################## 
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  }, height = 600)

    
})
