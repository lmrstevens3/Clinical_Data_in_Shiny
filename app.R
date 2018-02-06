library(shiny)
library(markdown)
library(corrplot)
library(ggplot2)
library(plotly)
library(htmltools)
library(plyr)
library(dplyr)
library(randomForest)
library(ROCR)
library(FSelector)
library(arm)
library(rjson)
library(devtools)
require(rCharts)
library(caret)
library(rattle)
library(rpart.plot)
require(rpart)
library(C50)
library(ipred)
library(DT)


multicol <- " .multicol {
                      
-webkit-column-count: 2; /* Chrome, Safari, Opera */

-moz-column-count: 2; /* Firefox */

column-count: 2;
}"

ui10 <- shinyUI(
  fluidPage(
  tags$head(tags$style(HTML(multicol))),
    
  tags$head(tags$style(HTML(mycss))),
  # Application title
  titlePanel("Survey Data Analysis with Shiny"),
  
  #####   SIDE PANEL    
  ################################################################################################################################################################################################################  
  # Sidebar 
  fluidRow(
  column(2, style = "background-color:#B0C4DE;",
         
    downloadButton("report", "Generate report"),
    br(), 
         
    selectInput("dataset", "Dataset", 
                c("esoph", "upload my own")),
    
    #### Data Plots and Stats
    ########################
    conditionalPanel("input.dataset === 'upload my own'",
                     fileInput("datafile", ""), 
                     textInput("datafile_sep", "Field Seperator", value = ",")),
    selectInput("tab", "Tab:", c("Data Plots and Statistical Tests", "Correlation and Multiple Regression", "Machine Learning", "Data", "ML Data", "Current Tab Plot Data")),
    conditionalPanel("input.tab === 'Data Plots and Statistical Tests'", 
      helpText("Plot 1 (left):"),               
      selectInput("plotType","Plot 1 Type (Left):", 
                  c("bar", "multibar", "histogram", "single boxplot", "boxplot", "grouped scatter",  "scatter")), 
          conditionalPanel("input.plotType === 'histogram'", sliderInput("bins","Number of bins:", min = 1, max = 50, value = 1)),      
      uiOutput("x"),   
      uiOutput("y"),
      uiOutput("group"),
      
      helpText("Plot 2 (Right):"), 
      selectInput("plotType2"," Plot 2 Type (Right):", 
                  c("bar", "multibar", "histogram", "single boxplot", "boxplot", "grouped scatter",  "scatter")),
      
          conditionalPanel("input.plotType2 === 'histogram'", sliderInput("bins2","Number of bins:", min = 1, max = 50, value = 1)),
      uiOutput("x2"),   
      uiOutput("y2"),
      uiOutput("group2"),
      checkboxInput("scb", "Conduct Statistical Test", FALSE),
      conditionalPanel("input.scb === true", 
        helpText("Statistical Testing for Left Plot Variables"),
        selectInput("statTest", "Statistical Tests", c("Shapiro-Wilk",  "Two sample t-test", "Wilcox rank sum - two sample", "Annova", "Kruskal–Wallis")),
        textInput('sig', "Significance Threshold", value = .05),
        helpText("Statistical Testing for Right Plot Variables"),
        selectInput("statTest2", "Statistical Tests", c("Shapiro-Wilk",  "Two sample t-test", "Wilcox rank sum - two sample", "Annova", "Kruskal–Wallis")),
        textInput('sig2', "Significance Threshold", value = .05))
        
      ),
    #### Correlation and MR
    ########################
    conditionalPanel("input.tab === 'Correlation and Multiple Regression'",
       selectInput("corMethod", "Correlation Method", c(eval(formals(cor)$method))),
       selectInput("corUse", "NA Action", c("everything", "complete.obs", "na.or.complete", "pairwise.complete.obs")), 
       selectInput("plotOrder", "Reorder Correlation", c("original", "AOE", "FPC", "hclust", "alphabet")),
       conditionalPanel("input.plotOrder === 'hclust'",
            wellPanel(selectInput("plotHclustMethod", "Method",c("ward", "single", "complete", "average", "mcquitty", "median", "centroid")),
            numericInput("plotHclustAddrect", "Number of Rectangles", 3, 0, NA))),
       checkboxInput("corrSig", "Significance Test", FALSE),
       conditionalPanel("input.corrSig === true", numericInput("sigLevel", "Significane Level", 0.05, 0, 1, 0.01), selectInput("sigAction", "Insignificant Action",
                                                                                                                               eval(formals(corrplot)$insig))),
 
       checkboxInput("multReg", "Build Multiple Regression Model", FALSE),
       conditionalPanel("input.multReg === true", checkboxInput("log", "Logistic", FALSE))
       ),
    #### Machine Learning
    ########################
    conditionalPanel("input.tab === 'Machine Learning'", 
      selectInput("rdataset", "Response Data", 
                 c("dataset", "upload another")),
      conditionalPanel("input.rdataset === 'upload another'",
                      fileInput("rdatafile", ""), 
                      textInput("rdatafile_sep", "Field Seperator", value = ",")),
      uiOutput("r"), 
      checkboxInput("factorResp", "Factor Response Variable", FALSE),
      conditionalPanel("input.factorResp === true", textInput("factorVal", "Factor Cut off Value", value = "0")),
      selectInput("fsMethod", "Feature Selection", c("average value" = "average value", "correlation with response" = "correlation with response", "information gain" = "information gain")), 
      selectInput('method', "Model Method", c("random forest", "bayesian generalized linear","CART", "C4.5 algorithm", "bagged CART", "generalized boosted modeling")), 
      selectInput('metric', "Test Metric for Method", c("Accuracy", "Kappa", "RMSE", "Rsquared")), 
      numericInput('number', "Folds for Cross Validation", value = 3, min = 1, max = 15, step = 1),
      numericInput('repeats', "Repeats", value = 1, min = 1, max = 10, step = 1),
      checkboxInput('addModel2', "Add Second Model", FALSE),
      conditionalPanel("input.addModel2 === true", 
          selectInput('method2', "Second Model", c("random forest", "bayesian generalized linear","CART", "C4.5 algorithm", "bagged CART", "generalized boosted modeling")),
          selectInput('metric2', "Second Model Metric", c("Accuracy", "Kappa", "RMSE", "Rsquared")),
          checkboxInput('addModel3', "Add Third Model", FALSE),
          conditionalPanel("input.addModel3 === true", selectInput('method3', "Third Model", c("random forest", "bayesian generalized linear","CART", "C4.5 algorithm", "bagged CART", "generalized boosted modeling")),
            selectInput('metric3', "Thrid Model Metric", c("Accuracy", "Kappa", "RMSE", "Rsquared"))
        )
    )
    )
    ),
  #####   MAIN PANEL     
  ################################################################################################################################################################################################################  
  # Show a plot of the generated distribution
  column(10,
    tabsetPanel(
      #### Data and Stats
      ########################
      tabPanel("Data Plots and Statistical Tests",
               fluidRow(
               splitLayout(cellWidths = c("50%", "50%"), uiOutput("plot1"), uiOutput("plot2"))
               ),
               br(),
               fluidRow(
               conditionalPanel("input.scb === true",
                 h3(textOutput("statT")),
                 h3(textOutput("statTxt")),
                 tags$head(tags$style("#statTxt{
                                      font-size: 12px;
                                      font-style: italic;
                                      }"
                         )), 
                 br(),
                 h3(textOutput("statTR"))
                 )
               ),
              br(),
              fluidRow(conditionalPanel("input.scb === true", splitLayout(cellWidths = c("50%", "50%"), DT::dataTableOutput("stat.res"),  DT::dataTableOutput("stat.res2"))))
      ),
      
      ####Correlation and MR
      ########################
      tabPanel("Correlation and Multiple Regression", 
               fluidRow(
                column(4, 
                       radioButtons("corrVariablesStyle", "Variable Selection Style", c("Checkbox", "Selectize"), inline = T),
                       helpText("Choose the variables to display"), 
                       conditionalPanel("input.corrVariablesStyle === 'Checkbox'",
                                        tags$div(class = 'multicol', checkboxGroupInput("corrVariablesCheckbox", "", c("Loading...")))),
                       conditionalPanel("input.corrVariablesStyle === 'Selectize'",
                                         selectizeInput("corrVariables", "", choices = c("Loading..."), multiple = T, options = list(plugins = list("drag_drop", "remove_button"), width = 500)))),
                column(8, plotOutput("corrPlot", width = 650, height = 700))
               ),
               br(),
               fluidRow(
                 conditionalPanel("input.multReg === true", column(2, selectizeInput("IMRVariables", "Multiple Regression Independent Variables", choices = c("Loading..."), multiple = T, options = list(plugins = list("drag_drop", "remove_button"), width = 500))),
                 column(2, selectizeInput("DMRVariable", "Multiple Regression Independent Variable", choices = c("Loading..."), multiple = F, width = 500)),
                 column(6, DT::dataTableOutput("mr")))
               ),
               br(),
               br()
              ),
      tabPanel("Machine Learning",
               h3(textOutput("vf.txt")),
               fluidRow(
               column(3, radioButtons("mlvariablesStyle", "Variable Selection Style", c("Checkbox", "Selectize"), inline = T),
                      helpText("Choose the variables to display"),
                      conditionalPanel('input.mlvariablesStyle === "Selectize"', selectizeInput("mlvariables", "", choices = c("Loading..."), multiple = T, options = list(plugins = list("drag_drop", "remove_button"), width = 900))),
                      conditionalPanel('input.mlvariablesStyle === "Checkbox"', tags$div( class = 'multicol', checkboxGroupInput("mlvariablesCheckbox", "", c("Loading..."))))),
               column(9, uiOutput("fPlot1"))
               ),
               br(), 
               h3(textOutput("iap.txt")),
               br(),
               fluidRow(
               column(6, uiOutput("iPlot")), column(6, uiOutput("aPlot"))
               ),
               br(), 
               h3(textOutput("at.txt")),
               br(),
               fluidRow(
                DT::dataTableOutput("a1"), 
                conditionalPanel("input.addModel2 == true", DT::dataTableOutput("a2")), 
                conditionalPanel("input.addModel3 == true", DT::dataTableOutput("a3"))
               ), 
               fluidRow(
                 conditionalPanel("input.factorResp === true", h3(textOutput("roc.txt")), tags$div(id = "plot-container", tags$img(src = "spinner.gif", id = "loading-spinner"), uiOutput("rPlot")))
                 ),
              br(),
              br()
              ),
      ###### Data tabs        
      #####################
      tabPanel("Data", DT::dataTableOutput("datasetTable")), 
      tabPanel("Feature and Response Data", DT::dataTableOutput("mldataTable")),
      tabPanel("Plots Data",
               fluidRow(
                 h3(textOutput("pd.txt")),
                 br(),
                 column(6, DT::dataTableOutput("plotvar1")),column(6, DT::dataTableOutput("plotvar2"))
               ),
               br(),
               fluidRow(
                 h3(textOutput("ml.fs.txt")),
                 br(),
                 DT::dataTableOutput("avgValTable"),
                 br(), 
                 DT::dataTableOutput("corrValTable"),
                 br(), 
                 DT::dataTableOutput("igTable"),
                 br(), 
                 h3(textOutput("ml.im.txt")),
                 br(),
                 DT::dataTableOutput("ipTable")
                )
        )
    )
  )
)
))


server10 <- shinyServer(function(input, output, session){

  
#####   DATA    
################################################################################################################################################################################################################  
  
  #allows user to input data set or try on test set of data 
  dataset <- reactive({
    datasource <- input$dataset
    if(datasource == "upload my own") {
      inFile <- input$datafile
      if(is.null(inFile)) {
        NULL
      } else {
        read.delim(inFile$datapath, sep = gsub("\\t", "\t", input$datafile_sep, fixed = TRUE))
      }
    } else {
      eval(parse(text = datasource))
    }
  })
  
  #allows user to input response data set or try on test set of data 
  rdataset <- reactive({
    datasource <- input$rdataset
    if(datasource == "upload another") {
      rinFile <- input$rdatafile
      if(is.null(rinFile)) {
        NULL
      } else {
        read.delim(rinFile$datapath, sep = gsub("\\t", "\t", input$rdatafile_sep, fixed = TRUE))
      }
    } else {
      dataset()
    }
  })
  
    #updates y, dependent variable for 2D plots 
    output$y <- renderUI({ 
      obj<-dataset()    
      var.opts<-c(colnames(obj))
      selectInput("y","Dependent Variable:", var.opts) # uddate UI                  
    }) 
    
    #updates x, independent variable for plots
    output$x <- renderUI({ 
      obj<-dataset()     
      var.opts<-c(colnames(obj))
      selectInput("x","Independent Variable:", var.opts) # uddate UI                 
    })
    
    #updates grouping variable for comparing data based on categorical variables such as gender 
    output$group <- renderUI({ 
      obj<-dataset()     
      var.opts<-c(colnames(obj))
      selectInput("group","Grouping Variable: ", var.opts) # uddate UI                 
    })
    
    #updates y for second plot , dependent variable for 2D plots 
    output$y2 <- renderUI({ 
      obj<-dataset()    
      var.opts<-c(colnames(obj))
      selectInput("y2","Dependent Variable:", var.opts) # uddate UI                  
    }) 
    
    #updates x for second plot, independent variable for plots
    output$x2 <- renderUI({ 
      obj<-dataset()     
      var.opts<-c(colnames(obj))
      selectInput("x2","Independent Variable:", var.opts) # uddate UI                 
    })
    
    #updates grouping variable for second plot for comparing data based on categorical variables such as gender 
    output$group2 <- renderUI({ 
      obj<-dataset()     
      var.opts<-c(colnames(obj))
      selectInput("group2","Grouping Variable: ", var.opts) # uddate UI                 
    })
    
    #updates response variable for machine learning tab
    output$r <- renderUI({ 
      obj<-rdataset()
      var.opts<-c(colnames(obj))
      selectInput("r","Response Variable: ", var.opts) # uddate UI                 
    })
    
    #get numeric data
    numericColumns <- reactive({
      df <- dataset()
      colnames(df)[sapply(df, is.numeric)]
    })
    
    
    #creates a plot object from the data so can create proper d3 plots 
    plot.obj <- function(x,y,g){
      plot.list<-list() 
      plot.list$data<- dataset()
      if(is.null(plot.list$data)){
        return(NULL)
      }
      if(!length(intersect(x, colnames(plot.list$data))) || !length(intersect(y, colnames(plot.list$data))) || !length(intersect(g, colnames(plot.list$data)))){
        return(NULL)
      }
      plot.list$x<-with(plot.list$data, get(x))
      plot.list$freqx <- as.data.frame(table(plot.list$x))
      colnames(plot.list$freqx) <- c('x', 'Frequency')
      plot.list$y<-with(plot.list$data,get(y))
      plot.list$freqy <- as.data.frame(table(plot.list$y))
      colnames(plot.list$freqy) <- c('y', 'Frequency')
      plot.list$group <- with(plot.list$data, get(g))
      plot.list$groups = data.frame(plot.list$x, plot.list$group)
      colnames(plot.list$groups) <- c('x', 'g')
      plot.list$variables <- data.frame(plot.list$x, plot.list$y)
      colnames(plot.list$variables) <- c('x','y')
      return(plot.list)
    }
    
    #Machine Learning Data object to run models and create plots
    ml.obj <- reactive({
      mlvariables <- input$mlvariables
      d <- dataset()
      rd <- rdataset()
      if(is.null(d) || !length(intersect(mlvariables, colnames(d)))){
        return(NULL)
      }
      if( !length(intersect(input$r, colnames(rd)))){
        return(NULL)
      }
      if(is.null(rd) || nrow(as.data.frame(d)) != nrow(as.data.frame(rd))){
        return(NULL)
      }
      
      ml.list <- list()
      if (input$r %in% c(mlvariables)){
        ml.list$features <- d[, !c(mlvariables) %in% input$r ]
        features = mlvariables[!c(mlvariables) %in% input$r]
      }
      else{
        ml.list$features <- d[,c(mlvariables)]
        features <- mlvariables
      }
      ml.list$features <- sapply(ml.list$features, as.numeric)
      ml.list$response <- with(rd,get(input$r))
      ml.list$response <- as.numeric(ml.list$response)
      ml.list$correct_factor <- 1
      if(input$factorResp == TRUE){
        above = sum(ml.list$response > input$factorVal)
        below = sum(ml.list$response < input$factorVal)
        if(above == 0 || below == 0){
          ml.list$correct_factor <- 0 
          return(NULL)
        }
        else{
          ml.list$response.f = factor(as.numeric(ml.list$response) > as.numeric(input$factorVal), labels = c(0,1))
          ml.list$response.f = sapply(ml.list$response.f, as.numeric)
        }
      }
      #set response variable based on factor 
      finalResp <- ifelse(input$factorResp, "response.f", "response")
      ml.list$featResp <- cbind.data.frame(ml.list$features, response = as.numeric(with(ml.list, get(finalResp))))
      
        #Filter Objects 
        ml.list$avgVal <- colMeans(ml.list$features, na.rm = T)
        ml.list$avgVal <- cbind.data.frame(names(ml.list$avgVal), ml.list$avgVal)
        colnames(ml.list$avgVal) <- c("Variable", "Column Average")
        ml.list$corrVal <- cor(ml.list$features, ml.list$featResp$response, use = "na.or.complete")
        ml.list$corrVal <- cbind.data.frame(rownames(ml.list$corrVal), ml.list$corrVal)
        colnames(ml.list$corrVal) <- c("Variable", "Correlation")
        ml.list$ig <- information.gain(response~., data = ml.list$featResp)
        ml.list$ig <- cbind.data.frame(rownames(ml.list$ig), ml.list$ig)
        colnames(ml.list$ig) <- c("Variable", "Feature Importance")

      
      # splitting to testing and training 
      samples = row.names(ml.list$featResp)
      train.percent = .75
      inTrain = samples %in% sample(samples, floor(train.percent*length(samples)))
      train.data = ml.list$featResp[inTrain,]
      test.data = ml.list$featResp[!inTrain,]
      
      ml.list$train.data <- train.data
      ml.list$test.data <- test.data
      
      
      response.name = paste("Response-", input$r, sep = "")
      colnames(ml.list$featResp) <- c(features, response.name)
       
      return(ml.list)
    })
  
    output$pd.txt <- renderText("Data for Left and Right Plots under Data Plots and Statistical Testing Tab")
    output$ml.fs.txt <- renderText("Data for Feature Selection Plots in Machine Learning Tab")
    output$ml.im.txt <- renderText("Data for Importance Plots in Machine Learning Tab")
    
##### Data Plots and Statistical Tests  
################################################################################################################################################################################################################      
    
    require(rCharts)
    require(ggplot2)
    require(plotly)
    options(RCHART_LIB = 'dimple')
    options(RCHART_LIB = 'nvd3')
    options(RCHART_LIB = 'polycharts')
    options(RCHART_LIB = 'morris')
    options(RCHART_LIB = 'highcharts')
    options(RCHART_WIDTH = 500)
    
    #makes sure correct rcharts library is referenced for each type of plot.  
    rchart_lib <- function(pt){
      r_lib <- switch(pt, 
                      boxplot = "highcharts",
                      multibar = "nvd3",
                      bar = "morris",
                      "grouped scatter" = "highcharts",
                      scatter = "highcharts", 
                      "single boxplot" = "highcharts"
      )
      return(r_lib)
    }
    
    #Plot Data
    output$plot1 <- renderUI({
      rchart_lib1 <- rchart_lib(input$plotType)
      switch(input$plotType, 
             "boxplot" = showOutput("p1", rchart_lib1),
             "histogram" =  plotlyOutput("p1hist", height = 500),
             "multibar" = showOutput("p1", rchart_lib1),
             "bar" = showOutput("p1", rchart_lib1),
             "grouped scatter" = showOutput("p1", rchart_lib1),
             "scatter" = showOutput("p1", rchart_lib1), 
             "single boxplot" = showOutput("p1", rchart_lib1)
             )
      
    })
    
    output$plot2 <- renderUI({
      rchart_lib2 <- rchart_lib(input$plotType2)
       plot <-  switch(input$plotType2, 
             "boxplot" = showOutput("p2", rchart_lib2),
             "histogram" = plotlyOutput("p2hist", height = 500),
             "multibar" = showOutput("p2", rchart_lib2),
             "bar" = showOutput("p2", rchart_lib2),
             "grouped scatter" = showOutput("p2", rchart_lib2),
             "scatter" = showOutput("p2", rchart_lib2), 
             "single boxplot" = showOutput("p2", rchart_lib2)
      )

    })
    
    output$p1 <- renderChart2({
      if(input$plotType == "boxplot" || input$plotType == "multibar" || input$plotType == "bar" || input$plotType == "grouped scatter" || input$plotType ==  "scatter" || input$plotType == "single boxplot"){
        p1 <- plot.Type(input$plotType, input$x, input$y, input$group, input$bins)
        p1$addParams(height = 500)
        print(p1)
      }else {
        return(rCharts$new())
      }
    })
    
    output$p2 <- renderChart2({
      if(input$plotType2 == "boxplot" || input$plotType2 == "multibar" || input$plotType2 == "bar" || input$plotType2 == "grouped scatter" || input$plotType2 ==  "scatter" || input$plotType2 == "single boxplot"){
        p2 <- plot.Type(input$plotType2, input$x2, input$y2, input$group2, input$bins2)
        p2$addParams(height = 500)
        print(p2)
      }
      else{
        return(rCharts$new())
      }
    })
    
    output$p1hist <- renderPlotly({
      if(input$plotType == "histogram"){
        p1hist <- plot.Type(input$plotType, input$x, input$y, input$group, input$bins)
        print(p1hist)
      }else{
        return(NULL)
      }
    })
    
    
    output$p2hist <- renderPlotly({
      if(input$plotType2 == "histogram"){
        p2hist <- plot.Type(input$plotType2, input$x2, input$y2, input$group2, input$bins2)
        print(p2hist)
      }else{
          return(NULL)
        }
    })

    plot.Type<-function(pt, x, y , g, bins){
      plot.df <- plot.obj(x,y,g)
      if(!is.null(plot.df$x) || !is.null(plot.df$y) || !is.null(plot.df$g)){
        freqx.dp <- plot.df$freqx
        colnames(freqx.dp) <- c(x, 'Frequency')
        var.hp <- plot.df$variables
        var.hp <- as.data.frame(sapply(var.hp, as.numeric))
        colnames(var.hp) <- c(x,y)
        var.hp.gs <- cbind.data.frame(plot.df$variables, plot.df$group)
        var.hp.gs <- as.data.frame(sapply(var.hp.gs, as.numeric))
        colnames(var.hp.gs) <- c(x, y , 'g')
        var.hp.gs$g <- as.factor(var.hp.gs$g)
        
        
        switch(pt,
               "single boxplot" = {
                 #box plot stats
                 dat = data.frame(boxplot(plot.df$variables$x, data = plot.df$variables, plot = F)$stats)
                 #data frame with no headers
                 bwstats = setNames(as.data.frame(dat[complete.cases(dat),]),nm = NULL)
                 
                 #make chart and set parameters 
                 p <- Highcharts$new()
                 p$set(series = list(list(name = x, data = bwstats)))
                 p$xAxis(title = list(text = x))
                 p$chart(type = 'boxplot')
               },
               "boxplot" = {
                 # compute boxplot statistics and cast it as a dataframe with no headers
                 dat = data.frame(boxplot(x ~ g, data = plot.df$groups, plot = F)$stats)
                 # compute boxplot statistics and cast it as a dataframe with no headers
                 bwstats = setNames(as.data.frame(dat[complete.cases(dat),]),nm = NULL)
                 
                 #initialize
                 p <- Highcharts$new()
                 
                 # pass data as a list of lists
                 p$set(series = list(list(name = g, data = bwstats)))
                 # set xaxis/yaxis titles and labels
                 p$xAxis(categories = levels(plot.df$groups$group), title = list(text = g))
                 p$yAxis(title = list(text = x))
                 p$chart(type = 'boxplot')
               },
               "histogram" = {
                 #bins for histogram
                  hx <- plot.df$variables[,1]
                  hx<- sapply(hx, as.numeric)
                  breaks<- seq(min(hx), max(hx), length.out = bins + 1)
                  hist <- hist(hx, breaks = breaks , plot = FALSE)
                  hist.df <- cbind.data.frame(hist$counts, hist$breaks[1:(length(hist$breaks) - 1)])
                  ggh <-ggplot(hist.df, aes(x = hist$counts), group = hist$counts) + geom_bar(alpha=0.5, position= "identity", fill = "skyblue3")
                  ggh <- ggh + labs( x = x) + theme(axis.line = element_line(colour = 'gray', size = .75), panel.background = element_blank(), plot.background = element_blank()) 
                  p <- ggplotly(ggh)
                },
               "multibar" 	= {p <-	nPlot(Freq ~ x , group = 'g', data = data.frame(table(plot.df$groups)), type = 'multiBarChart')
               },        
               "bar" =	{p <- mPlot(x = 'x', y = list('Frequency'), data = plot.df$freqx, type = 'Bar', labels = list("Count"))
               
               },
               "grouped scatter" = {p <- hPlot(x = x, y = y, data = var.hp.gs, type = 'scatter', group = 'g')
               },
               "scatter" ={p <- hPlot(x = x, y = y , data = var.hp , type = 'scatter')
               }
        )

      return(p)
      }  
    }
      
    #statistical Testing
    stat.tests <- function(st, sig, x, y, g){
      var.df <- plot.obj(x,y,g)
      if(!is.null(var.df$groups)){
        x.g <- table(var.df$groups)
        conf.level = 1 - as.numeric(sig)
        p.val = sig
        x.st = as.numeric(var.df$groups$x)
        x.g.sort <- var.df$groups[order(var.df$groups$g),]
        colnames(x.g.sort) <- c("x", "g")
        x.g.sort$x <-as.numeric(x.g.sort$x)
  
        t.dat <- switch(st, 
               "Shapiro-Wilk" = {t <- shapiro.test(x.st)
                                tab <- cbind(t$statistic, t$p.value)
                                colnames(tab) <- c("W-statistic", "P-value")
                                row.names(tab) <- c("Shapiro-Wilk")},
               "Two sample t-test" = {t <- t.test(x.g[,1], x.g[,2], conf.level = conf.level)
                                    tab <- cbind(t$statistic, t$p.value)
                                    colnames(tab) <- c("T-statistic", "P-value")
                                    row.names(tab) <- c("Welch Two Sample t-test")}, 
               "Wilcox rank sum - two sample" = {t <- wilcox.test(x.g[,1], x.g[,2], conf.level = conf.level)
                                    tab <- cbind(t$statistic, t$p.value)
                                    colnames(tab) <- c("W-statistic", "P-value")
                                    row.names(tab) <- c("Wilcoxon rank sum test with continuity correction")},
               "Annova" =  {t <- aov(x ~ g, data = x.g.sort)
                            tab <- as.data.frame(summary(t)[[1]])
                            row.names(tab) <- c(g,"Residuals")
                            },
               "Kruskal–Wallis" = {t <- kruskal.test(x  ~ g , data = x.g.sort)
                                  tab <- cbind(t$statistic, t$p.value)
                                  colnames(tab) <- c("Chi-squared", "P-value")}
               )
      return(tab)
      }
    }
    
    output$statT <- renderText("Statistical Testing")
    output$statTxt <- renderText("Shapiro Wilks test is conducted on the independent variables. Two sample tests, Annova, and Kruskal-Wallis are conducted on the independent variables
                                  and the grouping variables")
    output$statTR <- renderText(paste(sapply(input$statTest, simpleCap), "Results" ))
    
    output$stat.res <- DT::renderDataTable(stat.tests(input$statTest, input$sig, input$x, input$y, input$group), options = list(
      autoWidth = FALSE), class = 'table-bordered table-condensed table-striped table-compact')
    
    output$stat.res2 <- DT::renderDataTable(stat.tests(input$statTest2, input$sig2, input$x2, input$y2, input$group2), options = list(
      autoWidth = FALSE), class = 'table-bordered table-condensed table-striped table-compact')
    
##### Correlation and Multiple Regression
################################################################################################################################################################################################################      
    
    #update correlation variables 
    observe({
      updateCheckboxGroupInput(session, "corrVariablesCheckbox", choices = numericColumns(), selected = numericColumns())
      
      updateSelectInput(session, "corrVariables", choices = numericColumns(), selected = numericColumns())
    })
    
    #Link correlation variable Selection
    observe({
      if(input$corrVariablesStyle == "Checkbox") {
         updateCheckboxGroupInput(session, "corrVariablesCheckbox", selected = isolate(input$corrVariables))
      }
    })
    
    observe({
      updateSelectInput(session, "corrVariables", selected = input$corrVariablesCheckbox)
    })
    
    observe({
      if(input$multReg == TRUE)
        updateSelectInput(session, "IMRVariables", choices = numericColumns(), selected = numericColumns())
    })
    
    observe({
    if(input$multReg == TRUE)      
      updateSelectInput(session, "IMRVariables", selected = input$corrVariablesCheckbox)
    })
    
    observe({
      if(input$multReg == TRUE)
        updateSelectInput(session, "DMRVariable", choices = c(input$corrVariablesCheckbox))
    })
    
    observe({
      if(input$multReg == TRUE)
        updateSelectInput(session, "DMRVariable", selected = c(input$corrVariablesCheckbox)[1])
    })
    
    # NA output warning
    output$warning <- renderUI({
      val <- correlation()
      if(is.null(val)) {
        tags$i("Waiting for data input...")
      } else {
        isNA <- is.na(val)
        if(sum(isNA)) {
          tags$div(
            tags$h4("Warning: The following pairs in calculated correlation have been converted to zero because they produced NAs!"),
            helpText("Consider using an approriate NA Action to exclude missing data"),
            renderTable(expand.grid(attr(val, "dimnames"))[isNA,]))
        }
      }
    })
    
    #correlation and correlation test
    correlation <- reactive({
      data <- dataset()
      corrvariables <- input$corrVariables
      if(is.null(data) || !length(intersect(corrvariables, colnames(data)))) {
        NULL
      } else {
        cor(dataset()[,input$corrVariables], use = input$corUse, method = input$corMethod)
      }
    })
    
    #tests for significance 
    sigConfMat <- reactive({
      val <- correlation()
      if(!is.null(val))
        corTest(val, input$corrSig)
    })
    
    #Correlation Plot
    output$corrPlot <- renderPlot({
      corrvals <- correlation()
      if(is.null(corrvals)) return(NULL)
      corrvals[is.na(corrvals)] <- 0
      
      order = input$plotOrder
      p.mat = sigConfMat()[[1]]
      sig.level = ifelse(input$sigTest, input$sigLevel, NULL)
      
      #options for correlation plot     
      corrplot::corrplot.mixed(corrvals, upper = "circle", lower = "number", order = order, p.mat = p.mat , sig.level = sig.level, insig = "pch", tl.pos = "lt")
      
    })
    
    #correlation function for corrplot 
    corTest <- function(mat, conf.level = 0.95){
      mat <- as.matrix(mat)
      n <- ncol(mat)
      p.mat <- matrix(NA, n, n)
      diag(p.mat) <- 0
      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          try({
            tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
          }, silent = TRUE)
        }
      }
      return(list(p.mat))
    }
    
    #multiple regression
    mult.reg <- function(dvmr, cv, log, mr){
      if(mr == TRUE){
        fam = ifelse(log, 'binomial', 'gaussian')
        y.df = dataset()[,dvmr]
        x.df = dataset()[,!cv %in% dvmr]
        mr.df <- cbind.data.frame(x.df, y.df)
        colnames(mr.df) <- c(c(cv[!cv %in% dvmr]), 'y')
        fit.mr <- glm(y~., family = fam , data = mr.df )
        result.mr <- summary(fit.mr)$coefficients
        return(result.mr)
      }
    }
    
    output$mr <- DT::renderDataTable({
      mr.df <- as.data.frame(mult.reg(input$DMRVariable, input$IMRVariables, input$log, input$multReg))
      dat <- datatable(mr.df, options = list(autoWidth = FALSE), 
                                    class = 'table-bordered table-condensed table-striped table-compact', caption = "Multiple Regression Coefficient Results" 
                                    ) %>%
      formatStyle('Pr(>|t|)', color = styleInterval(0.05, c('red','black')), backgroundColor = styleInterval(0.05, c('yellow', 'white'))) %>% 
      formatRound(1:length(mr.df), 5)
      return(dat)
      })
    
 #####   MACHINE LEARNING    
################################################################################################################################################################################################################    
   
    #Update variable selection

    observe({
      updateCheckboxGroupInput(session, "mlvariablesCheckbox", choices = numericColumns(), selected = numericColumns())
      
      updateSelectizeInput(session, "mlvariables", choices = numericColumns(), selected = numericColumns())
    })
    
    #Link Variable Selection
    observe({
      if(input$mlvariablesStyle == "Checkbox") {
        updateCheckboxGroupInput(session, "mlvariablesCheckbox", selected = isolate(input$mlvariables))
      }
    })
    observe({
      updateSelectizeInput(session, "mlvariables", selected = input$mlvariablesCheckbox)
    })
    
    #Capitilize First letter of words for text output 
    simpleCap <- function(a) {
      s <- strsplit(a, " ")[[1]]
      paste(toupper(substring(s, 1,1)), substring(s, 2),
            sep="", collapse=" ")
    }
    
    output$vf.txt <- renderText("Select Variables for Features and Feature Selection Plot")
    output$iap.txt <- renderText(paste(sapply(input$method, simpleCap), "Feature Importance and Accuracy" ))
    output$at.txt <- renderText("Model Results")
    output$roc.txt <- renderText("ROC Curve for Factored Response")

    #Plot feature Selection Plot 
    output$fPlot1 <- renderUI({
      showOutput("fp1", "highcharts")
    })
    
    output$fp1 <- renderChart2({
      featp1 <- fs.plot(input$fsMethod)
      print(featp1)
    })
    
     
    fs.plot<-function(fst){
      mlo <- ml.obj()
        switch(fst,
               "average value" = {
                 fp <- Highcharts$new()
                 fp$chart(type = "bar")
                 fp$title(text = "Average Column Value of Features")
                 fp$xAxis(categories = rownames(mlo$avgVal))
                 fp$yAxis(title = list(text = "Average Value"))
                 fp$set(yAxis = TRUE)
                 fp$data(name = "Features", as.numeric(mlo$avgVal$'Column Average'))
               },
               "correlation with response" = {
                 fp <- Highcharts$new()
                 fp$chart(type = "bar")
                 fp$title(text = "Feature Correlation with Response Variable")
                 fp$xAxis(categories = rownames(mlo$corrVal))
                 fp$yAxis(title = list(text = "Correlation Value"))
                 fp$data(name = "Features", as.numeric(mlo$corrVal$Correlation))
               },
               "information gain" = {
                 fp <- Highcharts$new()
                 fp$chart(type = "bar")
                 fp$title(text = "Feature Information Gain")
                 fp$xAxis(categories = rownames(mlo$ig))
                 fp$yAxis(title = list(text = "Information Gain Value"))
                 fp$data(name = "Features", as.numeric(mlo$ig$'Feature Importance'))
               }
        )
      print(fp)  
      return(fp)
    }
    require(rCharts)
    options(RCHART_WIDTH = 500)
    
    #plot Importance
    output$iPlot <- renderUI({
      if(is.null("ip1") || is.null("ip.rp") || is.null("ip.bgl")) return(NULL)
      switch(input$method,  
             "random forest" = showOutput("ip1", "highcharts"), 
             "bayesian generalized linear" = DT::dataTableOutput("ip.bgl"),
             "CART" = plotOutput("ip.rp"),
             "C4.5 algorithm"= showOutput("ip1", "highcharts"),
             "bagged CART" = showOutput("ip1", "highcharts"),
             "generalized boosted modeling" = showOutput("ip1", "highcharts")
      )
    })
    
    output$ip1 <- renderChart2({
      if(input$method == "random forest" || input$method == "C4.5 algorithm" || input$method == "bagged CART" || input$method == "generalized boosted modeling"){
        ip1 <- var.imp(input$method, input$metric, input$number, input$repeats)[[2]]
        print(ip1)
      }
      else{
        return(rCharts$new)
      }
      
    })
    
    
    output$ip.bgl <- DT::renderDataTable({
      if(input$method == "bayesian generalized linear"){
        df <- var.imp(input$method, input$metric, input$number, input$repeats)[[2]] 
        dt <- datatable(df, options = list(autoWidth = FALSE), 
                       class = 'table-bordered table-condensed table-striped table-compact', caption = "Bayesian Generalized Linear Regression Results") %>%
          formatStyle('Pr(>|t|)', color = styleInterval(0.05, c('red','black')), backgroundColor = styleInterval(0.05, c('yellow', 'white'))) %>% 
          formatRound(1:length(df), 5)
        return(dt)
      }
    })
    
    output$ip.rp <-renderPlot({
      if(input$method == "CART"){
        ip.rp.list <- var.imp(input$method, input$metric, input$number, input$repeats)
        ip.rp <- ip.rp.list[[2]]
      }
    })
           
    #model based on input 
    ml.model <- function(mt, m, n, r){
      mlo <- ml.obj()
      print(head(mlo$response))
      print(head(mlo$train.data))
      if(is.null(mlo$response) || is.null(mlo$train.data)) return(NULL)
      nt = 1
      colnames(mlo$train.data)[ncol(mlo$train.data)] <- "response"
      train.data = mlo$train.data
      #Control for Cross Validation and Repeats
      number <- n
      repeats <-r
      metric <- m
      control <- trainControl(method="repeatedcv", number=number, repeats=repeats)

      withProgress(message = 'Machine Learning in progress', value=i, {
          incProgress(nt, detail = paste(n, input$method, input$fsMethod))

      #model 
      set.seed(7)
      model <- switch(mt, "random forest" = train(as.factor(response) ~., data = train.data,  method ="rf", metric=metric, trControl= control), 
                          "bayesian generalized linear" = train(response ~., data=na.omit(train.data), method= "bayesglm", metric = metric, trControl = control),
                          "CART" = train(as.factor(response)~., data=na.omit(train.data), method="rpart", metric=metric, trControl=control),
                          "C4.5 algorithm" =  train(as.factor(response)~., data=na.omit(train.data), method="C5.0", metric=metric, trControl=control),
                          "bagged CART" = train(response~., data=na.omit(train.data), method="treebag", metric="RMSE", trControl=control),
                          "generalized boosted modeling" = train(response~., data=na.omit(train.data), method="gbm", metric=metric, trControl=control, verbose=FALSE)
                      )

      incProgress(nt, detail = paste("next step"))
      })
      return(model)
    }
  
    #Importance plot or linear model summary based on input 
    var.imp <- function(mt, m, n, r){
      fit <- ml.model(mt, m, n, r)
      if(is.null(fit)) return(NULL)
        switch(mt, 
               "random forest" = {      
                 require(randomForest)
                 importance = varImp(fit, scale = FALSE)
                 imp.df = as.data.frame(cbind(rownames(importance$importance), importance$importance$Overall))
                 ip <- Highcharts$new()
                 ip$chart(type = "bar")
                 ip$title(text = "Importance Random Forest")
                 ip$xAxis(categories = rownames(importance$importance))
                 ip$yAxis(title = list(text = "Importance"))
                 ip$data(name = "Features", as.numeric(importance$importance$Overall))
                 ip$addParams(height = 700)
                },
               "bayesian generalized linear" = {
                 a = summary(fit)$coefficients[,4]
                 b = a[a<= .05]
                 sv = a[which(a %in% b)]
                 ip <- as.data.frame(summary(fit)$coefficients)
                 imp.df <- ip[names(sv),]
                 },
               "CART" = {ip <- fancyRpartPlot(fit$finalModel)
                  imp.df = summary(fit)
               }, 
               "C4.5 algorithm" = { 
                  importance = C5imp(fit$finalModel)
                  imp.df = as.data.frame(cbind(rownames(importance), importance$Overall))
                  ip <- Highcharts$new()
                  ip$chart(type = "bar")
                  ip$title(text = "Importance C4.5 Algorithm")
                  ip$xAxis(categories = rownames(importance$importance))
                  ip$yAxis(title = list(text = "Importance"))
                  ip$data(name = "Features", as.numeric(importance$Overall))
                  ip$addParams(height = 700)
                 },
               "bagged CART" = {
                  importance = varImp(fit)
                  imp.df = as.data.frame(cbind(rownames(importance$importance), importance$importance$Overall))
                  ip <- Highcharts$new()
                  ip$chart(type = "bar")
                  ip$title(text = "Importance Bagged CART")
                  ip$xAxis(categories = rownames(importance$importance))
                  ip$yAxis(title = list(text = "Importance"))
                  ip$data(name = "Features", as.numeric(importance$importance$Overall))
                  ip$addParams(height = 700)
               },
               "generalized boosted modeling" = {
                  imp.df = as.data.frame(summary(fit))
                  ip <- Highcharts$new()
                  ip$chart(type = "bar")
                  ip$title(text = "Relative Influence GBM")
                  ip$xAxis(categories = rownames(imp.df))
                  ip$yAxis(title = list(text = "Relative Influence"))
                  ip$data(name = "Features", as.numeric(imp.df$rel.inf))
                  ip$addParams(height = 700)
               }
        )
        df_ip <- list(imp.df, ip)
        print(df_ip)
        return(df_ip)
      }
        
    
    output$a1 <- DT::renderDataTable(accuracy(input$method, input$metric, input$number, input$repeats)[[1]], options = list(
      autoWidth = FALSE), class = 'table-bordered table-condensed table-striped table-compact', caption = paste("Model Results:", sapply(input$method, simpleCap)))
    output$a2 <- DT::renderDataTable(accuracy(input$method2, input$metric2, input$number, input$repeats)[[1]], options = list(
      autoWidth = FALSE), class = 'table-bordered table-condensed table-striped table-compact', caption = paste("Model 2 Results:", sapply(input$method2, simpleCap)))
    output$a3 <- DT::renderDataTable(accuracy(input$method3, input$metric3, input$number, input$repeats)[[1]], options = list(
      autoWidth = FALSE), class = 'table-bordered table-condensed table-striped table-compact', caption = paste("Model 3 Results:", sapply(input$method3, simpleCap)))

    
    output$rPlot <- renderUI({
      plotOutput("rp")
    })

    require(rCharts)
    options(RCHART_WIDTH = 650)
    
    output$aPlot <- renderUI({
        switch(input$method,  
             "random forest" = DT::dataTableOutput("ap.rf"), 
             "bayesian generalized linear" = showOutput("ap1", "highcharts"),
             "CART" = showOutput("ap1", "highcharts"),
             "C4.5 algorithm"= plotOutput("ap.c50"),
             "bagged CART" = showOutput("ap1", "highcharts"),
             "generalized boosted modeling" = showOutput("ap1", "highcharts")
        )
    })

    
    output$ap1 <- renderChart2({
      if(input$method == "bayesian generalized linear" || input$method == "CART" || input$method == "bagged CART" || input$method == "generalized boosted modeling"){
        ap.list <- accuracy(input$method, input$metric, input$number, input$repeats)
        ap1 <- ap.list[[2]]
      }
    })
    
    output$ap.rf <- DT::renderDataTable({
      if(input$method == "random forest"){
        rf.list <- accuracy(input$method, input$metric, input$number, input$repeats)
        rf.df <- rf.list[[2]]
        dt.rf <- datatable(rf.df, options = list(autoWidth = FALSE), 
                       class = 'table-bordered table-condensed table-striped table-compact', caption = "Random Forest Confusion Matrix")
        return(dt.rf)
      }
    })
      
    output$ap.c50 <- renderPlot({
      if(input$method == "C4.5 algorithm"){
        ap.list <- accuracy(input$method, input$metric, input$number, input$repeats)
        ap.c50 <- plot(ap.list[[2]], main = "Accuracy vs Number of Baggs")
      }

    })
    

    # Plot info TPR/FPR ROC with diagonal line and legend
    output$rp <- renderPlot({
      perf1 <- roc("random forest", input$metric, input$number, input$repeats)
      if(is.null(perf1)) return(NULL)
      print(perf1)
      perf2 <- roc("CART", input$metric2, input$number, input$repeats)
      if(is.null(perf2)) return(NULL)
      print(perf2)
      perf3 <- roc("C4.5 algorithm", input$metric3, input$number, input$repeats)
      print(perf3)
      if(is.null(perf3)) return(NULL)  
      rp <- {plot(perf1, lwd = 5, col = 'blue', yaxt='n', xaxt='n', ann=F )
        abline(a = 0, b= 1)
        lines(perf2@x.values[[1]], perf2@y.values[[1]], col = 'red', lwd = 1.5)
        lines(perf3@x.values[[1]], perf3@y.values[[1]], col = 'green', lwd = 1.5)
        legend("bottomright", "(x,y)", c("Model 1: Random Forest", "Model 2: CART", "Model 3: C4.5 algorithm"), lwd=2, lty=1,col=c('blue','red','green'), cex = 0.85 )}
    
      return(rp)
  })
              
    #ROC curve 
    roc <- function(mt, m, n, r){
      fit <- ml.model(mt, m, n, r)
      
      if(is.null(fit)) return(NULL)
      
      mlo <- ml.obj()
      if(is.null(mlo$response.f)) return(NULL)
      
      if(mlo$correct_factor == 0) return(NULL)
      
      test.data = mlo$test.data
      
      #get predicted values, predictions, and performance for ROC
      Predicted = predict(fit, test.data[,(1:ncol(test.data)-1)], type = 'prob')
      print(Predicted)
      pred <- prediction(Predicted[,2], as.data.frame(test.data$response), label.ordering = NULL)
      perf <- performance(pred, 'tpr', 'fpr')
      
      return(perf)
    }
    
    accuracy<- function(mt, m, n, r){
      mlo <- ml.obj()
      test.data = mlo$test.data
      fit <- ml.model(mt,m,n,r)
      if(is.null(fit)) return(NULL)
      if(is.null(mlo$response)) return(NULL)
      switch(mt,
            "random forest" = {
                 ap = fit$finalModel$confusion
                 a = fit$results
            }, 
            "bayesian generalized linear" = { 
               a = fit$results
               pred = predict(fit, test.data[,1:ncol(test.data)-1])
               actual =  test.data$response
               dat = mapply(c,as.numeric(pred), actual, SIMPLIFY = FALSE)
               dat = lapply(dat, as.list)
               act.dat = mapply(c, actual,actual, SIMPLIFY = FALSE)
               act.dat = lapply(act.dat, as.list)
               ap <- Highcharts$new()
               ap$chart(type = 'scatter')
               ap$title(text = 'Predicted vs. Actual: Bayesian Generalized Linear')
               ap$yAxis(title = list(text = 'Predicted'))
               ap$xAxis(title = list(text = 'Actual'))
               ap$data(name = 'Predicted vs Actual', dat)
               ap$series(name = 'Actual', type = 'line', data = act.dat)
               ap$addParams(height = 700)
            },
            "CART" = {
               a = fit$results
               pred = predict(fit, test.data[,1:ncol(test.data)-1])
               actual =  test.data$response
               dat = mapply(c,pred,actual, SIMPLIFY = FALSE)
               dat = lapply(dat, as.list)
               act.dat = mapply(c, actual, actual, SIMPLIFY = FALSE)
               act.dat = lapply(act.dat, as.list)
               ap <- Highcharts$new()
               ap$chart(type = 'scatter')
               ap$title(text = 'Predicted vs. Actual: CART')
               ap$yAxis(title = list(text = 'Predicted'))
               ap$xAxis(title = list(text = 'Actual'))
               ap$data(name = 'Predicted vs Actual', dat)
               ap$series(name = 'Actual', type = 'line', data = act.dat)
               ap$addParams(height = 700)
            },
            "C4.5 algorithm" = {
               a = fit$results
               ap = fit
            }, 
            "bagged CART" = { 
              a = fit$results
              pred = predict(fit, test.data[,1:ncol(test.data)-1])
              actual =  test.data$response
              dat = mapply(c,pred,actual, SIMPLIFY = FALSE)
              dat = lapply(dat, as.list)
              act.dat = mapply(c, actual,actual, SIMPLIFY = FALSE)
              act.dat = lapply(act.dat, as.list)
              ap <- Highcharts$new()
              ap$chart(type = 'scatter')
              ap$title(text = 'Predicted vs. Actual: Bagged CART')
              ap$yAxis(title = list(text = 'Predicted'))
              ap$xAxis(title = list(text = 'Actual'))
              ap$data(name = 'Predicted vs Actual', dat)
              ap$series(name = 'Actual', type = 'line', data = act.dat)
              ap$addParams(height = 700)
              },
            "generalized boosted modeling" = {
              a = fit$results
              pred = predict(fit, test.data[,1:ncol(test.data)-1])
              actual =  test.data$response
              dat = mapply(c,pred,actual, SIMPLIFY = FALSE)
              dat = lapply(dat, as.list)
              act.dat = mapply(c, actual,actual, SIMPLIFY = FALSE)
              act.dat = lapply(act.dat, as.list)
              ap <- Highcharts$new()
              ap$chart(type = 'scatter')
              ap$title(text = 'Predicted vs. Actual: Generalized Boosted Modeling')
              ap$yAxis(title = list(text = 'Predicted'))
              ap$xAxis(title = list(text = 'Actual'))
              ap$data(name = 'Predicted vs Actual', dat)
              ap$series(name = 'Actual', type = 'line', data = act.dat)
              ap$addParams(height = 700)
            }
      )
      acc_ap = list(a, ap)
      return(acc_ap)
    }
    
##### Data Tables  
################################################################################################################################################################################################################      
    
    mlTables <- function(tablename){
      mlo <- ml.obj() 
      tab <- with(mlo, get(tablename))
      return(tab)
    }
    
    
    output$datasetTable <- DT::renderDataTable(as.data.frame(dataset()), options = list(autoWidth = TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Dataset" )
    output$mldataTable <- DT::renderDataTable(mlTables('featResp'), options = list(autoWidth = TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Feature and Response variable Matrix" )
    output$avgValTable <- DT::renderDataTable(mlTables('avgVal'), options = list(autoWidth = TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Average Column Values for Feature Variables" )
    output$corrValTable <- DT::renderDataTable(mlTables('corrVal'), options = list(autoWidth = TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Correlation of Feature Variables with Response Variable" )
    output$igTable <- DT::renderDataTable(mlTables('ig'), options = list(autoWidth = TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Information Gain for Feature Variables" )
    output$ipTable <- DT::renderDataTable(var.imp(input$method, input$metric, input$number, input$repeats)[[1]], options = list(autoWidth = FALSE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Importance Plot Data" )
    output$plotvar1 <- DT::renderDataTable({
      p.df1 <- plot.obj(input$x, input$y, input$group)
      p.df1.xyg <- cbind.data.frame(p.df1$variables, p.df1$group)
      colnames(p.df1.xyg) <- c(input$x,input$y, input$group)
      p.dT1 <- datatable(p.df1.xyg, options = list(autoWidth = TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Independent, Dependent, and Group Variable Data for Left Plot" )
      return(p.dT1)
    })
    output$plotvar2 <- DT::renderDataTable({
      p.df2 <- plot.obj(input$x2, input$y2, input$group2)
      p.df2.xyg <- cbind.data.frame(p.df2$variables, p.df2$group)
      colnames(p.df2.xyg) <- c(input$x2,input$y2, input$group2)
      p.dT2 <- datatable(p.df2.xyg, options = list(autoWidth = TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Independent, Dependent, and Group Variable Data for Right Plot" )
      return(p.dT2)
    })
    
####Generate Report 
################################################################################################################################################################################################################      

    output$report <- downloadHandler(
      # For HTML output, change this to "shiny_analysis_report.html"
      filename = "shiny_analysis_report.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list("indepent variable 1" = input$x , "dependent variable 1" = input$y, "grouping variable 1" = input$group, "plot type 1" = input$plotType, "statistical test 1" = input$statTest , "significance threshold 1" = sig,
                       "indepent variable 2" = input$x2 , "dependent variable 2" = input$y2, "grouping variable 2" = input$group2, "plot type 2" = input$plotType2, "statistical test 2" = input$statTest2 , "significance threshold 2" = sig2,
                       "Correlation Variables" = c(input$corrVariables), "Multiple Regression Independent Variables" = c(input$IMRVariables), "Multiple Regression Dependent Variables" = input$DMRVariable,
                       "Machine Learning Feature Variables" = c(input$mlvariables[!input$mlvariables %in% input$r]), "Response Variable" = input$response
                        )
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
  )        
    
})


shinyApp(ui = ui10, server = server10)


