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
library(shinydashboard)
library(RColorBrewer)

multicol <- " .multicol {
                      
-webkit-column-count: 2; /* Chrome, Safari, Opera */

-moz-column-count: 2; /* Firefox */

column-count: 2;
}"

uiR <- shinyUI(
  dashboardPage( 
    
  # Application title
  dashboardHeader(title = "Clinical Survey Data Analysis with Shiny"),
  
  #####   SIDE PANEL    
  ################################################################################################################################################################################################################  
  # Sidebar 
  dashboardSidebar(
    tags$head(
      tags$script(
        HTML(
          "
          $(document).ready(function(){
          // Bind classes to menu items, easiet to fill in manually
          var ids = ['subItemOne','subItemTwo','subItemThree','subItemFour'];
          for(i=0; i<ids.length; i++){
          $('a[data-value='+ids[i]+']').addClass('my_subitem_class');
          }
          
          // Register click handeler
          $('.my_subitem_class').on('click',function(){
          // Unactive menuSubItems
          $('.my_subitem_class').parent().removeClass('active');
          })
          })
          "
        )
      )
      ),  
   width = 300,
   sidebarMenu(id = "sbm",
    downloadButton("report", "Generate report"),
    selectInput("dataset", "Dataset", c("","esoph", "upload my own")),
    conditionalPanel("input.dataset === 'upload my own'",
                     fileInput("datafile", ""), 
                     textInput("datafile_sep", "Field Seperator", value = ",")),
    helpText("Different Analysis Options"),
    #### Data Plots and Stats
    ########################
    menuItem("Data Plots and Statistical Testing", tabName = "pst", icon = icon("bar-chart")), 
    conditionalPanel("input.sbm === 'pst'", "",
      selectInput("plot", "Plot Variables", c("Plot 1 (Left)", "Plot 2 (Right)")),
      conditionalPanel("input.plot === 'Plot 1 (Left)'", "Plot 1 Variables",
        selectInput("plotType","Plot 1 Type :", 
                  c("bar","histogram", "multibar", "density", "single boxplot", "boxplot", "grouped scatter",  "scatter")), 
          conditionalPanel("input.plotType === 'histogram'", sliderInput("bins","Number of bins:", min = 1, max = 50, value = 3)),
          conditionalPanel("input.plotType === 'boxplot' || input.plotType === 'single boxplot'", checkboxInput("showPoints", "show points", TRUE)),
          conditionalPanel("input.plotType === 'scatter' || input.plotType === 'grouped scatter'", checkboxInput("addTrend", "add trendline", FALSE)),
        uiOutput("x"),   
        uiOutput("y"),
        uiOutput("group"),
        checkboxInput("factorG", "Factor Grouping Variable", FALSE),
        conditionalPanel("input.factorG === true", textInput("factorGVal", "Factor Cut off Value", value = "0"))
      ),
      conditionalPanel("input.plot === 'Plot 2 (Right)'", "Plot 2 Variables",
        selectInput("plotType2"," Plot 2 Type:", 
                  c("bar", "histogram", "multibar", "density", "single boxplot", "boxplot", "grouped scatter",  "scatter")),
          conditionalPanel("input.plotType2 === 'histogram'", sliderInput("bins2","Number of bins:", min = 1, max = 50, value = 3)),
          conditionalPanel("input.plotType2 === 'boxplot' || input.plotType2 === 'single boxplot'", checkboxInput("showPoints2", "show points", TRUE)),
          conditionalPanel("input.plotType2 === 'scatter' || input.plotType2 === 'grouped scatter'", checkboxInput("addTrend2", "add trendline", FALSE)),
        uiOutput("x2"),   
        uiOutput("y2"),
        uiOutput("group2"),
        checkboxInput("factorG2", "Factor Grouping Variable", FALSE),
        conditionalPanel("input.factorG2 === true", textInput("factorGVal2", "Factor Cut off Value", value = "0"))
      ),
      conditionalPanel("input.plot === 'Plot 1 (Left)'", "Statistical Testing for Plot 1 Variables",
        selectInput("statTest", "Statistical Tests", c("Shapiro-Wilk",  "Two sample t-test", "Wilcox rank sum - two sample", "Annova", "Kruskal–Wallis")),
        textInput('sig', "Significance Threshold", value = .05)
      ),
      conditionalPanel("input.plot === 'Plot 2 (Right)'", "Statistical Test for Plot 2 Variables",
        selectInput("statTest2", "Statistical Tests", c("Shapiro-Wilk",  "Two sample t-test", "Wilcox rank sum - two sample", "Annova", "Kruskal–Wallis")),
        textInput('sig2', "Significance Threshold", value = .05)
      )
      ),
    #### Correlation and MR
    ########################
    menuItem("Correlation and Multiple Regression", tabName = "cmr", icon = icon("th")),
      conditionalPanel("input.sbm === 'cmr'", "Response Data",
        selectInput("corMethod", "Correlation Method", c(eval(formals(cor)$method))),
        selectInput("corUse", "NA Action", c("everything", "complete.obs", "na.or.complete", "pairwise.complete.obs")), 
        selectInput("plotOrder", "Reorder Correlation", c("original", "AOE", "FPC", "hclust", "alphabet")),
        conditionalPanel("input.plotOrder === 'hclust'",
          wellPanel(selectInput("plotHclustMethod", "Method",c("ward", "single", "complete", "average", "mcquitty", "median", "centroid")),
          numericInput("plotHclustAddrect", "Number of Rectangles", 3, 0, NA))),
        checkboxInput("corrSig", "Significance Test", FALSE),
        conditionalPanel("input.corrSig === true", numericInput("sigLevel", "Significane Level", 0.05, 0, 1, 0.01), 
                        selectInput("inSig", "Insignificant Action", c("pch", "p-value", "blank", "n"))),
        helpText("Check Logistic for Logistic Regression"),
        checkboxInput("log", "Logistic", FALSE)
      ),
    #### Machine Learning
    ########################
    menuItem("Machine Learning", tabName = "ml", icon = icon("laptop")), 
    helpText("Response Data"),
     conditionalPanel("input.sbm === 'ml'", "",
      selectInput("rdataset", "Response Data", c("dataset", "upload another")),
      conditionalPanel("input.rdataset === 'upload another'",
                      fileInput("rdatafile", ""), 
                      textInput("rdatafile_sep", "Field Seperator", value = ",")),
    helpText("Testing/Training Set"),
     selectInput("testdata", "Testing/Training Set", c("25/75 split", "upload test data")),
     conditionalPanel("input.testdata === 'upload test data'", "", fileInput("tdatafile", ""), textInput("tdatafile_sep", "Field Seperator", value = ",")),
    helpText("Feature Selection and Modeling"),
      uiOutput("r"), 
      checkboxInput("factorResp", "Factor Response Variable", FALSE),
      conditionalPanel("input.factorResp === true", textInput("factorVal", "Factor Cut off Value", value = "0")),
      selectInput("fsMethod", "Feature Selection", c("average value" = "average value", "correlation with response" = "correlation with response", "information gain" = "information gain")), 
      selectInput('method', "Model Method", c("random forest", "bayesian generalized linear","CART", "C4.5 algorithm", "bagged CART", "generalized boosted modeling")), 
      selectInput('metric', "Test Metric for Method", c("Accuracy", "Kappa", "RMSE", "Rsquared")), 
      div(style = "display:inline-block",numericInput('number', "Folds for Cross Validation", value = 3, min = 1, max = 15, step = 1, width = '150px')),
      div(style = "display:inline-block",checkboxInput("LOOCV","Leave One Out", FALSE)),
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
    ),
    menuItem("Data", tabName = 'dt', icon= icon("th")), 
    menuItem("Feature and Response Data", tabName = "frd", icon = icon("th")), 
    menuItem("Plot Data", tabName = "pd", icon = icon("th"))
   )
  ),
  #####   MAIN PANEL     
  ################################################################################################################################################################################################################  
    dashboardBody(
      tags$head(tags$style(HTML(multicol))),
      tabItems(
      tabItem( tabName = "pst",
                  fluidRow(
                    box(status = "warning", collapsible = TRUE, width = 6, height = 90, solidHeader = FALSE, 
                        h3(textOutput("p1Txt")),
                        tags$head(tags$style("#p1Txt{
                                             font-size: 25px;
                                             font-weight: bold;
                                             }"
                        ))
                    ),
                    box(status = "warning", collapsible = TRUE, width = 6, height = 90, solidHeader = FALSE, 
                    h3(textOutput("p2Txt")),
                    tags$head(tags$style("#p2Txt{
                                             font-size: 25px;
                                             font-weight: bold;
                                             }"
                   )))),
                 fluidRow(
                    box(uiOutput("plot1"), status = "primary", collapsiable = TRUE),
                    box(uiOutput("plot2"), status = "primary", collapsiable = TRUE)
                  ),
                 fluidRow(
                   box(title = "Plot 1 Labels", status = "primary", collapsible = TRUE, collapsed = TRUE, width = 6, textInput("xAxisLabel", "x-axis", value = ""), textInput("yAxisLabel", "y-axis", value = ""), 
                       textInput("title", "Title", value = ""), textInput("legend", "Legend", value = ""), selectInput("fill", "Color Pallette", c('Blues', 'Solid Blue', 'BuGn', 'BuPu', 'GnBu', 'Greens','Solid Grey', 'Greys', 'Paired', 'Set1', 'Set3', 'Spectral', 'Solid Red', 'RdBu', 'RdGy'))
                   ), 
                   box(title = "Plot 2 Labels", status = "primary", collapsible = TRUE, collapsed = TRUE, width = 6, textInput("xAxisLabel2", "x-axis", value = ""), textInput("yAxisLabel2", "y-axis", value = ""), 
                       textInput("title2", "Title", value = ""), textInput("legend2", "Legend", value = ""), selectInput("fill2", "Color Pallette", c('Blues', 'Solid Blue', 'BuGn', 'BuPu', 'GnBu', 'Greens','Solid Grey', 'Greys', 'Paired', 'Set1', 'Set3', 'Spectral', 'Solid Red', 'RdBu', 'RdGy'))
                   )
                 ),
                  fluidRow(
                    box(title = "Statistical Test 1 Results", status = "primary", collapsible = TRUE, width = 6, "Shapiro Wilks test is conducted on the independent variable (x). Two sample tests, Annova, and Kruskal-Wallis are conducted on the independent variable (x)
                                  and the grouping variable.", br(), DT::dataTableOutput("stat.res")),
                    box(title = "Statistical Test 2 Results", status = "primary", collapsible = TRUE, width = 6, "Shapiro Wilks test is conducted on the independent variable (x). Two sample tests, Annova, and Kruskal-Wallis are conducted on the independent variable (x)
                                  and the grouping variable.", br(), DT::dataTableOutput("stat.res2"))
                  )
                ),
      ####Correlation and MR
      ########################
      tabItem(tabName = "cmr",
               fluidRow(
                 box(title = "Correlation Plot Variables",status = "primary", collapsible = TRUE, width = 5, 
                     radioButtons("corrVariablesStyle", "Variable Selection Style", c("Checkbox", "Selectize"), inline = T),
                     helpText("Choose the variables to display"), 
                     conditionalPanel("input.corrVariablesStyle === 'Checkbox'",
                                      tags$div(class = 'multicol', checkboxGroupInput("corrVariablesCheckbox", "", c("Loading...")))),
                     conditionalPanel("input.corrVariablesStyle === 'Selectize'",
                                      selectizeInput("corrVariables", "", choices = c("Loading..."), multiple = T, options = list(plugins = list("drag_drop", "remove_button"))))
                 ),
                 box(title = "Correlation Plot",status = "primary", collapsible = TRUE, width = 7, plotOutput("corrPlot", width = 600, height = 600))
               ),
               
               fluidRow(
                 box(title = "Multiple Regression Independent Variables", status = "primary", collapsible = TRUE, width = 5, selectizeInput("IMRVariables", "Multiple Regression Independent Variables", choices = c("Loading..."), multiple = T, options = list(plugins = list("drag_drop", "remove_button")))),
                 box(title = "Multiple Regression Dependent Variable", status = "primary", collapsible = TRUE, width = 3, selectizeInput("DMRVariable", "Multiple Regression Dependent Variable", choices = c("Loading..."), multiple = F))
               ),
               fluidRow(
                 box(title = "Multiple Regression Independent Variables", status = "primary", collapsible = TRUE, width = 10, DT::dataTableOutput("mr"))
               )
      ),
      tabItem(tabName = "ml",
               fluidRow(
                 box(title = "Feature Variables", status = "primary", collapsible = TRUE, width = 5, 
                        radioButtons("mlvariablesStyle", "Variable Selection Style", c("Checkbox", "Selectize"), inline = T),
                        helpText("Choose the variables to display"),
                        conditionalPanel('input.mlvariablesStyle === "Selectize"', selectizeInput("mlvariables", "", choices = c("Loading..."), multiple = T, options = list(plugins = list("drag_drop", "remove_button")))),
                        conditionalPanel('input.mlvariablesStyle === "Checkbox"', tags$div( class = 'multicol', checkboxGroupInput("mlvariablesCheckbox", "", c("Loading..."))))
                 ),
                 box(title = "Feature Selection Plot", status = "primary", collapsible = TRUE, width = 7, uiOutput("fPlot1"))
               ),
               fluidRow(
                 box(title = "Importance Plot from ML Model", status = "primary", collapsible = TRUE, uiOutput("iPlot")),
                 box(title = "Model Accuracy", status = "primary", collapsible = TRUE, uiOutput("aPlot"))
               ),
               fluidRow(
                 box(title = "Model Accuracy Tables", status = "primary", collapsible = TRUE, width = NULL, DT::dataTableOutput("a1"),
                     conditionalPanel("input.addModel2 == true", DT::dataTableOutput("a2")), 
                     conditionalPanel("input.addModel3 == true", DT::dataTableOutput("a3"))
                 )
               ),
              fluidRow(
                 box(title = "ROC Curve (For Factored or Two Class Response Variables)", status = "primary", collapsible = TRUE, width = NULL, uiOutput("rPlot"))
               )
      ),
      ###### Data tabs        
      #####################
      tabItem(tabName = "dt",
               fluidRow(
                 box(title = "Data", status = "primary", collapsible = TRUE, width = NULL, DT::dataTableOutput("datasetTable"))
               ) 
      ), 
      tabItem(tabName = "frd",
               fluidRow(
                 box(title = "Feature and Response Data", status = "primary", collapsible = TRUE, width = NULL,  DT::dataTableOutput("mldataTable"))
               )
      ), 
      tabItem(tabName = 'pd',
               fluidRow(
                 box(title = "Plot 1 Data", status = "primary", collapsible = TRUE, DT::dataTableOutput("plotvar1")), 
                 box(title = "Plot 2 Data", status = "primary", collapsible = TRUE, DT::dataTableOutput("plotvar2"))
               ), 
               fluidRow(
                 box(title = "Average Value for Features ", status = "primary", collapsible = TRUE, width = NULL, DT::dataTableOutput("avgValTable"))
               ),
               fluidRow(
                 box(title = "Corrlation between Features and Response Variable", status = "primary", width = NULL, collapsible = TRUE, DT::dataTableOutput("corrValTable"))
               ), 
               fluidRow(
                 box(title = "Information Gain for Features", status = "primary", collapsible = TRUE, width = NULL, DT::dataTableOutput("igTable"))
               ),
               fluidRow(
                 box(title = "Importance Data from Machine Learning Model", status = "primary", collapsible = TRUE, width = NULL, DT::dataTableOutput("ipTable"))
               )
               
    )
    )
    )
))


server16 <- shinyServer(function(input, output, session){

  
#####   DATA    
################################################################################################################################################################################################################  
  
  #allows user to input data set or try on test set of data 
  dataset <- reactive({
    datasource <- input$dataset
    validate(
      need(input$dataset != "", "Please select a data set")
    )
    if(datasource == "upload my own") {
      inFile <- input$datafile
      validate(
          need(!is.null(inFile), "Please select a data set or input a File")
          )
        read.delim(inFile$datapath, sep = gsub("\\t", "\t", input$datafile_sep, fixed = TRUE))
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
    
    #text headers for plots 
    output$p1Txt <- renderText({paste("Plot 1:", sapply(input$plotType, simpleCap))})
    output$p2Txt <- renderText({paste("Plot 2:", sapply(input$plotType, simpleCap))})
    
    #creates a plot object from the data so can create proper d3 plots 
    plot.obj <- function(x,y,g,fg,fgv){
      plot.list<-list() 
      plot.list$data<- dataset()
      
      validate(need(!is.null(plot.list$data), "Please make sure a dataset is loaded"))
      validate(need(length(intersect(x, colnames(plot.list$data))) || length(intersect(y, colnames(plot.list$data))) || length(intersect(g, colnames(plot.list$data))), "Please select a variable in dataset"))
      plot.list$x<-with(plot.list$data, get(x))
      plot.list$freqx <- as.data.frame(table(plot.list$x))
      colnames(plot.list$freqx) <- c('x', 'Frequency')
      plot.list$y<-with(plot.list$data,get(y))
      plot.list$freqy <- as.data.frame(table(plot.list$y))
      colnames(plot.list$freqy) <- c('y', 'Frequency')
      if(fg == TRUE){
        plot.list$group <- with(plot.list$data, get(g))
        plot.list$group <- sapply(plot.list$group, as.numeric)
        above = sum(plot.list$group > fgv)
        below = sum(plot.list$group < fgv)
        if(above == 0 || below == 0){
          plot.list$group <- sapply(plot.list$group, as.numeric)
          plot.list$group <- sapply(plot.list$group, as.factor)
        }
        else{
          plot.list$group = factor(as.numeric(plot.list$group) > as.numeric(fgv), labels = c(0,1))
          plot.list$group = sapply(plot.list$group, as.numeric)
        }
      }
      else{
        plot.list$group <- with(plot.list$data, get(g))
      }
      plot.list$variables <- data.frame(plot.list$x, plot.list$y, plot.list$group)
      colnames(plot.list$variables) <- c('x','y', 'g')
      plot.list$box = na.omit(data.frame(plot.list$group, plot.list$x))
      colnames(plot.list$box) <- c('g', 'x')
      
      return(plot.list)
    }
    
    #Machine Learning Data object to run models and create plots
    ml.obj <- reactive({
      mlvariables <- input$mlvariables
      d <- dataset()
      rd <- rdataset()
      validate(need(!is.null(d) || length(intersect(mlvariables, colnames(d))), "Please make sure a data set is selected and proper variables aret set"))
      
      validate(need(length(intersect(input$r, colnames(rd))), "please select response variable in response data/dataset"))
      
      validate(need(!is.null(rd) || nrow(as.data.frame(d)) == nrow(as.data.frame(rd)), "Response data has different number of rows than the dataset selected"))
      
      ml.list <- list()
      print(input$r)
      validate(need(length(input$r) != 0, "Please make sure response variable is set"))
      validate(need(length(c(mlvariables)) != 0, "Please make sure dataset is selected and loaded"))
      if (input$r %in% c(mlvariables)){
        ml.list$features <- d[, c(!c(mlvariables) %in% input$r) ]
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
        validate(need(above != 0 || below != 0, "Please set a Factor Value that splits the data into more than one group"))
        ml.list$response.f = factor(as.numeric(ml.list$response) > as.numeric(input$factorVal), labels = c(0,1))
        ml.list$response.f = sapply(ml.list$response.f, as.numeric)
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
      if(input$testdata ==  "upload test data"){
        train.data = ml.list$featResp
        tinFile <- input$datafile
        validate(need(!is.null(tinFile), "Please select a data set or input a File"))
        test.data =  read.delim(tinFile$datapath, sep = gsub("\\t", "\t", input$tdatafile_sep, fixed = TRUE))
        
      }
      else{
        samples = row.names(ml.list$featResp)
        train.percent = .75
        inTrain = samples %in% sample(samples, floor(train.percent*length(samples)))
        train.data = droplevels(ml.list$featResp[inTrain,])
        test.data = droplevels(ml.list$featResp[!inTrain,])
      }
      
      
      ml.list$train.data <- train.data
      ml.list$test.data <- test.data
      
      
      response.name = paste("Response-", input$r, sep = "")
      colnames(ml.list$featResp) <- c(features, response.name)
      
      return(ml.list)
    })
    
    
    ##### Data Plots and Statistical Tests  
    ################################################################################################################################################################################################################      
    
    require(ggplot2)
    require(plotly)
    
    #Plot Data
    output$plot1 <- renderUI({
      plotlyOutput("p1", height = 500)
    })
    
    output$plot2 <- renderUI({
      plotlyOutput("p2", height = 500)
    })
    
    output$p1 <- renderPlotly({
      validate(need(input$dataset != "", "Please select a data set"))
      p1 <- plot.Type(input$plotType, input$x, input$y, input$group, input$bins, input$factorG, input$factorGVal, input$showPoints, input$xAxisLabel, input$yAxisLabel, input$legend, input$title, input$fill, input$addTrend)
      print(p1)
      
    })
    
    output$p2 <- renderPlotly({
      validate(need(input$dataset != "", "Please select a data set"))
      p2 <- plot.Type(input$plotType2, input$x2, input$y2, input$group2, input$bins2, input$factorG2, input$factorGVal2, input$showPoints2, input$xAxisLabel2, input$yAxisLabel2, input$legend2, input$title2, input$fill2, input$addTrend2)
      print(p2)
      
    })
    
    plot.Type<-function(pt, x, y , g, bins, fg, fgv, ip, xa, ya, gl, t, c, at){
      plot.df <- plot.obj(x,y,g, fg, fgv)
      .theme <- theme(axis.line = element_line(colour = 'gray', size = .75), panel.background = element_blank(), plot.background = element_blank(), legend.position="right")  
      xaxisDf = switch(pt, "single boxplot" = x,
                       "boxplot" = paste(x, "by", g),
                       "histogram" = x, 
                       "density" = x, 
                       "multibar" = x,
                       "bar" = x,
                       "scatter" = x,
                       "groupedscatter" = x
      )
      yaxisDf = switch(pt, "single boxplot" = "",
                       "boxplot" = "",
                       "histogram" = paste('Frequency of', y), 
                       "density" = "Density", 
                       "multibar" = y,
                       "bar" = "Count",
                       "scatter" = y,
                       "groupedscatter" = y
      )
      legend = gl
      title = t
      colourCount  = switch(pt, "single boxplot" = 1,
                            "boxplot" = length(table(plot.df$box$g)),
                            "histogram" = 1, 
                            "density" = length(table(plot.df$variables$x)), 
                            "multibar" = length(table(plot.df$variables$g)),
                            "bar" = length(table(plot.df$variables$x)),
                            "scatter" = length(table(plot.df$variables$x)),
                            "groupedscatter" = length(table(plot.df$variables$x))
      )
      solid = ifelse(c == "Solid Blue" || c == "Solid Grey" || c == "Solid Red", TRUE, FALSE )
      if(solid == FALSE){
        getPalette = colorRampPalette(brewer.pal(9, c))
      }else{
        if(c == "Solid Blue"){ getPalette = brewer.pal(9, "Blues")[8]}
        if(c == "Solid Grey"){ getPalette = brewer.pal(9, "Greys")[6]}
        if(c == "Solid Red"){ getPalette = brewer.pal(9, "Reds")[7]}
      }
      
      xaxis = ifelse(xa == "", xaxisDf, xa)
      yaxis = ifelse(ya == "", yaxisDf, ya)
      
      switch(pt,
             "single boxplot" = {
               if(solid == FALSE){ fill = getPalette[7]}
               else{fill = getPalette}
               if(ip == TRUE){
                 gg <-ggplot(plot.df$box, aes(x = as.factor(0), y = x)) + geom_boxplot() + geom_point(position = "jitter", alpha = 0.5) 
                 gg <- gg + labs( fill = fill, x = xaxis, y = yaxis)  + ggtitle(title) + .theme + guides(fill=guide_legend(nrow=2))
                 p <- ggplotly(gg)
               }
               else{
                 gg <-ggplot(plot.df$box, aes(x = as.factor(0), y = x)) + geom_boxplot() 
                 gg <- gg + labs( fill = fill, x = xaxis, y = yaxis) + ggtitle(title) + .theme + guides(fill=guide_legend(nrow=2))
                 p <- ggplotly(gg) 
               }
             },
             "boxplot" = {
               if(ip == TRUE){
                 gg <-ggplot(plot.df$box, aes(x = as.factor(g), y = x, fill = as.factor(g))) + geom_boxplot() + geom_point(position = "jitter", alpha = 0.5) 
                 gg <- gg + labs(fill = legend, x = xaxis, y = yaxis) + scale_fill_manual(values = getPalette(colourCount)) + ggtitle(title) + .theme + guides(fill=guide_legend(nrow=2))
                 p <- ggplotly(gg) 
               }
               else{
                 gg <-ggplot(plot.df$box, aes(x = as.factor(g), y = x, fill = as.factor(g))) + geom_boxplot() 
                 gg <- gg + labs(fill = legend , x = xaxis, y = yaxis) + scale_fill_manual(values = getPalette(colourCount)) + ggtitle(title) + .theme + guides(fill=guide_legend(nrow=2))
                 p <- ggplotly(gg) 
               }
             },
             "histogram" = {
               if(solid == FALSE){ fill = getPalette[7]}
               else{fill = getPalette}
               stat_bin.test <- stat_bin(data = plot.df$variables, aes (x = x))
               print(stat_bin.test$stat_params$bins)
               validate(need(!is.null(stat_bin.test$stat_params$bins), "Histograms are for Continuous Variables, consider using a bar chart"))
               gg <-ggplot(plot.df$variables, aes(x = x)) + geom_histogram(alpha=0.5, position= "identity", fill = fill, bins = bins)
               gg <- gg + labs( x = xaxis, y = yaxis) + ggtitle(title) + .theme + guides(fill=guide_legend(nrow=2)) 
               p <- ggplotly(gg)
             },
             "density" = {
               gg <-ggplot(plot.df$variables, aes(x = x, fill = as.factor(x), group 	= as.factor(x))) + geom_density(alpha=.75)
               gg <- gg + labs(fill = legend, x = xaxis, y = yaxis) + scale_fill_manual(values = getPalette(colourCount)) + ggtitle(title) + .theme + guides(fill=guide_legend(nrow=2))
               p <- ggplotly(gg) 
             },
             "multibar" = {
               gg <- ggplot(plot.df$variables, aes(x = x, fill = as.factor(g), group = as.factor(g))) + geom_bar(position = "dodge") 
               gg <- gg + labs(fill = legend, x = xaxis, y = yaxis) + scale_fill_manual(values = getPalette(colourCount)) + ggtitle(title) + .theme + guides(fill=guide_legend(nrow=2))
               p <- ggplotly(gg) 
             },        
             "bar" =	{
               if(solid == FALSE){ 
                 fill = as.factor(x)
                 print(fill)
                 scale = getPalette(colourCount)
                 print(scale)
               }
               else{
                 fill = x
                 scale = getPalette
               }
               gg <-ggplot(plot.df$variables, aes(x = x, group = as.factor(x), fill = fill)) + geom_bar(position= "dodge")
               gg <- gg + labs( x = xaxis, y = yaxis) + scale_fill_manual(values = scale) + ggtitle(title) + .theme + guides(fill=guide_legend(nrow=2))
               p <- ggplotly(gg) 
             },
             "grouped scatter" = {
               gg <- ggplot(plot.df$variables, aes(x = x, y = y, group = g ,color = as.factor(g))) + geom_point() 
               gg <- gg + labs( color = legend, x = xaxis, y = yaxis) + scale_fill_manual(values = getPalette(colourCount)) + ggtitle(title) + .theme + guides(fill=guide_legend(nrow=2))
               if(at == TRUE){
                 gg <- gg + stat_smooth()
               }
               p <- ggplotly(gg) 
             },
             "scatter" ={
               gg <-ggplot(plot.df$variables, aes(x = x, y = as.factor(y))) + geom_point(color = c)
               gg <- gg + labs( x = xaxis, y = yaxis) + scale_fill_manual(values = getPalette(colourCount)) +  ggtitle(title) + .theme + guides(fill=guide_legend(nrow=2))
               if(at == TRUE){
                 gg <- gg + stat_smooth()
               }
               p <- ggplotly(gg) 
             }
      )
      
      return(p)
    }
    
    #statistical Testing
    stat.tests <- function(st, sig, x, y, g, fg, fgv){
      var.df <- plot.obj(x,y,g, fg, fgv)
      if(!is.null(var.df$variables)){
        groups = cbind.data.frame(as.numeric(var.df$x), droplevels(as.factor(var.df$group)))
        colnames(groups) <- c('x','g')
        conf.level = 1 - as.numeric(sig)
        p.val = sig
        x.st = as.numeric(var.df$x)
        x.g.sort <- groups[order(groups$g),]
        colnames(x.g.sort) <- c("x", "g")
        x.g.sort$x <-as.numeric(x.g.sort$x)
        
        t.dat <- switch(st, 
                        "Shapiro-Wilk" = {t <- shapiro.test(x.st)
                        tab <- cbind(t$statistic, t$p.value)
                        colnames(tab) <- c("W-statistic", "P-value")
                        row.names(tab) <- c("Shapiro-Wilk")},
                        "Two sample t-test" = {validate(need(nlevels(groups$g) == 2, "A t-test requires exactly two groups, consider using Annova or factoring the grouping variable"))
                          t <- t.test(x ~ g, conf.level = conf.level, data = groups)
                          tab <- cbind(t$statistic, t$p.value)
                          colnames(tab) <- c("T-statistic", "P-value")
                          row.names(tab) <- c("Welch Two Sample t-test")}, 
                        "Wilcox rank sum - two sample" = {validate(need(nlevels(groups$g) == 2, "A Wilcox rank sum test requires exactly two groups, consider using Annova or factoring the grouping variable"))
                          t <- wilcox.test(x ~ g, conf.level = conf.level, data = groups)
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
    
    output$stat.res <- DT::renderDataTable(stat.tests(input$statTest, input$sig, input$x, input$y, input$group, input$factorG, input$factorGVal), options = list(
      autoWidth = FALSE, scrollX=TRUE), class = 'table-bordered table-condensed table-striped table-compact',caption = paste(sapply(input$statTest, simpleCap), "Results" ))
    
    output$stat.res2 <- DT::renderDataTable(stat.tests(input$statTest2, input$sig2, input$x2, input$y2, input$group2,  input$factorG2, input$factorGVal2), options = list(
      autoWidth = FALSE, scrollX=TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = paste(sapply(input$statTest2, simpleCap), "Results" ))
    
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
        updateSelectInput(session, "IMRVariables", choices = numericColumns(), selected = numericColumns())
    })
    
    observe({
      updateSelectInput(session, "IMRVariables", selected = input$corrVariablesCheckbox)
    })
    
    observe({
        updateSelectInput(session, "DMRVariable", choices = c(input$corrVariablesCheckbox))
    })
    
    observe({
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
      validate(need(input$dataset != "", "Please select a data set"))
      if(!length(intersect(corrvariables, colnames(data)))) {
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
      sig.level = if(input$corrSig) {input$sigLevel} else {NULL}
      in.sig = if(input$corrSig) {input$inSig} else {NULL}

      
      #options for correlation plot     
      corrplot::corrplot(corrvals, method = "circle", order = order, p.mat = p.mat , sig.level = sig.level, insig = in.sig , tl.pos = "lt")
      
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
    mult.reg <- function(dvmr, cv, log){
      if(is.null(cv) || is.null(dvmr)){return(NULL)}
      fam = ifelse(log, 'binomial', 'gaussian')
      y.df = dataset()[,dvmr]
      x.df = dataset()[,c(cv[!cv %in% dvmr])]
      mr.df <- cbind.data.frame(x.df, y.df)
      colnames(mr.df) <- c(c(cv[!cv %in% dvmr]), 'y')
      fit.mr <- glm(y~., family = fam , data = mr.df )
      result.mr <- summary(fit.mr)$coefficients
      return(result.mr)
    }
    
    output$mr <- DT::renderDataTable({
      validate(need(input$DMRVariable != 0, "Please make sure a dataset is selected and the indpendent and dependent variables are set"))
      validate(need(input$IMRVariables != 0, "Please make sure a dataset is selected and the indpendent and dependent variables are set"))
      mr.df <- as.data.frame(mult.reg(input$DMRVariable, input$IMRVariables, input$log))
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
      return(fp)
    }
    require(rCharts)
    options(RCHART_WIDTH = 400)
    
    #plot Importance
    output$iPlot <- renderUI({
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
        ip1 <- var.imp(input$method, input$metric, input$number, input$repeats, input$LOOCV)[[2]]
        print(ip1)
      }
      else{
        return(Highcharts$new())
      }
      
    })
    
    
    output$ip.bgl <- DT::renderDataTable({
      if(input$method == "bayesian generalized linear"){
        df <- var.imp(input$method, input$metric, input$number, input$repeats, input$LOOCV)[[2]] 
        dt <- datatable(df, options = list(autoWidth = FALSE, scrollX=TRUE), 
                       class = 'table-bordered table-condensed table-striped table-compact', caption = "Bayesian Generalized Linear Regression Results") %>%
          formatStyle('Pr(>|t|)', color = styleInterval(0.05, c('red','black')), backgroundColor = styleInterval(0.05, c('yellow', 'white'))) %>% 
          formatRound(1:length(df), 5)
        return(dt)
      }
    })
    
    output$ip.rp <-renderPlot({
      if(input$method == "CART"){
        ip.rp.list <- var.imp(input$method, input$metric, input$number, input$repeats, input$LOOCV)
        ip.rp <- ip.rp.list[[2]]
      }
    })
           
    #model based on input 
    ml.model <- function(mt, m, n, r, ilo){
      mlo <- ml.obj()
      nt = 1

      colnames(mlo$train.data)[ncol(mlo$train.data)] <- "response"
      train.data = mlo$train.data

      #Control for Cross Validation and Repeats
      ifelse(ilo == TRUE, number <- "LOOCV", number <- n)
      print(number)
      repeats <-r
      metric <- m
      control <- trainControl(method="repeatedcv", number=number, repeats=repeats)

      withProgress(message = 'Machine Learning in progress', value=nt, {
          incProgress(nt/6, detail = paste(nt, input$method))

      #model 
      set.seed(7)
      model <- switch(mt, "random forest" = train(as.factor(response) ~., data = train.data,  method ="rf", metric=metric, trControl= control), 
                          "bayesian generalized linear" = train(response ~., data=na.omit(train.data), method= "bayesglm", metric = metric, trControl = control),
                          "CART" = train(as.factor(response)~., data=na.omit(train.data), method="rpart", metric=metric, trControl=control),
                          "C4.5 algorithm" =  train(as.factor(response)~., data=na.omit(train.data), method="C5.0", metric=metric, trControl=control),
                          "bagged CART" = train(response~., data=na.omit(train.data), method="treebag", metric="RMSE", trControl=control),
                          "generalized boosted modeling" = train(response~., data=na.omit(train.data), method="gbm", metric=metric, trControl=control, verbose=FALSE)
                      )
      nt = nt + 1
      incProgress(nt, detail = paste("next step"))
      })
      return(model)
    }
  
    #Importance plot or linear model summary based on input 
    var.imp <- function(mt, m, n, r, ilo){
      fit <- ml.model(mt, m, n, r, ilo)
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
        return(df_ip)
      }
        
    
    output$a1 <- DT::renderDataTable({
      validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
      a1.t <- accuracy(input$method, input$metric, input$number, input$repeats, input$LOOCV)[[1]]
      print(a1.t)
      a1.df <- as.list.data.frame(a1.t)
      a1 <- datatable(a1.df, options = list(
        autoWidth = FALSE), class = 'table-bordered table-condensed table-striped table-compact', caption = paste("Confusion Matrix with Test Data for Model:", input$method))
      return(a1)
    })
    output$a2 <- DT::renderDataTable({
      validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
      a2.t <- accuracy(input$method2, input$metric2, input$number, input$repeats, input$LOOCV)[[1]]
      a2.df <- as.list.data.frame(a1.t)
      a2 <- datatable(a2.df, options = list(
        autoWidth = FALSE), class = 'table-bordered table-condensed table-striped table-compact', caption = paste("Confusion Matrix with Test Data for Model:", input$method))
      return(a2)
    })
    output$a3 <- DT::renderDataTable({
      validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
      a3.t <- accuracy(input$method3, input$metric3, input$number, input$repeats, input$LOOCV)[[1]]
      a3.df <- as.list.data.frame(a3.t)
      a3 <- datatable(a3.df, options = list(
        autoWidth = FALSE), class = 'table-bordered table-condensed table-striped table-compact', caption = paste("Confusion Matrix with Test Data for Model:", input$method))
      return(a3)
    })

    
    output$rPlot <- renderUI({
      plotOutput("rp")
    })

    require(rCharts)
    options(RCHART_WIDTH = 400)
    
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
        ap.list <- accuracy(input$method, input$metric, input$number, input$repeats, input$LOOCV)
        ap1 <- ap.list[[2]]
      }
      else{
        return(Highcharts$new())
      }
    })

    output$ap.rf <- DT::renderDataTable({
      if(input$method == "random forest"){
        rf.list <- accuracy(input$method, input$metric, input$number, input$repeats, input$LOOCV)
        rf.df <- rf.list[[2]]
        dt.rf <- datatable(rf.df, options = list(autoWidth = FALSE, scrollX=TRUE), 
                       class = 'table-bordered table-condensed table-striped table-compact', caption = "Random Forest Confusion Matrix")
        return(dt.rf)
      }
    })
      
    output$ap.c50 <- renderPlot({
      if(input$method == "C4.5 algorithm"){
        ap.list <- accuracy(input$method, input$metric, input$number, input$repeats, input$LOOCV)
        ap.c50 <- plot(ap.list[[2]], main = "Accuracy vs Number of Baggs")
      }

    })
    

    # Plot info TPR/FPR ROC with diagonal line and legend
    output$rp <- renderPlot({
      perf1 <- roc("random forest", "Accuracy", input$number, input$repeats, input$LOOCV)
      if(is.null(perf1)) return(NULL)
      perf2 <- roc("CART", "Accuracy", input$number, input$repeats, input$LOOCV)
      if(is.null(perf2)) return(NULL)
      perf3 <- roc("C4.5 algorithm", "Accuracy", input$number, input$repeats, input$LOOCV)
      if(is.null(perf3)) return(NULL)  
      rp <- {plot(perf1, lwd = 5, col = 'blue', yaxt='n', xaxt='n', ann=F )
        abline(a = 0, b= 1)
        lines(perf2@x.values[[1]], perf2@y.values[[1]], col = 'red', lwd = 1.5)
        lines(perf3@x.values[[1]], perf3@y.values[[1]], col = 'green', lwd = 1.5)
        legend("bottomright", "(x,y)", c("Model 1: Random Forest", "Model 2: CART", "Model 3: C4.5 algorithm"), lwd=2, lty=1,col=c('blue','red','green'), cex = 0.85 )}
    
      return(rp)
  })
              
    #ROC curve 
    roc <- function(mt, m, n, r, ilo){
      fit <- ml.model(mt, m, n, r, ilo)
      mlo <- ml.obj()
      
      test.data = mlo$test.data
      twoClass = nlevels(as.factor(test.data$response))
      validate(need(twoClass == 2, "The ROC plot requires a response variable with two classes only"))
      
      #get predicted values, predictions, and performance for ROC
      Predicted = predict(fit, test.data[,(1:ncol(test.data)-1)], type = 'prob')
      pred <- prediction(Predicted[,2], as.data.frame(test.data$response), label.ordering = NULL)
      perf <- performance(pred, 'tpr', 'fpr')
      
      return(perf)
    }
    
    accuracy<- function(mt, m, n, r, ilo){
      mlo <- ml.obj()
      test.data = mlo$test.data
      fit <- ml.model(mt, m, n, r, ilo)
      switch(mt,
            "random forest" = {
                 ap = fit$finalModel$confusion
                 pred = predict(fit, test.data[,1:ncol(test.data)-1])
                 actual =  test.data$response
                 a = table(pred, actual)
            }, 
            "bayesian generalized linear" = { 
               pred = predict(fit, test.data[,1:ncol(test.data)-1])
               actual =  test.data$response
               a = table(pred, actual)
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
               pred = predict(fit, test.data[,1:ncol(test.data)-1])
               actual =  test.data$response
               a = table(pred, actual)
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
              pred = predict(fit, test.data[,1:ncol(test.data)-1])
              actual =  test.data$response
              a = table(pred, actual)
              ap = fit
            }, 
            "bagged CART" = { 
              pred = predict(fit, test.data[,1:ncol(test.data)-1])
              actual =  test.data$response
              a = table(pred, actual)
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
              pred = predict(fit, test.data[,1:ncol(test.data)-1])
              actual =  test.data$response
              a = table(pred,actual)
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
    
    
    output$datasetTable <- DT::renderDataTable(as.data.frame(dataset()), options = list(autoWidth = TRUE, scrollX=TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Dataset" )
    output$mldataTable <- DT::renderDataTable(mlTables('featResp'), options = list(autoWidth = TRUE, scrollX=TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Feature and Response variable Matrix" )
    output$avgValTable <- DT::renderDataTable(mlTables('avgVal'), options = list(autoWidth = TRUE, scrollX=TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Average Column Values for Feature Variables" )
    output$corrValTable <- DT::renderDataTable(mlTables('corrVal'), options = list(autoWidth = TRUE, scrollX=TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Correlation of Feature Variables with Response Variable" )
    output$igTable <- DT::renderDataTable(mlTables('ig'), options = list(autoWidth = TRUE, scrollX=TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Information Gain for Feature Variables" )
    output$ipTable <- DT::renderDataTable(var.imp(input$method, input$metric, input$number, input$repeats, input$LOOCV)[[1]], options = list(autoWidth = FALSE, scrollX=TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Importance Plot Data" )
    output$plotvar1 <- DT::renderDataTable({
      p.df1 <- plot.obj(input$x, input$y, input$group, input$factorG, input$factorGVal)
      p.df1.xyg <- cbind.data.frame(p.df1$variables, p.df1$group)
      colnames(p.df1.xyg) <- c(input$x,input$y, input$group)
      p.dT1 <- datatable(p.df1.xyg, options = list(autoWidth = TRUE, scrollX=TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Independent, Dependent, and Group Variable Data for Left Plot" )
      return(p.dT1)
    })
    output$plotvar2 <- DT::renderDataTable({
      p.df2 <- plot.obj(input$x2, input$y2, input$group2, input$factorG2, input$factorGVal2)
      p.df2.xyg <- cbind.data.frame(p.df2$variables, p.df2$group)
      colnames(p.df2.xyg) <- c(input$x2,input$y2, input$group2)
      p.dT2 <- datatable(p.df2.xyg, options = list(autoWidth = TRUE, scrollX=TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Independent, Dependent, and Group Variable Data for Right Plot" )
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


shinyApp(ui = uiR, server = server16)


