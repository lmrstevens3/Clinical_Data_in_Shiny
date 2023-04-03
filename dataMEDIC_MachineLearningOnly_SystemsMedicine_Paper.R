library(shiny)
library(markdown)
library(ggplot2)
library(plotly)
library(htmltools)
library(plyr)
library(dplyr)
library(DT)
library(shinydashboard)
library(shinyFiles)
library(survival)
library(ggfortify)
library(sparkline)
library(htmlwidgets)
source('/Users/laurastevens/Dropbox/Graduate School/Harmonization/Harmonization_Chris/user_interface/Harmonization UI/Harmonization Supporting Functions.R')
source('/Users/laurastevens/Dropbox/Graduate School/Harmonization/Harmonization_Chris/user_interface/Harmonization UI/RegressionModels.R')


ui <- shinyUI(
  # Application title
  dashboardPage(
    dashboardHeader(title = "ML-MEDIC", titleWidth = 400), 
    dashboardSidebar(disable = TRUE), 
    dashboardBody(
      tags$head(#adds Y scroll bar when needed and sets up dismiss and menu buttons at top
        tags$style(".buttOrder{background-color:#4D95C3;} .buttOrder{color: white;} .buttOrder{height: 30px}",
                  ".buttDismiss{background-color:#FF9C04;} .buttDismiss{color: white;}  .buttDismiss{height: 30px}"
                  )#close tags style
     ), #close tags head
     h3(helpText(div(style= "font-style: italic; font-size = 20px; font-color: #D54525;",'medical exploration and data inspired care'))), 
      tabsetPanel(
        tabPanel(title = "Data", icon = icon("th"),
          ####################data Overview and pick harmonized variables
          ####################
          fluidRow(
            column(width = 4,
                   box(title = "Harmonized Variables", width = NULL, status = "primary", collapsible = TRUE, solidHeader = F, 
                       selectizeInput("HarmCols","Harmonized Variables", choices = ('Loading...'), options = list(placeholder = "Please select a harmonized variable"), multiple = T))
                    ),#end column
                   column(width= 8, 
                        box(title = "Harmonized Data Overview", width = NULL, status = "primary", collapsible = TRUE, solidHeader = F, getDependency('sparkline'), DT::dataTableOutput("dataOverview"))#end column 
                  )#end column
          ), #end row
          #########Ribbon for editing options
          ##################
          fluidRow(
                  div(style = 'display:inline-block', shinySaveButton("save", "Save Dataset", "Save file as ...", filetype=list(csv="csv"), class = "buttOrder")),
                  div(style = 'display:inline-block', actionButton('removeNAButton', "Remove All Missing Values", class = "buttDismiss"))
          ), #end row
            ####Data
            ########################
          br(),
          fluidRow(
              box(title = "Data", status = "primary", collapsible = TRUE, width = NULL, solidHeader = F, div(style = 'overflow-x: scroll', DT::dataTableOutput("cleanDataTable")))
          )#end row
        ),#end tab
    #######################
    #regresssion
      tabPanel(title = "Build Model", icon = icon("line-chart"),
              fluidRow(column(width = 6, div(style = "font-size: 25px; font-style: italic;", h1("Regression Analysis"))),
                       column(width = 6, selectizeInput("RegStudies", "Studies Included In Regression Analysis", choices = c("Loading..."), multiple  = T, width = '300px'))
                       ),# end row
              br(),
              fluidRow(
                column(width = 2,
                  checkboxInput("log", "Check for Logistic Regression", FALSE),
                  selectizeInput("IMRVariables", "Regression Independent Variables", choices = c("Select Below..."), multiple = T, options = list(placeholder = "Waiting for data...", plugins = list("drag_drop", "remove_button"))),
                  selectizeInput("DMRVariable", "Regression Dependent Variable", choices = c("Select Below..."), multiple = F),
                  numericInput("regPvalue", "Regression P.value", 0.05, 0, 1, 0.01),
                  actionButton("regButton", "Run Regression", class = 'buttOrder')
                ), #end column
                column(width = 5, uiOutput("mrForestPlot")),
                column(width = 5, uiOutput("mr"))
              ) #end row
        ), #end tab
    ######SURVIVAL######
    ########################
      tabItem(tabName = "Training and Implementation",
          fluidRow(column(width = 4,div(style = "font-size: 25px; font-style: italic;", h1("Survival Analysis"))),
                   column(width = 8, selectizeInput("survStudies", "Studies Included In Survival Analysis", choices = c("Loading..."), multiple  = T))),
          br(),
        fluidRow(
          column(width = 2,
             selectizeInput("survVariables", "Survival Input Variables", choices = c("Waiting for data..."), multiple = T, options = list(plugins = list("drag_drop", "remove_button"))),
             selectizeInput("survOutVariable", "Outcome Variable", choices = c("Waiting for data..."), multiple = F),
             selectizeInput("survTimeVariable", "Time to Event Variable", choices = c("Please select a variable below..."), multiple = F),
             numericInput("survPvalue", "Survival P.value", 0.05, 0, 1, 0.01),
             actionButton("survButton", "Run Survival", class = 'buttOrder')
          ),# end column
             column(width = 5,  uiOutput("coxForestPlot")),
             column(width = 5, uiOutput("cox"))
          ),#end row
        fluidRow(uiOutput('survPlots'))
      ), # end tab
      ####Stats
      ########################
      #########Ribbon for stats
      tabPanel(title = "Testing and Evaluation", icon = icon("bar-chart"),
             fluidRow(column(width = 4,div(style = "font-size: 25px; font-style: italic;", h1("Statistical Hypothesis Testing and Tables"))), 
                      column(width = 8, selectizeInput("statsStudies", "Studies Included In Statistical Analysis", choices = c("Loading..."), multiple  = T))
                      ),
             br(),
             fluidRow(
               helpText("Shapiro Wilks test is conducted on the independent variable (x). Two sample tests, Annova, and Kruskal-Wallis are conducted on the independent variable (x)
                        and the grouping variable.")
               ),#end row
             br(),
            fluidRow(
              column(width = 2,
                     #conditionalPanel("input.HarmCols === 'Loading...'", uiOutput("SelectHarmColsTxt")),
                  selectizeInput("statTest", "Statistical Tests", c("", "Shapiro-Wilk",  "Two sample t-test", "Paired t-test", "Wilcox rank sum - two sample", "Annova", "Kruskal–Wallis", "Chi-Squared", "Fisher's Exact"), selected = ""),
                  selectizeInput("var1","Variable 1:", choices = c("Please Select Below..."), selected = "", options = list(plugins = list("drag_drop")), multiple = F),
                  selectizeInput("var2","Variable 2/Grouping Variable:", choices = c("Please Select Below..."), options = list(plugins = list("drag_drop")), multiple = F),
                  numericInput('sig', "Significance Threshold", 0.05, 0, 1, 0.001)
              ),#end column
              column(width = 5,  uiOutput("statsPlots")),
              column(width = 5, uiOutput("stat.res"))
            )# end row
          ), # end tab 
    tabPanel(title = "Table One", icon = icon("th"),
             fluidRow(br(), div(style = "font-size: 25px; font-style: italic;", h1("Table One"))),
             fluidRow(
                selectizeInput("tableOneVariables", "Table One Variables", choices = c("Waiting for data..."), multiple = T, options = list(plugins = list("drag_drop", "remove_button"))),
                selectizeInput("tableOneResponse", "Table One Outcome Variable", choices = c("Waiting for data..."), multiple = F)
              ), #end column 
             fluidRow(
               column(width = 9, div(style = 'overflow-x: scroll', DT::dataTableOutput("tableOne"))),
               column(width = 3, 
                      box(title = "Change Table One Column Names", status = "primary", collapsible = TRUE, width = NULL, 
                          uiOutput("tableOneColNamesChange"), uiOutput("tableOneNewColName"), actionButton("tableOneColNamesChangeButton", "Change")),
                      box(title = "Change Table One Row Names", status = "primary", collapsible = TRUE, width = NULL, 
                          uiOutput("tableOneRowNamesChange"), uiOutput("tableOneNewRowName"), actionButton("tableOneRowNamesChangeButton", "Change"))
               )#end column
             )#end row
          )# end panel
      )#end tab set panel
    )#end dashboard body
  )#end dashboard page
)#end ui

server <- shinyServer(function(input, output, session){  
 
  harmonizedData <- reactive({
    harmonizedData <- read.csv('~/Dropbox/Graduate School/ADAPT-HF/all_HarmonizedSampleDataset.csv')
  })
  
  
  observe({
    harmonizedData <- read.csv('~/Dropbox/Graduate School/ADAPT-HF/all_HarmonizedSampleDataset.csv')
    updateSelectizeInput(session, "HarmCols", choices = colnames(harmonizedData), options = list(placeholder = "Please select a harmonized variable", plugins = list("drag_drop", "remove_button")))
  })
  
  #allows user to input data set or try on test set of data 
  options(shiny.maxRequestSize = 50*1024^2)
  values <- reactiveValues(cleanedData = NULL, tableOne = NULL)
  
  observe(if(length(input$HarmCols == 0)){values$cleanData <- NULL})
  
  dataset <- reactive({
    validate(need(length(input$HarmCols) > 1, "Please select atleast two harmonized variable to make a dataset"))
    if(length(values$cleanedData) > 0 && isTRUE(all.equal(colnames(values$cleanedData), input$HarmCols))){
      dataset <- values$cleanedData
    }
    else{
      startData <- harmonizedData()
      startData <- startData[,input$HarmCols]
      values$cleanedData <- startData
      dataset <- startData
    }
    return(dataset)
  })
  
  #Data
  ################################################################################################################################################################################################################
  datanames <- reactive({
    col = colnames(dataset())
    row = rownames(dataset())
    datanames = c(list(col),list(row))
    return(datanames)
  })
  
  
  output$SelectHarmColsTxt <- renderUI({
    tags$style('font-size: 25px;', 'font-weight: bold;', 'color: red;', h1("Please select at least two harmonized variables below to create a dataset"))
  })
  
  output$dataTypeText <- renderUI({
    str1 <- "To make columns  categorical,"
    str2 <- "character, or numeric, select them,"
    str3 <- "and hit the appropriate button"
    HTML(paste(str1, str2, str3, sep = '<br/>'))
  })
  
  output$colNamesChange <- renderUI({
    obj <- dataset()  
    var.opts <- colnames(obj)
    selectizeInput("colNamesChange", "Select Column Label to Change", choices = var.opts) # uddate UI          
  })
  
  output$newColName <- renderUI({
    textInput( "newColName", "New Column Label")
  })
  
  #get numeric data
  numericColumns <- reactive({
    df <- dataset()
    num <- colnames(df)[sapply(df, is.numeric)]
    fact <- colnames(df)[sapply(df, is.factor)]
    numericColumns <- unique(c(num,fact))
    return(numericColumns)
  })
  
  observeEvent(input$removeNAButton, {
    cleanedData <- dataset()
    cleanedData = na.omit(cleanedData)
    values$cleanedData <- cleanedData
    print(dim(values$cleanedData))
  })
  
  observeEvent(input$colNamesChangeButton,{
    colNameChange  = input$colNamesChange
    newColName = input$newColName
    index = which(colnames(values$cleanedData) %in% colNameChange)
    colnames(values$cleanedData)[index] = newColName
  })
  
  
  observe({
    volumes <- c("wd"=".")
    shinyFileSave(input, "save", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save)
    data <- values$cleanedData
    if (nrow(fileinfo) > 0) {
      write.csv(data, as.character(fileinfo$datapath))
    }
  })
  
  staticRender_cb <- JS('function(){debugger;HTMLWidgets.staticRender();}')
  
  output$dataOverview <- DT::renderDataTable({
    
    data <- harmonizedData()
    bcc <- getBarChartCategories(data)
    #make NA data and get all counts data
    sparkLineData <- do.call("rbind", bcc)
    NAcolSum = apply(data, 2, function(x) sum(is.na(x)))
    
    #overall info for caption
    dataDim = dim(data)
    NASum = sum(is.na(data))
    
    #get na info
    naDf <- cbind.data.frame(colnames(data), NAcolSum)
    colnames(naDf) <- c("Var", "Missingness")
    captionText = paste0("Data Dimensions: ", dataDim[1], " X ", dataDim[2], " Total Missing Values: ", NASum)
    
    #get frequency from tabulated data
    sld = sparkLineData%>%
      group_by(Var) %>%
      summarize(Freq = spk_chr(Freq, type = "bar", barWidth = 12, chartRangeMin = 0, highlightColor = 'orange'))
    
    #merge all data together and plot
    dataOverview <- merge(naDf, sld, by = 'Var')
    dt <- spk_add_deps(datatable(dataOverview, colnames = c('Variable Name', 'Missing Data', 'Count'), rownames = F, escape = FALSE, class = 'cell-border stripe hover condensed',
                                                                    options = list(columnDefs = list(list(className = 'dt-center', targets = c(0,2))), drawCallback = staticRender_cb),
                                                                    caption = htmltools::tags$caption(style = 'text-align: center; color: grey', captionText)) %>% formatStyle("Missingness", 
                                                                                                                                                                               background = styleColorBar(range(dataOverview$Missingness), 'lightblue'),
                                                                                                                                                                               backgroundSize = '98% 50%',
                                                                                                                                                                               backgroundRepeat = 'no-repeat',
                                                                                                                                                                               backgroundPosition = 'center'))
    return(dt)
    })
  
  
  output$cleanDataTable <- DT::renderDataTable(dataset(), escape = FALSE, server = T, selection = list(target = 'row+column'),  extensions = c('Buttons', 'ColReorder'), 
                                               options = list(dom = 'BRrltpi', autoWidth = FALSE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, 
                                                              buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'))), class = 'table-bordered table-condensed table-striped table-compact')
  
 
  #Regression 
  ################################################################################################################################################################################################################                                                                                                                                                              
  observe({
    updateSelectizeInput(session, "IMRVariables", choices = c("", numericColumns()))
  })
  
  observe({
    updateSelectizeInput(session, "DMRVariable", choices = c("", numericColumns()))
  })
  
  observe({
    harmData <- harmonizedData()
    studies <- names(table(harmData$study))
    updateSelectizeInput(session, "RegStudies", choices = c(studies), selected = c(studies))
  })

  mr.df <- eventReactive(input$regButton, {
    validate(need(length(input$HarmCols) > 1, "Please select atleast two harmonized variable, a input variable, and an depenent variable"))
    validate(need(input$DMRVariable != "", "Please make sure  the indpendent and dependent variables are set"))
    validate(need(input$IMRVariables != "", "Please make sure the indpendent and dependent variables are set"))
    hd <- harmonizedData()
    hd <- hd[,c(input$HarmCols, "study")]
    model = 'linear'
    if(input$log){
      validate(need(all.equal(levels(as.factor(hd[,dvmr])), c(0,1)), "Please make sure the outcome variable has values 0 and 1 for logistic regression"))
      model = "logistic"
    }
    mr.df <- getStudiesModelData(hd, model, input$DMRVariable, NULL, input$IMRVariables, input$RegStudies)
  })
  
  output$mrForestPlot <- renderUI({
    plotlyOutput("mrFP")
  })
  
  
  output$mrFP <- renderPlotly({
    model  = 'linear'
    if(input$log){model = 'logistic'}
    mrFP <- getallStudiesForestPlot(mr.df(), input$DMRVariable, model, input$IMRVariables)
    print(mrFP)
  })
  
  output$mr <- renderUI({
     DT.list <- allRegTables(mr.df(), input$regPvalue)
     TabNames = names(DT.list)
     tabs = lapply(TabNames, function(x){tabPanel(x, div(style = 'overflow-x: scroll', DT.list[[x]]))})
     do.call(tabsetPanel, tabs)
   })
  
  #Survival 
  ################################################################################################################################################################################################################
  
  observe({
    updateSelectizeInput(session, "survVariables",  choices = c("", datanames()[[1]]), selected = "")
  })
  
  observe({
    updateSelectInput(session, "survOutVariable", choices = c("", datanames()[[1]]), selected = "")
  })
  
  observe({
    updateSelectInput(session, "survTimeVariable", choices = c("", datanames()[[1]]), selected = "")
  })
  
  observe({
    harmData <- harmonizedData()
    studies <- names(table(harmData$study))
    updateSelectizeInput(session, "survStudies", choices = c(studies), selected = c(studies))
  })

    
  coxList <- eventReactive(input$survButton, {
    validate(need(length(input$HarmCols) > 1, "Please select atleast two harmonized variables, a input variable, a time to event variable, and an outcome variable"))
    validate(need(input$survOutVariable != "", "Please make sure the indpendent and dependent variables are set"))
    validate(need(length(input$survVariables) != 0, "Please make sure the indpendent and dependent variables are set"))
    validate(need(input$survTimeVariable != "", "Please make sure the indpendent and dependent variables are set"))
    hd <- harmonizedData()
    hd <- hd[,c(input$HarmCols, "study")]  
    coxList <- getStudiesModelData(hd, "coxPH", input$survOutVariable, input$survTimeVariable, input$survVariables, input$survStudies)
  })

  output$coxForestPlot <- renderUI({
    plotlyOutput("coxFP")
  })
  
  output$coxFP <- renderPlotly({
    coxFP <- getallStudiesForestPlot(coxList(), input$survOutVariable, "coxPH", input$survVariables)
    print(coxFP)
  })
  
  output$cox <- renderUI({
    DT.list <- allRegTables(coxList(), input$survPvalue)
    TabNames = names(DT.list)
    tabs = lapply(TabNames, function(x){tabPanel(x, div(style = 'overflow-x: scroll', DT.list[[x]]))})
    do.call(tabsetPanel, tabs)
  })
  
  survFit <- eventReactive(input$survButton, {
    validate(need(length(input$HarmCols) > 1, "Please select atleast two harmonized variables, a input variable, a time to event variable, and an outcome variable"))
    validate(need(input$survOutVariable != "", "Please make sure the indpendent and dependent variables are set"))
    validate(need(length(input$survVariables) == 1, "For Kaplan-Meier Survival Plot please use One Survival Variable"))
    validate(need(input$survTimeVariable != "", "Please make sure the indpendent and dependent variables are set"))
    survFit <- getStudiesSurvPlotData(harmonizedData(), "DeathStatus", "dthd", "DM", input$survStudies)
  })

  output$survPlots <- renderUI({
    plotsList <- getallStudiesSurvPlots(survFit())
    TabNames = names(plotsList[[1]])
    tabs = lapply(TabNames, function(x){tabPanel(x, plotsList[[1]][[x]])})
    do.call(tabsetPanel, tabs)
  })

  #Statistical Tests  
  ################################################################################################################################################################################################################    
  observe({
    harmData <- dataset()
    var.opts <- colnames(harmData)
    updateSelectizeInput(session, "var1", choices = c("", numericColumns()))
  })
  
  observe({
    harmData <- dataset()
    var.opts <- colnames(harmData)
    updateSelectizeInput(session, "var2", choices = c("", numericColumns()))
  })
  
  observe({
    harmData <- harmonizedData()
    studies <- names(table(harmData$study))
    updateSelectizeInput(session, "statsStudies", choices = c(studies), selected = c(studies))
  })
  
  
  statsList <- reactive({
    validate(need(length(input$HarmCols) > 1, "Please select atleast two harmonized variables, a input variable, a time to event variable, and an outcome variable"))
    validate(need(input$var1 != "", "Please make sure the both testing variables are set"))
    validate(need(input$var2 != "", "Please make sure the both testing variables are set"))
    validate(need(input$statTest != "", "Please make sure the type of statistical test is set"))
    var.df <- harmonizedData()[,c(input$var1, input$var2, "study")]
    colnames(var.df) <- c('x', 'g', "study")  
    statsList <- getStudiesStatsTestData(var.df, input$statTest, input$sig, input$statsStudies)
  })
  
  
  output$stat.res <- renderUI({
    DT.list <- allStatsTables(statsList(), input$survPvalue)
    TabNames = names(DT.list)
    tabs = lapply(TabNames, function(x){tabPanel(x, div(style = 'overflow-x: scroll', DT.list[[x]]))})
    do.call(tabsetPanel, tabs)
  })
  
  #creates a plot object from the data so can to plot stats tests
  output$statsPlots <- renderUI({
    validate(need(length(input$HarmCols) > 1, "Please select atleast two harmonized variables, a input variable, a time to event variable, and an outcome variable"))
    validate(need(input$var1 != "", "Please make sure the both testing variables are set"))
    validate(need(input$var2 != "", "Please make sure the both testing variables are set"))
    validate(need(input$statTest != "", "Please make sure the type of statistical test is set"))
    var.df <- harmonizedData()[,c(input$var1, input$var2, 'study')]
    plotsList <- getallStudiesStatsPlots(var.df, st, input$statsStudies)
    TabNames = names(plotsList[[1]])
    tabs = lapply(TabNames, function(x){tabPanel(x, plotsList[[1]][[x]])})
    do.call(tabsetPanel, tabs)
  })

  #Table One
  ################################################################################################################################################################################################################    
  
  observe({
    updateSelectInput(session, "tableOneVariables", choices = datanames()[[1]])
  })

  observe({
    obj <- datanames()[[1]]
    var.opts<-unique(c(colnames(dataset()), obj))
    updateSelectizeInput(session, "tableOneResponse", choices = c("", var.opts))
  })
  
  output$tableOneNewColName <- renderUI({
    textInput( "tableOneNewColName", "New Column Label")
  })
  
  output$tableOneColNamesChange <- renderUI({
    var.opts <- colnames(values$tableOne)
    selectizeInput("tableOneColNamesChange", "Select Column Label to Change", choices = c("", var.opts))
  })
  
  output$tableOneNewRowName <- renderUI({
    textInput( "tableOneNewRowName", "New Row Label")
  })
  
  output$tableOneRowNamesChange <- renderUI({
    var.opts <- row.names(values$tableOne)
    selectizeInput("tableOneRowNamesChange", "Select Row Label to Change", choices = var.opts)
  })
  
  
  observeEvent(input$tableOneRowNamesChangeButton, {
    print(values$tableOne)
    t <- values$tableOne
    rowNameChange  = input$tableOneRowNamesChange
    newRowName = input$tableOneNewRowName
    index = which(row.names(t) %in% rowNameChange)
    print(index)
    print("newRowName")
    print(newRowName)
    print(row.names(t))
    row.names(t)[index] <- newRowName
    print(row.names(t))
    values$tableOne <- t
    print(values$tableOne)
  })
  
  observeEvent(input$tableOneColNamesChangeButton, {
    print(values$tableOne)
    t <- values$tableOne
    colNameChange  = input$tableOneColNamesChange
    newColName = input$tableOneNewColName
    index = which(colnames(t) %in% colNameChange)
    colnames(t)[index] = newColName
    values$tableOne <- t
  })
  
  observe({
    validate(need(length(input$tableOneVariables) != 0, "please make sure all table variables are set"))
    validate(need(input$tableOneResponse != "", "please make sure all table variables are set"))
    if(length(input$tableOneResponse) > 0){
      levelsResp <- nlevels(as.factor(dataset()[,input$tableOneResponse]))
      outcome <- input$tableOneResponse
      validate(need(levelsResp < 10, "please choose an outcome variable that is a factor less than 10 categories"))
    }
    else{outcome <- NULL}
    tableOne <- makeTableOne(dataset(), outcome, input$tableOneVariables)
    values$tableOne <- tableOne
  })
  
  
  t1 <- reactive({
    t1 <- values$tableOne
    return(values$tableOne)
  })
  
  
  output$tableOne <- DT::renderDataTable(server =  FALSE , {
    validate(need(length(input$tableOneVariables) != 0, "please make sure all table variables are set"))
    if(length(input$tableOneResponse) > 0){
      levelsResp <- nlevels(as.factor(dataset()[,input$tableOneResponse]))
      outcome <- input$tableOneResponse
      validate(need(levelsResp < 10, "please choose an outcome variable that is a factor less than 10 categories"))
    }
    span = as.character(ncol(t1) + 1)
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan = 1, " "),
          th(colspan = ncol(t1()), outcome)
        ),
        tr(lapply(c(" ", colnames(t1())), th))
      )
    ))
    
    tableOne <- datatable(t1(), escape = FALSE, rownames = TRUE, container = sketch, extensions = list('Buttons' = NULL, 'ColReorder' = NULL), 
                          options = list(dom = 'BRrltpi', ordering=F, autoWidth = T, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE,
                                         buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'), I('colvis'))), 
                          class = 'table-bordered table-condensed table-striped table-compact') %>%
      formatStyle(colnames(t1())[1],
                  target = 'row',
                  backgroundColor = styleEqual(c("  "), c('silver')))
    
    
    return(tableOne)
  })
  
  output$tableOneContVar <- renderPrint({
    continuous <- continuousVar()
    continuous<- continuous[continuous %in% input$tableOneVariables]
    validate(need(length(input$tableOneVariables) != 0, "please make sure all table variables are set"))
    data <- dataset()
    if(length(continuous) > 0){
      varList <- lapply(1:length(continuous), function(i) {
        paste(continuous[i], nlevels(as.factor(data[,continuous[i]])), "categories")
      })
      print(varList)
    }
    else{
      str("There are no variables in Table One with more than 10 levels")
    }
  })
  
  
  ####Generate Report 
  ################################################################################################################################################################################################################      
  
  output$report <- downloadHandler(
    # For pdf output, change this to "shiny_analysis_report.pdf"
    filename = "shiny_analysis_report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      #tempReport <- file.path(tempdir(), "report.Rmd")
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      file.create(src, 'report.Rmd', overwrite = TRUE)
      
      
      # Set up parameters to pass to Rmd document
      params <- list("indepent variable 1" = input$x , "dependent variable 1" = input$y, "grouping variable 1" = input$group, "plot type 1" = input$plotType, "statistical test 1" = input$statTest , "significance threshold 1" = input$sig,
                     "indepent variable 2" = input$x2 , "dependent variable 2" = input$y2, "grouping variable 2" = input$group2, "plot type 2" = input$plotType2, "statistical test 2" = input$statTest2 , "significance threshold 2" = input$sig2,
                     "Correlation Variables" = c(input$corrVariables), "Multiple Regression Independent Variables" = c(input$IMRVariables), "Multiple Regression Dependent Variables" = input$DMRVariable,
                     "Machine Learning Feature Variables" = c(input$mlvariables[!input$mlvariables %in% input$r]), "Response Variable" = input$r)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      out <- rmarkdown::render(src, output_file = file,
                               params = params,
                               envir = new.env(parent = globalenv()))
      
      file.rename(out, file)
      
    }
  )        
  
})


shinyApp(ui = ui, server = server)



