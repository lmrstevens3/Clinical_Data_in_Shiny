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
library(shinyFiles)
library(RColorBrewer)
library(survival)
library(ggfortify)

multicol <- " .multicol {

-webkit-column-count: 2; /* Chrome, Safari, Opera */

-moz-column-count: 2; /* Firefox */

column-count: 2;
}"

# Sys.setenv("plotly_username"="ClinicalDataShiny")
# Sys.setenv("plotly_api_key"="QhkkwI5BVgSRhmjTZDB1")

ui_msp <- shinyUI(
  dashboardPage( 
    
    # Application title
    dashboardHeader(title = "DATA MINE - Data Analysis Testing And Modeling using Interaction and Numerical Exploration", titleWidth = 800),
    
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
            var ids = ['dac','pst','cmr','ml', 'frd', 'pd'];
            for(i=0; i<ids.length; i++){
            $('a[data-value='+ids[i]+']').addClass('my_menuitem_class');
            }
            
            // Register click handeler
            $('.my_menuitem_class').on('click',function(){
            // Unactive menuItems
            $('.my_menuitem_class').parent().removeClass('active');
            })
            })
            "
          )
        ),
        tags$style(".wrapper {overflow: visible !important;}")
      ),  
      width = 300,
      sidebarMenu(id = "sbm",
                  menuItem("Upload and Merge Data", tabName = "umd", icon = icon("th")),
                  conditionalPanel("input.sbm === 'umd'",
                                   selectInput("startDataset", "Dataset", c("","esoph", "upload my own")),
                                   conditionalPanel("input.startDataset === 'upload my own'",
                                                    fileInput("datafile", ""), 
                                                    textInput("datafile_sep", "Field Separator", value = ",")),
                                   checkboxInput("combine", "Combine with another dataset", FALSE),
                                   conditionalPanel("input.combine === true",
                                                    selectInput("mergeby", "How to Merge Data", c("Merge data and keep all data", "Merge data and only keep rows in both datasets", "Merge data and keep all rows in initial data, but not rows from merged dataset")),
                                                    selectInput('colMerge', "Column Name", c("Loading..."), multiple = T),
                                                    fileInput("combinefile", ""), 
                                                    textInput("combinefile_sep", "Field Seperator", value = ","))
                                   ),
                  menuItem("Data Overview and Cleaning", tabName = "dac", icon = icon("th")),
                  conditionalPanel("input.sbm === 'dac'",
                                  selectInput("cleanOptions", "Cleaning Options", c("",  "replace values", "replace NAs", "remove NAs", "subset or add data" )), 
                                  conditionalPanel("input.cleanOptions === 'replace NAs'", textInput("replacementNA", "Replacement NA Value", "")),
                                  conditionalPanel("input.cleanOptions === 'replace values'", selectInput("replaceValues", "Replacement Options", c("values in entire dataset", "values in columns")),
                                                   conditionalPanel("input.replaceValues === 'values in entire dataset'", uiOutput("replacedAll"), textInput("replacementAll", "Replacement Value", ""), div(style="text-align: center" , actionButton('replaceButtonAll', "Replace All Values"))),
                                                   conditionalPanel("input.replaceValues === 'values in columns'", helpText("Select the Column you would"), helpText("like to replace values in the Data"), helpText("and enter replaced and replacement values"), uiOutput("replacedCol"), textInput("replacementCol", "Replacement Value", ""), div(style="text-align: center" , actionButton('replaceButtonCol', "Replace Column Values")))
                                  ),
                                  conditionalPanel("input.cleanOptions === 'subset or add data'", selectInput("subAddType", "Subset or Add Data", c("Subset data by column(s) value(s)", "Add Column To Data")),
                                                  conditionalPanel("subAddType === 'Add Column To Data'", selectInput("mutateType", "What to Add", c("Column from formula", "Average columns", "Sum columns"))),
                                                  conditionalPanel("subAddType === 'subset data by column(s) values(s)'", uiOutput('subsetConstraints'))
                                  ),
                                  div(style="text-align: center", helpText("To make certain columns in the data a factor,"), helpText("character, or numeric type, select them in the data,"), helpText(" and check the appropriate box")),
                                  div(style = "display:inline-block",checkboxInput('makeFactor', "Make columns a Factor", FALSE, width = '100px')),
                                  div(style = "display:inline-block",checkboxInput('makeChar', "Make columns Characters", FALSE, width = '100px')),
                                  div(style = "display:inline-block",checkboxInput('makeNum', "Make columns Numeric", FALSE, width = '100px'))
                                  
                 )
            )
    ),
    #####   MAIN PANEL     
    ################################################################################################################################################################################################################  
    dashboardBody(
      tags$head(tags$style(HTML(multicol))),
      tabItems(
        tabItem(tabName = "umd", 
                fluidRow(
                  column(width = 2, 
                         box(title = "Data and Merge Data Overview Options", status = "warning", collapsible = TRUE, width = NULL, solidHeader = TRUE, 
                             selectInput("look", "Look at NAs, Characters, and Numeric values in Dataset", c("Total Dataset", "NAs By Row", "Data Type By Column"))), 
                         box(title = "Merge/Restore Dataset", status = "primary", collapsible = TRUE, width = NULL, actionButton("mergeButton", "Merge Data"), actionButton("restoreOriginalData", "Restore"))),
                  column(width = 5, box(title = "Data Overview", status = "warning", collapsible = TRUE, width = NULL , soildHeader = TRUE, div(style = 'overflow-x: scroll', DT::dataTableOutput("lookStartDataTable")))),
                  column(width = 5, box(title = "Data Overview", status = "warning", collapsible = TRUE, width = NULL , soildHeader = TRUE, div(style = 'overflow-x: scroll', DT::dataTableOutput("lookMergeDataTable"))))
                ),
                fluidRow(
                  box(title = "Start Data", status = "primary", collapsible = TRUE, width = NULL, solidHeader = TRUE, div(style = 'overflow-x: scroll', DT::dataTableOutput("startDataTable"))),
                  box(title = "Data to Merge", status = "primary", collapsible = TRUE, width = NULL, solidHeader = TRUE, div(style = 'overflow-x: scroll', DT::dataTableOutput("mergeDataTable")))
                ),
                fluidRow(
                  box(title = "Combined Data", status = "primary", collapsible = TRUE, width = NULL, solidHeader = TRUE, div(style = 'overflow-x: scroll', DT::dataTableOutput("combinedDataTable")))
                )
                ), 
        tabItem(tabName = "dac", 
                fluidRow(
                  column(width = 3, 
                         box(title = "Data Overview Options", status = "warning", collapsible = TRUE, width = NULL, solidHeader = TRUE, 
                             selectInput("look", "Look at NAs, Characters, and Numeric values in Dataset", c("Total Dataset", "NAs By Row", "Data Type By Column"))), 
                         box(title = "Remove Rows/Columns in Data" , status = "primary", collapsible = TRUE, width = NULL, 
                             conditionalPanel('input.look ==  "NAs By Row"', h5(helpText("Rows Selected in Data Overview: "))),
                             conditionalPanel('input.look == "Data Type By Column"', h5(helpText("Columns Selected in Data Overview: "))), h5(textOutput('lookTxt')),
                             h5(helpText("Rows Selected in Data: ")), h5(textOutput('dataTxt_rows')), h5(helpText("Columns Selected in Data: ")), h5(textOutput('dataTxt_cols')), actionButton("removeButton", "Remove Selected Rows/Columns")),
                         box(title = "Save Cleaned Data to File", status = "primary", collapsible = TRUE, width = NULL, shinySaveButton("save", "Save Cleaned Data", "Save file as ...", filetype=list(csv="csv"))),
                         box(title = "Restore Data to Original Dataset", status = "primary", collapsible = TRUE, width = NULL, actionButton("restoreOriginalData", "Restore")),
                         box(title = "Change Column Names", status = "primary", collapsible = TRUE, width = NULL, uiOutput("colNamesChange"), uiOutput("newColName"), actionButton("colNamesChangeButton", "Change"))
                         
                  ), 
                  column(width = 9, 
                         box(title = "Data Overview", status = "warning", collapsible = TRUE, width = NULL , soildHeader = TRUE, div(style = 'overflow-x: scroll', DT::dataTableOutput("lookTable"))),
                         box(title = "Remove Rows/Columns By Number of NAs", status = "primary", collapsible = TRUE, width = NULL, solidHeader = F,
                             h5(helpText("To remove rows/columns with greater than a certain number of NAs, enter the NA cut off and hit the remove button for rows or columns")), 
                             div(style = "display:inline-block", uiOutput('removeNARow')), div( style = "display:inline-block", actionButton("removeNARowButton", "remove rows with NA total > cut off")),
                             br(),
                             div(style = "display:inline-block", uiOutput('removeNACol')), div( style = "display:inline-block", actionButton("removeNAColButton", "remove cols with NA total > cut off"))),
                         box(title = "Subset and Add Data Options", status = "primary", collapsible = TRUE, width = NULL, solidHeader = F, textOutput('subAddText'), uiOutput("subAdd"), uiOutput('subAddButton'))
                  )
                ), 
                fluidRow(
                  box(title = "Data", status = "primary", collapsible = TRUE, width = NULL, solidHeader = TRUE, div(style = 'overflow-x: scroll', DT::dataTableOutput("cleanDataTable")))
                ),
                fluidRow(
                         box(title = "Columns Removed From Dataset",status = "primary", collapsible = TRUE, width = 6,
                             helpText("To restore columns in data, select the clean this dataset checkbox and select 'restore to original dataset in box above'"), 
                             uiOutput("remCol")),
                         box(title = "Rows Removed From Dataset", status = "primary", collapsible = TRUE, width = 6, 
                             helpText("To restore rows in data, select the clean this dataset checkbox and select 'restore to original dataset in box above'"),
                             uiOutput("remRow"))
                )

              ))
    )
))



server22 <- shinyServer(function(input, output, session){
  
  options(shiny.maxRequestSize = 100*1024^2)
  #####   DATA    
  ################################################################################################################################################################################################################  
  
  #allows user to input data set or try on test set of data 
  values <- reactiveValues(cleanedData = NULL, mlvariables = NULL, tableOne = NULL, datasource = NULL)
  
  startDataset <- reactive({
    datasource <- input$startDataset
    values$datasource <- NULL
    validate(need(input$startDataset != "", "Please select a dataset"))
    if(datasource == "upload my own") {
      inFile <- input$datafile
      validate(need(!is.null(inFile), "Please select a dataset or input a File"))
      read.delim(inFile$datapath, sep = gsub("\\t", "\t", input$datafile_sep, fixed = TRUE))
    }
    else {
      eval(parse(text = datasource))
    }
    
  })
  
  
  dataset <- reactive({
    validate(need(input$startDataset != "", "Please select a dataset"))
    
    if(is.null(values$cleanedData) || is.null(values$datasource)){
      startData <- startDataset()
      values$cleanedData <- startData
      dataset <- values$cleanedData
      values$datasource <- input$startDataset
      
      
    }
    else{
      validate(need(length(values$cleanedData) >= 2 , "Please have atleast 2 columns in your dataset"))
      validate(need(length(row.names(values$cleanedData)) >= 2 , "Please have atleast 2 rows in your dataset"))
      dataset <- cleanData() 
      
    }
    return(dataset)
  })
  
  
  
  datanames <- reactive({
    col = colnames(dataset())
    row = rownames(dataset())
    datanames = c(list(col),list(row))
    return(datanames)
  })
  
  observe({
    obj <- dataset()     
    var.opts <- c(colnames(obj))
    updateSelectInput(session, "colMerge", choices = var.opts, selected = var.opts[1])
  })
  
  output$removeNACol <- renderUI({
    obj <- dataset()     
    NASum <- as.data.frame(apply(obj, 2, function(x) sum(is.na(x))))
    colnames(NASum) <- 'ColNASum'
    var.opts <- unique(sort(NASum$ColNASum, decreasing = T))
    selectInput("removeNACol", "NA Cut off for Columns", choices = var.opts, selected = max(var.opts))
  })
  
  output$removeNARow <- renderUI({
    obj <- dataset()     
    NASum <- apply(obj, 1, function(x) sum(is.na(x)))
    var.opts <- unique(sort(NASum, decreasing = T))
    selectInput("removeNARow", "NA Cut off for Rows" , choices = var.opts, selected = max(var.opts))
  })
  
  #updates y, dependent variable for 2D plots 
  output$y <- renderUI({ 
    obj <- dataset()    
    var.opts <- c(" ", colnames(obj))
    selectInput("y","Dependent Variable:", var.opts) # uddate UI                  
  }) 
  
  #updates x, independent variable for plots
  output$x <- renderUI({ 
    obj <- dataset()     
    var.opts <- c(" ", colnames(obj))
    selectInput("x","Independent Variable:", var.opts) # uddate UI                 
  })
  
  #updates grouping variable for comparing data based on categorical variables such as gender 
  output$group <- renderUI({ 
    obj <- dataset()     
    var.opts <- c(" ",colnames(obj))
    selectInput("group","Grouping Variable: ", var.opts) # uddate UI                 
  })
  
  #updates y for second plot , dependent variable for 2D plots 
  output$y2 <- renderUI({ 
    obj <- dataset()    
    var.opts <- c(" ", colnames(obj))
    selectInput("y2","Dependent Variable:", var.opts) # uddate UI                  
  }) 
  
  #updates x for second plot, independent variable for plots
  output$x2 <- renderUI({ 
    obj <- dataset()     
    var.opts <- c(" ", colnames(obj))
    selectInput("x2","Independent Variable:", var.opts) # uddate UI                 
  })
  
  #updates grouping variable for second plot for comparing data based on categorical variables such as gender 
  output$group2 <- renderUI({ 
    obj <- dataset()     
    var.opts<-c(" ", colnames(obj))
    selectInput("group2","Grouping Variable: ", var.opts) # uddate UI                 
  })
  
  #update cleanData variables

  
  output$remCol <- renderUI({
    obj <- datanames()[[1]]
    var.opts <- obj[c(!obj %in% colnames(dataset()))]
    validate(need(length(var.opts) < 1000, "Too many columns Removed to Display"))
    selectizeInput("remCol", "Columns Removed in Dataset", choices = var.opts, multiple = T, selected = var.opts, options = list(plugins = list("drag_drop")))
  })
  
  output$remRow <- renderUI({
    obj <- datanames()[[2]]
    var.opts.r <- obj[c(!obj %in% row.names(dataset()))]
    validate(need(length(var.opts.r) < 1000, "Too many Rows Removed to Display"))
    if(length(var.opts.r) > 1000) {var.opts.r = "Too many Rows Removed to Display"}
    selectizeInput( "remRow", "Rows Removed in Dataset", choices = var.opts.r, multiple = T, selected = var.opts.r, options = list(plugins = list("drag_drop")))
  })
  
  output$colNamesChange <- renderUI({
    obj <- dataset()  
    var.opts <- colnames(obj)
    selectizeInput("colNamesChange", "Select Column Label to Change", choices = var.opts)
  })
  
  output$newColName <- renderUI({
    textInput( "newColName", "New Column Label")
  })
  
  
  #updates response variable for machine learning tab
  output$r <- renderUI({ 
    obj <- rdataset()
    var.opts<-c(colnames(obj))
    selectInput("r","Response Variable: ", var.opts) # uddate UI                 
  })
  
  #get numeric data
  numericColumns <- reactive({
    df <- dataset()
    colnames(df)[sapply(df, is.numeric)]
  })
  
  cont_cat <- function(){
    df <- as.data.frame(dataset())
    f_df <- lapply(df, as.factor)
    f_levels <- sapply(f_df, nlevels)
    continuous <- names(f_levels[f_levels > 10])
    categorical <- names(f_levels[f_levels <= 10])
    cont_cat <- continuous
    return(cont_cat)
  }
  
  mergeData <- reactive({
    if(input$combine == TRUE){
      inCFile <- input$combinefile
      validate(
        need(!is.null(inCFile), "Please input a File to combine")
      )
      read.delim(inCFile$datapath, sep = gsub("\\t", "\t", input$combinefile_sep, fixed = TRUE))
    }
  })
  
  observeEvent(input$mergeButton,{
    mergedata = mergeData()
    data = dataset()
    switch(input$mergeby, 
           "Merge data and keep all Data" = {
             validate(need(length(data) == length(mergedata), "The two datasets for combining, do not have the same number of rows. Consider 'merge by a colum value' or upload a dataset to combine with the same number of rows as the original dataset"))
             combinedData <- full_join(data, mergedata, by = c(input$colMerge))
           },
           "Merge data and only keep rows in both datasets" = {
             validate(need((input$colMerge %in% colnames(data)), "The column you selected for merging the datasets on is not in the original dataset, check both datasets have the column, or consider changing the column name"))
             validate(need((input$colMerge %in% colnames(mergedata)), "The column you selected for merging the datasets on is not in the merge dataset, check both datasets have the column, or consider changing the column name"))
             combinedData <- inner_join(data, mergedata, by = c(input$colMerge))
           },
           "Merge data and keep all rows in initial data, but not rows from merged dataset" = {
             validate(need(length(data) == length(mergedata), "The two datasets for combining, do not have the same number of rows. Consider 'merge by a colum value' or upload a dataset to combine with the same number of rows as the original dataset"))
             combinedData <- full_join(data, mergedata, by = c(input$colMerge))
           }
    )
    values$cleanedData <- combinedData
  })
  
  output$subsetConstraints <- renderUI({
      numericInput("subsetConstraints", "Number of Constraints", value = 1, min = 1, step = 1)
    })
    
    
  output$subsetText <- renderText("To take a subset of the data that meets multiple conditions, increase the number of subset constraints")
    
  output$mutationText <- renderText("To Add a Colum such as two columns divided by each other or a column multiplied by a certain value, 
                                      type the forumla in the formula box and click 'Add Column(s)'.")
  
    
  output$subsetOptions <- renderUI({
        lapply(1:input$subsetConstraints, function(i) {
          if(input$subsetConstraints == i){
            div(style = "display:inline-block", selectInput(paste("subsetColumn",i, sep = ""), "Column to set constraint", choices = datanames()[[1]]), 
                selectInput(paste('equals',i, sep = ""), "Equals/notEqual", c("equals", "is not equal to", "greater than or equal to", "less than or equal to")), 
                textInput(paste('subsetValue',i, sep = ""), "Value for constraint"))
          }
          else{
          div(displaystyle="height: 30px;", div(style = "display:inline-block", selectInput(paste("subsetColumn",i, sep = ""), "Column to set constraint", choices = datanames()[[1]]), 
                                                selectInput(paste('equals',i, sep = ""), "Equals/notEqual", c("equals", "is not equal to", "greater than or equal to", "less than or equal to")), 
                                                textInput(paste('subsetValue',i, sep = ""), "Value for constraint"), selectInput(paste('andOr',i, sep = ""), "And/Or", c("and", "or"), width = 50)))
            }
        })
      
    })
    
    output$mutateOptions <- renderUI({
      switch(input$addColumn,
                        "Column from formula" = textInput("formula", "Formula"),
                        "Average columns" = selectInput("mutateAvgCol", "Column to average", choices = datanames()[[1]], multiple = T), 
                        "Sum columns" = selectInput("mutateSumCol", "Columns to sum", choices = datanames()[[1]], multiple = T)
      )
    })
    
   output$subAdd <- renderUI({
      switch(input$subAddType, 
             "subset data by column(s) value(s)" = uiOutput('subsetOptions'),
             "Add Column To Data" = uiOutput('mutateOptions')
      )
   })   
    
   output$subAddText <- renderText({
     switch(input$subAddType, 
            "subset data by column(s) value(s)" = textOutput('subsetText'),
            "Add Column To Data" = textOutput('mutateText')
     )
   }) 
   
   output$subAddButton <- renderUI({
     switch(input$subAddType, 
            "subset data by column(s) value(s)" = actionButton("mutateButton", "Add Column(s)"),
            "Add Column To Data" = actionButton("subsetButton", "Subset")
     )
   })
   
   equalsVal <- function(inputEqual){
     equalsVal <- switch(inputEqual,
            "equals" = '==', 
            "is not equal to" = '!=',
            "greater than or equal to" = '>=',
            "less than or equal to" = '<='
            )
     return(equalsVal)
   }
   
   andOrVal <- function(inputAndOr){
     andOrVal <- switch(inputAndOr,
                      "and" = '&', 
                      "or" = '|'
     )
     return(andOrVal)
   }
   

    observeEvent(input$subsetButton, {
      data <- values$cleanData
      fList <- lapply(1:input$subsetConstraints, function(i) {
          if(i == input$subsetConstraints){
            equal <- equalsVal(input$equals)
            paste0(input[[paste0('subsetColumn',i)]], ' ', equal,' ',"'", input[[paste0('subsetValue',i)]],"'")
          }
          else{
          equal <- equalsVal(input[[paste0(equals,i)]])
          and <- andOrVal(input[[paste0(andOr,i)]])
          paste0(input[[paste0('subsetColumn',i)]], ' ', equal,' ',"'", input[[paste0('subsetValue',i)]],"' ", and, " ")
          }
      })
      f <- paste(fList, collapse = '')
      
      values$cleanedData <- data %>% mutate(eval(parse(text = f)))
    })
    
    observeEvent(input$mutateButton, {
      data <- values$cleanData
      dataNames <- colnames(data)
      switch(input$addColumn,
               "Column from formula" = { 
                 data <- data %>% mutate(eval(parse(text = input$forumla)))
                 colnames(data) <- c(dataNames, abbreviate(formula, minlength = 20))},
               "Average columns" = {
                 validate(need(length(input$mutateAvgCol) > 2, "Please select at least 2 columns to average"))
                 data <- data %>% mutate(rowMeans(data[,c(input$mutateAvgCol)], na.rm = T))
                 colnames(data) <- c(dataNames, paste0("Mean-",input$mutateAvgCol[1],"..", input$mutateAvgCol[length(input$mutateAvgCol)]))},
               "Sum columns" = {
                 validate(need(length(input$mutateSumCol) > 2, "Please select at least 2 columns to sum"))
                 data <- data %>% mutate(rowSums(data[,c(input$mutateSumCol)], na.rm = T))
                 colnames(data) <- c(dataNames, paste0("Sum-",input$mutateSumCol[1],"..", input$mutateSumCol[length(input$mutateSumCol)]))}
      )
      values$cleanedData <- data
    })
  
  cleanData <- reactive({
    cleanOption = input$cleanOptions
    cleanedData <- values$cleanedData

      if(cleanOption == "remove NAs"){
        cleanedData = na.omit(cleanedData)
      }
      
      if(cleanOption == "replace NAs"){
        cleanedData[is.na(cleanedData)] <- input$replacementNA
      }
    
    values$cleanedData <- cleanedData
    return(values$cleanedData)
  })
      
  observeEvent(input$makeFactor == TRUE, {
    cleanedData <- values$cleanedData
    fList = c(colnames(cleanedData)[input$lookTable_columns_selected], colnames(cleanedData)[input$cleanDataTable_columns_selected])
    
        if(length(fList) > 0){
          
          cleanedData[fList] = lapply(cleanedData[fList], as.factor)
          cleanedData = cleanedData
        }
    values$cleanedData <- cleanedData
      }) 
  
    observeEvent(input$makeChar == TRUE, {
      cleanedData <- values$cleanedData
      cList = c(colnames(cleanedData)[input$lookTable_columns_selected], colnames(cleanedData)[input$cleanDataTable_columns_selected])
    
        if(length(cList) > 0){
          
          cleanedData[cList] = lapply(cleanedData[cList], as.character)
        }
      values$cleanedData <- cleanedData
      })
  
      observeEvent(input$makeNum == TRUE, {
        cleanedData <- values$cleanedData
        nList = c(colnames(cleanedData)[input$lookTable_columns_selected], colnames(cleanedData)[input$cleanDataTable_columns_selected])
        
        if(length(nList) > 0){
          
          cleanedData[,nList] = lapply(cleanedData[nList], as.numeric)
        }
         values$cleanedData <- cleanedData
       })

    

  
  lookData  <- reactive({
    validate(need(input$startDataset != "", "Please select a dataset"))
    data <- dataset()
    dataDim = dim(data)
    NASum = sum(is.na(data))
    NARowSum = apply(data, 1, function(x) sum(is.na(x)))
    NumericSum = sum(sapply(data, is.numeric))
    CharacterSum = sum(sapply(data,is.character))
    IntegerSum = sum(sapply(data, is.integer))
    FactorSum = sum(sapply(data, is.factor))
    NASumColumn = apply(data, 2, function(x) sum(is.na(x)))
    NumericSumColumn = sapply(data, is.numeric)
    CharacterSumColumn = sapply(data,is.character)
    IntegerSumColumn = sapply(data, is.integer)
    FactorSumColumn = sapply(data, is.factor)
    
    df = data.frame(c(paste(dim(data)[[1]][1], "X", dim(data)[[2]][1]),paste(dim(na.omit(data))[[1]][1], "X", dim(na.omit(data))[[2]][1]), NASum, CharacterSum, NumericSum, IntegerSum, FactorSum))
    df.column = t(cbind.data.frame( NASumColumn, CharacterSumColumn, NumericSumColumn, IntegerSumColumn, FactorSumColumn, row.names = NULL))
    colnames(df) = c("Sum Total")
    row.names(df) = c("Data Dimensions (RXC)","Data Dimensions (RXC) If All NAs Removed", "NA Sum", "Character Sum", "Numeric (double) Sum", "Integer Sum", "Factor Sum")
    row.names(df.column) = c("Column NA Sum", "Column  is Character", "Column is Numeric (double)", "Column is Integer", "Column is Factor")
    colnames(df.column) = c(colnames(data))
    df.row = data.frame(NARowSum)
    row.names(df.row) = row.names(data)
    
    lookDataTable <- switch(input$look,
                            "Total Dataset" = df,
                            "NAs By Row" = df.row,
                            "Data Type By Column" = df.column
    )
    return(lookDataTable)
  }) 
  
  observeEvent(input$restoreOriginalData, {
    data = startDataset()
    values$cleanedData <- data
  })
  
  observeEvent(input$colNamesChangeButton, {
    colNameChange  = input$colNamesChange
    newColName = input$newColName
    index = which(colnames(values$cleanedData) %in% colNameChange)
    colnames(values$cleanedData)[index] = newColName
  })
  
  
  observeEvent(input$replaceButtonAll, {
        replaced = c(input$replacedAll)
        replacement = c(input$replacementAll)
        validate(need(length(replaced) == length(replacement), "Please make sure replacements and replaced values are same length"))
        cleanedData <- as.data.frame(apply(values$cleanedData, 2, mapvalues, from=replaced, to=replacement))
    values$cleanedData <- cleanedData
  })
  
  output$replacedCol <- renderUI({
    data <- dataset()
    r = c(colnames(data)[input$lookTable_columns_selected], colnames(data)[input$cleanDataTable_columns_selected])
    validate(need(length(r) != 0, "Please select columns in Data using table footer to replace column values"))
    data <- data[,r]
    validate(need(length(data) != 0, "Please select columns in Data using table footer to replace column values"))
    if(length(data) == 1){
      var.opts <- names(table(data))
    }
    else{
      namesList <- lapply(1:ncol(data), function(i,data) {categories <- names(table(data[,i]))
        return(categories)}, data = data)
      var.opts <- unique(unlist(namesList))
    }
    if(length(var.opts) < 200){
      replacedCol <- selectInput("replacedCol", "Value to Replace in Column", choices = c(var.opts))
    }
    else{
      replacedCol <- textInput("replacedCol", "Value to Replace in Column", "")
    }
  })
  
  output$replacedAll <- renderUI({
    data <- dataset()
    namesList <- lapply(1:ncol(data), function(i,data) {categories <- names(table(data[,i]))
    return(categories)}, data = data)
    var.opts <- unique(unlist(namesList))
    if(length(var.opts) < 200){

      replacedAll <- selectInput("replacedAll", "Value to Replace", choices = c(var.opts))
    }
    else{
      replacedAll <- textInput("replacedALL", "Value to Replace ", "")
    }
    
  })
  
  observeEvent(input$replaceButtonCol,{
        cleanedData <- values$cleanedData
        r = c(colnames(cleanedData)[input$lookTable_columns_selected], colnames(cleanedData)[input$cleanDataTable_columns_selected])
        validate(need(length(r) != 0, "Please Select Column(s) to Replace Values"))
        validate(need(length(cleanedData[,r]) != 0, "Please select columns in Data using table footer to replace column values"))
        replaced = c(input$replacedCol)
        replacement = c(input$replacementCol)
        validate(need(length(replaced) == length(replacement), "Please make sure replacements and replaced values are same length"))
        if(length(cleanedData[,r]) == 1){ 
          cleanedData[,r] <- as.data.frame(mapvalues(cleanedData[,r], from=replaced, to=replacement))
        }
        else{
          cleanedData <- as.data.frame(apply(cleanedData[,r], 2, mapvalues, from=replaced, to=replacement))
        }
        
    values$cleanedData <- cleanedData
  })
  
  observeEvent(input$removeButton, {
    cleanedData <- dataset()
    
    rr = c(row.names(cleanedData)[input$lookTable_rows_selected], row.names(cleanedData)[input$cleanDataTable_rows_selected])
    rc = c(colnames(cleanedData)[input$lookTable_columns_selected], colnames(cleanedData)[input$cleanDataTable_columns_selected])
    validate(need(length(rr) != 0 || length(rc) != 0 , "Please select columns or rows in Data/Data Overview using table footer to remove data"))
    
    newrow = row.names(cleanedData)[!row.names(cleanedData) %in% rr]
    newcol = colnames(cleanedData)[!colnames(cleanedData) %in% rc]
    
    cleanedData = cleanedData[newrow,]
    cleanedData = cleanedData[,newcol]
    
    values$cleanedData <- cleanedData
    
  })
  
  observeEvent(input$removeNAColButton, {
    cleanedData <- dataset()
    
    NAColSum <- apply(cleanedData, 2, function(x) sum(is.na(x)))
    NACol_filtered <- NAColSum[NAColSum > as.numeric(input$removeNACol)]
    
    newcol = colnames(cleanedData)[!colnames(cleanedData) %in% names(NACol_filtered)]
    cleanedData = cleanedData[,newcol]
    
    values$cleanedData <- cleanedData
  })
  
  observeEvent(input$removeNARowButton, {
    cleanedData <- dataset()
    row.names(cleanedData) <- sample(1:(dim(cleanedData)[1]), dim(cleanedData)[1], replace=FALSE)
    NARowSum = apply(cleanedData, 1, function(x) sum(is.na(x)))
    
    NARow_filter <- NARowSum[NARowSum > as.numeric(input$removeNARow)]
    
    
    newrow = row.names(cleanedData)[!row.names(cleanedData) %in% names(NARow_filter)]
    cleanedData = cleanedData[newrow,]
    
    values$cleanedData <- cleanedData
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
  
  
  output$lookTable <- DT::renderDataTable(lookData(), escape = FALSE, server = T, selection = list(target = 'row+column'),  extensions = c('Buttons', 'ColReorder'), 
                                          options = list(dom = 'BRrltpi', autoWidth = FALSE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE,  
                                                         buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'))), class = 'table-bordered table-condensed table-striped table-compact')
  
  output$cleanDataTable <- DT::renderDataTable(dataset(), escape = FALSE, server = T, selection = list(target = 'row+column'),  extensions = c('Buttons', 'ColReorder'), 
                                               options = list(dom = 'BRrltpi', autoWidth = FALSE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, 
                                                              buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'))), class = 'table-bordered table-condensed table-striped table-compact')
  
  output$lookTxt <- renderText({
    lookText <- switch(input$look, 
                       "Total Dataset" = " ",
                       "NAs By Row" =  {row.names(dataset())[input$lookTable_rows_selected]},
                       "Data Type By Column" = {colnames(dataset())[input$lookTable_columns_selected]}
    )
  })
  
  output$dataTxt_rows <- renderText({row.names(dataset())[input$cleanDataTable_rows_selected]})
  output$dataTxt_cols <- renderText({colnames(dataset())[input$cleanDataTable_columns_selected]})
  
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
  
  #text headers for plots 
  output$p1Txt <- renderText({paste("Plot 1:", sapply(input$plotType, simpleCap))})
  output$p2Txt <- renderText({paste("Plot 2:", sapply(input$plotType2, simpleCap))})
  
  #creates a plot object from the data so can create proper d3 plots 
  plot.obj <- function(x,y,g,fg,fgv,f,fv,f2, fv2, f3, fv3){
    plot.list<-list() 
    plot.list$data<- dataset()
    
    validate(need(!is.null(plot.list$data), "Please make sure a dataset is loaded"))
    validate(need(length(intersect(x, colnames(plot.list$data))) & length(intersect(y, colnames(plot.list$data))) & length(intersect(g, colnames(plot.list$data))), "Please set all Independent, Dependent, and Grouping Variables"))
    plot.list$x<-with(plot.list$data, get(x))
    plot.list$y<-with(plot.list$data,get(y))
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
    variablesDF = data.frame(plot.list$x, plot.list$y, plot.list$group)
    colnames(variablesDF) = c('x', 'y', 'g')
    
    if(f == TRUE && f2 == FALSE && f3 == FALSE){
      validate(need(fv != "", "please enter a filter value"))
      plot.list$variables = subset(variablesDF, as.numeric(variablesDF$x) > as.numeric(fv))
    }
    else if(f2 == TRUE && f == FALSE && f3 == FALSE){
      validate(need(fv2 != "", "please enter a filter value"))
      plot.list$variables = subset(variablesDF, as.numeric(variablesDF$y) > as.numeric(fv2))
    }
    else if(f3 == TRUE && f == FALSE && f2 == FALSE){
      validate(need(fv3 != "", "please enter a filter value"))
      plot.list$variables = subset(variablesDF, as.numeric(variablesDF$g) > as.numeric(fv3))
    }
    else if(f2 == TRUE && f == TRUE && f3 == FALSE){
      validate(need(fv2 != "", "please enter a filter value"))
      validate(need(fv != "", "please enter a filter value"))
      plot.list$variables = subset(variablesDF, as.numeric(variablesDF$y) > as.numeric(fv2))
      plot.list$variables = subset(plot.list$variables, as.numeric(plot.list$variables$x) > as.numeric(fv))
    }
    else if(f == TRUE && f2 == TRUE && f3 == TRUE){
      validate(need(fv != "", "please enter a filter value"))
      validate(need(fv2 != "", "please enter a filter value"))
      validate(need(fv3 != "", "please enter a filter value"))
      plot.list$variables = subset(variablesDF, variablesDF$g > fv3 )
      plot.list$variables = subset(plot.list$variables, as.numeric(plot.list$variables$x) > as.numeric(fv))
      plot.list$variables = subset(plot.list$variables, as.numeric(plot.list$variables$y) > as.numeric(fv2))
    }
    else if(f == TRUE && f2 == FALSE && f3 == TRUE){
      validate(need(fv != "", "please enter a filter value"))
      validate(need(fv3 != "", "please enter a filter value"))
      plot.list$variables = subset(variablesDF, as.numeric(variablesDF$g) > as.numeric(fv3))
      plot.list$variables = subset(plot.list$variables, as.numeric(plot.list$variables$x) > as.numeric(fv))
    }
    else if(f == FALSE && f2 == TRUE && f3 == TRUE){
      validate(need(fv2 != "", "please enter a filter value"))
      validate(need(fv3 != "", "please enter a filter value"))
      plot.list$variables = subset(variablesDF, variablesDF$y > as.numeric(fv2))
      plot.list$variables = subset(plot.list$variables, as.numeric(plot.list$variables$g) > as.numeric(fv3))
    }
    else{
      plot.list$variables <- data.frame(plot.list$x, plot.list$y, plot.list$group)
    }
    colnames(plot.list$variables) <- c('x','y','g')
    plot.list$box = na.omit(data.frame(plot.list$group, plot.list$x))
    colnames(plot.list$box) <- c('g', 'x')
    
    return(plot.list)
  }
  
  
  #Machine Learning Data object to run models and create plots
  ml.obj <- function(mlvariables, d, rd, factorVal, factorResp, testdata, tinFile, r ){
    
    validate(need(!is.null(d) || length(intersect(mlvariables, colnames(d))), "Please make sure a data set is selected and proper variables are set"))
    
    validate(need(length(intersect(input$r, colnames(rd))), "please select response variable in response data/dataset"))
    
    validate(need(!is.null(rd) || nrow(as.data.frame(d)) == nrow(as.data.frame(rd)), "Response data has different number of rows than the dataset selected"))
    
    ml.list <- list()
    validate(need(length(r) != 0, "Please make sure response variable is set"))
    validate(need(length(c(mlvariables)) != 0, "Please make sure dataset is selected and loaded"))
    if (r %in% c(mlvariables)){
      ml.list$features <- d[, c(!c(mlvariables) %in% r) ]
      features = mlvariables[!c(mlvariables) %in% r]
    }
    else{
      ml.list$features <- d[,c(mlvariables)]
      features <- mlvariables
    }
    ml.list$features <- sapply(ml.list$features, as.numeric)
    ml.list$response <- with(rd,get(r))
    ml.list$response <- as.numeric(ml.list$response)
    ml.list$correct_factor <- 1
    if(factorResp == TRUE){
      above = sum(ml.list$response > factorVal)
      below = sum(ml.list$response < factorVal)
      validate(need(above != 0 || below != 0, "Please set a Factor Value that splits the data into more than one group"))
      ml.list$response.f = factor(as.numeric(ml.list$response) > as.numeric(factorVal), labels = c(0,1))
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
    if(testdata ==  "upload test data"){
      train.data = ml.list$featResp
      tinFile 
      validate(need(!is.null(tinFile), "Please select a data set or input a File"))
      test.data =  read.delim(tinFile$datapath, sep = gsub("\\t", "\t", input$tdatafile_sep, fixed = TRUE))
      
    }
    else{
      samples = row.names(ml.list$featResp)
      train.percent = .75
      inTrain = samples %in% sample(samples, floor(train.percent*length(samples)))
      train.data = droplevels(ml.list$featResp[inTrain,])
      test.data = ml.list$featResp[!inTrain,]
    }
    
    
    ml.list$train.data <- train.data
    ml.list$test.data <- test.data
    
    
    response.name = paste("Response-", r, sep = "")
    colnames(ml.list$featResp) <- c(features, response.name)
    
    return(ml.list)
  }
  
  observeEvent(input$mlButton, {
    values$mlvariables <- input$mlvariables
    values$mlmetric <- input$metric
    values$mlmetric2 <- input$metric2
    values$mlmetric3 <- input$metric3
    values$mlnumber <- input$number
    values$mlrepeats <- input$repeats
    values$mlLOOCV <- input$LOOCV
  })
  
 
  ##### Data Plots and Statistical Tests  
  ################################################################################################################################################################################################################      
  
  require(ggplot2)
  require(plotly)
  
  #Plot Data
  output$plot1 <- renderUI({
    plotlyOutput("p1", height = 400)
  })
  
  output$plot2 <- renderUI({
    plotlyOutput("p2", height = 400)
  })
  
  output$p1 <- renderPlotly({
    p1 <- plot.Type(input$plotType, input$x, input$y, input$group, input$bins, input$factorG, input$factorGVal, input$showPoints, input$xAxisLabel, input$yAxisLabel, input$legend, input$legendLabels1, input$title, input$fill, input$addTrend, input$filter, input$filterVal, input$filter2, input$filterVal2, input$filter3, input$filterVal3)
    print(p1)
    
  })
  
  output$p2 <- renderPlotly({
    validate(need(input$startDataset != "", "Please select a dataset"))
    p2 <- plot.Type(input$plotType2, input$x2, input$y2, input$group2, input$bins2, input$factorG2, input$factorGVal2, input$showPoints2, input$xAxisLabel2, input$yAxisLabel2, input$legend2, input$legendLabels1, input$title2, input$fill2, input$addTrend2, input$filterp2, input$filterValp2, input$filterp22, input$filterValp22, input$filterp23, input$filterValp23)
    print(p2)
  })
  
  plot.Type<-function(pt, x, y , g, bins, fg, fgv, ip, xa, ya, gl, leglab, t, c, at, f, fv, f2, fv2, f3, fv3){
    plot.df <- plot.obj(x,y,g, fg, fgv, f, fv, f2, fv2, f3, fv3)
    .theme <- theme(axis.line = element_line(colour = 'gray', size = .75), panel.background = element_blank(), plot.background = element_blank())  
    xaxisDf = switch(pt, "single boxplot" = x,
                     "boxplot" = paste(x, "by", g),
                     "histogram" = x, 
                     "density" = x, 
                     "multibar" = x,
                     "bar" = x,
                     "scatter" = x,
                     "grouped scatter" = x
    )
    yaxisDf = switch(pt, "single boxplot" = "",
                     "boxplot" = "",
                     "histogram" = paste('Frequency of', x), 
                     "density" = "Density", 
                     "multibar" = "Count",
                     "bar" = "Count",
                     "scatter" = y,
                     "grouped scatter" = y
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
                          "grouped scatter" = length(table(plot.df$variables$g))
    )
    solid = ifelse(c == "Solid Blue" || c == "Solid Grey" || c == "Solid Red", TRUE, FALSE )
    if(solid == FALSE){
      getPalette = colorRampPalette(brewer.pal(9, c))
    }else{
      if(c == "Solid Blue"){ getPalette = brewer.pal(9, "Blues")[8]}
      if(c == "Solid Blue"){ getPalette = brewer.pal(9, "Greys")[6]}
      if(c == "Solid Blue"){ getPalette = brewer.pal(9, "Reds")[7]}
    }
    
    xaxis = ifelse(xa == "", xaxisDf, xa)
    yaxis = ifelse(ya == "", yaxisDf, ya)
    
    switch(pt,
           "single boxplot" = {
             if(solid == FALSE){ fill = getPalette(9)[7]}
             else{fill = getPalette}
             if(ip == TRUE){
               gg <-ggplot(plot.df$box, aes(x = as.factor(0), y = x)) + geom_boxplot() + geom_point(position = "jitter", alpha = 0.5) 
               gg <- gg + labs( fill = fill, x = xaxis, y = yaxis)  + ggtitle(title) + .theme 
               p <- ggplotly(gg)
               p$x$data[[1]]$name = input$legendName
             }
             else{
               gg <-ggplot(plot.df$box, aes(x = as.factor(0), y = x)) + geom_boxplot() 
               gg <- gg + labs( fill = fill, x = xaxis, y = yaxis) + ggtitle(title) + .theme 
               p <- ggplotly(gg) 
             }
           },
           "boxplot" = {
             if(ip == TRUE){
               gg <-ggplot(plot.df$box, aes(x = as.factor(g), y = x, fill = as.factor(g))) + geom_boxplot() + geom_point(position = "jitter", alpha = 0.5) 
               gg <- gg + labs(fill = legend, x = xaxis, y = yaxis) + scale_fill_manual(values = getPalette(colourCount+ 2)[-(1:2)]) + ggtitle(title) + .theme + guides(fill=guide_legend(ncol=2))
               p <- ggplotly(gg) 
             }
             else{
               gg <-ggplot(plot.df$box, aes(x = as.factor(g), y = x, fill = as.factor(g))) + geom_boxplot() 
               gg <- gg + labs(fill = legend , x = xaxis, y = yaxis) + scale_fill_manual(values = getPalette(colourCount+ 2)[-(1:2)]) + ggtitle(title) + .theme + guides(fill=guide_legend(ncol=2))
               p <- ggplotly(gg) 
             }
           },
           "histogram" = {
             if(solid == FALSE){ fill = getPalette(9)[7]}
             else{fill = getPalette}
             stat_bin.test <- stat_bin(data = plot.df$variables, aes (x = x))
             print(stat_bin.test)
             #validate(need(!is.null(stat_bin.test$stat_params$bins), "Histograms are for Continuous Variables, consider using a bar chart"))
             gg <-ggplot(plot.df$variables, aes(x = x)) + geom_histogram(alpha=0.5, position= "identity", fill = fill, bins = bins)
             gg <- gg + labs( x = xaxis, y = yaxis) + ggtitle(title) + .theme + guides(fill=guide_legend(ncol=2)) 
             p <- ggplotly(gg)
           },
           "density" = {
             gg <-ggplot(plot.df$variables, aes(x = x, fill = as.factor(x), group 	= as.factor(x))) + geom_density(alpha=.75)
             gg <- gg + labs(fill = legend, x = xaxis, y = yaxis) + scale_fill_manual(values = getPalette(colourCount+ 2)[-(1:2)]) + ggtitle(title) + .theme + guides(fill=guide_legend(ncol=2))
             p <- ggplotly(gg) 
           },
           "multibar" = {
             gg <- ggplot(plot.df$variables, aes(x = x, fill = as.factor(g), group = as.factor(g))) + geom_bar(position = "dodge") 
             gg <- gg + labs(fill = legend, x = xaxis, y = yaxis) + scale_fill_manual(values = getPalette(colourCount + 5)[-(1:2)]) + ggtitle(title) + .theme + guides(fill=guide_legend(ncol=2))
             p <- ggplotly(gg) 
           },        
           "bar" =	{
             if(solid == FALSE){ 
               gg <-ggplot(plot.df$variables, aes(x = x, group = as.factor(x), fill = as.factor(x))) + geom_bar(position= "dodge")
               scale = getPalette(colourCount + 2)[-(1:2)]}
             else{
               scale = getPalette
               gg <-ggplot(plot.df$variables, aes(x = x, group = as.factor(x))) + geom_bar(position= "dodge", fill = scale)
               
             }
             gg <- gg + labs( x = xaxis, y = yaxis, fill = legend) + scale_fill_manual(values = scale) + ggtitle(title) + .theme + guides(fill=guide_legend(ncol=2))
             p <- ggplotly(gg) 
           },
           "grouped scatter" = {
             gg <- ggplot(plot.df$variables, aes(x = x, y = y, group = g ,color = as.factor(g))) + geom_point() 
             gg <- gg + labs( color = legend, x = xaxis, y = yaxis) + scale_color_manual(values = getPalette(colourCount+ 2)[-(1:2)]) + ggtitle(title) + .theme + guides(fill=guide_legend(ncol=2))
             if(at == TRUE){
               gg <- gg + stat_smooth(method = lm)
             }
             p <- ggplotly(gg) 
           },
           "scatter" ={
             gg <-ggplot(plot.df$variables, aes(x = x, y = as.factor(y))) + geom_point(color = getPalette(9)[7])
             gg <- gg + labs( x = xaxis, y = yaxis) +  ggtitle(title) + .theme + guides(fill=guide_legend(ncol=2))
             if(at == TRUE){
               gg <- gg + stat_smooth(method = lm)
             }
             p <- ggplotly(gg) 
           }
    )
    print(leglab)
    if(leglab != ""){
      print(leglab)
      print(length(leglab))
      print(length(length(p$x$data)))
      #validate(need(length(leglab) == length(p$x$data), paste("Please enter all legend labels separated by a comma. Legend has", length(p$x$data), "labels") ))
      for(i in 1:length(leglab)){p.t$x$data[[i]]$name <- leglab[i]}
    }
    return(p)
  }
  
  #statistical Testing
  stat.tests <- function(st, sig, x, y, g, fg, fgv, f, fv, f2, fv2, f3, fv3){
    var.df <- plot.obj(x,y,g, fg, fgv, f, fv, f2, fv2, f3, fv3)
    if(!is.null(var.df$variables)){
      groups = cbind.data.frame(as.numeric(var.df$x), droplevels(as.factor(var.df$group)))
      colnames(groups) <- c('x','g')
      conf.level = 1 - as.numeric(sig)
      p.val = sig
      x.st = as.numeric(var.df$x)
      x.g.sort <- groups[order(groups$g),]
      colnames(x.g.sort) <- c("x", "g")
      x.g.sort$x <-as.numeric(x.g.sort$x)
      contig = table(x.g.sort$x, x.g.sort$g)
      fish.dim = dim(contig)
      
      t.dat <- switch(st, 
                      "Shapiro-Wilk" = { validate(need(length(groups$x) < 4000, "Your Data contains more than 4000 samples, under the central limit theorem, you should be able to assume normality."))
                        t <- shapiro.test(x.st)
                        tab <- cbind(t$statistic, t$p.value)
                        colnames(tab) <- c("W-statistic", "P-value")
                        row.names(tab) <- c("Shapiro-Wilk")},
                      "Two sample t-test" = {validate(need(nlevels(groups$g) == 2, "A t-test requires exactly two groups, consider using Annova or factoring the grouping variable"))
                        t <- t.test(x ~ g, conf.level = conf.level, data = groups)
                        tab <- cbind(t$statistic, t$p.value)
                        colnames(tab) <- c("T-statistic", "P-value")
                        row.names(tab) <- c("Welch Two Sample t-test")}, 
                      "Paired t-test" = {validate(need(nlevels(groups$g) == 2, "A t-test requires exactly two groups, consider using Annova or factoring the grouping variable"))
                        validate(need(table(groups$g)[1] == table(groups$g)[2], "A Paired t-test requires paired observations for each group, consider using two sample t-test"))
                        t <- t.test(x ~ g, conf.level = conf.level, data = groups, paired = TRUE)
                        tab <- cbind(t$statistic, t$p.value)
                        colnames(tab) <- c("T-statistic", "P-value")
                        row.names(tab) <- c("Welch Paired t-test")
                      },
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
                      colnames(tab) <- c("Chi-squared", "P-value")
                      },
                      "Chi-Squared" = {t <- chisq.test(table(contig), correct = FALSE)
                      tab <- cbind(t$statistic, t$p.value)
                      colnames(tab) <- c("Chi-Squared", "P-value")
                      }, 
                      "Fisher's Exact" = {validate(need(fish.dim[1] & fish.dim[2] == 2, "Please use variables that are binary (2 levels), consider factoring the variable or chosing a different test"))
                        t <- fisher.test(contig)
                        tab <- cbind(t$estimate, t$p.value)
                        colnames(tab) <- c("Odds Ratio", "P-Value")
                        row.names(tab) <- c("Fisher's Exact")
                      }
      )
      return(tab)
    }
  }
  
  output$statT <- renderText("Statistical Testing")
  output$statTxt <- renderText("Shapiro Wilks test is conducted on the independent variables. Two sample tests, Annova, and Kruskal-Wallis are conducted on the independent variables
                               and the grouping variables")
  
  output$stat.res <- DT::renderDataTable(stat.tests(input$statTest, input$sig, input$x, input$y, input$group, input$factorG, input$factorGVal, input$filter, input$filterVal, input$filter2, input$filterVal2, input$filter3, input$filterVal3), server = F, escape = FALSE,
                                         extensions = c('Buttons', 'ColReorder' = NULL), 
                                         options = list(dom = 'BRrltpi', autoWidth = FALSE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, 
                                                        buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'))), class = 'table-bordered table-condensed table-striped table-compact')
  
  output$stat.res2 <- DT::renderDataTable(stat.tests(input$statTest2, input$sig2, input$x2, input$y2, input$group2,  input$factorG2, input$factorGVal2input$filterp2, input$filterValp2, input$filterp22, input$filterValp22, input$filterp23, input$filterValp23), server = F, escape = FALSE,
                                          extensions = 'Buttons', options = list(dom = 'Bfrtip', autoWidth = FALSE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, 
                                                                                 buttons = list('copy', 'print', 'csv')), class = 'table-bordered table-condensed table-striped table-compact')
  
  ##### Correlation and Multiple Regression
  ################################################################################################################################################################################################################
  
  #update correlation variables 
  observe({
    if(length(cont_cat()) < 2){selected = numericColumns()} else{selected = cont_cat()}
    updateCheckboxGroupInput(session, "corrVariablesCheckbox", choices = numericColumns(), selected = selected)
    
    updateSelectInput(session, "corrVariables", choices = numericColumns(), selected = selected)
  })
  
  #Link correlation variable Selection
  observe({
    if(input$corrVariablesStyle == "Checkbox") {
      updateCheckboxGroupInput(session, "corrVariablesCheckbox", choices = numericColumns(), selected = input$corrVariables)
    }
  })
  
  observe({
    updateSelectInput(session, "corrVariables", selected = input$corrVariablesCheckbox)
  })
  
  observe({
    if (input$selectallCV > 0) {
      if (input$selectallCV %% 2 == 0){
        updateCheckboxGroupInput(session=session, "corrVariablesCheckbox", choices = numericColumns(), selected = numericColumns())
        
      }
      else {
        updateCheckboxGroupInput(session=session, "corrVariablesCheckbox", choices = numericColumns(), selected = c(character(0)))
        
      }}
  })
  
  
  observe({
    updateSelectInput(session, "IMRVariables", choices = numericColumns())
  })
  
  observe({
    updateSelectInput(session, "DMRVariable", choices = numericColumns(), selected = numericColumns()[1])
  })
  
  observe({
    updateSelectInput(session, "survVariables", choices = numericColumns())
  })
  
  observe({
    updateSelectInput(session, "survOutVariable", choices = numericColumns(), selected = numericColumns()[1])
  })

  observe({
    updateSelectInput(session, "survTimeVariable", choices = numericColumns())
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
    corrvariables <- input$corrVariablesCheckbox
    validate(need(input$startDataset != "", "Please select a dataset"))
    validate(need(length(corrvariables) >= 2 , "Please select atleast 2 varialbes for correlation plot"))
    if(!length(intersect(corrvariables, colnames(data)))) {
      NULL
    } else {
      cor(dataset()[,corrvariables], use = input$corUse, method = input$corMethod)
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
  mult.reg <- function(dataset, dvmr, cv, log){
    if(is.null(cv) || is.null(dvmr)){return(NULL)}
    fam = ifelse(log, 'binomial', 'gaussian')
    if(log){
      validate(need(all.equal(levels(as.factor(dataset[,dvmr])), c(0,1)), "Please make sure the dependent variable is two levels and has values 0 and 1"))
    }
    y.df = dataset[,dvmr]
    x.df = dataset[,c(cv[!cv %in% dvmr])]
    mr.df <- cbind.data.frame(x.df, y.df)
    colnames(mr.df) <- c(c(cv[!cv %in% dvmr]), 'y')
    fit.mr <- glm(y~., family = fam , data = mr.df )
    result.mr <- summary(fit.mr)$coefficients
    colnames(result.mr) <- c(colnames(result.mr)[1:3],  'Pr(>|z or t|)')
    return(result.mr)
  }
  
  cox.surv <- function(dataset, TtoE, outv, sv){
    if(is.null(sv) || is.null(outv) || is.null(TtoE)){return(NULL)}
    y <- paste('Surv(',TtoE,',' ,outv,')')
    n <- c(sv)
    print('cox, sv var')
    f <- as.formula(paste(y, "~", paste(n[!n %in% y], collapse = " + ")))
    f
    print(f)
    cox.surv <- coxph(f,data=dataset)
    cox <- cbind.data.frame(summary(cox.surv)$coefficients, summary(cox.surv)$logtest[1], summary(cox.surv)$logtest[3])
    colnames(cox) <- c("Coefficient", "Exp(Coefficient)", "Std. Error", "z", 'p-value', "log test score", "log test p-value")
    
    sfit <- survfit(f, data = dataset)
    survResults <- c(list(cox), list(sfit))
    print(survResults)
    return(survResults)
  }
  
  output$cox <- DT::renderDataTable(server = F, {
    validate(need(input$startDataset != "", "Please select a dataset"))
    validate(need(input$survOutVariable != 0, "Please make sure a dataset is selected and the indpendent and dependent variables are set"))
    validate(need(input$survVariables != 0, "Please make sure a dataset is selected and the indpendent and dependent variables are set"))
    validate(need(input$survTimeVariable != 0, "Please make sure a dataset is selected and the indpendent and dependent variables are set"))
    validate(need(all.equal(levels(as.factor(dataset()[,input$survOutVariable])), c(0,1)), "Please make sure the outcome variable is two levels and has values 0 and 1"))
    cox.df <- cox.surv(dataset(), input$survTimeVariable, input$survOutVariable, input$survVariables)[[1]]
    survTable <- datatable(cox.df, escape = FALSE, extensions = list('Buttons' = NULL, 'ColReorder' = NULL, 'RowReorder' = NULL), 
                           options = list(dom = 'BRrltpi', autoWidth = TRUE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, RowReorder = TRUE, 
                                          buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'), I('colvis'))), 
                           class = 'table-bordered table-condensed table-striped table-compact', caption = "Cox Proportional Hazards Results") %>%
      formatStyle(c('p-value','log test p-value'), color = styleInterval(input$survPvalue, c('red','black')), backgroundColor = styleInterval(input$survPvalue, c('yellow', 'white'))) %>%
      formatRound(1:length(cox.df), 5)
    return(survTable)
  })
  
  output$survTable <- DT::renderDataTable(server = F, {
    validate(need(input$startDataset != "", "Please select a dataset"))
    validate(need(input$survOutVariable != 0, "Please make sure a dataset is selected and the indpendent and dependent variables are set"))
    validate(need(length(input$survVariables) == 1, "For Kaplan-Meier Data and Survival Plot please use One Survival Variable"))
    validate(need(input$survTimeVariable != 0, "Please make sure a dataset is selected and the indpendent and dependent variables are set"))
    validate(need(all.equal(levels(as.factor(dataset()[,input$survOutVariable])), c(0,1)), "Please make sure the outcome variable is two levels and has values 0 and 1"))
    surv <- cox.surv(dataset(), input$survTimeVariable, input$survVariables, input$survOutVariable)[[2]]
    surv.df <- as.data.frame(summary(surv)$table)
    survTable <- datatable(surv.df,  escape = FALSE, extensions = list('Buttons', 'ColReorder', 'RowReorder'), 
                           options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'pdf', 'print'), autoWidth = FALSE, colReorder = TRUE, rowReorder = TRUE),
                           class = 'table-bordered table-condensed table-striped table-compact', caption = "Survival Data Summary") %>% 
      formatRound(1:length(surv.df), 5)
    return(survTable)
  })
  
  output$survPlot <- renderUI({
    validate(need(input$startDataset != "", "Please select a dataset"))
    validate(need(input$survOutVariable != 0, "Please make sure a dataset is selected and the indpendent and dependent variables are set"))
    validate(need(input$survVariables != 0, "Please make sure a dataset is selected and the indpendent and dependent variables are set"))
    validate(need(input$survTimeVariable != 0, "Please make sure a dataset is selected and the indpendent and dependent variables are set"))
    validate(need(all.equal(levels(as.factor(dataset()[,input$survOutVariable])), c(0,1)), "Please make sure the outcome variable is two levels and has values 0 and 1"))
    plotlyOutput("survP", height = 400)
  })
  
  
  output$survP <- renderPlotly({
    TtoE <- input$survTimeVariable
    sv <- input$survVariables
    outv <- input$survOutVariable
    if(is.null(sv) || is.null(outv) || is.null(TtoE)){return(NULL)}
    y <- paste('Surv(',TtoE,',' ,outv,')')
    n <- c(sv)
    validate(need(length(n) == 1, "For Kaplan-Meier Data and Survival Plot please use One Survival Variable"))
    f <- as.formula(paste(y, "~", paste(n[!n %in% y], collapse = " + ")))
    f
    sfit <- survfit(f, data = dataset())
    ggs <- autoplot(sfit, censor.shape = '*', censor.size = .3, conf.int = FALSE)
    ggs <- plotly_build(ggs)
    survP <- ggplotly(ggs)
    print(ggs)
  })
  
  output$mr <- DT::renderDataTable(server = F, {
    validate(need(input$startDataset != "", "Please select a dataset"))
    validate(need(input$DMRVariable != 0, "Please make sure a dataset is selected and the indpendent and dependent variables are set"))
    validate(need(input$IMRVariables != 0, "Please make sure a dataset is selected and the indpendent and dependent variables are set"))
    mr.df <- as.data.frame(mult.reg(dataset(), input$DMRVariable, input$IMRVariables, input$log))
    dat <- datatable(mr.df, escape = FALSE, extensions = list('Buttons' = NULL, 'ColReorder' = NULL, 'RowReorder' = NULL), 
                     options = list(dom = 'BRrltpi', autoWidth = TRUE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, RowReorder = TRUE, 
                                    buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'), I('colvis'))), 
                     class = 'table-bordered table-condensed table-striped table-compact', caption = "Multiple Regression Coefficient Results") %>%
      formatStyle('Pr(>|z or t|)', color = styleInterval(input$pvalue, c('red','black')), backgroundColor = styleInterval(input$pvalue, c('yellow', 'white'))) %>% 
      formatRound(1:length(mr.df), 5)
    return(dat)
  })
  
  #####   MACHINE LEARNING    
  ################################################################################################################################################################################################################    
  
  #Update ML variable selection
  observe({
    updateCheckboxGroupInput(session, "mlvariablesCheckbox", choices = numericColumns(), selected = numericColumns())
    
    updateSelectInput(session, "mlvariables", choices = numericColumns(), selected = numericColumns())
  })
  
  #Link ML variable Selection
  observe({
    if(input$mlvariablesStyle == "Checkbox") {
      updateCheckboxGroupInput(session, "mlvariablesCheckbox", selected = isolate(input$mlvariables))
    }
  })
  
  observe({
    updateSelectizeInput(session, "mlvariables", selected = input$mlvariablesCheckbox)
  })
  
  observe({
    if (input$selectallML > 0) {
      if (input$selectallML %% 2 == 0){
        updateCheckboxGroupInput(session=session, "mlvariablesCheckbox", selected = numericColumns())
        
      }
      else {
        updateCheckboxGroupInput(session=session, "mlvariablesCheckbox", selected = c(character(0)))
        
      }}
  })
  
  observe({
    updateSelectInput(session, "tableOneVariables", choices = datanames()[[1]])
  })
  
  observe({
    updateSelectInput(session, "tableOneVariables", choices = datanames()[[1]])
  })
  
  observe({
    obj <- datanames()[[1]]
    var.opts<-unique(c(colnames(rdataset()), obj))
    updateSelectInput(session, "tableOneResponse", choices = var.opts)
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
    validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    validate(need(!is.null(values$datasource), "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    showOutput("fp1", "highcharts")
  })
  
  
  require(rCharts)
  options(RCHART_WIDTH = 400)
  
  #plot Importance
  output$iPlot <- renderUI({
    validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    validate(need(!is.null(values$datasource), "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    switch(input$method,  
           "random forest" = showOutput("ip1", "highcharts"), 
           "bayesian generalized linear" = DT::dataTableOutput("ip.bgl"),
           "CART" = plotOutput("ip.rp"),
           "C4.5 algorithm"= showOutput("ip1", "highcharts"),
           "bagged CART" = showOutput("ip1", "highcharts"),
           "generalized boosted modeling" = showOutput("ip1", "highcharts")
    )
  })
  
  
  output$rPlot <- renderUI({
    validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    validate(need(!is.null(values$datasource), "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    plotOutput("rp")
  })
  
  require(rCharts)
  options(RCHART_WIDTH = 650)
  
  output$aPlot <- renderUI({
    validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    validate(need(!is.null(values$datasource), "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    switch(input$method,  
           "random forest" = div(style = 'overflow-x: scroll', dataTableOutput("ap.rf")), 
           "bayesian generalized linear" = showOutput("ap1", "highcharts"),
           "CART" = showOutput("ap1", "highcharts"),
           "C4.5 algorithm"= plotOutput("ap.c50"),
           "bagged CART" = showOutput("ap1", "highcharts"),
           "generalized boosted modeling" = showOutput("ap1", "highcharts")
    )
  })
  
  
  output$ip1 <- renderChart2({
    validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    validate(need(!is.null(values$datasource), "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    if(input$method == "random forest" || input$method == "C4.5 algorithm" || input$method == "bagged CART" || input$method == "generalized boosted modeling"){
      ip1  <- var.imp(input$method, values$mlmetric, values$mlnumber,  values$mlrepeats, values$mlLOOCV)[[2]]
      print(ip1)
    }
    else{
      return(Highcharts$new())
    }
    
  })
  
  
  
  
  output$fp1 <- renderChart2({
    validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    validate(need(!is.null(values$datasource), "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    featp1 <- fs.plot(input$fsMethod)
    print(featp1)
  })
  
  
  output$ip.bgl <- DT::renderDataTable(server = F, {
    validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    validate(need(!is.null(values$datasource), "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    if(input$method == "bayesian generalized linear"){
      df  <- var.imp(input$method, values$mlmetric, values$mlnumber,  values$mlrepeats, values$mlLOOCV)[[2]]
      dt <- datatable(df, escape = FALSE, options = list(dom = 'BRrltpi', autoWidth = FALSE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, RowReorder = TRUE, 
                                         buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'), I('colvis'))), 
                      class = 'table-bordered table-condensed table-striped table-compact', caption = "Bayesian Generalized Linear Regression Results") %>%
        formatStyle('Pr(>|t|)', color = styleInterval(0.05, c('red','black')), backgroundColor = styleInterval(0.05, c('yellow', 'white'))) %>% 
        formatRound(1:length(df), 5)
      return(dt)
    }
  })
  
  output$ip.rp <-renderPlot({
    validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    validate(need(!is.null(values$datasource), "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    if(input$method == "CART"){
      ip.rp.list <- var.imp(input$method,  values$mlmetric, values$mlnumber,  values$mlrepeats, values$mlLOOCV)
      ip.rp <- ip.rp.list[[2]]
    }
  })
  
  output$a1 <- DT::renderDataTable(server = F, {
    validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    validate(need(!is.null(values$datasource), "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    a1.df <- accuracy(input$method, values$mlmetric, values$mlnumber,  values$mlrepeats, values$mlLOOCV)[[1]]
    a1 <- datatable(a1.df, escape = FALSE, options = list(dom = 'BRrltpi', autoWidth = FALSE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, RowReorder = TRUE, 
                                          buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'), I('colvis'))),
                    class = 'table-bordered table-condensed table-striped table-compact', caption = paste("Confusion Matrix with Test Data for Model:", input$method))
    return(a1)
  })
  output$a2 <- DT::renderDataTable(server = F, {
    validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    validate(need(!is.null(values$datasource), "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    a2.df <- accuracy(input$method2, values$mlmetric2,  values$mlnumber,  values$mlrepeats, values$mlLOOCV)[[1]]
    a2 <- datatable(a2.df, escape = FALSE, options = list(dom = 'BRrltpi', autoWidth = FALSE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, RowReorder = TRUE, 
                                          buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'), I('colvis'))), 
                    class = 'table-bordered table-condensed table-striped table-compact', caption = paste("Confusion Matrix with Test Data for Model:", input$method2))
    return(a2)
  })
  output$a3 <- DT::renderDataTable(server = F, {
    validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    validate(need(!is.null(values$datasource), "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    a3.df <- accuracy(input$method3, values$mlmetric3,  values$mlnumber,  values$mlrepeats, values$mlLOOCV)[[1]]
    a3 <- datatable(a3.df, escape = FALSE, options = list(dom = 'BRrltpi', autoWidth = FALSE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, RowReorder = TRUE, 
                                          buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'), I('colvis'))), 
                    class = 'table-bordered table-condensed table-striped table-compact', caption = paste("Confusion Matrix with Test Data for Model:", input$method3))
    return(a3)
  })
  
  output$ap1 <- renderChart2({
    if(input$method == "bayesian generalized linear" || input$method == "CART" || input$method == "bagged CART" || input$method == "generalized boosted modeling"){
      ap.list <-accuracy(input$method, values$mlmetric, values$mlnumber,  values$mlrepeats, values$mlLOOCV)
      ap1 <- ap.list[[2]]
    }
    else{
      return(Highcharts$new())
    }
  })
  
  output$ap.rf <- DT::renderDataTable(server = F, {
    if(input$method == "random forest"){
      rf.list <- accuracy(input$method,  values$mlmetric, values$mlnumber,  values$mlrepeats, values$mlLOOCV)
      rf.df <- as.list.data.frame(rf.list[[2]])
      sketch = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(colspan = 1, " "),
            th(colspan = ncol(rf.df), 'Actual')
            
          ),
          tr(lapply(c("Predicted", colnames(rf.df)), th))
        )
      ))
      dt.rf <- datatable(rf.df, escape = FALSE, container = sketch, options = list(dom = 'BRrltpi', autoWidth = FALSE, scrollX = TRUE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, RowReorder = TRUE, 
                                                                   buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'), I('colvis'))), 
                         class = 'table-bordered table-condensed table-striped table-compact', caption = "Random Forest Confusion Matrix")
      return(dt.rf)
    }
  })
  
  output$ap.c50 <- renderPlot({
    if(input$method == "C4.5 algorithm"){
      ap.list <- accuracy(input$method,  values$mlmetric, values$mlnumber,  values$mlrepeats, values$mlLOOCV)
      ap.c50 <- plot(ap.list[[2]], main = "Accuracy vs Number of Baggs")
    }
    
  })
  
  
  # Plot info TPR/FPR ROC with diagonal line and legend
  output$rp <- renderPlot({
    perf1 <- roc("random forest", "Accuracy",  values$mlnumber,  values$mlrepeats, values$mlLOOCV)
    if(is.null(perf1)) return(NULL)
    perf2 <- roc("CART", "Accuracy",  values$mlnumber,  values$mlrepeats, values$mlLOOCV)
    if(is.null(perf2)) return(NULL)
    perf3 <- roc("C4.5 algorithm", "Accuracy",  values$mlnumber,  values$mlrepeats, values$mlLOOCV)
    if(is.null(perf3)) return(NULL)  
    rp <- {plot(perf1, lwd = 5, col = 'blue', yaxt='n', xaxt='n', ann=F )
      abline(a = 0, b= 1)
      lines(perf2@x.values[[1]], perf2@y.values[[1]], col = 'red', lwd = 1.5)
      lines(perf3@x.values[[1]], perf3@y.values[[1]], col = 'green', lwd = 1.5)
      legend("bottomright", "(x,y)", c("Model 1: Random Forest", "Model 2: CART", "Model 3: C4.5 algorithm"), lwd=2, lty=1,col=c('blue','red','green'), cex = 0.85 )}
    
    return(rp)
  })
  
  
  
  #model based on input 
  ml.model <- function(mt, m, n, r, ilo){
    mlo <- ml.obj(values$mlvariables, dataset(), rdataset(), input$factorVal, input$factorResp, input$testdata, input$datafile, input$r)
    nt = 1
    
    
    colnames(mlo$train.data)[ncol(mlo$train.data)] <- "response"
    train.data = mlo$train.data
    
    
    #Control for Cross Validation and Repeats
    ifelse(ilo == TRUE, number <- "LOOCV", number <- n)
    repeats <- r
    metric <- m
    control <- trainControl(method="repeatedcv", number=number, repeats=repeats)
    
    
    withProgress(message = 'Machine Learning in progress', value=nt, {
      incProgress(nt/6, detail = paste(nt, mt))
      
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
             colnames(imp.df) <- c('Variable', 'Importance')
             print(imp.df)
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
             colnames(imp.df) <- c('Variable', 'Importance')
           },
           "CART" = {ip <- fancyRpartPlot(fit$finalModel)
           imp.df = summary(fit)
           colnames(imp.df) <- c('Variable', 'Importance')
           }, 
           "C4.5 algorithm" = { 
             importance = C5imp(fit$finalModel)
             imp.df = as.data.frame(cbind(rownames(importance), importance$Overall))
             colnames(imp.df) <- c('Variable', 'Importance')
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
             colnames(imp.df) <- c('Variable', 'Importance')
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
             colnames(imp.df) <- c('Variable', 'Importance')
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
  
  fs.plot<-function(fst){
    mlo <- ml.obj(values$mlvariables, dataset(), rdataset(), input$factorVal, input$factorResp, input$testdata, input$datafile, input$r) 
    validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    validate(need(!is.null(values$datasource), "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
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
  #ROC curve 
  roc <- function(mt, m, n, r, ilo){
    fit <- ml.model(mt, m, n, r, ilo)
    mlo <- ml.obj(values$mlvariables, dataset(), rdataset(), input$factorVal, input$factorResp, input$testdata, input$datafile, input$r) 
    
    test.data = mlo$test.data
    twoClass = nlevels(as.factor(test.data$response))
    validate(need(twoClass == 2, "The ROC plot requires a response variable with two classes only"))
    
    #get predicted values, predictions, and performance for ROC
    Predicted = predict(fit, test.data[,(1:ncol(test.data)-1)], type = 'prob')
    pred <- prediction(Predicted[,2], as.data.frame(test.data$response), label.ordering = NULL)
    perf <- performance(pred, 'tpr', 'fpr')
    
    return(perf)
  }
  
  accuracy <- function(mt, m, n, r, ilo){
    mlo <- ml.obj(values$mlvariables, dataset(), rdataset(), input$factorVal, input$factorResp, input$testdata, input$datafile, input$r) 
    test.data = mlo$test.data
    fit <- ml.model(mt, m, n, r, ilo)
    switch(mt,
           "random forest" = {
             pred = predict(fit, test.data[,1:ncol(test.data)-1])
             actual =  test.data$response
             cm = confusionMatrix(pred, actual)
             ap = cm$table
             a = as.data.frame(cm$overall)
             colnames(a) <- 'Model Statistics'
           }, 
           "bayesian generalized linear" = { 
             pred = predict(fit, test.data[,1:ncol(test.data)-1])
             actual =  test.data$response
             a = fit$results
             colnames(a) <- 'Model Statistics'
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
             a = fit$results
             colnames(a) <- 'Model Statistics'
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
             cm = confusionMatrix(pred,actual)
             a = as.data.frame(cm$overall)
             colnames(a) <- 'Model Statistics'
             ap = fit
           }, 
           "bagged CART" = { 
             pred = predict(fit, test.data[,1:ncol(test.data)-1])
             actual =  test.data$response
             a = fit$results
             colnames(a) <- 'Model Statistics'
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
             a = fit$results
             colnames(a) <- 'Model Statistics'
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
  output$tableOneNewColName <- renderUI({
    textInput( "tableOneNewColName", "New Column Label")
  })
  
  output$tableOneColNamesChange <- renderUI({
    var.opts <- colnames(values$tableOne)
    selectizeInput("tableOneColNamesChange", "Select Column Label to Change", choices = var.opts)
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

  makeTableOne <- function(dataset, outcome, tabVar){
    if(is.null(outcome) || is.null(tabVar)){return(NULL)}
    if(length(outcome) == 0  || length(tabVar) == 0){return(NULL)}
    continuousVar <- cont_cat()
    continuousVar <- continuousVar[continuousVar %in% tabVar]
    data = dataset[,c(outcome,tabVar)]
    var_tables <- lapply(1:length(tabVar), function(i,data) {
      if(tabVar[i] %in% continuousVar){
               data_var_out <- na.omit(data[,c(tabVar[i],outcome)])
               colnames(data_var_out) <-  c('var', 'out')
               data_var_out$var <- as.numeric(data_var_out$var)
               dfmean <- group_by(data_var_out, out) %>% summarize(m = mean(var))
               m <- t(as.data.frame((round(dfmean[,2],2))))
               colnames(m) <- c(names(table(data_var_out$out)))
               meantbl <- m
               
               var_sep <- t(as.data.frame(c( rep("  ", length(table(data[,outcome]))))))
               colnames(var_sep) <- c(names(table(data[,outcome])))

               mq <- group_by(data_var_out, out) %>% summarize(m = median(var), q1 = quantile(var, probs=0.25), q3 = quantile(var, probs=0.75))
               q3_1 <- mq[,'q3']-mq[,'q1']
               mq.v <- unlist(lapply(1:nrow(mq), function(x) paste(mq[x,'m'], " (", q3_1[x,'q3'], ")", sep = '')))
               mean_med_df <- rbind(var_sep, meantbl, unlist(mq.v))
               row.names(mean_med_df) <- c(tabVar[i], " mean", ' median (Q3-Q1)')
               mean_med_tbl <- as.data.frame(mean_med_df)
              
               
               return(as.data.frame(mean_med_tbl))
      }
      else{
        counts <- table(data[,tabVar[i]], data[,outcome])
        percent <- round(prop.table(counts)*100,2)
        count_percent <- paste(counts,' (', percent, '%)', sep="")
        tbl_cp <- as.data.frame(matrix(count_percent, ncol = ncol(counts), byrow = TRUE))
        colnames(tbl_cp) <- c(names(table(data[,outcome])))
        var_sep <- t(as.data.frame(c( rep("  ", length(table(data[,outcome]))))))
        colnames(var_sep) <- c(names(table(data[,outcome])))
        tbl <- rbind.data.frame(var_sep,tbl_cp)
        indent <- paste(" ", row.names(tbl)[2:nrow(tbl)], sep = " ")
        row.names(tbl) <- c(tabVar[i], indent)
        return(as.data.frame(tbl))
      }
    }, 
    data = data)
  
   totals <- paste(table(data[,outcome]),' (', round(prop.table(table(data[,outcome]))*100,2), '%)', sep="")
   tot_tbl <- t(as.data.frame(totals))
   colnames(tot_tbl) <- c(names(table(data[,outcome])))
   row.names(tot_tbl) <- c("Total (N (%))")
   
   tableOne <-  do.call("rbind", c(list(as.data.frame(tot_tbl)), var_tables))
   tableONe <- cbind.data.frame(row.names(tableOne), tableOne)
   values$tableOne <- tableOne
   return(tableOne)
  }
  
  observe({
    validate(need(length(input$tableOneVariables) != 0, "please make sure all table variables are set"))
    validate(need(length(input$tableOneResponse) != 0, "please make sure all table variables are set"))
    levelsResp <- nlevels(as.factor(dataset()[,input$tableOneResponse]))
    outcome <- input$tableOneResponse
    validate(need(levelsResp < 10, "please choose an outcome variable that is a factor less than 10 levels/classes"))
    makeTableOne(dataset(), input$tableOneResponse, input$tableOneVariables)
  })
  
  
  t1 <- reactive({
      t1 <- values$tableOne
      return(values$tableOne)
  })
  
   
  output$tableOne <- DT::renderDataTable(server =  FALSE , {
    validate(need(length(input$tableOneVariables) != 0, "please make sure all table variables are set"))
    validate(need(length(input$tableOneResponse) != 0, "please make sure all table variables are set"))
    levelsResp <- nlevels(as.factor(dataset()[,input$tableOneResponse]))
    outcome <- input$tableOneResponse
    validate(need(levelsResp < 10, "please choose an outcome variable that is a factor less than 10 levels/classes"))
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
                          options = list(dom = 'BRrltpi', ordering=F, autoWidth = FALSE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE,
                                         buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'), I('colvis'))), 
                          class = 'table-bordered table-condensed table-striped table-compact') %>%
      formatStyle(colnames(t1())[1],
                            target = 'row',
                            backgroundColor = styleEqual(c("  "), c('silver')))
    
    
    return(tableOne)
  })
  
  output$tableOneContVar <- renderPrint({
    continuousVar <- cont_cat()
    continuousVar <- continuousVar[continuousVar %in% input$tableOneVariables]
    validate(need(length(input$tableOneVariables) != 0, "please make sure all table variables are set"))
    validate(need(length(input$tableOneResponse) != 0, "please make sure all table variables are set"))
    data <- dataset()
    print(continuousVar)
    if(length(continuousVar) > 0){
      varList <- lapply(1:length(continuousVar), function(i) {
         paste(continuousVar[i], nlevels(as.factor(data[,continuousVar[i]])), "levels")
      })
      print(varList)
    }
    else{
        str("There are no variables in Table One with more than 10 levels")
    }
  })
  
  ##### Data Tables  
  ################################################################################################################################################################################################################      
  
  mlTables <- function(tablename){
    mlo <- ml.obj(values$mlvariables, dataset(), rdataset(), input$factorVal, input$factorResp, input$testdata, input$datafile, input$r) 
    tab <- with(mlo, get(tablename))
    return(tab)
  }
  
  
  
  output$mldataTable <- DT::renderDataTable({
    validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    validate(need(!is.null(values$datasource), "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    mldataTable <- datatable(mlTables('featResp'), escape = FALSE, options = list(dom = 'BRrltpi', autoWidth = TRUE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, RowReorder = TRUE, 
                                                                                buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'), I('colvis'))),
                                             class = 'table-bordered table-condensed table-striped table-compact', caption = "Feature and Response variable Matrix" ) 
    })
  output$avgValTable <- DT::renderDataTable({
    validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    validate(need(!is.null(values$datasource), "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    avgValTable <- datatable(mlTables('avgVal'), escape = FALSE,  options = list(dom = 'BRrltpi', autoWidth = TRUE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, RowReorder = TRUE, 
                                                                                                                 buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'), I('colvis'))), class = 'table-bordered table-condensed table-striped table-compact', 
                                            caption = "Average Column Values for Feature Variables" ) 
    })
  output$corrValTable <- DT::renderDataTable({
    validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    validate(need(!is.null(values$datasource), "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    corrValTable <- datatable(mlTables('corrVal'), escape = FALSE, options = list(dom = 'BRrltpi', autoWidth = TRUE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, RowReorder = TRUE, 
                                                                                 buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'), I('colvis'))), 
                                             class = 'table-bordered table-condensed table-striped table-compact', caption = "Correlation of Feature Variables with Response Variable" ) 
    })
  output$igTable <- DT::renderDataTable({
    validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    validate(need(!is.null(values$datasource), "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    igTable <- datatable(mlTables('ig'), escape = FALSE, options = list(dom = 'BRrltpi', autoWidth = TRUE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, RowReorder = TRUE, 
                                                                       buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'), I('colvis'))),
                                         class = 'table-bordered table-condensed table-striped table-compact', caption = "Information Gain for Feature Variables" )
    })
  output$ipTable <- DT::renderDataTable({
    validate(need(input$mlButton != 0 , "Please set Machine Learning Variables and Options and Click the 'Run Machine Learning' button"))
    varImp <- var.imp(input$method, values$mlmetric, values$mlnumber,  values$mlrepeats, values$mlLOOCV)[[1]]
    varImpSorted <- varImp[with(varImp, order(-Importance)), ]
    ipTable <- datatable(varImpSorted, escape = FALSE, rownames = FALSE, extensions = list('Buttons' = NULL, 'ColReorder' = NULL, 'RowReorder' = NULL), 
                         options = list(dom = 'BRrltpi', autoWidth = TRUE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, RowReorder = TRUE, 
                                        buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'), I('colvis'))), 
                         class = 'table-bordered table-condensed table-striped table-compact', caption = "Importance Plot Data")
    return(ipTable)
    })
  
  output$plotvar1 <- DT::renderDataTable({
    p.df1 <- plot.obj(input$x, input$y, input$group, input$factorG, input$factorGVal, input$filter, input$filterVal, input$filter2, input$filterVal2, input$filter3, input$filterVal3)
    p.df1.xyg <- cbind.data.frame(p.df1$variables, p.df1$group)
    colnames(p.df1.xyg) <- c(input$x,input$y, input$group)
    p.dT1 <- datatable(p.df1.xyg, escape = FALSE, options = list(dom = 'BRrltpi', autoWidth = TRUE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, RowReorder = TRUE, 
                                                 buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'), I('colvis'))),
                       class = 'table-bordered table-condensed table-striped table-compact', caption = "Independent, Dependent, and Group Variable Data for Left Plot" )
    return(p.dT1)
  })
  
  output$plotvar2 <- DT::renderDataTable({
    p.df2 <- plot.obj(input$x2, input$y2, input$group2, input$factorG2, input$factorGVal2, input$filterp2, input$filterValp2, input$filterp22, input$filterValp22, intput$filterp23, input$filterValp23)
    p.df2.xyg <- cbind.data.frame(p.df2$variables, p.df2$group)
    colnames(p.df2.xyg) <- c(input$x2,input$y2, input$group2)
    p.dT2 <- datatable(p.df2.xyg, escape = FALSE, options = list(dom = 'BRrltpi', autoWidth = TRUE, lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')), ColReorder = TRUE, RowReorder = TRUE, 
                                                 buttons = list('copy', 'print', list(extend = 'collection', buttons= c('csv', 'pdf'), text = 'Download'), I('colvis'))),
                       class = 'table-bordered table-condensed table-striped table-compact', caption = "Independent, Dependent, and Group Variable Data for Right Plot" )
    return(p.dT2)
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

shinyApp(ui = ui22, server = server22)

  output$mutations <- renderUI({
    numericInput("mutations", "Number of Columns to Add", value = 1, min = 1, step = 1)
  })
                               output$mutateFormula <- renderUI({
                               if(input$mutations == 1){
                               textInput("formula", "Formula")
                               }
                               else{
                               lapply(1:length(input$mutations), function(i) {
                               counter = 1
                               div(displaystyle="height: 30px;", textInput(paste0("formula",counter), "Formula"))
                               counter = counter + 1 
                               })
                               }
                               })
                               
                               output$mutateAverage <- renderUI({
                               if(input$mutations == 1){
                               selectInput("mutateAvgCol", "Columns to Average", choices = datanames()[[1]], multiple = T)
                               }
                               else{
                               lapply(1:length(input$mutations), function(i) {
                               counter = 1
                               div(displaystyle="height: 30px;", selectInput(paste("mutateAvgCol", counter), "Columns to Average", choices = datanames()[[1]], multiple = T))
                               counter = counter + 1 
                               })
                               }
                               })
                               
                               output$mutateSum <- renderUI({
                               if(input$mutations == 1){
                               selectInput("mutateSumCol", "Columns to Sum", choices = datanames()[[1]], multiple = T)
                               }
                               else{
                               lapply(1:length(input$mutations), function(i) {
                               counter = 1
                               div(displaystyle="height: 30px;", selectInput(paste("mutateSumCol", counter), "Columns to Sum", choices = datanames()[[1]], multiple = T))
                               counter = counter + 1 
                               })
                               }
                               })

