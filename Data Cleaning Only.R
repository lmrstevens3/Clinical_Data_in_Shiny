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

multicol <- " .multicol {

-webkit-column-count: 2; /* Chrome, Safari, Opera */

-moz-column-count: 2; /* Firefox */

column-count: 2;
}"


ui22 <- shinyUI(
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
                  downloadButton("report", "Generate report"),
                  menuItem("Data  and Cleaning", tabName = "dac", icon = icon("th")),
                  conditionalPanel("input.sbm === 'dac'",
                                   selectInput("startDataset", "Dataset", c("","esoph", "upload my own")),
                                   conditionalPanel("input.startDataset === 'upload my own'",
                                                    fileInput("datafile", ""), 
                                                    textInput("datafile_sep", "Field Seperator", value = ",")),
                                   checkboxInput("clean","Clean This Dataset", FALSE),
                                   conditionalPanel("input.clean === true", 
                                                    checkboxInput("combine", "Combine with another dataset", FALSE),
                                                    conditionalPanel("input.combine === true",
                                                                     selectInput("mergeby", "How to Merge Data", c("Merge Data by Adding Column", "Merge Data by Column Value")),
                                                                     conditionalPanel("input.mergeby === 'Merge Data by Column Value'", selectInput('colMerge', "Column Name", c("Loading..."))),
                                                                     fileInput("combinefile", ""), 
                                                                     textInput("combinefile_sep", "Field Seperator", value = ",")),
                                                    selectInput("cleanOptions", "Cleaning Options", c("",  "replace values", "replace NAs", "remove NAs" )), 
                                                    conditionalPanel("input.cleanOptions === 'replace NAs'", textInput("replacementNA", "Replacement NA Value", "")),
                                                    conditionalPanel("input.cleanOptions === 'replace values'", selectInput("replaceValues", "Replacement Options", c("values in entire dataset", "values in columns")), helpText("To replace multiple values separate values with a"), helpText("comma and provide same number of values"), helpText("for replaced and replacement"),
                                                                     conditionalPanel("input.replaceValues === 'values in entire dataset'", textInput("replacedAll", "Value to Replace", ""), textInput("replacementAll", "Replacement Value", ""), div(style="text-align: center" , actionButton('replaceButtonAll', "Replace All Values"))),
                                                                     conditionalPanel("input.replaceValues === 'values in columns'", uiOutput("repCol"), textInput("replacedCol", "Value to Replace in Column", ""), textInput("replacementCol", "Replacement Value", ""), div(style="text-align: center" , actionButton('replaceButtonCol', "Replace Column Values")))
                                                    ),
                                                    div(style = "display:inline-block",checkboxInput('makeFactor', "Make columns a Factor", FALSE, width = '100px')),
                                                    div(style = "display:inline-block",checkboxInput('makeChar', "Make columns Characters", FALSE, width = '100px')),
                                                    div(style = "display:inline-block",checkboxInput('makeNum', "Make columns Numeric", FALSE, width = '100px')),
                                                    conditionalPanel("input.makeFactor === true", uiOutput("factorList")),
                                                    conditionalPanel("input.makeChar === true", uiOutput("charList")),
                                                    conditionalPanel("input.makeNum === true", uiOutput("numList"))
                                   )),
                 
      )
      ),
    #####   MAIN PANEL     
    ################################################################################################################################################################################################################  
    dashboardBody(
      tags$head(tags$style(HTML(multicol))),
      tabItems(
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
                         box(title = "Restore Data to Original Dataset", status = "primary", collapsible = TRUE, width = NULL, actionButton("restoreOriginalData", "Restore"))
                         
                  ), 
                  column(width = 9, box(title = "Data Overview", status = "warning", collapsible = TRUE, width = NULL , soildHeader = TRUE, div(style = 'overflow-x: scroll', DT::dataTableOutput("lookTable"))),
                         box(title = "Remove Rows/Columns By Number of NAs", status = "primary", collapsible = TRUE, width = NULL, solidHeader = F,
                             h5(helpText("To remove rows/columns with greater than a certain number of NAs, enter the NA cut off and hit the remove button for rows or columns")), 
                             div(style = "display:inline-block", uiOutput('removeNARow')), div( style = "display:inline-block", actionButton("removeNARowButton", "remove rows with NA total > cut off")),
                             br(),
                             div(style = "display:inline-block", uiOutput('removeNACol')), div( style = "display:inline-block", actionButton("removeNAColButton", "remove cols with NA total > cut off")))
                  )
                ), 
                fluidRow(
                  box(title = "Data", status = "primary", collapsible = TRUE, width = NULL, solidHeader = TRUE, div(style = 'overflow-x: scroll', DT::dataTableOutput("cleanDataTable")))
                ),
                fluidRow(
                  column(width = 9, 
                         box(title = "Columns Removed From Dataset",status = "primary", collapsible = TRUE, width = NULL,
                             helpText("To restore columns in data, select the clean this dataset checkbox and select 'restore to original dataset in box above'"), 
                             uiOutput("remCol")),
                         box(title = "Rows Removed From Dataset", status = "primary", collapsible = TRUE, width = NULL, 
                             helpText("To restore rows in data, select the clean this dataset checkbox and select 'restore to original dataset in box above'"),
                             uiOutput("remRow"))
                  ),
                  column(width = 3, 
                         box(title = "Change Column Names", status = "primary", collapsible = TRUE, width = NULL, 
                             uiOutput("colNamesChange"), 
                             uiOutput("newColName"), actionButton("colNamesChangeButton", "Change"))
                  )
                )),
        
        )
        )
))

server22 <- shinyServer(function(input, output, session){
  
  options(shiny.maxRequestSize = 50*1024^2)
  #####   DATA    
  ################################################################################################################################################################################################################  

values <- reactiveValues(cleanedData = NULL, mlvariables = NULL)

startDataset <- reactive({
  datasource <- input$startDataset
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
  if(is.null(values$cleanedData)){
    print('calling startDataset')
    startData <- startDataset()
    values$cleanedData <- startData
    dataset <- values$cleanedData 
    
  }
  else{
    validate(need(length(values$cleanedData) >= 2 , "Please have atleast 2 columns in your dataset"))
    validate(need(length(row.names(values$cleanedData)) >= 2 , "Please have atleast 2 rows in your dataset"))
    dataset <- cleanData() 
    
  }
  return(dataset)
})



datanames <- reactive({
  col = colnames(startDataset())
  row = rownames(startDataset())
  if(input$combine == TRUE){
    col = c(colnames(cleanData()))
    row = c(row.names(cleanData()))
  }
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
  var.opts <- c(colnames(obj))
  selectInput("y","Dependent Variable:", var.opts) # uddate UI                  
}) 

#updates x, independent variable for plots
output$x <- renderUI({ 
  obj <- dataset()     
  var.opts <- c(colnames(obj))
  selectInput("x","Independent Variable:", var.opts) # uddate UI                 
})

#updates grouping variable for comparing data based on categorical variables such as gender 
output$group <- renderUI({ 
  obj <- dataset()     
  var.opts <- c(colnames(obj))
  selectInput("group","Grouping Variable: ", var.opts) # uddate UI                 
})

#updates y for second plot , dependent variable for 2D plots 
output$y2 <- renderUI({ 
  obj <- dataset()    
  var.opts <- c(colnames(obj))
  selectInput("y2","Dependent Variable:", var.opts) # uddate UI                  
}) 

#updates x for second plot, independent variable for plots
output$x2 <- renderUI({ 
  obj <- dataset()     
  var.opts <- c(colnames(obj))
  selectInput("x2","Independent Variable:", var.opts) # uddate UI                 
})

#updates grouping variable for second plot for comparing data based on categorical variables such as gender 
output$group2 <- renderUI({ 
  obj <- dataset()     
  var.opts<-c(colnames(obj))
  selectInput("group2","Grouping Variable: ", var.opts) # uddate UI                 
})

#update cleanData variables
output$factorList <- renderUI({ 
  obj <- datanames()   
  var.opts <- obj[[1]]
  selectizeInput("factorList", "Select Variables to factor", choices = var.opts, multiple = T,  options = list(plugins = list("drag_drop", "remove_button")))
})

output$charList <- renderUI({ 
  obj <- datanames()   
  var.opts <- obj[[1]]
  selectizeInput("charList", "Select Variables to make characters", choices = var.opts, multiple = T,  options = list(plugins = list("drag_drop", "remove_button")))
})

output$numList <- renderUI({ 
  obj <- datanames()   
  var.opts <- obj[[1]]
  selectizeInput("numList", "Select Variables to make numeric", choices = var.opts, multiple = T,  options = list(plugins = list("drag_drop", "remove_button")))
})

output$repCol <- renderUI({ 
  obj <- datanames()   
  var.opts <- obj[[1]]
  selectizeInput("repCol", "Select Column Variables to repalce values", choices = var.opts, multiple = T,  options = list(plugins = list("drag_drop", "remove_button")))
})

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
  selectizeInput("colNamesChange", "Select Column Label to Change", choices = var.opts, selected = colnames(dataset())[1])
})

output$newColName <- renderUI({
  obj <- colnames(dataset())   
  startCol <- obj[[1]][1]
  textInput( "newColName", "New Column Label",startCol)
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

cont_cat <- reactive({
  df <- dataset()
  f_df <- lapply(df, as.factor)
  f_levels <- sapply(f_df, nlevels)
  continuous <- names(f_levels[f_levels > 10])
  categorical <- names(f_levels[f_levels <= 10])
  cont_cat <- continuous
  return(cont_cat)
})

mergeData <- reactive({
  if(input$combine == TRUE){
    inCFile <- input$combinefile
    validate(
      need(!is.null(inCFile), "Please input a File to combine")
    )
    read.delim(inCFile$datapath, sep = gsub("\\t", "\t", input$combinefile_sep, fixed = TRUE))
  }
})


replace_function <- function (Data, replaced, replacement) {
  validate(need(length(replaced) == length(replacement), "replacement and replaced lists need to be the same length"))
  Data[Data == replaced] <- replacement
  return(Data)
}

cleanData <- reactive({
  cleanOption = input$cleanOptions
  cleanedData <- values$cleanedData
  if(input$clean == FALSE){
    return(cleanedData)}
  else{
    if(input$combine == TRUE){
      mergedata = mergeData()
      switch(input$mergeby, 
             "Merge Data by Adding Column" = {
               validate(need(length(cleanedData) == length(mergedata), "The two datasets for combining, do not have the same number of rows. Consider 'merge by a colum value' or upload a dataset to combine with the same number of rows as the original dataset"))
               cleanedData = cbind.data.frame(cleanedData, mergedata)
             },
             "Merge Data by Column Value" = {
               validate(need((input$colMerge %in% colnames(cleanedData)), "The column you selected for merging the datasets on is not in the original dataset, check both datasets have the column, or consider changing the column name"))
               validate(need((input$colMerge %in% colnames(mergedata)), "The column you selected for merging the datasets on is not in the merge dataset, check both datasets have the column, or consider changing the column name"))
               # row.names(cleanedData) = cleanedData[,input$colMerge]
               # row.names(mergedata) = mergedata[,input$colMerge]
               # #combine data sets with same values
               # samples.in.both = intersect(row.names(cleanedData), row.names(mergedata))
               # cleanedData = cbind.data.frame(cleanedData[samples.in.both,], mergedata[samples.in.both,])
               cleanedData = merge(cleanedData, mergedata, by = input$colMerge)
             }
      )
    }
    
    if(cleanOption == "remove NAs"){
      cleanedData = na.omit(cleanedData)
    }
    
    if(cleanOption == "replace NAs"){
      cleanedData[is.na(cleanedData)] <- input$replacementNA
    }
    
    if(input$makeFactor == TRUE){
      fList = c(input$factorList)
      if(length(fList) > 0){
        validate(need(sum(fList %in% colnames(cleanedData)) != 0, "Please make sure the columns you selected are in the current dataset"))
        cleanedData[fList] = lapply(cleanedData[fList], as.factor)
        cleanedData = cleanedData
      }
    }
    if(input$makeChar == TRUE){ 
      cList = c(input$charList)
      if(length(cList) > 0){
        validate(need(sum(cList %in% colnames(cleanedData)) != 0, "Please make sure the columns you selected are in the current dataset"))
        cleanedData[cList] = lapply(cleanedData[cList], as.character)
      }
    }
    if(input$makeNum == TRUE){
      nList = c(input$numList)
      if(length(nList) > 0){
        validate(need(sum(nList %in% colnames(cleanedData)) != 0, "Please make sure the columns you selected are in the current dataset"))
        cleanedData[,nList] = lapply(cleanedData[nList], as.numeric)
        
      }
    }
  } 
  
  values$cleanedData <- cleanedData
  return(values$cleanedData)
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
  cleanOption = input$cleanOptions
  cleanedData <- dataset()
  if(cleanOption == "replace values"){
    if(input$replaceValues == "values in entire dataset"){
      replaced = c(input$replacedAll)
      replacement = c(input$replacementAll)
      for(i in 1:length(replacement)){
        cleanedData <- replace_function(cleanedData, replaced[i], replacement[i])
      }
    }
  }
  values$cleanedData <- cleanedData
})

observeEvent(input$replaceButtonCol,{
  cleanOption = input$cleanOptions
  cleanedData <- dataset()
  if(cleanOption == "replace values"){
    if(input$replaceValues == "values in columns"){
      validate(need(length(input$repCol) != 0, "Please Select A Column to Replace Values"))
      r = c(input$repCol)
      replaced = c(input$replaced)
      for(i in 1:length(replacement)){
        cleanedData[,r] <- replace_function(cleanedData[,r], replaced[i], replacement[i])
      }
    }
  }
  values$cleanedData <- cleanedData
})

observeEvent(input$removeButton, {
  cleanedData <- dataset()
  rr = c(row.names(cleanedData)[input$lookTable_rows_selected], row.names(cleanedData)[input$cleanDataTable_rows_selected])
  rc = c(colnames(cleanedData)[input$lookTable_columns_selected], colnames(cleanedData)[input$cleanDataTable_columns_selected])
  
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


output$lookTable <- DT::renderDataTable(lookData(), server = T, selection = list(target = 'row+column'), class = 'table-bordered table-condensed table-striped table-compact')

output$cleanDataTable <- DT::renderDataTable(dataset(), server = T, selection = list(target = 'row+column'), class = 'table-bordered table-condensed table-striped table-compact')

output$lookTxt <- renderText({
  lookText <- switch(input$look, 
                     "Total Dataset" = " ",
                     "NAs By Row" =  {row.names(dataset())[input$lookTable_rows_selected]},
                     "Data Type By Column" = {colnames(dataset())[input$lookTable_columns_selected]}
  )
})

output$dataTxt_rows <- renderText({row.names(dataset())[input$cleanDataTable_rows_selected]})
output$dataTxt_cols <- renderText({colnames(dataset())[input$cleanDataTable_columns_selected]})
})