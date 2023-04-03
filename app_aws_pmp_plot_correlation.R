library(shiny)
library(markdown)
library(corrplot)
library(ggplot2)
library(plotly)
library(htmltools)
library(plyr)
library(dplyr)
library(DT)
library(shinydashboard)
library(RColorBrewer)
#starting Sparklyr session
sc <- spark_connect(master = "local")

multicol <- " .multicol {
                      
-webkit-column-count: 2; /* Chrome, Safari, Opera */

-moz-column-count: 2; /* Firefox */

column-count: 2;
}"

ui_aws_pmpf <- shinyUI(
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
            var ids = ['dac','pst','cmr','ml', 'dt', 'frd', 'pd'];
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
        )
        ),  
      width = 300,
      sidebarMenu(id = "sbm",
                  menuItem("Data  and Cleaning", tabName = "dac", icon = icon("th")),
                  conditionalPanel("input.sbm === 'dac'",
                                   selectInput("startDataset", "Dataset", c("","Framingham", "esoph","upload my own")),
                                   conditionalPanel("input.startDataset === 'upload my own'",
                                                    fileInput("datafile", ""), 
                                                    textInput("datafile_sep", "Field Seperator", value = ","))
                                   ),
                  #### Data Plots and Stats
                  ########################
                  menuItem("Plots and Statistical Testing", tabName = "pst", icon = icon("bar-chart")), 
                  conditionalPanel("input.sbm === 'pst'",
                                   selectInput("plot", "Plot Variables", c("Plot 1 (Left)", "Plot 2 (Right)")),
                                   conditionalPanel("input.plot === 'Plot 1 (Left)'",
                                                    selectInput("plotType","Plot 1 Type :", 
                                                                c("bar","histogram", "multibar", "density", "single boxplot", "boxplot", "grouped scatter",  "scatter")), 
                                                    conditionalPanel("input.plotType === 'histogram'", sliderInput("bins","Number of bins:", min = 1, max = 50, value = 3)),
                                                    conditionalPanel("input.plotType === 'boxplot' || input.plotType === 'single boxplot'", checkboxInput("showPoints", "show points", TRUE)),
                                                    conditionalPanel("input.plotType === 'scatter' || input.plotType === 'grouped scatter'", checkboxInput("addTrend", "add trendline", FALSE)),
                                                    uiOutput("x"),   
                                                    uiOutput("y"),
                                                    uiOutput("group"),
                                                    checkboxInput("factorG", "Factor Grouping Variable", FALSE),
                                                    conditionalPanel("input.factorG === true", textInput("factorGVal", "Factor Cut off Value", value = "0")),
                                                    checkboxInput("filter", "Filter Indpendent Variable", FALSE),
                                                    conditionalPanel("input.filter === true", textInput("filterVal", "Filter Cut off Value", value = "0")),
                                                    checkboxInput("filter2", "Filter Dependent Variable", FALSE),
                                                    conditionalPanel("input.filter2 === true", textInput("filterVal2", "Filter Cut off Value", value = "0")),
                                                    checkboxInput("filter3", "Filter Grouping Variable", FALSE),
                                                    conditionalPanel("input.filter3 === true", textInput("filterVal3", "Filter Cut off Value", value = "0"))
                                                    
                                   ),
                                   conditionalPanel("input.plot === 'Plot 2 (Right)'",
                                                    selectInput("plotType2"," Plot 2 Type:", 
                                                                c("bar", "histogram", "multibar", "density", "single boxplot", "boxplot", "grouped scatter",  "scatter")),
                                                    conditionalPanel("input.plotType2 === 'histogram'", sliderInput("bins2","Number of bins:", min = 1, max = 50, value = 3)),
                                                    conditionalPanel("input.plotType2 === 'boxplot' || input.plotType2 === 'single boxplot'", checkboxInput("showPoints2", "show points", TRUE)),
                                                    conditionalPanel("input.plotType2 === 'scatter' || input.plotType2 === 'grouped scatter'", checkboxInput("addTrend2", "add trendline", FALSE)),
                                                    uiOutput("x2"),   
                                                    uiOutput("y2"),
                                                    uiOutput("group2"),
                                                    checkboxInput("factorG2", "Factor Grouping Variable", FALSE),
                                                    conditionalPanel("input.factorG2 === true", textInput("factorGVal2", "Factor Cut off Value", value = "")), 
                                                    checkboxInput("filterp2", "Filter Independent Variable", FALSE),
                                                    conditionalPanel("input.filterp2 === true", textInput("filterValp2", "Filter Cut off Value", value = "")),
                                                    checkboxInput("filterp22", "Filter Dependent Variable", FALSE),
                                                    conditionalPanel("input.filterp22 === true", textInput("filterValp22", "Filter Cut off Value", value = "")),
                                                    checkboxInput("filterp23", "Filter Grouping Variable", FALSE),
                                                    conditionalPanel("input.filterp23 === true", textInput("filterValp23", "Filter Cut off Value", value = ""))
                                                    
                                   ),
                                   conditionalPanel("input.plot === 'Plot 1 (Left)'", "Statistical Testing for Plot 1 Variables",
                                                    selectInput("statTest", "Statistical Tests", c( "Two sample t-test", "Annova", "Kruskal–Wallis")),
                                                    textInput('sig', "Significance Threshold", value = .05)
                                   ),
                                   conditionalPanel("input.plot === 'Plot 2 (Right)'", "Statistical Test for Plot 2 Variables",
                                                    selectInput("statTest2", "Statistical Tests", c("Two sample t-test",  "Annova", "Chi-Squared")),
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
                  menuItem("Data", tabName = 'dt', icon= icon("th")), 
                  menuItem("Plot Data", tabName = "pd", icon = icon("th"))
      )
        ),
    #####   MAIN PANEL     
    ################################################################################################################################################################################################################  
    dashboardBody(
      tags$head(tags$style(HTML(multicol))),
      tabItems(
        tabItem(tabName = "dac", 
                fluidRow(
                  box(title = "Data Overview Options", status = "warning", collapsible = TRUE, width = 3, solidHeader = TRUE, selectInput("look", "Look at NAs, Characters, and Numeric values in Dataset", c("Total Dataset", "NAs By Row", "Data Type By Column"))), 
                  box(title = "Data Overview Options", status = "warning", collapsible = TRUE, width = 9 , soildHeader = TRUE, DT::dataTableOutput("lookTable"))
                ), 
                fluidRow(
                  box(title = "Data", status = "primary", collapsible = TRUE, width = NULL, solidHeader = TRUE, DT::dataTableOutput("cleanDataTable"))
                )),
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
                       textInput("title", "Title", value = ""), textInput("legend", "Legend", value = ""), selectInput("fill", "Color Pallette", c('Blues','Set1', 'Set3', 'Solid Blue', 'BuGn', 'BuPu', 'GnBu', 'Greens','Solid Grey', 'Greys', 'Paired', 'Spectral', 'Solid Red', 'RdBu', 'RdGy'))
                   ), 
                   box(title = "Plot 2 Labels", status = "primary", collapsible = TRUE, collapsed = TRUE, width = 6, textInput("xAxisLabel2", "x-axis", value = ""), textInput("yAxisLabel2", "y-axis", value = ""), 
                       textInput("title2", "Title", value = ""), textInput("legend2", "Legend", value = ""), selectInput("fill2", "Color Pallette", c('Blues','Set1', 'Set3', 'Solid Blue', 'BuGn', 'BuPu', 'GnBu', 'Greens','Solid Grey', 'Greys', 'Paired', 'Spectral', 'Solid Red', 'RdBu', 'RdGy'))
                   )
                 ),
                 fluidRow(
                   box(title = "Statistical Test 1 Results", status = "primary", collapsible = TRUE, width = 6, "Data is assumed to be Normal due to large sample sizes. Two sample tests, Annova, and Chi-Squared are conducted on the independent variable (x).",
                       br(), DT::dataTableOutput("stat.res")),
                   box(title = "Statistical Test 2 Results", status = "primary", collapsible = TRUE, width = 6, "Data is assumed to be Normal due to large sample sizes. Two sample tests, Annova, and Chi-Squared are conducted on the independent variable (x).", 
                       br(), DT::dataTableOutput("stat.res2"))
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
        ###### Data tabs        
        #####################
        tabItem(tabName = "dt",
                fluidRow(
                  box(title = "Data", status = "primary", collapsible = TRUE, width = NULL, DT::dataTableOutput("datasetTable"))
                ) 
        ), 
        tabItem(tabName = 'pd',
                fluidRow(
                  box(title = "Plot 1 Data", status = "primary", collapsible = TRUE, DT::dataTableOutput("plotvar1")), 
                  box(title = "Plot 2 Data", status = "primary", collapsible = TRUE, DT::dataTableOutput("plotvar2"))
                ) 
        )
    )
  )
))


server_aws_pmpf<- shinyServer(function(input, output, session){
  
  
  #####   DATA    
  ################################################################################################################################################################################################################  
  
    #update variable and group based on dataset
    startDataset <- reactive({
      datasource <- input$startDataset
      if(datasource == "Framingham") {
        inputbucket <- "fhs-processedatasets"
        inputschema <- "FHS_TRAINING_Harmonized_20160711"
        inputpath <- paste("s3n://",inputbucket,"/schema=",inputschema,sep="")
        inputpath_dictionary <- paste(inputpath,"/filetype=csv",sep="")
        
        spark_data <- spark_read_csv(sc, name = "sample_data", path = paste(inputpath_dictionary,"/table=all_phenotypes/part-r-00009-54ba3349-08eb-42fd-a321-98e117bc3be2.csv",sep=""), header = TRUE, infer_schema = TRUE, delimiter = ",")
        df_data_only_no_genomics <- dplyr::select(spark_data, -Id, -VariantId, -ChromPOS, -z_notebookHTML, -z_outputschemapath, -HasGenomicsSequence,-HasGenomicsVariants, -Gene, -z_schema)
        as.data.frame(df_data_only_no_genomics)
        } else {
        eval(parse(text = datasource))
      }
    })
  
    dataset <- reactive({

        startDataset()
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
    
    #get numeric data
    numericColumns <- reactive({
      df <- dataset()
      colnames(df)[sapply(df, is.numeric)]
    })
    
    #text headers for plots 
    output$p1Txt <- renderText({paste("Plot 1:", sapply(input$plotType, simpleCap))})
    output$p2Txt <- renderText({paste("Plot 2:", sapply(input$plotType, simpleCap))})
    
    #creates a plot object from the data so can create proper d3 plots 
    plot.obj <- function(x,y,g,fg,fgv,f,fv,f2, fv2, f3, fv3){
      plot.list<-list() 
      plot.list$data<- dataset()
      
      validate(need(!is.null(plot.list$data), "Please make sure a dataset is loaded"))
      validate(need(length(intersect(x, colnames(plot.list$data))) || length(intersect(y, colnames(plot.list$data))) || length(intersect(g, colnames(plot.list$data))), "Please select a variable in dataset"))
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
      p1 <- plot.Type(input$plotType, input$x, input$y, input$group, input$bins, input$factorG, input$factorGVal, input$showPoints, input$xAxisLabel, input$yAxisLabel, input$legend, input$title, input$fill, input$addTrend, input$filter, input$filterVal, input$filter2, input$filterVal2, input$filter3, input$filterVal3)
      print(p1)
      
    })
    
    output$p2 <- renderPlotly({
      validate(need(input$dataset != "", "Please select a data set"))
      p2 <- plot.Type(input$plotType2, input$x2, input$y2, input$group2, input$bins2, input$factorG2, input$factorGVal2, input$showPoints2, input$xAxisLabel2, input$yAxisLabel2, input$legend2, input$title2, input$fill2, input$addTrend2, input$filterp2, input$filterValp2, input$filterp22, input$filterValp22, input$filterp23, input$filterValp23)
      print(p2)
      
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
      validate(need(input$startDataset != "", "Please select a data set"))
      p1 <- plot.Type(input$plotType, input$x, input$y, input$group, input$bins, input$factorG, input$factorGVal, input$showPoints, input$xAxisLabel, input$yAxisLabel, input$legend, input$title, input$fill, input$addTrend, input$filter, input$filterVal, input$filter2, input$filterVal2, input$filter3, input$filterVal3)
      print(p1)
      
    })
    
    output$p2 <- renderPlotly({
      validate(need(input$startDataset != "", "Please select a data set"))
      p2 <- plot.Type(input$plotType2, input$x2, input$y2, input$group2, input$bins2, input$factorG2, input$factorGVal2, input$showPoints2, input$xAxisLabel2, input$yAxisLabel2, input$legend2, input$title2, input$fill2, input$addTrend2, input$filterp2, input$filterValp2, input$filterp22, input$filterValp22, input$filterp23, input$filterValp23)
      print(p2)
      
    })
    
    plot.Type<-function(pt, x, y , g, bins, fg, fgv, ip, xa, ya, gl, t, c, at, f, fv, f2, fv2, f3, fv3){
      plot.df <- plot.obj(x,y,g, fg, fgv, f, fv, f2, fv2, f3, fv3)
      .theme <- theme(axis.line = element_line(colour = 'gray', size = .75), panel.background = element_blank(), plot.background = element_blank())  
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
                 gg <- gg + labs( fill = fill, x = xaxis, y = yaxis)  + ggtitle(title) + .theme + guides(fill=guide_legend(ncol=2))
                 p <- ggplotly(gg)
               }
               else{
                 gg <-ggplot(plot.df$box, aes(x = as.factor(0), y = x)) + geom_boxplot() 
                 gg <- gg + labs( fill = fill, x = xaxis, y = yaxis) + ggtitle(title) + .theme + guides(fill=guide_legend(ncol=2))
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
               print(stat_bin.test$stat_params$bins)
               validate(need(!is.null(stat_bin.test$stat_params$bins), "Histograms are for Continuous Variables, consider using a bar chart"))
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
               gg <- gg + labs(fill = legend, x = xaxis, y = yaxis) + scale_fill_manual(values = getPalette(colourCount+ 2)[-(1:2)]) + ggtitle(title) + .theme + guides(fill=guide_legend(ncol=2))
               p <- ggplotly(gg) 
             },        
             "bar" =	{
               if(solid == FALSE){ 
                 gg <-ggplot(plot.df$variables, aes(x = x, group = as.factor(x), fill = as.factor(x))) + geom_bar(position= "dodge")
                 scale = getPalette(colourCount + 2)[-(1:2)]}
               else{
                 scale = getPalette(9)[7]
                 gg <-ggplot(plot.df$variables, aes(x = x, group = as.factor(x))) + geom_bar(position= "dodge", fill = scale)
                 
               }
               
               gg <- gg + labs( x = xaxis, y = yaxis, fill = legend) + scale_fill_manual(values = scale) + ggtitle(title) + .theme + guides(fill=guide_legend(ncol=2))
               p <- ggplotly(gg) 
             },
             "grouped scatter" = {
               gg <- ggplot(plot.df$variables, aes(x = x, y = y, group = g ,color = as.factor(g))) + geom_point() 
               gg <- gg + labs( color = legend, x = xaxis, y = yaxis) + scale_fill_manual(values = getPalette(colourCount+ 2)[-(1:2)]) + ggtitle(title) + .theme + guides(fill=guide_legend(ncol=2))
               if(at == TRUE){
                 gg <- gg + stat_smooth()
               }
               p <- ggplotly(gg) 
             },
             "scatter" ={
               gg <-ggplot(plot.df$variables, aes(x = x, y = as.factor(y))) + geom_point(color = getPalette(9)[7])
               gg <- gg + labs( x = xaxis, y = yaxis) + scale_fill_manual(values = getPalette(colourCount + 2)[-(1:2)]) +  ggtitle(title) + .theme + guides(fill=guide_legend(ncol=2))
               if(at == TRUE){
                 gg <- gg + stat_smooth()
               }
               p <- ggplotly(gg) 
             }
      )
      
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
        
        t.dat <- switch(st, 
                        "Two sample t-test" = {validate(need(nlevels(groups$g) == 2, "A t-test requires exactly two groups, consider using Annova or factoring the grouping variable"))
                          t <- t.test(x ~ g, conf.level = conf.level, data = groups)
                          tab <- cbind(t$statistic, t$p.value)
                          colnames(tab) <- c("T-statistic", "P-value")
                          row.names(tab) <- c("Welch Two Sample t-test")},
                        "Annova" =  {t <- aov(x ~ g, data = x.g.sort)
                        tab <- as.data.frame(summary(t)[[1]])
                        row.names(tab) <- c(g,"Residuals")
                        },
                        "Chi-Squared" = {t <- chisq.test(x.g.sort$x, x.g.stort$g, correct=FALSE)
                        tab <- cbind(t$statistic, t$p.value)
                        colnames(tab) <- c("Chi-squared", "P-value")}
        )
        return(tab)
      }
    }
    
    output$stat.res <- DT::renderDataTable(stat.tests(input$statTest, input$sig, input$x, input$y, input$group, input$factorG, input$factorGVal, input$filter, input$filterVal, input$filter2, input$filterVal2, input$filter3, input$filterVal3), 
                                           options = list(autoWidth = FALSE, scrollX=TRUE), class = 'table-bordered table-condensed table-striped table-compact',caption = paste(sapply(input$statTest, simpleCap), "Results" ))
    
    output$stat.res2 <- DT::renderDataTable(stat.tests(input$statTest2, input$sig2, input$x2, input$y2, input$group2,  input$factorG2, input$factorGVal2input$filterp2, input$filterValp2, input$filterp22, input$filterValp22, input$filterp23, input$filterValp23),
                                            options = list(autoWidth = FALSE, scrollX=TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = paste(sapply(input$statTest2, simpleCap), "Results" ))
    
    ##### Correlation and Multiple Regression
    ################################################################################################################################################################################################################      
    #update cleanData variables

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
      colnames(result.mr) <- c(colnames(result.mr)[1:3],  'Pr(>|z or t|)')
      return(result.mr)
    }
    
    output$mr <- DT::renderDataTable({
      validate(need(input$dataset != "", "Please select a data set"))
      validate(need(input$DMRVariable != 0, "Please make sure a dataset is selected and the indpendent and dependent variables are set"))
      validate(need(input$IMRVariables != 0, "Please make sure a dataset is selected and the indpendent and dependent variables are set"))
      mr.df <- as.data.frame(mult.reg(input$DMRVariable, input$IMRVariables, input$log))
      dat <- datatable(mr.df, options = list(autoWidth = FALSE), 
                       class = 'table-bordered table-condensed table-striped table-compact', caption = "Multiple Regression Coefficient Results" 
      ) %>%
        formatStyle('Pr(>|z or t|)', color = styleInterval(0.05, c('red','black')), backgroundColor = styleInterval(0.05, c('yellow', 'white'))) %>% 
        formatRound(1:length(mr.df), 5)
      return(dat)
    })
    
  
  #Capitilize First letter of words for text output 
  simpleCap <- function(a) {
    s <- strsplit(a, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  
  ##### Data Tables  
  ################################################################################################################################################################################################################      
  

  
  output$plotvar1 <- DT::renderDataTable({
    p.df1 <- plot.obj(input$x, input$y, input$group, input$factorG, input$factorGVal, input$filter, input$filterVal, input$filter2, input$filterVal2, input$filter3, input$filterVal3)
    p.df1.xyg <- cbind.data.frame(p.df1$variables, p.df1$group)
    colnames(p.df1.xyg) <- c(input$x,input$y, input$group)
    p.dT1 <- datatable(p.df1.xyg, options = list(autoWidth = TRUE, scrollX=TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Independent, Dependent, and Group Variable Data for Left Plot" )
    return(p.dT1)
  })
  output$plotvar2 <- DT::renderDataTable({
    p.df2 <- plot.obj(input$x2, input$y2, input$group2, input$factorG2, input$factorGVal2, input$filterp2, input$filterValp2, input$filterp22, input$filterValp22, intput$filterp23, input$filterValp23)
    p.df2.xyg <- cbind.data.frame(p.df2$variables, p.df2$group)
    colnames(p.df2.xyg) <- c(input$x2,input$y2, input$group2)
    p.dT2 <- datatable(p.df2.xyg, options = list(autoWidth = TRUE, scrollX=TRUE), class = 'table-bordered table-condensed table-striped table-compact', caption = "Independent, Dependent, and Group Variable Data for Right Plot" )
    return(p.dT2)
  })
  
  
})


shinyApp(ui = ui_aws_pmpf, server = server_aws_pmpf)


