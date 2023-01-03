#
#
# File Owner:   Andrew Disher
# Date Created: 12/30/2022
# Topic:        Data Visualization and Dashboarding
# 
# TASK: Create an R Shiny dashboard to report on the gross income performance of a company across its stores and departments. 
#
#

# Packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)

library(echarts4r)
library(dplyr)
library(lubridate)
library(tidyr)
library(data.tree)


# Set the working directory
setwd("C://Users//thebr//Documents//Data Analysis For Fun//Career Enhancement Projects//Company Sales Shiny Dashboard//Data//")

# Import the data
salesData <- read.csv("supermarket_sales - Sheet1.csv")

# Create a new column for the date (makes later tasks easier)
salesData <- salesData |> 
  arrange(Date, Branch) |>
  mutate(New.date = format(as.Date(Date, format="%m/%d/%Y"),"%b-%Y")) 

# Acquire Dates info for later use
Dates <- unique(salesData$New.date)

# Store base echarts colors for usage in doughnut charts
apache_Colors <- c('#5470c6', '#91cc75', '#fac858', '#ee6666', '#73c0de', '#3ba272', '#fc8452', '#9a60b4', '#ea7ccc')



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Dashboard UI Components (header, sidebar, body) -------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Header Content
# -------------------------------------------------------------------------

header <-  dashboardHeader(title = "Sales Dashboard", titleWidth = 250)


# -------------------------------------------------------------------------
# Sidebar Content
# -------------------------------------------------------------------------

sidebar <- dashboardSidebar(
  
  tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
  
  uiOutput("userpanel"),
  
  
  width = 250,
  sidebarMenu(style = "position: Scroll; overflow: visible;", id = "sidebarMenu",
              menuItem(text = "Gross Income", tabName = "grossIncome", icon = icon("chart-pie")),
              
              
              
              tabPanel(
              #conditionalPanel(
                #condition = "input.sidebarMenu === 'donut'",
                
                useShinyjs(),
                div(id = "ConditionalPanel2",
                    tags$hr(),
                    selectInput("selectionBranch", 
                                label = h4("Select a Branch"), 
                                choices = c(All = 'All', 
                                            Yangon = "A", 
                                            Naypyitaw = "B", 
                                            Mandalay = "C"),
                                selected = "All")
                    
                )
              )
              
              
              
              
  )
)


# -------------------------------------------------------------------------
# Body Content 
# -------------------------------------------------------------------------


body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'grossIncome', 
            
            fluidRow(
              
              box(width = 12, 
                  title = tags$b(paste("Gross Income Donut Charts")), 
                  solidHeader = TRUE, 
                  status = 'primary',
                  
                  fluidRow(
                    
                    column(width = 2, 
                           
                           selectInput("selectionDate", 
                                       label = h4("Select a Date"), 
                                       choices = c('All', Dates),
                                       selected = 'All')), 
                    
                    column(width = 2, 
                           
                           selectInput("percentCurrency", 
                                       label = h4("Show Percent or Currency?"), 
                                       choices = c(Percent = "percent", 
                                                   `Currency (USD)` = "currency"),
                                       selected = "percent")), 
                    column(width = 2, 
                           
                           valueBoxOutput('grossIncomeTotal', width = 12))
                    
                  ), 
                  
                  tags$br(), 
                  
                  fluidRow(
                  
                  # ----- Gross Income Donut Plots -----
                  column(width = 3, echarts4rOutput('gender', height = '175px')), 
                  
                  column(width = 3, echarts4rOutput('productLine', height = '175px')), 
                  
                  column(width = 3, echarts4rOutput('paymentMethod', height = '175px')), 
                  
                  column(width = 3, echarts4rOutput('customerStatus', height = '175px'))  
                  )
  
                  )
              
            ), 
            
            fluidRow(
              
              box(title = tags$b(paste("Gross Income Sunburst Chart (All Branches)")), 
                  solidHeader = TRUE, 
                  status = 'primary', 
                  
                  
                  # ----- Category Inputs for Sunburst Chart ----- 
                  fluidRow(
                    column(width = 3, 
                           
                           # Input One
                           selectInput("selectionSunburstOne", 
                                       label = h4("Select Category 1"), 
                                       choices = c(Gender = 'Gender', 
                                                   `Customer Status` = 'Customer.type',
                                                   `Payment Method` = 'Payment',
                                                   `Product Line` = 'Product.line'),
                                       selected = 'Gender')), 
                    
                    column(width = 3, 
                           
                           # Input Two
                           selectInput("selectionSunburstTwo", 
                                       label = h4("Select Category 2"), 
                                       choices = c(Gender = 'Gender', 
                                                   `Customer Status` = 'Customer.type',
                                                   `Payment Method` = 'Payment',
                                                   `Product Line` = 'Product.line'),
                                       selected = 'Customer.type')), 
                    
                    column(width = 3, 
                           
                           # Input Three
                           selectInput("selectionSunburstThree", 
                                       label = h4("Select Category 3"), 
                                       choices = c(Gender = 'Gender', 
                                                   `Customer Status` = 'Customer.type',
                                                   `Payment Method` = 'Payment',
                                                   `Product Line` = 'Product.line'),
                                       selected = 'Payment')), 
                    
                    column(width = 3, 
                           
                           # Input Four
                           selectInput("selectionSunburstFour", 
                                       label = h4("Select Category 4"), 
                                       choices = c(Gender = 'Gender', 
                                                   `Customer Status` = 'Customer.type',
                                                   `Payment Method` = 'Payment',
                                                   `Product Line` = 'Product.line'),
                                       selected = 'Product.line'))
                  ), 
                  
                  fluidRow(column(width = 12, 
                                  
                                  echarts4rOutput("sunburstChart")))
                  
              ), 
              
                     box(title = tags$b(paste("Gross Income Over Time")), 
                         solidHeader = TRUE, 
                         status = 'primary',
                         
                         # ----- Category Input for Time Series -----
                         fluidRow( 
                           column(width = 4, 
                                  
                                  selectInput("selectionCategory", 
                                              label = h4("Select a Category"), 
                                              choices = c(All = 'All',
                                                          Gender = 'Gender', 
                                                          `Customer Status` = 'Customer.type',
                                                          `Payment Method` = 'Payment',
                                                          `Product Line` = 'Product.line'),
                                              selected = 'Product.line'))
                           
                           ),
                         
                         # ----- Gross Income Time Series Plot -----
                         fluidRow(column(width = 12, 
                                         
                                         echarts4rOutput('timeSeriesGI', height = '400px')))
                         
                         )
                     
            )
            )
    )
  )


# -------------------------------------------------------------------------
# Assemble UI Components
# -------------------------------------------------------------------------

ui <- dashboardPage(header, sidebar, body, skin = "blue",)



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Dashboard Server Content ------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  # -------------------------------------------------------------------------
  # Donut Charts ------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  donutData <- reactive({
    
    # Begin with all data
    tempData <- salesData
    
    if(input$selectionDate != 'All'){
      
      tempData <- tempData |> 
        # Filter the data by date
        filter(New.date %in% input$selectionDate)
    }
    if(input$selectionBranch != 'All'){
      
      tempData <- tempData |> 
        # Filter by company branch
        filter(Branch %in% input$selectionBranch)
    }
    
    return(tempData)
    
  })
  
  # -------------
  # - Value Box -
  # -------------
  
  output$grossIncomeTotal <- renderValueBox({
    value1 <- donutData() |>
      summarise(total = sum(gross.income))
    
    valueBox(paste0('$', round(value1)), 'Total for Date', icon = icon("sack-dollar"))
  })
  
  
  # ----------------
  # - EX 1: Gender -
  # ----------------
  
  output$gender <- renderEcharts4r({
    
    chart1 <- (donutData() |>
      
      # Find percentage of total gross income by Gender 
      select(Gender, gross.income) |>
      group_by(Gender) |>
      summarise(AGGR = sum(gross.income)) |>
      as.data.frame() |>
      mutate(Percent = AGGR/sum(AGGR)) |>
      
      # Retrieve and assign needed colors
      mutate(color = apache_Colors[1:length(Gender)]) |>
      
      # Reorder rows in descending order
      arrange(desc(AGGR)))
    
      
    if(input$percentCurrency == 'percent'){
      
      chart1 |> 
        # Create chart with percent values
        e_charts(Gender) |>
        e_pie(Percent, radius = c("25%", "60%")) |>
        e_tooltip(trigger = "item",
                  e_tooltip_choro_formatter('percent', digits = 0)
        ) |>
        e_title("By Gender", textStyle = list(fontSize = 12), left = '40%')  |>
        
        e_legend(show = FALSE) |>
        e_add_nested("itemStyle", color)
      
    } else if(input$percentCurrency == 'currency'){
      
      chart1 |> 
      # Create chart with currency values (raw numbers)
        e_charts(Gender) |>
        e_pie(AGGR, radius = c("25%", "60%")) |>
        e_tooltip(trigger = "item",
                  e_tooltip_choro_formatter('currency', digits = 0)
        ) |>
        e_title("By Gender", textStyle = list(fontSize = 12), left = '40%')  |>
        e_legend(show = FALSE) |>
        e_add_nested("itemStyle", color)
      
    }
    
      
    
  })
  
  # ----------------------
  # - EX 2: Product Line -
  # ----------------------
  
  output$productLine <- renderEcharts4r({
    
    chart2 <- (donutData() |>
      
      # Find percentage of total gross income by Product Line
      select(Product.line, gross.income) |>
      group_by(Product.line) |>
      summarise(AGGR = sum(gross.income)) |>
      as.data.frame() |>
      mutate(Percent = AGGR/sum(AGGR)) |>
      
      # Retrieve and assign needed colors
      mutate(color = apache_Colors[1:length(Product.line)]) |>
      
      # Reorder rows in descending order
      arrange(desc(AGGR))) 
    
    
    if(input$percentCurrency == 'percent'){
      
      chart2 |>
        # Create chart with percent values
        e_charts(Product.line) |>
        e_pie(Percent, radius = c("25%", "60%")) |>
        e_tooltip(trigger = "item",
                  e_tooltip_choro_formatter('percent', digits = 0)
        ) |>
        e_title("By Product Line", textStyle = list(fontSize = 12), left = '35%') |>
        e_legend(show = FALSE) |>
        e_add_nested("itemStyle", color)
      
    } else if(input$percentCurrency == 'currency'){
      
      chart2 |>
      # Create chart with currency values (raw numbers)
        e_charts(Product.line) |>
        e_pie(AGGR, radius = c("25%", "60%")) |>
        e_tooltip(trigger = "item",
                  e_tooltip_choro_formatter('currency', digits = 0)
        ) |>
        e_title("By Product Line", textStyle = list(fontSize = 12), left = '35%') |>
        e_legend(show = FALSE) |>
        e_add_nested("itemStyle", color)
        
    }
    
  })
  
  
  # ------------------------
  # - EX 3: Payment Method -
  # ------------------------
  
  output$paymentMethod <- renderEcharts4r({
    
    chart3 <- (donutData() |>
      
      # Find percentage of total gross income by Payment Method
      select(Payment, gross.income) |>
      group_by(Payment) |>
      summarise(AGGR = sum(gross.income)) |>
      as.data.frame() |>
      mutate(Percent = AGGR/sum(AGGR)) |>
      
      # Retrieve and assign needed colors
      mutate(color = apache_Colors[1:length(Payment)]) |>
      
      # Reorder rows in descending order
      arrange(desc(AGGR))) 
    
    
    if(input$percentCurrency == 'percent'){
      
      chart3 |>
        # Create chart with percent values
        e_charts(Payment) |>
        e_pie(Percent, radius = c("25%", "60%")) |>
        e_tooltip(trigger = "item",
                  e_tooltip_choro_formatter("percent", digits = 0)
        ) |>
        e_title("By Payment Method", textStyle = list(fontSize = 12), left = '30%') |>
        e_legend(show = FALSE) |>
        e_add_nested("itemStyle", color)
      
    } else if(input$percentCurrency == 'currency'){
      
      chart3 |>
        # Create chart with currency values (raw numbers)
        e_charts(Payment) |>
        e_pie(AGGR, radius = c("25%", "60%")) |>
        e_tooltip(trigger = "item",
                  e_tooltip_choro_formatter("currency", digits = 0)
        ) |>
        e_title("By Payment Method", textStyle = list(fontSize = 12), left = '30%') |>
        e_legend(show = FALSE) |>
        e_add_nested("itemStyle", color)
      
    }
    
    
  })
  
  
  # -------------------------
  # - EX 4: Customer Status -
  # -------------------------
  
  output$customerStatus <- renderEcharts4r({
    
    chart4 <- (donutData() |>
      
      # Find percentage of total gross income by Customer Status
      select(Customer.type, gross.income) |>
      group_by(Customer.type) |>
      summarise(AGGR = sum(gross.income)) |>
      as.data.frame() |>
      mutate(Percent = AGGR/sum(AGGR)) |>
      
      # Retrieve and assign needed colors
      mutate(color = apache_Colors[1:length(Customer.type)]) |>
      
      # Reorder rows in descending order
      arrange(desc(AGGR)))
    
    
    if(input$percentCurrency == 'percent'){
      
      chart4 |>
        # Create chart with percent values
        e_charts(Customer.type) |>
        e_pie(Percent, radius = c("25%", "60%")) |>
        e_tooltip(trigger = "item",
                  e_tooltip_choro_formatter("percent", digits = 0)
        ) |>
        e_title("By Customer Status", textStyle = list(fontSize = 12), left = '30%') |>
        e_legend(show = FALSE) |>
        e_add_nested("itemStyle", color)
      
    } else if(input$percentCurrency == 'currency'){
      
      chart4 |>
        # Create chart with currency values (raw numbers)
        e_charts(Customer.type) |>
        e_pie(AGGR, radius = c("25%", "60%")) |>
        e_tooltip(trigger = "item",
                  e_tooltip_choro_formatter("currency", digits = 0)
        ) |>
        e_title("By Customer Status", textStyle = list(fontSize = 12), left = '30%') |>
        e_legend(show = FALSE) |>
        e_add_nested("itemStyle", color)
      
    }
    

      
    
  })
  
  
  # -------------------------------------------------------------------------
  # Time Series Charts ------------------------------------------------------
  # -------------------------------------------------------------------------
  

  output$timeSeriesGI <- renderEcharts4r({
    
    # Begin with unfiltered data
    dataTS <- salesData 
    
    # Filter by branch, if necessary
    if(input$selectionBranch != 'All'){
      
      dataTS <- dataTS |> 
                  filter(Branch %in% input$selectionBranch)
      
    }
    
    # Construct data set for graphing the time series  
    if(input$selectionCategory != 'All'){
      dataTS <- (dataTS |>
                   select(input$selectionCategory, New.date, gross.income) |> 
                   dplyr::group_by_('New.date', input$selectionCategory) |> 
                   summarise(AGGR = sum(gross.income)) |>
                   spread(input$selectionCategory, AGGR) |>
                   as.data.frame() |>
                   
                   # Reformat the dates in New.Date
                   mutate(New.date = paste(New.date, "-01", sep = "") |> 
                            as.Date(format = "%b-%Y-%d"))
      )
    }
    else{
      dataTS <- (dataTS |>
         select(New.date, gross.income) |> 
         dplyr::group_by(New.date) |> 
         summarise(Total = sum(gross.income)) |>
         as.data.frame() |>
         
         # Reformat the dates in New.Date
         mutate(New.date = paste(New.date, "-01", sep = "") |> 
                  as.Date(format = "%b-%Y-%d"))
      )
    }
    
    
    # Reorder the rows by date
    dataTS <- dataTS[order(dataTS$New.date),]
    
   
    # Build the time series chart
    chartTS <- dataTS |> 
      e_charts(New.date)
    
    for (series in 2:ncol(dataTS)){
      chartTS <-  chartTS |>
        e_line_(colnames(dataTS)[series])
    }
    
    chartTS <- chartTS |>
      e_tooltip(trigger = 'axis', 
                e_tooltip_pointer_formatter("currency", digits = 0)
                ) |>
      e_datazoom(x_index = 0, type = "slider") |>
      e_axis_labels(x = 'Date')  |>
      e_legend(right = '50px')
      
    return(chartTS)
      
      
      
  })
  
  
  
  # -------------------------------------------------------------------------
  # Sunburst Charts -- ------------------------------------------------------
  # -------------------------------------------------------------------------
  
  
  # ------------------------------------------
  # - Filter and prepare sunburst chart data -
  # ------------------------------------------
  
  # Get temporary data frames
  
  # -----------------------------------------------
  # - Preliminary DF (Filtered by Branch, or Not) -
  # -----------------------------------------------
  
  sunburstDataPrelim <- reactive({
    
    # Filter by branch, if necessary
    # if(input$selectionBranch != 'All'){ 
    # This is commented out because there is insufficient data to represent the sunburst chart for some branches. They have insufficient data, so filtering by branch will produce errors that cannot be resolved.
    if(FALSE){
      preliminaryDF <- salesData |>
        filter(Branch %in% input$selectionBranch)
    }
    else{
      preliminaryDF <- salesData
    }
    
    return(preliminaryDF)
    
  })
  
  
  
  # ------------
  # - First DF -
  # ------------
  
  sunburstDataOne <- reactive({
    
    tempDF1 <- sunburstDataPrelim() |> 
      select(Gender, Customer.type, Payment, Product.line, gross.income) |> 
      group_by_(input$selectionSunburstOne) |> 
      summarise(AGGR = sum(gross.income)) |> 
      as.data.frame()
    
    # NOTE: Category one is special, since we need to append a column dynamically to it, but not to the other three dataframes
    
    # Append a special column to the beginning of this dataframe, which is dependent on selectionSunburstOne input
    if(input$selectionSunburstOne == "Payment"){
      tempDF1 <- cbind(c("Everything", "Everything", "Everything"), tempDF1)
    }
    else if(input$selectionSunburstOne == "Product.line"){
      tempDF1 <- cbind(c("Everything", "Everything", "Everything", "Everything", "Everything", "Everything"), tempDF1)
    }
    else{
      # In the cases of Gender or Customer Type
      tempDF1 <- cbind(c("Everything", "Everything"), tempDF1)
    }
    
    # Rename the columns, for when each temp data frame must be bound together later
    colnames(tempDF1) <- c("Parents", "Labels", "Value")
    
    return(tempDF1)
    
  })
  
  # -------------
  # - Second DF -
  # -------------
  
  sunburstDataTwo <- reactive({
    
    tempDF2 <- sunburstDataPrelim() |> 
      select(Gender, Customer.type, Payment, Product.line, gross.income) |> 
      group_by_(input$selectionSunburstOne, input$selectionSunburstTwo) |> 
      summarise(AGGR = sum(gross.income)) |> 
      as.data.frame()
    
    # Rename the columns
    colnames(tempDF2) <- c("Parents", "Labels", "Value")
    
    return(tempDF2)
    
  })
  
  # ------------
  # - Third DF -
  # ------------
  
  sunburstDataThree <- reactive({
    
    tempDF3 <- sunburstDataPrelim() |> 
      select(Gender, Customer.type, Payment, Product.line, gross.income) |> 
      group_by_(input$selectionSunburstOne, input$selectionSunburstTwo, input$selectionSunburstThree) |> 
      summarise(AGGR = sum(gross.income)) |> 
      as.data.frame()
    
    # Retrieve only necessary columns
    tempDF3 <- tempDF3[, 2:4]
    
    colnames(tempDF3) <- c("Parents", "Labels", "Value")
    
    return(tempDF3)
    
  })
  
  # -------------
  # - Fourth DF -
  # -------------
  
  sunburstDataFour <- reactive({
    
    tempDF4 <- sunburstDataPrelim() |> 
      select(Gender, Customer.type, Payment, Product.line, gross.income) |> 
      group_by_(input$selectionSunburstOne, input$selectionSunburstTwo, input$selectionSunburstThree, input$selectionSunburstFour) |> 
      summarise(AGGR = sum(gross.income)) |> 
      as.data.frame() 
    
    # Retrieve only necessary columns
    tempDF4 <- tempDF4[, 3:5]
    
    colnames(tempDF4) <- c("Parents", "Labels", "Value")
    
    return(tempDF4)
    
  })
  
  
  # ------------------------
  # - Final, Aggregated DF -
  # ------------------------
  
  sunburstDataAggregated <- reactive({
    
    finalDF <- rbind(c("", "Everything", 0),
                     
                     sunburstDataOne(), 
                     sunburstDataTwo(), 
                     sunburstDataThree(), 
                     sunburstDataFour()
    )
    
    finalDF$Value <- as.numeric(finalDF$Value)
    
    DF_tree <- data.tree::FromDataFrameNetwork(finalDF)
    
    finalDF2 <- ToDataFrameNetwork(DF_tree)
    
    finalDF2$value <- finalDF$Value
    
    DF_tree2 <- FromDataFrameNetwork(finalDF2)
    
    return(DF_tree2)
    
  })
  
  
  output$sunburstChart <- renderEcharts4r({
    
    # Sunburst
    sunburstDataAggregated() |>
      e_charts() |>
      e_sunburst() |>
      e_tooltip(formatter = e_tooltip_choro_formatter("currency")) |>
      e_labels(show = FALSE)
    
  })
  
  
 
  
  
}


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Run the application -----------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


shinyApp(ui = ui, server = server)



