library(shiny)
library(shinyalert)
library(tidyverse)
library(DT)
library(ggplot2)

source('Helpers.r')

# surpress scientific notation
options(scipen = 999)

# read in data set
df <- read_csv('Data/Melbourne_housing_FULL.csv', na = ('#N/A'))

# clean data set
df <- df |>
  filter(!is.na(Price), BuildingArea > 0, Landsize > 0) |>
  # convert sq meters to sq ft
  mutate(
    BuildingArea = round(BuildingArea*10.7639, 2),
    Landsize = round(Landsize*10.7639, 2)
  ) |>
  # Create Year Sold Variable
  mutate(YearSold = year(dmy(Date))) |>
  # convert categorical variables to factors
  mutate(
    across(
      c('Regionname', 'Type', 'CouncilArea', 'Suburb', 'YearSold'),
      as.factor)
  ) |>
  # Re-code 'type' factor levels
  mutate(
    Type = fct_recode(
      Type,
      'House' = 'h',
      'Unit/Duplex' = 'u',
      'Townhouse' = 't')
  )

# Define app UI
ui <- fluidPage(
  # App title
  titlePanel('Melbourne Housing Data Exploration'),
  
  sidebarLayout(
    sidebarPanel(
      # Categorical variable selection
      h2('Choose a Subset of the Data'),
      h3('Categorical Variables:'),
      selectizeInput('region', 'Region:', 
                     choices = c("All", levels(df$Regionname)),
                     selected = 'All'),
      selectizeInput('type', 'Property Type:', 
                     choices = c("All", levels(df$Type)),
                     selected = 'All'),
      br(),
      
      # Numeric Variable
      h3('Numeric Variables:'),
      selectizeInput('num1', '',
                 choices = numeric_vars,
                 selected = numeric_vars[1]),
      uiOutput('num1_slider'),
      
      selectizeInput('num2', '',
                     choices = numeric_vars,
                     selected = numeric_vars[2]),
      uiOutput('num2_slider'),
      
      actionButton('subset_btn', 'Apply Filters', class = 'btn-primary')
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel('About',
                 h3('About This App'),
                 p('This app explores the Melbourne housing market using data from properties sold between February of 2016 and March of 2018.'),
                 h4('Data Source'),
                 p(a('Click here to view the Melbourne Housing Market from Kaggle.com',
                     href = 'https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market/data?select=Melbourne_housing_FULL.csv')),
                 h4('App Navigation'),
                 p('Use the side panel to filter the data and select variables to investigate.'),
                 p('Visit the Data Download tab to view and download the data set.'),
                 p('Then click on the Data Exploration tab to view graphical and numerical summaries.'),
                 img(src='https://www.cruiseandtravel.co.uk/_gatsby/file/4b9f6add774f6a00098fdfa2f32d358f/98543_iStock-876026224-1024x705.jpg?imwidth=960',
                     width = '100%')
        ),
        tabPanel('Data Download',
                 DTOutput('data_table'),
                 downloadButton('download_data', 'Download CSV')
        ),
        tabPanel('Data Exploration',
                 radioButtons('summary_type', 'Choose variable type to summarize:',
                              choices = c('Numeric Summaries', 'Categorical Summaries'),
                              inline = TRUE),
                 uiOutput('explore_ui'),
                 
                 # sub tabs for graphical and numerical summaries
                 tabsetPanel(
                   tabPanel("Graphical Summary",
                            plotOutput('plot')),
                   tabPanel("Numerical Summary",
                            verbatimTextOutput('summary'))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(subset_data = NULL)
  
  observeEvent(input$num1, {
    num1 <- input$num1
    num2 <- input$num2
    choices <- numeric_vars
    
    if (num1 != num2) {
      choices <- choices[-which(choices == num1)]
      updateSelectizeInput(session,
                           "num2",
                           choices = choices,
                           selected = num2)
    }
  })
  
  # Update num1 choices when num2 changes
  observeEvent(input$num2, {
    num1 <- input$num1
    num2 <- input$num2
    choices <- numeric_vars
    
    if (num1 != num2) {
      choices <- choices[-which(choices == num2)]
      updateSelectizeInput(session,
                           "num1",
                           choices = choices,
                           selected = num1)
    }
  })
  
  # Slider for first numeric variable
  output$num1_slider <- renderUI({
    req(input$num1)
    rng <- range(df[[input$num1]], na.rm = TRUE)
    sliderInput("num1_range",
                paste("Filter", input$num1),
                min = rng[1], max = rng[2],
                value = rng)
  })
  
  # Slider for second numeric variable
  output$num2_slider <- renderUI({
    req(input$num2)
    rng <- range(df[[input$num2]], na.rm = TRUE)
    sliderInput("num2_range",
                paste("Filter", input$num2),
                min = rng[1], max = rng[2],
                value = rng)
  })
  
  # Subset data when user clicks button
  observeEvent(input$subset_btn, {
    data_sub <- df
    
    # categorical filters
    if (input$region != "All") {
      data_sub <- data_sub[data_sub$Regionname == input$region, ]
    }
    if (input$type != "All") {
      data_sub <- data_sub[data_sub$Type == input$type, ]
    }
    
    # update numeric variable when slider is changed
    data_sub <- data_sub |>
      filter(
        between(.data[[input$num1]], input$num1_range[1], input$num1_range[2]),
        between(.data[[input$num2]], input$num2_range[1], input$num2_range[2])
      )
    
    # store in reactiveValues
    rv$subset_data <- data_sub
  })
  
  # Data download tab
  output$data_table <- DT::renderDataTable({
    validate(
      need(!is.null(rv$subset_data), "Please subset the data and click 'Apply filters' using the sidebar.")
    )
    datatable(rv$subset_data, options = list(pageLength = 10))
  })
  
  output$download_data <- downloadHandler(
    filename = function() {"subset_data.csv"},
    content = function(file) {
      write.csv(rv$subset_data, file, row.names = FALSE)
    }
  )
  
  # Data exploration tab
  output$explore_ui <- renderUI({
    if (input$summary_type == "Numeric Summaries") {
      selectizeInput("num_var_summary", "Choose numeric variable to summarize:",
                     choices = numeric_vars,
                     selected = "Price")
    } else {
      selectizeInput("cat_var", "Choose categorical variable to summarize:",
                     choices = cat_vars,
                     selected = "Regionname")
    }
  })
  
  # summary plot
  output$plot <- renderPlot({
    validate(
      need(!is.null(rv$subset_data), "Please subset the data and click 'Apply filters' using the sidebar.")
    )
    
    # Create histogram for numeric variables
    if (input$summary_type == "Numeric Summaries") {
      ggplot(rv$subset_data, aes(x = .data[[input$num_var_summary]])) +
        geom_histogram(bins = 30,
                       fill = 'deepskyblue4',
                       color = "white") +
        theme_minimal()
    # create bar chart for categorical variables
    } else {
      ggplot(rv$subset_data, aes(x = .data[[input$cat_var]])) +
        geom_bar(fill = 'deepskyblue4') +
        coord_flip() +
        theme_minimal()
    }
  })
  
  # Numeric
  output$summary <- renderPrint({
    validate(
      need(!is.null(rv$subset_data), 
           "Please subset the data and click 'Apply filters' using the sidebar.")
    )
    if (input$summary_type == "Numeric Summaries") {
      validate(
        need(!is.null(input$num_var_summary),
             "Please select a numeric variable to summarize.")
      )
      summary(rv$subset_data[[input$num_var_summary]])
    } else {
      validate(
        need(!is.null(input$cat_var),
             "Please select a categorical variable to summarize.")
      )
      table(rv$subset_data[[input$cat_var]])
    }
  })
}

shinyApp(ui, server)