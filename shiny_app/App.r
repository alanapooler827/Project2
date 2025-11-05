library(shiny)
library(shinyalert)
library(tidyverse)
library(DT)
library(ggplot2)

source('Helpers.r')

# suppress scientific notation, print all rows of tibble
options(scipen = 999, tibble.print_max = Inf)

# read in data set
df_raw <- read_csv('Data/Melbourne_housing_FULL.csv', na = ('#N/A'))

# clean data set
df <- df_raw |>
  # removing missing/invalid values
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
      h2('Select a Subset of the Data'),
      h3('Categorical Variables:'),
      selectizeInput('region', 'Region:', 
                     choices = c("All", levels(df$Regionname)),
                     selected = 'All'),
      selectizeInput('yearsold', 'Year Sold:', 
                     choices = c("All", levels(df$YearSold)),
                     selected = 'All'),
      br(),
      
      # Numeric variable selection
      h3('Numeric Variables:'),
      selectizeInput('num1', '',
                 choices = numeric_vars,
                 selected = numeric_vars[1]),
      uiOutput('num1_slider'),
      
      selectizeInput('num2', '',
                     choices = numeric_vars,
                     selected = numeric_vars[2]),
      uiOutput('num2_slider'),
      
      # Button to apply filters
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
                 
                 # add option for grouping numeric variable summary
                 conditionalPanel(
                   "input.summary_type == 'Numeric Summaries'",
                   selectizeInput('group_var_num', 'Group by:',
                                  choices = c("None", cat_vars),
                                  selected = "None")
                 ),
                 # add option to facet graph
                 conditionalPanel(
                   "input.summary_type == 'Numeric Summaries' && input.summary_tabs == 'Graphical Summary'",
                   selectizeInput('facet_var_num', 'Facet by:',
                                  choices = c("None", facet_vars),
                                  selected = "None")
                 ),
                 
                 # add option to select numeric variable for categorical graph
                 conditionalPanel(
                   "input.summary_type == 'Categorical Summaries' && input.summary_tabs == 'Graphical Summary'",
                   selectizeInput('num_for_cat', 'Choose numeric variable:',
                                  choices = c("None", numeric_vars),
                                  selected = "None")
                 ),
                 # add option to select second variable for contingency table
                 conditionalPanel(
                   "input.summary_type == 'Categorical Summaries' && input.summary_tabs == 'Numerical Summary'",
                   selectizeInput('second_cat', 'Choose second categorical variable:',
                                  choices = c("None", cat_vars),
                                  selected = "None")
                 ),
                 
                 # sub tabs for graphical and numerical summaries
                 tabsetPanel(
                   id = "summary_tabs",
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
  
  # Update num1 choices when num2 changes
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
  
  # Update num2 choices when num1 changes
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
                paste("Filter", names(numeric_vars)[numeric_vars == input$num1]),
                min = rng[1], max = rng[2],
                value = rng)
  })
  
  # Slider for second numeric variable
  output$num2_slider <- renderUI({
    req(input$num2)
    rng <- range(df[[input$num2]], na.rm = TRUE)
    sliderInput("num2_range",
                paste("Filter", names(numeric_vars)[numeric_vars == input$num2]),
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
    if (input$yearsold != "All") {
      data_sub <- data_sub[data_sub$YearSold == input$yearsold, ]
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
  # display table
  output$data_table <- DT::renderDataTable({
    validate(
      need(!is.null(rv$subset_data), "Please subset the data and click 'Apply filters' using the sidebar.")
    )
    datatable(rv$subset_data, options = list(pageLength = 10))
  })
  
  # download table
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
    
    if (input$summary_type == "Numeric Summaries") {
      num_var <- input$num_var_summary
      group_var <- input$group_var_num
      facet_var <- input$facet_var_num
      
      p <- ggplot(rv$subset_data, aes(x = .data[[num_var]]))
      
      # Color by group if grouping variable is selected
      if (group_var != "None") {
        p <- p + geom_histogram(aes(fill = .data[[group_var]]), position = "identity", alpha = 0.6)
      } else {
        p <- p + geom_histogram(fill = 'deepskyblue4', color = "white")
      }
      
      # Add facet wrap if facet variable is selected
      if (facet_var != "None") {
        p <- p + facet_wrap(vars(.data[[facet_var]]))
      }
      
      p + theme_minimal()
      
      # create bar charts for categorical variables
    } else {
      cat_var <- input$cat_var
      num_var <- input$num_for_cat
      
      # create bar chart if only one variable is selected
      if (num_var == "None") {
        ggplot(rv$subset_data, aes(x = .data[[cat_var]])) +
          geom_bar(fill = 'deepskyblue4') +
          coord_flip() +
          theme_minimal()
        # create bar chart of cat var vs avg of numeric variable if selected
      } else {
        # Compute mean numeric value per category
        summary_df <- rv$subset_data |>
          group_by(.data[[cat_var]]) |>
          summarize(Mean_Value = mean(.data[[num_var]], na.rm = TRUE),
                    .groups = "drop")
        
        ggplot(summary_df, aes(x = .data[[cat_var]], y = Mean_Value)) +
          geom_col(fill = 'deepskyblue4') +
          coord_flip() +
          theme_minimal() +
          labs(y = paste("Mean", num_var))
     }
    }
  })
  
  # Numeric summaries
  output$summary <- renderPrint({
    validate(
      need(!is.null(rv$subset_data), 
           "Please subset the data and click 'Apply filters' using the sidebar.")
    )
    
    # create 5 number summary if numeric summary is selected
    if (input$summary_type == "Numeric Summaries") {
      validate(need(!is.null(input$num_var_summary),
                    "Please select a numeric variable."))
      
      num_var <- input$num_var_summary
      group_var <- input$group_var_num
      
      if (group_var == "None") {
        print(summary(rv$subset_data[[num_var]]))
      } else {
        cat("Summary statistics for", num_var, "by", group_var, ":\n")
        print(rv$subset_data |>
                group_by(.data[[group_var]]) |>
                summarize(across(all_of(num_var), list(
                  Min = min,
                  Q1 = ~quantile(.x, 0.25),
                  Median = median,
                  Mean = mean,
                  Q3 = ~quantile(.x, 0.75),
                  Max = max
                ), .names = "{.fn}")))
      }
      
      # create contingency table if categorical summary is selected
    } else {
      cat_var <- input$cat_var
      second_cat <- input$second_cat
      
      # create one or two way contingency table depending on # of variables selected
      if (second_cat == "None") {
        tbl <- table(rv$subset_data[[cat_var]])
        print(tbl)
      } else {
        tbl <- table(rv$subset_data[[cat_var]], rv$subset_data[[second_cat]])
        print(tbl)
      }
    }
  })
}

shinyApp(ui, server)