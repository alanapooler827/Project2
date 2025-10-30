library(shiny)
library(shinyalert)
library(tidyverse)
library(ggplot2)

# read in data set
df <- read_csv('Data/Melbourne_housing_FULL.csv', na = ('#N/A'))

# clean data set
df <- df |>
  filter(!is.na(Price), BuildingArea > 0, Landsize > 0) |>
  mutate(
    BuildingArea = round(BuildingArea*10.7639, 2),
    Landsize = round(Landsize*10.7639, 2)
  ) |>
  mutate(
    across(
      c(Regionname, Type, Method, SellerG, CouncilArea, Suburb),
      as.factor
    ))

# Define app UI
ui <- fluidPage(
  # App title
  titlePanel('Melbourne Housing Data Exploration'),
  
  sidebarLayout(
    sidebarPanel(
      # Categorical variable selection
      h2('Choose a Subset of the Data:'),
      selectizeInput('region', 'Region:', 
                  choices = c('All', sort(unique(df$Regionname)))),
      selectizeInput('type', 'Property Type:', 
                  choices = c('All', sort(unique(df$Type)))),
      br(),
      
      # Numeric Variables
      h2('Choose Numeric Variables:'),
      selectizeInput('num1', 'First Numeric Variable:',
                 choices = c('Price', 'Landsize', 'BuildingArea')),
      uiOutput('num1_slider'),
      
      selectizeInput('num2', 'Second Numeric Variable:',
                     choices = c('Price', 'Landsize', 'BuildingArea')),
      uiOutput('num2_slider'),
      
      actionButton('subset_btn', 'Apply Filters', class = 'btn-primary')
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel('About',
                 h2('About This App'),
                 p('This app explores Melbourne housing data.'),
                 p('The data set includes properties in Melbourne sold between February of 2016 and March of 2018.'),
                 p('Data Source:',
                   a('Kaggle Melbourne Housing Market',
                     href = 'https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market/data?select=Melbourne_housing_FULL.csv')),
                 p('Use the side panel to filter the data and select variables to explore.
                   Then click on the Data Exploration tab to view summaries.'),
                 p('Visit the Data Download tab to view and download the data set.'),
                 img(src='https://www.cruiseandtravel.co.uk/_gatsby/file/4b9f6add774f6a00098fdfa2f32d358f/98543_iStock-876026224-1024x705.jpg?imwidth=960',
                     width = '100%')
          ),
        tabPanel('Data Download',
                 DTOutput('data_table'),
                 downloadButton('download_data', 'Download CSV')
        ),
        tabPanel('Data Exploration',
                 radioButtons('summary_type', 'Choose summary type:',
                              choices = c('Numeric Summaries', 'Categorical Summaries')),
                 uiOutput('explore_ui'),
                 plotOutput('plot'),
                 verbatimTextOutput('summary')
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)