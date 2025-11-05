# Define numeric variables
numeric_vars <- c(
  "Sale Price" = "Price",
  "Building Area" = "BuildingArea",
  "Land Size" = "Landsize",
  "Number of Bedrooms" = 'Bedroom2',
  'Number of Bathrooms' = 'Bathroom',
  'Distance from City Center (KM)' = 'Distance')

# Define categorical variables
cat_vars <- c('Region Name' = 'Regionname',
              'Type' = 'Type',
              'Council Area' = 'CouncilArea',
              'Suburb' = 'Suburb',
              'Year Sold' = 'YearSold')

# Define variables to facet plot by
facet_vars <- c('Type' = 'Type',
                'Year Sold' = 'YearSold')