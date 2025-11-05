# Project 2: Melbourne Housing Data Exploration

This repository contains the code to build an interactive Shiny app that explores [Melbourne Housing Market Data](https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market/data?select=Melbourne_housing_FULL.csv).

------------------------------------------------------------------------

### App Overview

This Shiny app provides an interactive UI that allows users to:

-   **Apply filters:** Filter the data by categorical and/or numerical variables.

    -   Categorical: users can select a subset of Region or Year Sold

    -   Numerical: users can filter the range of numeric variables (sale price, building area, land size, bedrooms, bathrooms, distance from city center)

    -   Users can select up to two numerical variables to filter the data by

-   **View and download data:** After applying filters, users can view and download the resulting data.

-   **Visualize data:** Generate graphical and numerical summaries for selected variables.

    -   Choose variables to group and/or facet plots and summaries by

    -   Create one- and two-way contingency tables, bar charts, and histograms

#### App Navigation

-   **About:** contains information about the data and the app.

-   **Data Download:** allows users to view and download data after applying filters.

-   **Data exploration:** displays graphical and numerical summaries for selected variables.

-   **Sidebar:** allows the user to select different subsets of the data.

The code to build the Shiny app can be found [here](shiny_app\App.r).

------------------------------------------------------------------------

### Melbourne Housing Market Data

The data set used throughout this repository is from Kaggle.com.

**Data Notes:**

-   This data set contains information from properties sold in Melbourne between February of 2016 and March of 2018.

-   Some data cleaning is applied in the [static_eda](static_eda.qmd) notebook and when the app is launched. This cleaning includes:

    -   Categorical variables are converted to factors where appropriate

    -   Factor levels are renamed as needed for clarity

    -   Rows where 'Price' is missing are removed

    -   Rows where 'BuildingArea' or 'Landsize' are 0 are removed

    -   'BuildingArea' and 'Landsize' are converted from square meters to square feet.

    -   The year is extracted from 'Date' to create the variable 'YearSold'

-   The data set contains 8,272 rows after cleaning.
