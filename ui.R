######################################
#                                    #
# Author: Laura María Lidón Tárraga  #
# Date: 2025/01/13                   #
# Project: Data Visualization PR2    #
#                                    #
######################################


# Libraries---------------------------------------------------------------------
library(shiny)
library(leaflet)
library(shinyjs)

# UI definition ----------------------------------------------------------------
ui <- fluidPage(
    useShinyjs(), # Enable JavaScript features
    h3("Data Visualization (PR2): Main Characteristics of New Passenger Cars in Europe"),
    
    fluidRow(
        # Left column: Contains filters and the map
        column(6,
            fluidRow(
               # Slider to select a range of years
               column(4,sliderInput("year", "Year", min = 2015, max = 2023, value = c(2020, 2021), step = 1)),
               column(8, fluidRow(
                   # Checkbox inputs for selecting fuel types
                   column(4, checkboxInput("diesel", "Diesel", TRUE)),
                   column(4, checkboxInput("diesel_electric", "Diesel/Electric", TRUE)),
                   column(4, checkboxInput("e85", "E85", TRUE)),
                   
                   column(4, checkboxInput("electric", "Electric", TRUE)),
                   column(4, checkboxInput("hydrogen", "Hydrogen", TRUE)),
                   column(4, checkboxInput("lpg", "LPG", TRUE)),
                   
                   column(4, checkboxInput("ng", "NG and NG biomethane", TRUE)),
                   column(4, checkboxInput("petrol", "Petrol", TRUE)),
                   column(4, checkboxInput("petrol_electric", "Petrol/Electric", TRUE))),
                   )
               )
               ,
               leafletOutput("map", height = "700px"))
        ,
        # Right column: Dynamic UI panel for additional details
        column(6,
            uiOutput("details_panel")
        )
    ),
    
    # Footer with author info, data source, and license
    HTML("<br><br><p style='display: flex; justify-content: space-between;'>
                    <a href='mailto:llidont@uoc.edu'> <strong>Author:</strong> Laura M Lidón (llidont@uoc.edu)</a>
                    <a href='https://github.com/Llidont/PR2_llidont/' target='_blank'><strong>Readme and code</strong>
                    <a href='https://co2cars.apps.eea.europa.eu/' target='_blank'><strong>Original data</strong> </a>
                    <a href='https://creativecommons.org/licenses/by-nc/4.0/deed.en'> <strong>License:</strong> CC BY-NC 4.0 (Attribution-NonCommercial)</a> </p>")
)