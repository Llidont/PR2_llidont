######################################
#                                    #
# Author: Laura María Lidón Tárraga  #
# Date: 2025/01/13                   #
# Project: Data Visualization PR2    #
#                                    #
######################################


# Libraries---------------------------------------------------------------------
library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(plotly)
library(readr)
library(maps)
library(sf)
library(tidyr)
library(rnaturalearth)
library(rnaturalearthdata)

# server definition ------------------------------------------------------------
server <- function(input, output, session) {
    # Load initial datasets
    initial_emissions <- read_csv("processed/emissions.csv")
    initial_makes <- read_csv("processed/makes.csv")
    initial_mass <- read_csv("processed/mass.csv")
    initial_engine_power <- read_csv("processed/engine_power.csv")
    
    # Load country boundaries and filter to match the emissions dataset
    country_boundaries <- ne_countries(scale = "medium", returnclass = "sf")%>%
        filter(iso_a2_eh%in%initial_emissions$Country)%>%
        select(name, iso_a2_eh, label_x, label_y)
    
    # Create a named list of country codes and names for dropdown menus
    country_choices <- setNames(country_boundaries$iso_a2_eh, country_boundaries$name)
    
    # Reactive function to track selected fuel types
    fuel_type <- reactive({
        checked <- c()
        
        if (input$diesel) checked <- c(checked, "diesel")
        if (input$diesel_electric) checked <- c(checked, "diesel/electric")
        if (input$e85) checked <- c(checked, "e85")
        if (input$electric) checked <- c(checked, "electric")
        if (input$hydrogen) checked <- c(checked, "hydrogen")
        if (input$lpg) checked <- c(checked, "lpg")
        if (input$ng) checked <- c(checked, "ng", "ng-biomethane")
        if (input$petrol) checked <- c(checked, "petrol")
        if (input$petrol_electric) checked <- c(checked, "petrol/electric")
        checked
    })
    
    # Dynamically update the year slider based on the data range
    observe({
        min_year <- min(initial_emissions$year, na.rm = TRUE)
        max_year <- max(initial_emissions$year, na.rm = TRUE)
        updateSliderInput(
            session, "year",
            min = min_year, max = max_year,
            value = c(min_year,max_year), step=1
        )
    })
    
    # Reactive filters for datasets based on user inputs
    filtered_emissions <- reactive({
        initial_emissions %>%
            filter(year >= input$year[1], year <= input$year[2], Ft %in% fuel_type())
    })
    
    filtered_makes <- reactive({
        initial_makes %>%
            filter(year >= input$year[1], year <= input$year[2], Ft %in% fuel_type())
    })
    
    filtered_engine_power <-  reactive({
        initial_engine_power %>%
            filter(year >= input$year[1], year <= input$year[2], Ft %in% fuel_type())
    })
    
    filtered_mass <-  reactive({
        initial_mass %>%
            filter(year >= input$year[1], year <= input$year[2], Ft %in% fuel_type())
    })
        
    
    # Render the map with emissions data
    output$map <- renderLeaflet({
        # Join emissions data with country boundaries
        emissions_map <- filtered_emissions()%>%
            group_by(Country) %>%
            summarise(average_emissions = sum(bin * count, na.rm = TRUE) / sum(count),
                      total_cars = sum(count))%>%
            left_join(country_boundaries, by = c("Country"="iso_a2_eh"))
        
        pal <- colorNumeric("RdYlGn", domain = -emissions_map$average_emissions)
        
        emissions_map <- emissions_map %>%
            mutate(color = pal(-average_emissions))
        
        
        leaflet() %>%
            addProviderTiles("CartoDB.Positron", options = tileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(
                data = emissions_map,
                lng = ~label_x,
                lat = ~label_y,
                radius = ~10 + 5 * log10(total_cars/100000),
                color = ~ color,
                stroke = TRUE,
                fillOpacity = 0.8,
                layerId = ~Country
            )
    })
    
    # Update the selected country when a map marker is clicked
    observeEvent(input$map_marker_click, {
        updateSelectInput(session, "country", selected = input$map_marker_click$id)
    })
    
    
    # Filter country choices based on available data
    country_choices_filtered <- reactive({
        country_choices_filtered <- country_choices[which(country_choices%in%filtered_makes()$Country)]
    })
    
    # Render the details panel dynamically
    output$details_panel <- renderUI({
        tagList(
            fluidRow(
                column(6,selectInput("country", "Country", choices = country_choices_filtered(), selected = "SP")),
                column(6,htmlOutput("description_text"))),
            # 2x2 grid layout for the plots
            fluidRow(
                column(6, plotlyOutput("emissions_hist", height = "350px")),
                column(6, plotlyOutput("makes_pie", height = "350px"))
            ),
            fluidRow(
                column(6, plotlyOutput("mass_hist", height = "350px")),
                column(6, plotlyOutput("engine_power_box", height = "350px"))
            )
        )
    })
    
    # Toggle visibility of the details panel
    observeEvent(input$toggle, {
        toggle("details_panel")
    })
    
    # Generate description text dynamically based on selected country
    output$description_text <- renderUI({
        car_reg <- filtered_makes() %>%
            filter(Country == input$country) %>%
            summarize(total = sum(count)) %>%
            pull(total)
        common_make <- filtered_makes() %>%
            filter(Country == input$country) %>%
            group_by(Mk) %>%
            summarise(total = sum(count)) %>%
            arrange(desc(total)) %>%
            slice(1) %>%
            pull(Mk)
        common_model <- filtered_makes() %>%
            filter(Country == input$country) %>%
            group_by(Cn) %>%
            summarise(total = sum(count)) %>%
            arrange(desc(total)) %>%
            slice(1) %>%
            pull(Cn)
        HTML(paste0("Between period, with fuel types selected.",
        "<br/>", "<br/>", "Registered cars: ",car_reg,
        "<br/>", "Most common make: ",common_make,
        "<br/>", "Most common model: ",common_model
        ))
    })
    
    
    # Render histograms and boxplots for filtered data
    output$emissions_hist <- renderPlotly({
        p <- filtered_emissions() %>%
            filter(Country==input$country)%>%
            group_by(bin)%>%
            summarize(count=sum(count))%>%
            mutate(tooltip = paste0(bin, "g/km<br>", count, " cars"))%>%
            ggplot(aes(x = bin)) +
            geom_bar(stat = "identity", aes(y = count, text=tooltip), fill = "blue", color = "black") +
            ggtitle("Emissions Histogram (Ewltp)") +
            theme_minimal() +
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank(),
                legend.position = "none"
            )
        
        # Convert ggplot to plotly with custom tooltips
        ggplotly(p, tooltip = "text") 
    })
    
    output$makes_pie <- renderPlotly({
        p <- filtered_makes() %>%
            filter(Country == input$country) %>%
            group_by(Mk) %>%
            summarize(count = sum(count)) %>%
            arrange(desc(count)) %>%
            mutate(Mk = ifelse(row_number() <= 30, Mk, "OTHERS")) %>%
            group_by(Mk) %>%
            summarize(count = sum(count)) %>%
            mutate(tooltip = paste0("Make ", Mk, "<br>", count, " cars"))%>%
            arrange(desc(count)) %>%
            ggplot(aes(x = reorder(Mk, -count), y = count, fill = Mk)) + # Reorder bars by count
            geom_bar(stat = "identity", aes(text=tooltip), color = "black") +
            scale_fill_manual(values = c("OTHERS" = "red", rep("green", 30))) + # Set color for "OTHERS" to red
            ggtitle("Most Used Makes Histogram") +
            theme_minimal() +
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank(),
                legend.position = "none"
            )
        
        # Convert ggplot to plotly with custom tooltips
        ggplotly(p, tooltip="text") 
    })
    

    output$mass_hist <- renderPlotly({
        p <- filtered_mass()%>%
            filter(Country==input$country)%>%
            group_by(bin)%>%
            summarize(count=sum(count))%>%
            mutate(tooltip = paste0(bin, "kg<br>", count, " cars"))%>%
            ggplot(aes(x = bin)) +
            geom_bar(stat = "identity", aes(y = count, text=tooltip), fill = "green", color = "black") +
            ggtitle("Mass Histogram") +
            theme_minimal() +
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank(),
                legend.position = "none"
            )
        
        # Convert ggplot to plotly with custom tooltips
        ggplotly(p, tooltip = "text") 
    })
    
    output$engine_power_box <- renderPlotly({
        # Data processing
        data <- filtered_engine_power() %>%
            filter(Country == input$country, !is.na(bin)) %>%
            group_by(bin) %>%
            summarize(count = sum(count), .groups = "drop")
        
        # Calculate cumulative frequencies and total count
        data <- data %>%
            arrange(bin) %>%
            mutate(
                cumulative_count = cumsum(count),
                total_count = sum(count)
            )
        
        # Function to calculate quantiles based on weighted counts
        get_quantile <- function(data, quantile) {
            target <- quantile * data$total_count[1]
            data %>%
                filter(cumulative_count >= target) %>%
                slice(1) %>%
                pull(bin)
        }
        
        # Calculate boxplot statistics
        q1 <- get_quantile(data, 0.25)
        median <- get_quantile(data, 0.50)
        q3 <- get_quantile(data, 0.75)
        iqr <- q3 - q1
        lower_whisker <- max(min(data$bin), q1 - 1.5 * iqr)
        upper_whisker <- min(max(data$bin), q3 + 1.5 * iqr)
        
        # Identify outliers
        outliers <- data %>%
            filter(bin < lower_whisker | bin > upper_whisker) %>%
            pull(bin)
        
        # Create the boxplot
        p <- plot_ly() %>%
            add_boxplot(
                y = list(""),
                lowerfence = list(lower_whisker),
                q1 = list(q1),
                median = list(median),
                q3 = list(q3),
                upperfence = list(upper_whisker),
                name = "Boxplot",
                marker = list(color = "blue"),
                line = list(color = "black")
            )
        
        # Add outliers as scatter points
        if (length(outliers) > 0) {
            p <- p %>%
                add_trace(
                    x = outliers,
                    y = rep("", length(outliers)), 
                    type = "scatter",
                    mode = "markers",
                    marker = list(color = "red", symbol = "circle"),
                    name = "Outliers"
                )
        }
        
        # Final layout with titles
        p %>%
            layout(
                title = "Engine Power Boxplot",
                xaxis = list(title = "Engine Power"),
                yaxis = list(title = "")
            )
    })
    
    
    # Stop the app when the session ends
    session$onSessionEnded(function() {
        stopApp()
    })
}
