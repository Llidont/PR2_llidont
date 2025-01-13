# Data Visualization (PR2): Main Characteristics of New Passenger Cars in Europe"

This repository contains an R Shiny application for visualizing the main characteristics of new passenger cars in Europe. The application includes interactive maps and plots that explore emissions, mass, engine power, and popular car makes. Due to the size of the dataset, preprocessing is required before running the Shiny app.

## Repository Structure

```
├── preprocess.R       # Preprocessing script for large datasets
├── ui.R               # User interface definition for the Shiny app
├── server.R           # Server logic for the Shiny app
├── processed/         # Directory for processed data files
│   ├── emissions.csv
│   ├── makes.csv
│   ├── mass.csv
│   ├── engine_power.csv
└── data/              # Directory for raw input CSV files (not included)
```

## Features

- **Interactive Map:** Displays average emissions and total cars by country. They can be clicked in order to be selected.
- **Histograms:** Explore distributions of emissions, mass and most popular car makes.
- **Boxplot:** Analyze engine power distributions with outlier detection.
- **Custom Filters:** Select year ranges and fuel types for detailed analysis.

## Requirements

### R Libraries
The application requires the following R (version 4.4.1) packages:
- `dplyr` (version 1.1.4)
- `readr` (version 2.1.5)
- `shiny` (version 1.9.1)
- `leaflet` (version 2.2.2)
- `shinyjs` (version 2.1.0)
- `ggplot2` (version 3.5.1)
- `plotly` (version 4.10.4)
- `maps` (version 3.4.2)
- `sf` (version 1.0-19)
- `tidyr` (version 1.3.1)
- `rnaturalearth` (version 1.0.1)
- `rnaturalearthdata` (version 1.0.0)

Other compatibilities may work but have not been tested.

### Data
The raw dataset is too large to be included in this repository. To update the data preprocessed if needed, you must:
1. Download the raw data from the [European Environment Agency's CO2 emissions database](https://co2cars.apps.eea.europa.eu/).
2. Place the downloaded CSV files in the `data/` directory.

## Usage

### Preprocessing
Run the `preprocess.R` script to generate the required processed data files:

```R
source("preprocess.R")
```
This script reads the raw data from the `data/` directory, processes it, and outputs the following files to the `processed/` directory:
- `emissions.csv`
- `makes.csv`
- `mass.csv`
- `engine_power.csv`

### Running the Shiny App
To start the Shiny application:

1. Ensure all required libraries are installed.
2. Run the following commands in R:

```R
library(shiny)
runApp()
```

The application will open in your default web browser.

## Notes

- The `preprocess.R` script is designed to handle large datasets efficiently by summarizing and grouping data before generating output files.
- The Shiny app dynamically filters and visualizes data based on user inputs, ensuring responsive interaction even with large datasets.

## License
This project is licensed under the [CC BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/deed.en) license.

## Tools and Contributions
This project utilized [ChatGPT](https://openai.com/chatgpt) as a tool for assistance in code development and documentation.

## Author
Laura M. Lidón  
Email: [llidont@uoc.edu](mailto:llidont@uoc.edu)

