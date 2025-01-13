######################################
#                                    #
# Author: Laura María Lidón Tárraga  #
# Date: 2025/01/13                   #
# Project: Data Visualization PR2    #
#                                    #
######################################

library(dplyr)
library(readr)


files <- list.files("data", pattern = "*.csv", full.names = TRUE)

emissions <- data.frame(year=c(), Ft=c(), Country=c(), bin=c(), count=c())
makes <- data.frame()
mass <- data.frame()
engine_power <- data.frame()

for(file in files){
    data <- read_csv(file)
    
    emissions <- data %>%
        mutate(bin = round(`Ewltp (g/km)`/10,0)*10,
               Ft=tolower(Ft)) %>%
        group_by(year, Ft, Country, bin)%>%
        summarize(count = n(), .groups = "drop")%>%
        bind_rows(emissions)
    
    makes <- data %>%
        mutate(Ft=tolower(Ft))%>%
        group_by(year, Ft, Country, Mk, Cn) %>%
        summarize(count = n(), .groups = "drop") %>%
        bind_rows(makes)
    
    mass <- data %>%
        mutate(bin = round(`m (kg)`/100,0)*100,
               Ft=tolower(Ft)) %>%
        group_by(year, Ft, Country, bin) %>%
        summarize(count=n(), .groups = "drop") %>%
        bind_rows(mass)
    
    # Generate engine_power.csv
    engine_power <- data %>%
        mutate(bin = round(`ep (KW)`/10,0)*10,
               Ft=tolower(Ft)) %>%
        group_by(year, Ft, Country, bin) %>%
        summarize(count=n(), .groups = "drop") %>%
        bind_rows(engine_power)
}


write_csv(emissions, "processed/emissions.csv")
write_csv(makes, "processed/makes.csv")
write_csv(mass, "processed/mass.csv")
write_csv(engine_power, "processed/engine_power.csv")


