#####################################################################
# Project: API query for Australian BRUV synthesis
# Data:    2024 BRUV Syntheses metadata, count, length and covariates
# Task:    Use GlobalArchive API to query data and save as an RDS
# Author:  Brooke Gibbons and Claude Spencer
# Date:    Feb 2024
#####################################################################

# Install CheckEM package ----
options(timeout = 9999999) # the package is large, so need to extend the timeout to enable the download.
remotes::install_github("GlobalArchiveManual/CheckEM") # If there has been any updates to the package then CheckEM will install, if not then this line won't do anything

# Load libraries needed -----
library(CheckEM)
library(httr)
library(tidyverse)
library(RJSONIO)
library(devtools)
library(leaflet)

# Set your API token to access GlobalArchive data shared with you ----
# It is extremely important that you keep your API token out of your scripts, and github repository!
# This function will ask you to put your API token in the console
# It will then create a folder in your project folder called "secrets" and saves your API token to use in the functions later
# The function adds the token into the .gitignore file so it will never be put in version control with Git
CheckEM::ga_api_set_token()

# Load the saved token
token <- readRDS("secrets/api_token.RDS")

# Load the metadata, count and length ----
# This way does not include the zeros where a species isn't present - it returns a much smaller dataframe
CheckEM::ga_api_all_data(synthesis_id = "21", # change the synthesis_id here
                         token = token,
                         dir = "data/raw/",
                         include_zeros = FALSE)

## This way DOES include the zeros where a species isn't present - it returns a much, much, larger dataframe
# CheckEM::ga_api_all_data(synthesis_id = "XXXX", # change the synthesis_id here
#                          token = token,
#                          dir = "data/raw/",
#                          include_zeros = TRUE)

# Example to filter count data to a species of interest ----
count_filtered <- count %>%
  dplyr::mutate(scientific = paste(genus, species)) %>%
  dplyr::filter(scientific %in% "Pseudocaranx spp") %>%
  left_join(metadata) %>%
  glimpse()

# Visualise the species abundance data spatially
leaflet(data = count_filtered) %>%                     
  addTiles() %>%                                                    
  addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
  addLayersControl(baseGroups = c("Open Street Map", "World Imagery"), options = layersControlOptions(collapsed = FALSE)) %>%
  addCircleMarkers(data = count_filtered, lat = ~ latitude_dd, lng = ~ longitude_dd, radius = ~ count / 10, fillOpacity = 0.5, stroke = FALSE, label = ~ as.character(sample))

# Example to filter length data to a species of interest ----
length_filtered <- length %>%
  dplyr::mutate(scientific = paste(genus, species)) %>%
  dplyr::filter(scientific %in% "Pseudocaranx spp") %>%
  left_join(metadata) %>%
  glimpse()

# Visualise the length data as a histogram
ggplot(data = length_filtered, aes(length_mm)) +
  geom_histogram(fill = "#7cbbeb", colour = "#0c64a8", bins = 20) +
  theme_classic() +
  labs(x = "Length (mm)", y = "Abundance")
