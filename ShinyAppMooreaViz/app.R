#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load Packages ----
library(shiny)
library(leaflet)
library(tidyverse)
library(shinyWidgets)

### not sure if we need all of these, they are from the layers.Rmd
library(here)
library(janitor)
library(raster)
library(viridis)
library(sf)
library(gstat)
library(sp)
library(automap)
library(testthat)
library(htmlwidgets)
library(ggvoronoi)
library(car)
library(dismo)
library(spatstat)


# Load Data ----

#nitrogen data
nitrogen_data <- read_csv(here("data", "csv", "N_summary_2016.csv")) %>% 
    clean_names()

#bleaching data
bleach <- read_csv(here("data", "csv", "percent_bleach_2016.csv")) %>% 
    clean_names()

#10x10 grid for kriging
grd_sp <- readRDS(here("data", "krig_grid", "grd_sp"))

#sewage data
sewage_data <- read.csv(here("Data/Predicted_nuts.csv")) %>% 
    clean_names()

sewage_data <- cbind(nitrogen_data, sewage_data)


# User Interface ----
# Creates the structure for your app's look and appearance 
# Define UI for application that draws a histogram

ui <- fluidPage(

    # Application title
    titlePanel("Moorea Coral Reef LTER"),
    sidebarLayout(
        sidebarPanel(),
        mainPanel(img(src = "mcr_logo.png", height = 60, width = 150, align = "right"), 
                  img(src = "lter_logo.png", height = 60, width = 70, align = "right"), 
                  img(src = "nsf_logo.png", height = 60, width = 60, align = "right"))),

    navbarPage("App Title", 
               tabPanel("Home"),
               navbarMenu("Spatial",
                          tabPanel(title = "Map", 
                                   
                                   #----
                                   #leaflet map inputs
                                   
                                   pickerInput(inputId = "Year",
                                               label = "Select a Year:",
                                               choices = c("2016",
                                                           "2017",
                                                           "2018"),
                                                multiple = FALSE),
                                   shinyWidgets::pickerInput(inputId = "Month",
                                                             label = "Select a Month:",
                                                             choices = c("January", 
                                                                         "July", 
                                                                         "May"),
                                                             multiple = FALSE), 
                                   shinyWidgets::pickerInput(inputId = "Variable",
                                                             label = "Select a Variable:",
                                                             choices = c("Nitrogen", 
                                                                         "Isotopic Nitrogen", 
                                                                         "Coral Bleaching", 
                                                                         "Predicted Sewage"),
                                                             multiple = FALSE), 
                                   shinyWidgets::pickerInput(inputId = "Other",
                                                             label = "Select an Add on:",
                                                             choices = c("LTER Sites", 
                                                                         "Observations"),
                                                             multiple = TRUE),
                                   
                                   #---- 
                                   #leaflet map outputs
                                   
                                   leafletOutput(outputId = "leaflet_layers")),
                          
                        
                          
                          tabPanel("Metadata")), 
               navbarMenu("Temporal",
                          tabPanel("Figures"),
                          tabPanel("Metadata")), 
               tabPanel("Data")
    )
)




# Server ----
# Runs the r code to make the visualizations and transform the data for your app to function
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$leaflet_layers <- renderLeaflet({
        leaflet()
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
