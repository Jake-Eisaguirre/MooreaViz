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
sewage_data <- read.csv(here("Data/csv/Predicted_nuts.csv")) %>% 
    clean_names()

sewage_data <- cbind(nitrogen_data, sewage_data)

temporal_data <- read.csv(here("data/csv/temporal_data_joined.csv"))


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
                          tabPanel("Figures by Variable",
                                   (pickerInput(inputId = "Variable",
                                               label = "Select a Variable",
                                               choices = c("Crown of Thorns", 
                                                           "Coral Cover", 
                                                           "Fish Biomass", 
                                                           "Algae"),
                                               multiple = FALSE)),
                                   plotOutput(outputId = "faceted_plot")),
                                   
                                   
                          tabPanel("Figures by Site",
                                   sidebarPanel(checkboxGroupInput(inputId = "site", 
                                                                   label = h4("Choose your Site"),
                                                                   selected = "LTER 1",
                                                                   choices = list("Site 1" = "LTER 1",
                                                                     "Site 2" = "LTER 2", 
                                                                     "Site 3" = "LTER 3", 
                                                                     "Site 4" = "LTER 4", 
                                                                     "Site 5" = "LTER 5", 
                                                                     "Site 6" = "LTER 6"))),
                                   mainPanel(plotOutput(outputId = "variables_by_site_plot"))),
                          
                          tabPanel("Metadata")), 
                          
                          tabPanel("Data")
    )
)

# plots 
# faceted COTS plot
cots_facet <- ggplot(data = temporal_data, aes(x = year, y = cots_density)) +
    geom_point(aes(color = site)) # testing with basic plot


# Server ----
# Runs the r code to make the visualizations and transform the data for your app to function
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$leaflet_layers <- renderLeaflet({
        leaflet()
            
    })
    
    output$faceted_plot <- renderPlot({
        ggplot(data = temporal_data, aes(x = year, y = cots_density)) +
            geom_point(aes(color = site)) +
            geom_line(aes(group = site, color = site)) +
            facet_wrap(~site) +
            labs(title = 'Crown of Thorns Sea Stars - Annual Site Densities',
                 subtitle = 'Moorea, French Polynesia (2005 - 2018)',
                 y = 'Density (count/m^2)',
                 x = 'Year',
                 color = 'Site') +
            scale_color_manual(values = c('#40B5AD', '#87CEEB', '#4682B4', '#6F8FAF', '#9FE2BF', '#6495ED')) +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  axis.title.x = element_text(size=14),
                  axis.title.y = element_text(size = 14),
                  plot.title = element_text(size = 16))
        
        
        # # NOT SURE WHY THE BELOW IF STATEMENTS DO NOT WORK TO SWITCH THE PLOTS BASED ON INPUT
        # if (input$Variable == "Crown of Thorns"){
        #     p <- ggplot(data = temporal_data, aes(x = year, y = cots_density)) +
        #         geom_point(aes(color = site))
        #     }
        # if (input$Variable == "Coral Cover"){
        #     p <- ggplot(data = temporal_data, aes(x = year, y = mean_coral_cover)) +
        #         geom_point(aes(color = site))
        # }
        # if (input$Variable == "Fish Biomass"){
        #     p <- ggplot(data = temporal_data, aes(x = year, y = mean_biomass_p_consumers)) +
        #         geom_point(aes(color = site))
        # }
        # if (input$Variable == "Algae"){
        #     p <- ggplot(data = temporal_data, aes(x = year, y = mean_algae_cover)) +
        #         geom_point(aes(color = site))
        # }
        # 
        # print(p)

        })

    temporal_reactive_df <- reactive({validate(
        need(length(input$site) > 0, "Please select at least one site to visualize.")
    )
        temporal_data %>% 
            filter(site %in% input$site)
    }) 
    
    output$variables_by_site_plot <- renderPlot({
        # insert plot here 
        ggplot(na.omit(temporal_reactive_df()), aes(x = year, y = mean_coral_cover)) +
            geom_point(aes(color = site)) +
            geom_line(aes(group = site, color = site))
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
