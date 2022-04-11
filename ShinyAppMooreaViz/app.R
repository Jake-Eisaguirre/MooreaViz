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
sewage_data <- read.csv(here("data/csv/Predicted_nuts.csv")) %>% 
    clean_names()

sewage_data <- cbind(nitrogen_data, sewage_data)

#raster brick minus lidar
spatial_brick <- here("data", "spatial_brick.nc")

spatial_brick <- brick(spatial_brick)

#crs 
crs <- 2976


# Tidy Nitrogen Data
n_data <- nitrogen_data %>% 
    mutate(percent_n_jan = percent_n_jan *100,
           percent_n_may = percent_n_may *100,
           percent_n_july = percent_n_july *100) %>% #turning them into %s 
    pivot_longer(!1:5, names_to = "type", values_to = "percent_n") %>% 
    separate(type, into = c("method","random", "date"), sep = "_") %>% 
    dplyr::select(-random)

#Pals
# percent nitrogen 
jan_data <- as.data.frame(rasterToPoints(spatial_brick[[1]]))
pal_jan <- colorNumeric(palette = viridis((25), option = "plasma"), domain = jan_data$var1.pred, reverse = TRUE)

may_data <- as.data.frame(rasterToPoints(spatial_brick[[2]]))
pal_may <- colorNumeric(palette = viridis((25), option = "plasma"), domain = may_data$var1.pred, reverse = TRUE)

july_data <- as.data.frame(rasterToPoints(spatial_brick[[3]]))
pal_july <- colorNumeric(palette = viridis((25), option = "plasma"), domain = july_data$var1.pred, reverse = TRUE)

# isotopic nitrogen 

jan_i_data <- as.data.frame(rasterToPoints(spatial_brick[[4]]))
pal_jan_i <- colorNumeric(palette = viridis((25), option = "plasma"), domain = jan_i_data$var1.pred, reverse = TRUE)

may_i_data <- as.data.frame(rasterToPoints(spatial_brick[[5]]))
pal_may_i <- colorNumeric(palette = viridis((25), option = "plasma"), domain = may_i_data$var1.pred, reverse = TRUE)

july_i_data <- as.data.frame(rasterToPoints(spatial_brick[[6]]))
pal_july_i <- colorNumeric(palette = viridis((25), option = "plasma"), domain = july_i_data$var1.pred, reverse = TRUE)

bleach_data <- as.data.frame(rasterToPoints(spatial_brick[[7]]))
pal_bleach <- colorNumeric(palette = viridis((25), option = "plasma"), domain = bleach_data$var1.pred, reverse = TRUE)

# sewage 
sew_dat <- as.data.frame(rasterToPoints(spatial_brick[[8]]))
pal_sewage <- colorNumeric(palette = viridis((25), option = "plasma"), domain = sewage_data$var1.pred, reverse = TRUE)

# lidar 
# bathy_df <- as.data.frame(rasterToPoints(bathy_raster_filtered))
# pal_bathy <- colorNumeric(palette = viridis((25), option = "plasma"), domain = bathy_df$layer, reverse = TRUE)

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
                                   
                                   sidebarPanel(pickerInput(inputId = "Year",
                                               label = "Select a Year:",
                                               choices = c("2016",
                                                           "2017",
                                                           "2018"),
                                                multiple = FALSE,
                                               width = 80),
                                   shinyWidgets::pickerInput(inputId = "Month",
                                                             label = "Select a Month:",
                                                             choices = c("January", 
                                                                         "July", 
                                                                         "May"),
                                                             multiple = FALSE,
                                                             width = 80), 
                                   shinyWidgets::pickerInput(inputId = "Variable",
                                                             label = "Select a Variable:",
                                                             choices = c("Nitrogen", 
                                                                         "Isotopic Nitrogen", 
                                                                         "Coral Bleaching", 
                                                                         "Predicted Sewage"),
                                                             multiple = FALSE,
                                                             width = 80), 
                                   shinyWidgets::pickerInput(inputId = "Other",
                                                             label = "Select an Add on:",
                                                             choices = c("LTER Sites", 
                                                                         "Observations"),
                                                             multiple = TRUE,
                                                             width = 80)),
                                   mainPanel(leafletOutput(outputId = "leaflet_base"))),
                          
                          
                        
                          
                          tabPanel("Metadata")), 
               
               
               
               navbarMenu("Temporal",
                          tabPanel("Figures"),
                          tabPanel("Metadata", 
                          "This is where the metadata goes")), 
               
               
              
    )
)




# Server ----
# Runs the r code to make the visualizations and transform the data for your app to function
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$leaflet_base <- renderLeaflet({
        
        #base map
        leaflet(crs) %>% 
            addProviderTiles("Esri.WorldImagery") %>% 
            setView(-149.829529, -17.538843, zoom = 11.5)
            
    })
    
    #interactive map components
    proxy <- leafletProxy("leaflet_base")
    
    observe({
        proxy %>% addCircleMarkers(lng = n_data$longitude, lat = n_data$latitude,
                                   color = "black", group = "Observations", radius = 1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
