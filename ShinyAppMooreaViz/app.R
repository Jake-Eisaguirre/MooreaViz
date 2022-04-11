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

#site polygons
site_poly <- read_csv(here("data", "csv", "site_poly.csv"))

#sewage data

sewage_data <- read.csv(here("data/csv/Predicted_nuts.csv")) %>% 
    clean_names()

sewage_data <- cbind(nitrogen_data, sewage_data)


temporal_data <- read.csv(here("data/csv/temporal_data_joined.csv"))


#sewage
sewage_2016 <- sewage_data %>% 
    dplyr::select(longitude, latitude, urb_nuts) %>% 
    na.omit()
#Select column bleach
bleaching_data <- bleach %>%
    dplyr::select(longitude, latitude, percent_bleached) %>%
    na.omit() %>% 
    group_by(longitude, latitude) %>% 
    summarise(percent_bleached = mean(percent_bleached))
# selecting n-july data
july_ni_data <- n_data %>% 
    filter(date == "july", method == "d15n") %>%
    dplyr::select(longitude, latitude, percent_n) %>% 
    na.omit()
# selecting n-may data
may_ni_data <- n_data %>% 
    filter(date == "may", method == "d15n") %>%
    dplyr::select(longitude, latitude, percent_n) %>% 
    na.omit()
# selecting n-jan data
jan_ni_data <- n_data %>% 
    filter(date == "jan", method == "d15n") %>%
    dplyr::select(longitude, latitude, percent_n) %>% 
    na.omit()
# selecting n-july data
july_np_data <- n_data %>% 
    filter(date == "july", method == "percent") %>%
    dplyr::select(longitude, latitude, percent_n) %>% 
    na.omit()
# selecting n-may data
may_np_data <- n_data %>% 
    filter(date == "may", method == "percent") %>%
    dplyr::select(longitude, latitude, percent_n) %>% 
    na.omit()
# selecting n-jan data
jan_np_data <- n_data %>% 
    filter(date == "jan", method == "percent") %>%
    dplyr::select(longitude, latitude, percent_n) %>% 
    na.omit()


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
                                   
                                   sidebarPanel(width = 2,
                                       # Code block for incorporating more years of data
                                       #pickerInput(inputId = "Year",
                                                   #label = "Select a Year:",
                                                   #choices = c("2016",
                                                               #"2017",
                                                               #"2018"),
                                                   #multiple = FALSE,
                                                   #width = 80),
                                       checkboxGroupButtons(inputId = "Month",
                                                             label = "Select a Month:",
                                                             choices = c("January", 
                                                                         "May", 
                                                                         "July"),
                                                             width = 80), 
                                       checkboxGroupButtons(inputId = "Variable",
                                                             label = "Select a Variable:",
                                                             choices = c("Percent Nitrogen", 
                                                                         "Isotopic Nitrogen", 
                                                                         "Percent Coral Bleached", 
                                                                         "Predicted Sewage"),
                                                             width = 80), 
                                       checkboxGroupButtons(inputId = "Other",
                                                             label = "Select an Add on:",
                                                             choices = c("LTER Sites", 
                                                                         "Observations"),
                                                             width = 80)),
                                   mainPanel(leafletOutput(outputId = "leaflet_base", 
                                                           width = 900,
                                                           height = 500))),
                          
                          
                        
                          
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
server <- function(input, output, session) {

    output$leaflet_base <- renderLeaflet({
        
        #base map
        leaflet(crs) %>% 
            addProviderTiles("Esri.WorldImagery") %>% 
            setView(-149.829529, -17.538843, zoom = 11.5)
            
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
   
    
    # reactive observations and data filtering
    Observations <- reactive({
        
        n_data %>% 
            dplyr::select(latitude, longitude)
    })
    
    
    # reactive polygons and data filtering
    polgyons <- reactive({
        
        site_poly %>% 
            group_by(site)
    })
    
    
    # observations and polygons reactive 

    observeEvent(input$Other, {
        proxy <- leafletProxy("leaflet_base")
        if (!is.null(input$Other) && input$Other == "Observations") { 
            proxy %>% addCircles(data = Observations(), color = "black", group = "Observations", radius = 3, opacity = 0.2,
                                 popup = paste("Longitude:", round(n_data$longitude, 4), "<br>", 
                                               "Latitude:", round(n_data$latitude, 4), "<br>",
                                               "January Percent N:", jan_np_data$percent_n, "%", "<br>",
                                               "May Percent N:", may_np_data$percent_n,"%", "<br>",
                                               "July Percent N:", july_np_data$percent_n,"%", "<br>",
                                               "January Isotopic N:", jan_ni_data$percent_n,"δ15N", "<br>", 
                                               "May Isotopic N:", may_ni_data$percent_n,"δ15N", "<br>",
                                               "July Isotopic N:", july_ni_data$percent_n,"δ15N", "<br>",
                                               "Percent Coral Bleached:", round(bleaching_data$percent_bleached, 2),"%", "<br>",
                                               "Predicted Sewage Index:", round(sewage_2016$urb_nuts, 4), "<br>"))}
        else {
            proxy %>% clearGroup("Observations")
        } 
        
        if (!is.null(input$Other) && input$Other == "LTER Sites") { 
            proxy %>% 
                addPolylines(data = polgyons(), lng = ~longitude, lat = ~latitude, group = "LTER Sites",
                                   popup = ~site )}
        else {
            proxy %>% clearGroup("LTER Sites")
        } 
    }, ignoreNULL = F)
    
    
    
    
    # reactive jan n
  
    jan_n <- reactive({
        
        spatial_brick[[1]]
    })
    
    
    # reactive may n
    may_n <- reactive({
        
        spatial_brick[[2]]
    })
    
    # reactive july n
    july_n <- reactive({
        
        spatial_brick[[3]]
    })
    

    #sync button
    proxy <- leafletProxy("leaflet_base", session)
    
        observeEvent({
            input$Month},
            {
                
                if(!is.null(input$Month) && input$Month == "January" ){
                    proxy  %>% 
                        addRasterImage(jan_n(), colors = "plasma", group = "January N", opacity = 0.7, 
                                              layerId = "January")}
                else if(!is.null(input$Month) && input$Month == "May" ){
                    proxy %>% clearImages() %>% 
                        addRasterImage(may_n(), colors = "plasma", group = "May N", opacity = 0.7, 
                                       layerId = "May")}
                else if (!is.null(input$Month) && input$Month == "July"){ 
                    
                    proxy %>% addRasterImage(july_n(), colors = "plasma", group = "July N", opacity = 0.7, 
                                             layerId = "July")}
                
                
                else {
                    proxy %>%  clearImages()
                }
            }, ignoreNULL = F)               
   
    # reactive coral belach
    bleach <- reactive({
        
        spatial_brick[[7]]
    })

   
    #sync button
    observeEvent({
        input$Variable},
        {
            proxy <- leafletProxy("leaflet_base") 
            if(!is.null(input$Variable) && input$Variable == "Percent Coral Bleached" ){
                proxy  %>% addRasterImage(bleach(), colors = "plasma", group = "Percent Coral Bleached",
                                          opacity = 0.7, layerId = "Percent Coral Bleached")}
            
            else {
                proxy %>% clearImages()
            }
        }, ignoreNULL = T)
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
