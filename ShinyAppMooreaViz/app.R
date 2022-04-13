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

sewage_2016 <- sewage_data %>% 
    dplyr::select(longitude, latitude, urb_nuts) %>% 
    na.omit()

#temporal data 
temporal_data <- read.csv(here("data/csv/temporal_data_joined.csv"))

#Select column bleach
bleaching_data <- bleach %>%
    dplyr::select(longitude, latitude, percent_bleached) %>%
    na.omit() %>% 
    group_by(longitude, latitude) %>% 
    summarise(percent_bleached = mean(percent_bleached))

#Tidy Nitrogen Data
n_data <- nitrogen_data %>% 
    mutate(percent_n_jan = percent_n_jan *100,
           percent_n_may = percent_n_may *100,
           percent_n_july = percent_n_july *100) %>% #turning them into %s 
    pivot_longer(!1:5, names_to = "type", values_to = "percent_n") %>% 
    separate(type, into = c("method","random", "date"), sep = "_") %>% 
    dplyr::select(-random)

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
# n_data <- nitrogen_data %>% 
#     mutate(percent_n_jan = percent_n_jan *100,
#            percent_n_may = percent_n_may *100,
#            percent_n_july = percent_n_july *100) %>% #turning them into %s 
#     pivot_longer(!1:5, names_to = "type", values_to = "percent_n") %>% 
#     separate(type, into = c("method","random", "date"), sep = "_") %>% 
#     dplyr::select(-random)

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

    # Application title ----
    titlePanel("Moorea Coral Reef LTER"),
    sidebarLayout(
        sidebarPanel(),
        mainPanel(img(src = "mcr_logo.png", height = 60, width = 150, align = "right"), 
                  img(src = "lter_logo.png", height = 60, width = 70, align = "right"), 
                  img(src = "nsf_logo.png", height = 60, width = 60, align = "right"))),
    
# Navigatition bar ----
    navbarPage("App Title", 
               
               #home page ----
               tabPanel("Home", 
                        img(src = "mcr_logo.png", height = 60, width = 150, align = "center"), 
                        p("The Moorea Coral Reef (MCR) LTER site, established in 2004, is an interdisciplinary, landscape-scale program whose goal is to advance understanding of key mechanisms that modulate ecosystem processes and community structure of coral reefs through integrated research, education and outreach. Our site is the coral reef complex that encircles the 60 km perimeter of Moorea (17°30'S, 149°50'W), French Polynesia."), 
                
                        p("A fundamental goal of the the Moorea Coral Reef (MCR) LTER site is to advance understanding that enables accurate forecasts of the behavior of coral reef ecosystems to environmental forcing. To this end we seek to understand the mechanistic basis of change in coral reefs by: (i) elucidating major controls over reef dynamics, and (ii) determining how they are influenced by the major pulse disturbances (e.g., cyclones, coral bleaching, coral predator outbreaks) and local press drivers (e.g., fishing, nutrient enrichment) to which they are increasingly being subjected, against a background of slowly changing environmental drivers associated with global climate change and ocean acidification.")),

               
         
               
               #spatial page ----

               navbarMenu("Spatial",
                          
                          #spatial map ----
                          tabPanel(title = "Map",
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
                                                                         "Isotopic Nitrogen"),
                                                             width = 80), 
                                       checkboxGroupButtons(inputId = "Additional",
                                                            label = "Select Aditional Layer",
                                                            choices = c("Percent Coral Bleached", 
                                                                        "Predicted Sewage",
                                                                        "Bathymetry"),
                                                            width = 80),
                                       checkboxGroupButtons(inputId = "Other",
                                                             label = "Select an Add on:",
                                                             choices = c("LTER Sites", 
                                                                         "Observations"),
                                                             width = 80)),
                                   mainPanel(leafletOutput(outputId = "leaflet_base", 
                                                           width = 900,
                                                           height = 500))),
                          
                          
                        
                          #spatila metadata ----
                          tabPanel("Metadata")), 
              
               #Temporal page ----
               navbarMenu("Temporal",
                          
                          #figures by variable panel ----
                          tabPanel("Figures by Variable",
                                   (pickerInput(inputId = "Temp_Variable",
                                               label = "Select a Variable",
                                               # choices = c("Crown of Thorns", 
                                               #             "Coral Cover", 
                                               #             "Fish Biomass", 
                                               #             "Algae"),
                                               choices = c("Mean Coral Cover" = "mean_coral_cover",
                                                           "Mean Algae Cover" = "mean_algae_cover",
                                                           "Mean Fish Biomass" = "mean_biomass_p_consumers",
                                                           "COTS Density" = "cots_density"), 
                                               multiple = FALSE)),
                                   plotOutput(outputId = "faceted_plot")),
                                   
                          #figures by site panel ----        
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
                          #temporal metadata ----
                          tabPanel("Metadata")), 
                         
    )
)




# Server ----
# Runs the r code to make the visualizations and transform the data for your app to function
# Define server logic required to draw a histogram
server <- function(input, output, session) {

    #leaflet outputs ----
    output$leaflet_base <- renderLeaflet({
        
        #base map
        leaflet(crs) %>% 
            addProviderTiles("Esri.WorldImagery") %>% 
            setView(-149.829529, -17.538843, zoom = 11.5)
            
    })
    
    # temporal_reactive_df_variables <- reactive({
    #     
    #     temporal_data %>% 
    #         dplyr::select(year, site, input$Variable)
    # }) 
    
    output$faceted_plot <- renderPlot({
        ggplot(data = temporal_data, aes_string(x = "year", y = input$Temp_Variable)) +
            geom_point(aes(color = site)) # +
            # geom_line(aes(group = site, color = site)) # +
            # facet_wrap(~site) +
            # labs(title = 'INSERT TITLE',
            #      subtitle = 'Moorea, French Polynesia (2005 - 2018)',
            #      y = 'Density (count/m^2)',
            #      x = 'Year',
            #      color = 'Site') +
            # scale_color_manual(values = c('#40B5AD', '#87CEEB', '#4682B4', '#6F8FAF', '#9FE2BF', '#6495ED')) +
            # theme_bw() +
            # theme(axis.text.x = element_text(angle = 90, hjust = 1),
            #       panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
            #       panel.grid.minor.y = element_blank(),
            #       axis.title.x = element_text(size=14),
            #       axis.title.y = element_text(size = 14),
            #       plot.title = element_text(size = 16))
        })


    # output$faceted_plot <- renderPlot({
    #     ggplot(data = temporal_data, aes(x = year, y = cots_density)) +
    #         geom_point(aes(color = site)) +
    #         geom_line(aes(group = site, color = site)) +
    #         facet_wrap(~site) +
    #         labs(title = 'Crown of Thorns Sea Stars - Annual Site Densities',
    #              subtitle = 'Moorea, French Polynesia (2005 - 2018)',
    #              y = 'Density (count/m^2)',
    #              x = 'Year',
    #              color = 'Site') +
    #         scale_color_manual(values = c('#40B5AD', '#87CEEB', '#4682B4', '#6F8FAF', '#9FE2BF', '#6495ED')) +
    #         theme_bw() +
    #         theme(axis.text.x = element_text(angle = 90, hjust = 1),
    #               panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    #               panel.grid.minor.y = element_blank(),
    #               axis.title.x = element_text(size=14),
    #               axis.title.y = element_text(size = 14),
    #               plot.title = element_text(size = 16))
    #     
        
        # # NOT SURE WHY THE BELOW IF STATEMENTS DO NOT WORK TO SWITCH THE PLOTS BASED ON INPUT
        # if (input$Variable == "Crown of Thorns"){
        #     p <- ggplot(data = temporal_data, aes(x = year, y = cots_density)) +
        #         geom_point(aes(color = site))
        # } else if (input$Variable == "Coral Cover"){
        #     p <- ggplot(data = temporal_data, aes(x = year, y = mean_coral_cover)) +
        #         geom_point(aes(color = site))
        # } else if (input$Variable == "Fish Biomass"){
        #     p <- ggplot(data = temporal_data, aes(x = year, y = mean_biomass_p_consumers)) +
        #         geom_point(aes(color = site))
        # }
        # else (input$Variable == "Algae"){
        #     p <- ggplot(data = temporal_data, aes(x = year, y = mean_algae_cover)) +
        #         geom_point(aes(color = site))
        # }
        # 
        # print(p)

 #       })


    #temporal outputs ----
    temporal_reactive_df <- reactive({validate(
        need(length(input$site) > 0, "Please select at least one site to visualize.")
    )
        temporal_data %>%
            filter(site %in% input$site)
    }) 
    

    #variables by site outputs ----

    output$variables_by_site_plot <- renderPlot({
        coral_plot <- ggplot(data = temporal_reactive_df(), aes(x = year, y = mean_coral_cover)) +
            geom_point(aes(color = site)) +

            geom_line(aes(group = site, color = site)) +
            labs(x = "",
                 y = expression(atop("Mean Coral Cover", paste(paste("(% per 0.25 ", m^{2}, ")"))))) 
        
        cots_plot <- ggplot(data = temporal_reactive_df(), aes(x = year, y = cots_density)) +
            geom_point(aes(color = site)) +
            geom_line(aes(group = site, color = site)) +
            labs(x = "",
                 y = expression(atop("COTS Density", paste(paste("(Count per ", m^{2}, ")")))))
        
        biomass_plot <- ggplot(data = temporal_reactive_df(), aes(x = year, y = mean_biomass_p_consumers)) +
            geom_point(aes(color = site)) +
            geom_line(aes(group = site, color = site)) +
            labs(x = "",
                 y = expression(atop("Mean Fish Biomass", paste(paste("(% per 0.25 ", m^{2}, ")")))))
        
        algae_plot <- ggplot(data = temporal_reactive_df(), aes(x = year, y = mean_algae_cover)) +
            geom_point(aes(color = site)) +
            geom_line(aes(group = site, color = site)) +
            labs(x = "Year",
                 y = expression(atop("Mean Algae Cover", paste(paste("(% per 0.25 ", m^{2}, ")")))))
        
        coral_plot/cots_plot/biomass_plot/algae_plot +
            plot_layout(guides = 'collect') # combines the legends 
            # plot_layout(heights = unit(c(3.5, 3.5, 3.5, 3.5), c('cm', 'null'))) 
        })
    
    # reactive observations and data filtering
    Observations <- eventReactive(input$Other, {

            geom_line(aes(group = site, color = site))
    })
   
  
    # reactive observations and data filtering ----
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
        
        else if (!is.null(input$Other) && input$Other == "LTER Sites") { 
            proxy %>% 
                addPolylines(data = polgyons(), lng = ~longitude, lat = ~latitude, group = "LTER Sites",
                             popup = ~site )}
       
         else {
            proxy %>% clearGroup("LTER Sites") %>%  clearGroup("Observations")
        } 
        


    }, ignoreNULL = F)
    
    
    
    
    # reactive jan n %
  
    jan_n <- reactive({
        
        spatial_brick[[1]]
    })
    
    
    # reactive may n %
    may_n <- reactive({
        
        spatial_brick[[2]]
    })
    
    # reactive july n %
    july_n <- reactive({
        
        spatial_brick[[3]]
    })
    
    # reactive jan n i
    
    jan_n_i <- reactive({
        
        spatial_brick[[4]]
    })
    
    
    # reactive may n i
    may_n_i <- reactive({
        
        spatial_brick[[5]]
    })
    
    # reactive july n i
    july_n_i <- reactive({
        
        spatial_brick[[6]]
    })
    

    #sync button
    proxy <- leafletProxy("leaflet_base", session)
    
        observeEvent({
            c(input$Month, input$Variable)},
            {
                
                if(!is.null(input$Month) && !is.null(input$Variable) && input$Month == "January" 
                   && input$Variable == "Percent Nitrogen"){
                    proxy  %>% 
                        addRasterImage(jan_n(), colors = "plasma", group = "January N", opacity = 0.7, 
                                       layerId = "January")
                }
                
                else if(!is.null(input$Month) && !is.null(input$Variable) && input$Month == "January" 
                   && input$Variable == "Isotopic Nitrogen"){
                    proxy  %>% 
                        addRasterImage(jan_n_i(), colors = "plasma", group = "January N", opacity = 0.7, 
                                       layerId = "January")
                }
                    
                else if(!is.null(input$Month) && !is.null(input$Variable) && input$Month == "May" 
                        && input$Variable == "Percent Nitrogen"){
                    proxy %>% 
                        addRasterImage(may_n(), colors = "plasma", group = "May N", opacity = 0.7, 
                                       layerId = "May")
                }
                
                else if(!is.null(input$Month) && !is.null(input$Variable) && input$Month == "May" 
                        && input$Variable == "Isotopic Nitrogen"){
                    proxy %>% 
                        addRasterImage(may_n_i(), colors = "plasma", group = "May N", opacity = 0.7, 
                                       layerId = "May")
                }
                else if (!is.null(input$Month) && !is.null(input$Variable) && input$Month == "July"
                         && input$Variable == "Percent Nitrogen"){ 
                    
                    proxy %>% addRasterImage(july_n(), colors = "plasma", group = "July N", opacity = 0.7, 
                                             layerId = "July")
                }
                
                else if(!is.null(input$Month) && !is.null(input$Variable) && input$Month == "July" 
                        && input$Variable == "Isotopic Nitrogen"){
                    proxy %>% 
                        addRasterImage(july_n_i(), colors = "plasma", group = "May N", opacity = 0.7, 
                                       layerId = "July")
                }
                
                
                else {
                    proxy %>%  clearImages()
                }
            }, ignoreNULL = F)               
   
    # reactive coral belach
    bleach <- reactive({
        
        spatial_brick[[7]]
    })

    # reactive sewage
    sewage <- reactive({
        
        spatial_brick[[8]]
    })
   
    # reactive lidar
    bathy <- reactive({
        
        spatial_brick[[9]]
    })
    
    
    #sync button coral bleach
    observeEvent({
        input$Additional},
        {
            if(!is.null(input$Additional) && input$Additional == "Percent Coral Bleached" ){
                
                proxy  %>% addRasterImage(bleach(), colors = "plasma", group = "Percent Coral Bleached",
                                          opacity = 0.7, layerId = "Percent Coral Bleached")}
            else if (!is.null(input$Additional) && input$Additional == "Predicted Sewage" ){
                
                proxy  %>% addRasterImage(sewage(), colors = "plasma", group = "Predicted Sewage",
                                          opacity = 0.7, layerId = "Predicted Sewage")}
            else if (!is.null(input$Additional) && input$Additional == "Bathymetry" ){
                
                proxy  %>% addRasterImage(bathy(), colors = "plasma", group = "Bathymetry",
                                          opacity = 0.7, layerId = "Bathymetry")}
            
            else {
                proxy %>% clearImages()
            }
        }, ignoreNULL = F)
    

}

# Run the application 
shinyApp(ui = ui, server = server)
