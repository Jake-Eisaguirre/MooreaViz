
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




source(here("ShinyAppMooreaViz", "global.R"))




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
      geom_point(aes(color = site)) +
      geom_line(aes(group = site, color = site)) +
      facet_wrap(~site) +
    labs(title = 'INSERT TITLE',
         subtitle = 'Moorea, French Polynesia (2005 - 2018)',
         # y = 'Density (count/m^2)', # use case_when() to designate label based on user input of variable?
         y = case_when(input$Temp_Variable == "mean_coral_cover" ~ "coral axis",
                       input$Temp_Variable == "mean_algae_cover" ~ "algae axis",
                       input$Temp_Variable == "mean_biomass_p_consumers" ~ "fish axis",
                       input$Temp_Variable == "cots_density" ~ "cots axis"),
         x = 'Year',
         color = 'Site',
         title = case_when(input$Temp_Variable == "mean_coral_cover" ~ "coral title",
                           input$Temp_Variable == "mean_algae_cover" ~ "algae title",
                           input$Temp_Variable == "mean_biomass_p_consumers" ~ "fish title",
                           input$Temp_Variable == "cots_density" ~ "cots title")) +
    scale_color_manual(values = c('#40B5AD', '#87CEEB', '#4682B4', '#6F8FAF', '#9FE2BF', '#6495ED')) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size = 14),
          plot.title = element_text(size = 16))
  })
  

  
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
      plot_layout(guides = 'collect') + # combines the legends
      plot_layout(ncol = 1, heights = c(1, 1, 1, 1))
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

