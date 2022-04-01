##############################
# install packages
##############################

library(shiny)
library(tidyverse)
library(kableExtra)
library(RColorBrewer)
library(leaflet)
library(here)
library(shinythemes)

##############################
# load data
##############################

# complete district data with enrollment by district (includes polygons and lat long)
COUNTY_INCOME_DATA <- st_read(here("CA_schools_app", "COUNTY_INCOME_DATA.shp"))


# complete district data with enrollment by district (includes polygons and lat long)
DISTRICT_DATA <- st_read(here("CA_schools_app", "DISTRICT_DATA.shp"))

# tri-county enrollment data
TRI_COUNTY <- read_csv(here("CA_schools_app", "sc_en_tri.csv")) %>% 
  rename(gender_uncap = gender) %>% 
  mutate(gender = case_when(
    gender_uncap == "female" ~ "Female",
    gender_uncap == "male" ~ "Male"))

##############################
# define server logic
##############################

shinyServer(function(input, output, session) {
  
  # -----------Tab 2 (Map)----------- #
  output$CA_Map <- renderLeaflet({
    
  # static map components 
  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>% #OpenStreetMap
    addPolygons(data = DISTRICT_DATA, fillOpacity = 0.05, weight = 1, color = "blue") %>% 
    addPolygons(data = COUNTY_INCOME_DATA, fillOpacity = 0.05, weight = 2.5, color = "black") %>% 
    setView(lng = -119.4179,
            lat = 36.7783,
            zoom = 6)
  })
  
  # interactive map components
  proxy <- leafletProxy("CA_Map") 
  
  observe({
    if(input$county!="") {
      
      # get all the county information
      county_polygon <- subset(COUNTY_INCOME_DATA, COUNTY_INCOME_DATA$NAME == input$county)
      county_latitude <- county_polygon$Latitud
      county_longitude <- county_polygon$Longitd
      county_population <- county_polygon$Popultn
      county_income <- county_polygon$MdFmlyI

      # -- Map Stuff & Outputs-- #
      # remove any previously highlighted polygon
      proxy %>% clearGroup("highlighted_county_polygon")
      # center the view on the county polygon
      proxy %>% setView(lng = county_longitude, lat = county_latitude, zoom = 7)
      # add slightly thicker yellow polygon on top of the selected one
      proxy %>% addPolylines(stroke = TRUE, weight = 4, color="yellow", data = county_polygon, group = "highlighted_county_polygon")

      # county info table which filters population and median income info by the user-selected county
      output$county_table <- function() {
        county_data <- as.data.frame(COUNTY_INCOME_DATA) %>%
        dplyr::filter(NAME == input$county) %>%
        dplyr::select(NAME, Popultn, MdFmlyI)

        # make county table nice with kable
        county_table <- county_data %>%
          knitr::kable(format = "html", col.names = c("County", "Population", "Median Family Income ($)")) %>%
          kable_styling(bootstrap_options = c("striped", "bordered")) %>%
          add_header_above(c("County Data" = 3))  # background = "skyblue"
      }
    }
  })
  
  observe({
    if(input$district!=""){
      
      # get all the district information
      district_polygon <- subset(DISTRICT_DATA, DISTRICT_DATA$DISTRIC == input$district) 
      district_latitude <- district_polygon$Latitud
      district_longitude <- district_polygon$Longitd
      district_enrollment <- district_polygon$totl_nr
      district_lunches <- district_polygon$prc_lnc
      district_requirement <- district_polygon$prc_rqr

      # -- Map Stuff & Outputs-- #
      # remove any previously highlighted polygon
      proxy %>% clearGroup("highlighted_district_polygon") 
      # center the view on the county polygon
      proxy %>% setView(lng = district_longitude, lat = district_latitude, zoom = 7)
      # add a slightly thicker red polygon on top of the selected one
      proxy %>% addPolylines(stroke = TRUE, weight = 4, color="red", data = district_polygon, group = "highlighted_district_polygon")
    
    # district info table which filters enrollment, %FRMP, %meeting UC requirements info by the user-selected district  
     output$district_table <- function() {
        district_data <- as.data.frame(DISTRICT_DATA) %>% 
        dplyr::filter(DISTRIC == input$district) %>%
        dplyr::select(DISTRIC, totl_nr, prc_lnc, prc_rqr)

        # make district table nice with kable
        district_table <- district_data %>%
         knitr::kable(format = "html", col.names = c("District", "Total Enrollment", "Students Qualified for FRMP (%)", "Graduates Meeting UC/CSU Requirements (%)")) %>%
         kable_styling(bootstrap_options = c("striped", "bordered")) %>%
         add_header_above(c("District Data" = 4))
      }
    }
  })
  
  # -----------Tab 3 (School Demographics)----------- #
  # first select box, pick a county
  observe({
    updateSelectInput(session, 
                      "county2", 
                      choices = unique(TRI_COUNTY$COUNTY))
  })
  
  # second select box, pick a district
  observe({
    updateSelectInput(session,
                      "district2",
                      choices = TRI_COUNTY %>%
                        filter(COUNTY == input$county2) %>%
                        dplyr::select(DISTRICT) %>%
                        .[[1]])
  })
  
  # third select box, pick a school
  observe({
    updateSelectInput(session,
                      "school",
                      choices = TRI_COUNTY %>%
                        filter(DISTRICT == input$district2) %>%
                        dplyr::select(SCHOOL) %>%
                        .[[1]])
  
  })
  
  # fourth select box, pick a grade
  observe({
    updateSelectInput(session,
                      "grades",
                      choices = TRI_COUNTY %>%
                        filter(SCHOOL == input$school &
                                 grade_full != "NA") %>%
                        dplyr::select(grade_full) %>%
                        .[[1]])
  
  })
  
  # render bar plot of demographic data
  output$column_plot <- renderPlot({
    
    # wrangle data; filter information by user-selected school
    school_data <- TRI_COUNTY %>%
      filter(SCHOOL == input$school,
             race_eth_name != "NA") %>% 
      arrange(race_eth_name) %>%
      group_by(gender, race_eth_name) %>%
      summarize(total = sum(students))
  
    school_data$race_eth_name = str_wrap(school_data$race_eth_name, width = 11)

  # ggplot column plot of total district enrollment by race, faceted by gender
    ggplot(school_data, aes(x = reorder(race_eth_name, -total), y = total)) +
      geom_bar(stat = "identity",
               position = "dodge",
               aes(fill = race_eth_name),
               show.legend = FALSE) +
      scale_y_continuous(expand = c(0,0), 
                         limits = c(0, max(school_data$total) + max(school_data$total)*0.05)) +
      scale_fill_brewer(palette = "Spectral") +
      geom_text(aes(label = total),
                position = position_dodge(width = 0.9), vjust = -0.25) +
      facet_wrap(~gender) +
      theme_bw() +
      theme(strip.background = element_rect(fill = "skyblue")) +
      theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid = element_line(color = "white")) +
      labs(x = "Race",
           y = "Total students enrolled")
  
  })
  
  # female student table which filters enrollment by user-selected grade
  output$female_grade_table <- function() {
    female <- TRI_COUNTY %>%
      filter(SCHOOL == input$school &
               grade_full == input$grades &
               gender == "Female") %>%
      dplyr::select(race_eth_name, students) %>% 
      arrange(-students)

    # make female table nice with kable  
    female_grade_table <- female %>%
      knitr::kable("html",
                   col.names = c("Race", "Number of Students")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered")) %>%
      add_header_above(c("Female" = 2)) #background = "skyblue"
  }
  
  # male student table which filters enrollment by user-selected grade
  output$male_grade_table <- function() {
    male <- TRI_COUNTY %>%
      filter(SCHOOL == input$school &
               grade_full == input$grades &
               gender == "Male") %>% 
      select(race_eth_name, students) %>% 
      arrange(-students)
    
    # make male table nice with kable
    male_grade_table <- male %>% 
      knitr::kable("html",
                   col.names = c("Race", "Number of Students")) %>% 
      kable_styling(bootstrap_options = c("striped", "bordered")) %>%
      add_header_above(c("Male" = 2)) # background = "skyblue")
  }
})

