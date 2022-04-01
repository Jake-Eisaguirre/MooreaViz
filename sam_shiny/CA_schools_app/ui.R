##############################
# install packages
##############################

library(tidyverse)
library(shiny)
library(shinythemes)
library(sf)
library(leaflet)

##############################
# build ui
##############################

 # Define UI for application and choose theme
 shinyUI(fluidPage(
   theme = shinytheme("flatly"),

   # Application title
   titlePanel("Assessing the need for academic outreach initiatives across California school districts"),
   
   tabsetPanel(
     
        # -----------Tab 1 (Instructions)----------- #
         tabPanel("Instructions",
           mainPanel(
             
             # importance/motivation for the creation of this app
             h3("Academic outreach in California"),
             p("Academic outreach programs are often recognized as important initiatives for improving the rates of student retention (Quigley & Leon 2003) as well as increasing the percentage of students from underrepresented groups that advance through the academic pipeline (Cooper et al. 2002, Gullatt & Jan 2003, Loza 2003). The University of California's Student Academic Preparation and Educational Partnership (SAPEP) is just one example and comprises a variety of programs to prepare CA students for postsecondary education, including students from socioeconomically disadvantaged backgrounds."),
	     p("Continued assessment of student status is critical for identifying and implementing such programs throughout the state. This application is intended to be used as a tool for outreach coordinators in CA to target districts and schools that could effectively utilize additional or continued education outreach program support to advance the academic success of their students. We explore economic and demographic data across CA school districts and present information that may be useful for developing models to predict college preparedness based on these socioeconomic variables."),
	     
	     # 23 billion justification
	     h3("Data justification"),
	     p("We choose to use data on recipients of free/reduced lunch benefits and racial demographics of school districts because these metrics encompass social inequality in a fundamental way: poor communities of color receive less federal funding for their school districts than rich, white communities (see", tags$a(href = "https://edbuild.org/content/23-billion", "'23 Billion'"), "). In California, non-white school districts receive on average ~$2400 less/student than white districts, and poor non-white districts receive ~$4000 less/student than poor white districts. This funding chasm is even more monumental when considering that non-white districts on average serve 3x the national average of students enrolled in any given district (~11000 students, compared to ~3500), while white districts on average serve ~1400 students. Therefore, outreach initiatives are most effective when working to serve school districts that lack federal funding to enrich curricula and serve the greatest proportion of students in the community."),
	     
	     # terms and definitions that will be useful for understanding variables considered in this app
	     h3("Terms and definitions"),
	     p(strong("Free & Reduced Meal Program (FRMP):"), "Under the FRMP, which is subsidized by the National School Lunch program, students can receive free or reduced-price breakfast and lunch.", tags$a(href = "https://www.cde.ca.gov/ls/nu/rs/scales1819.asp", "Eligibility"), "is based on family income."),
	     p(strong("Graduates Meeting UC/CSU Requirements:"), "The California Department of Education (CA DoE)", tags$a(href = "https://www.cde.ca.gov/ds/sd/sd/fsgradaf09.asp", "defines"), "students meeting UC/CSU requirements as twelfth-grade graduates who have completed all required courses for entry into the UC/CSU system with a grade of C or higher."),
	     
	     # links to data sources
	     h3("Data sources"),
	     p("Education data is made publically available by the CA CoE. Original data can be accessed using the links below."),
	     tags$a(href = "https://www.cde.ca.gov/ds/sd/sd/filesenr.asp", "Enrollment by School"),
	     br(),
	     tags$a(href = "https://www.cde.ca.gov/ds/sd/sd/filessp.asp", "Free or Reduced Meal Program (FRMP)"),
	     br(),
	     tags$a(href = "https://www.cde.ca.gov/ds/sd/sd/filesgradaf.asp", "Graduates Meeting UC/CSU Requirements"),
	     br(),
	     br(),
	     p("Population and income data is available through the United States Census Bureau."),
	     tags$a(href = "https://www.census.gov/en.html", "Population & Income by County"),
	     br(),
	     br(),
	     p("Spatial data is available from CA.gov and the United States Census Bureau."),
	     tags$a(href = "https://data.ca.gov/dataset/ca-geographic-boundaries", "CA County Boundaries"),
	     br(),
	     tags$a(href = "https://www.census.gov/geo/maps-data/data/cbf/cbf_sd.html", "CA District Boundaries (Elementary, Secondary & Unified School Districts)"),
	     
	     # literature cited
	     h3("Literature"),
	     p("Cooper CR, Cooper Jr. RG, Azmitia M, Cavira G, Gullatt Y", strong("(2002)"), "Bridging multiple worlds: How African Americans and Latino youth in academic outreach programs navigate math pathways to college.", em("Applied Developmental Science."), "6:73-87."),
	     p("EdBuild", strong("(February 2019)"), "23 Billion. Report."),
	     p("Gullatt Y", strong("(2003)"), "How do pre-collegiate acadmic outreach programs impact college-going among underrepresented students?", em("Pathways to College Network.")),
	     p("Loza PP", strong("(2003)"), "A system at risk: College outreach programs and the educational neglect of underachieving latino high school students.", em("The Urban Review."), "35:43-57."),
	     p("Quigley DD & Leon S", strong("(2003)"), "The early academic outreach program (EAOP) and its impact on high school students' completion of the University of California's prepatory coursework.", em("CSE Tech Report 589."))
	     )
    ),
         
	      # -----------Tab 2 (Map)----------- #
         tabPanel("Population, Income & District Statistics",
                  h3("Statewide County and District Information"),
                  sidebarLayout(
                    # create sidebar panel for widgets
                    sidebarPanel(
                      # display text about selectInputs
                      helpText("Choose a county from the dropdown list below to highlight its location and display population & median family income."),
                      # select counties widget # https://shiny.rstudio.com/reference/shiny/1.2.0/selectInput.html
                      selectInput("county", label = "Select County", COUNTY_INCOME_DATA$NAME),
                      # display text about district selectInputs
                      helpText("Choose a district to highlight its location and display student enrollment, participation in the FRMP, and college preparedness."),
                      # select districts widget
                      selectInput("district", label = "Select District", DISTRICT_DATA$DISTRIC),
                      # note about missing data
                      p("*Note: Some information may be missing. If a county or district is not highlighted upon selection, spatial data is not currently available.")
                    ),
                    # create main panel for map to poplate
                    mainPanel(
                      column(8,
                        # create output for map
                        leafletOutput("CA_Map", width = 900, height = 700)
                      ),
                      # creat output for county and district info tables below map
                      fluidRow(
                        column(10, tableOutput("county_table")),
                        column(10, tableOutput("district_table"))
                      )
                    )
                  )),

# -----------Tab 3 (School Demographics)----------- #
tabPanel("School Demographics",
         h3("Tri-County (Ventura, Santa Barbara, San Luis Obispo) School Demographics"),
         sidebarLayout(
           sidebarPanel(
             helpText("Explore data on racial demographics for schools in the tri-county area below."),
             selectizeInput("county2","County", choices = unique(TRI_COUNTY$COUNTY)),
             selectInput("district2", "District", choices = ""),
             selectInput("school", "School", choices = ""),
             helpText("Select a school grade level for higher resolution demographic data by gender and race within schools."),
             selectInput("grades", "Grade", choices = "")
           ),
           mainPanel(
             plotOutput("column_plot"),
             fluidRow(
                column(6, tableOutput("female_grade_table")),
                column(6, tableOutput("male_grade_table"))
             )
           )
         )
)
)
))
