#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import sf
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import leaflet
#' @import shinythemes
#' @import plotly
#' @noRd



app_ui <- function(request) {

  # forest_data <- read_sf(paste0(here::here(), "/data-raw/MenYah_Veg_Dissovled/Mendotat_Yahara_Polygons_dissolved.shp")) %>%
  #   st_transform(4326)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = shinytheme("yeti"),

      #titlePanel("Mendota & Yahara Forest Change"),

      #project description

        HTML('
               <h2>Mendota/Yahara Conservation Priority Pilot Project </h2>
               <br>
               <p>Forests are important carbon sinks: trees and soil absorb and store carbon which would otherwise reach the atmosphere.
               When a forest is lost, much of this stored carbon is released. Protecting existing forest land is thus a key strategy to mitigating climate change.</p>
               <p>The <a href="https://www.capitalarearpc.org/">Capital Area Regional Planning Commission</a>
               and the <a href ="https://www.urbantreealliance.org/1628460467349/what-we-do/"> Urban Tree Alliance </a>
               received funding from the <a href = "https://bockfoundation.com/"> John C. Bock Foundation </a> in 2023 to map historical forest loss in the Mendota and Yahara watersheds.
               The Bock Foundation was established to preserve and protect Wisconsin’s old growth forests, particularly in the Lake Mendota watershed. </p>
              <p> This project used aerial photography and satellite imagery to track forest change from 1937 to 2022.
              The results show a concerning decline of nearly 25% of old growth forest during this time, primarily from loss to agricultural conversion.
              We hope to expand this project in the future, both to understand the changing landscape of our county and to protect remaining old growth forest. </p>
               <p> <a href="https://www.capitalarearpc.org/category/tree-canopy/"> Learn about ongoing work to map and inventory the regional tree canopy at CARPC. </a></p>
               <br>
               <p>Use the year slider below to see how the forest in the Mendota/Yahara area has changed.
               The "Map" tab shows geographical information - you can zoom and pan around the map.
               The "Plot" tab shows a graphical summary of the same data.
               <!-- If you are a property owner in this area, you may qualify for a conservation easement, which provides you payment
               in exchange for your help in protecting valuable forest. Click on the "Easement" tab for more information. --> </p>
               <br>
              '),

          sliderInput("year", "Year:",
                      min = 1940, max = 2022,
                      value = 1940, step = 10,
                      pre = "", sep = "",
                      dragRange = FALSE)
        ,
    tabsetPanel(
      tabPanel("Map", leafletOutput("mapl", height = "1000")),
      tabPanel("Plot",
               br(),
               plotlyOutput("cumulative_plot"),
               br(),
               plotlyOutput("change_plot"))
      # tabPanel("Easements",
      #          selectInput("address_search", "Address Search", choices = character(0)),
      #          textOutput("address_search"))
  )))
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Conservaton.Prioritization"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
