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
#' @import gghighlight
#' @import shinythemes
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
               <p>This project uses historical aerial photography and satellite imagery to track forest
               change from 1937 to 2022. This project was created by the
               <a href="https://www.capitalarearpc.org/">Capital Area Regional Planning Commission </a>.</p>
               <br>
               <h2>Why Track Forest Change?</h2>
               <br>
               <p>Trees are a significant source of carbon capture: they absorb carbon
               that would otherwise reach the atmosphere, which helps to mitigate climate change.
               The project area shows a concerning trend of old growth forest being converted to agriculture,
               which releases carbon captured by the canopy and soil.
               </p>
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
      tabPanel("Plot", br(), plotOutput("cumulative_plot"), br(), plotOutput("change_plot"))
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
