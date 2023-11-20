#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd


app_server <- function(input, output, session) {
  # Your application server logic

  output$mapl <- renderLeaflet({
    # only include aspects of the map that won't need to change dynamically
    leaflet() %>%
      setView(lng = -89.48124981308376, lat = 43.150945262909595, zoom = 12) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Basemap") %>%
      #addProviderTiles(providers$USGS.USImageryTopo, group = "Satellite") %>% impossible to see these colors on a satellite map
      addLegend(colors = c("#3fab63", "#4e79cf", "#cf4e4e","#f5ac53"),
                labels = c("Old Growth Forest", "Early Successional Forest", "Old Growth Forest Lost", "Early Successional Forest Lost"), opacity = 1) %>%
      addLayersControl(
        overlayGroups = c("Old Growth Forest", "Early Successional Forest", "Old Growth Forest Lost", "Early Successional Forest Lost"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  map_data <- reactive({
    input$mapl_zoom
  })


  observe({

    #find which year to grab
    col_as_num <- as.numeric(colnames(forest_data)[4:11])
    if (input$year == 2022 ) {
      display_year <- 11
    } else {
      display_year <- 3+min(which(input$year < col_as_num))-1

    }

    name <- colnames(forest_data[display_year])
    forest_data_filter <- forest_data[, display_year]

    #function to create a filler row in an empty dataframe, otherwise can't check if it's empty
    placeholder <- function(df) {
      if (nrow(df) == 0) {
        df[1,] <- NA
      }
      return(df)
    }

    #uncomment if using st_union instead of st_combine
    #sf_use_s2(FALSE)

    #seperate into dataframes for coloring
    not_forest <- filter(forest_data_filter, !!as.name(name) == "Not Forest") %>%
      placeholder() %>%
      st_combine()
    original_forest <- filter(forest_data_filter, !!as.name(name) == "Original Forest") %>%
      placeholder() %>%
      st_combine()
    deforested <- filter(forest_data_filter, !!as.name(name) == "Deforested") %>%
      placeholder() %>%
      st_combine()
    new_forest <- filter(forest_data_filter, !!as.name(name) == "New Forest") %>%
      placeholder() %>%
      st_combine()
    new_forest_lost <- filter(forest_data_filter, !!as.name(name) == "New Forest Lost") %>%
      placeholder() %>%
      st_combine()

    leafletProxy("mapl") %>%
      clearShapes() %>%
      addPolygons(data = not_forest, color = "grey", weight = .1, layerId = "Study Area") %>%
      {if(sum(st_is_empty(original_forest)) == 0)
        (addPolygons(., data = original_forest, color = "#3fab63", weight = 1, layerId = "forest", group = "Old Growth Forest"))
        else .} %>%
      {if(sum(st_is_empty(new_forest_lost)) == 0)
        (addPolygons(., data = new_forest_lost, color = "#f5ac53", weight = 1, layerId = "reforestation_lost", group = "Early Successional Forest Lost"))
        else .} %>%
      {if(sum(st_is_empty(deforested)) == 0)
        (addPolygons(., data = deforested, color = "#cf4e4e", weight = 1, layerId = "deforested", group = "Old Growth Forest Lost"))
        else .} %>%
      {if(sum(st_is_empty(new_forest)) == 0)
        (addPolygons(., data = new_forest, color = "#4e79cf", weight = 1, layerId = "reforested", group = "Early Successional Forest"))
        else .}
  })


  output$cumulative_plot <- renderPlot ({

    col_as_num <- as.numeric(colnames(forest_data)[4:11])
    if (input$year == 2022 ) {
      display_year <- 11
    } else {
      display_year <- 3+min(which(input$year < col_as_num))-1
    }

    name <- colnames(forest_data[display_year])[[1]]

    ggplot(filter(cumulative_summary, value != "Not Forest"), aes(x=as.numeric(name), y=total, color=factor(value, levels = c("Old Growth Forest", "Early Successional Forest", "Old Growth Forest Lost", "Early Successional Forest Lost")))) +
      geom_point(size=4) +
      scale_color_manual(values = c("#3fab63", "#4e79cf", "#cf4e4e", "#f5ac53")) +
      #highlight the year closest to the slider input
      gghighlight(name == !!name, label_key = value, keep_scales = TRUE, use_direct_label = FALSE,
                  unhighlighted_params = list(colour = NULL, alpha = 0.3)) +
      labs(title="Acreage Total by Decade",
           x ="Year", y = "Acres") +
      scale_x_continuous(breaks=c(1937, 1955, 1968, 1976, 1987, 2000, 2010, 2022), labels=c("1937", "1955", "1968", "1976", "1987", "2000", "2010", "2022")) +
      theme_minimal() +
      theme(text = element_text(size=20), legend.title= element_blank())

  })


  output$change_plot <- renderPlot ({

    col_as_num <- as.numeric(colnames(forest_data)[4:11])
    if (input$year == 2022 ) {
      display_year <- 11
    } else {
      display_year <- 3+min(which(input$year < col_as_num))-1
    }

    name <- colnames(forest_data[display_year])[[1]]

    ggplot(change_summary, aes(x=name, y=change, color=factor(value, levels = c("Early Successional Forest", "Old Growth Forest")))) +
      geom_point(size=4) +
      scale_color_manual(values = c("#4e79cf", "#3fab63")) +
      #highlight the year closest to the slider input
      gghighlight(name == !!name, label_key = value, keep_scales = TRUE, use_direct_label = FALSE,
                  unhighlighted_params = list(colour = NULL, alpha = 0.3)) +
      labs(title="Acreage Change by Decade",
           x ="Year", y = "Acreage Change") +
      scale_x_continuous(breaks=c(1937, 1955, 1968, 1976, 1987, 2000, 2010, 2022), labels=c("", "1955", "1968", "1976", "1987", "2000", "2010", "2022")) +
      theme_minimal() +
      theme(text = element_text(size=20), legend.title= element_blank()) +
      geom_hline(yintercept=0, linetype="dashed", color = "red")

  })
  # updateSelectizeInput(session, "address_search", choices = parcels_2023_short$address, server = TRUE)
  #
  # output$address_search <- renderText({
  #   paste(input$address_search, "is in the study area.")
  #
  #   parcel_geo <- filter(parcels_2023_short, address == input$address_search)
  #
  #   sf_use_s2(FALSE)
  #   #look at just the forest for 2022
  #   cur_original_forest <- select(forest_data, `2022`) %>%
  #     filter(`2022` == "Original Forest") %>%
  #     st_combine()
  #
  #   cur_new_forest <- select(forest_data, `2022`) %>%
  #     filter(`2022` == "New Forest") %>%
  #     st_combine()
  #
  #   intersect_original <- st_intersection(parcel_geo, cur_original_forest)
  #   intersect_new <- st_intersection(parcel_geo, cur_new_forest)
  #
  #
  #   if (nrow(intersect_original) == 0) {
  #     original_acres = 0
  #   } else {
  #     original_acres <- round(as.numeric(st_area(intersect_original))/4047, 2)
  #   }
  #
  #   if (nrow(intersect_new) == 0) {
  #     new_acres = 0
  #   } else {
  #     new_acres <- round(as.numeric(st_area(intersect_new))/4047, 2)
  #   }
  #
  #   paste("There are", original_acres, "acres of original forest and", new_acres, "acres of new forest on your property.")
  #
  # })
}
