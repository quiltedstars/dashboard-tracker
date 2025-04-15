library(shiny)
library(tidyverse)
library(tidyr)
library(sf)
library(leaflet)
library(ndjson)
library(dplyr)
library(bslib)
library(glue)
library(knitr)
library(shinydashboard)
library(rmarkdown)


# read the markdown file (thanks to stack overflow: https://stackoverflow.com/questions/33499651/rmarkdown-in-shiny-application)
rmdfiles <- c("writeup.Rmd")
sapply(rmdfiles, knit, quiet = T)

# read data and create column in order to have a clean date
data <- read_csv("olddata.csv")
data$cleandate <- suppressWarnings(as.numeric(data$date))
data$cleandate[data$cleandate <= 1000 |
                 data$cleandate >= 2025] <- NA

# read the new data
newdata <- read_csv('all_data.csv')

# read the countries geojson
countries <- sf::read_sf("ne.geojson")

# find the differences between new and old data, create number to call in app
difference <- setdiff(data$id, newdata$id)
comp <- data %>% filter(id %in% difference)
number <- nrow(comp)


# oodles and oodles of cleanup
countries$NAME_LONG <- str_replace(countries$NAME_LONG, "Vatican", "Vatican City")
countries$NAME_LONG <- str_replace(countries$NAME_LONG, "United Republic of Tanzania", "Tanzania")
countries$NAME_LONG <- str_replace(countries$NAME_LONG, "Kingdom of eSwatini", "eSwatini")
countries$NAME_LONG <- str_replace(countries$NAME_LONG, "Republic of Korea", "South Korea")
countries$NAME_LONG <- str_replace(countries$NAME_LONG, "Russian Federation", "Russia")
countries$NAME_LONG <- str_replace(countries$NAME_LONG, "Dem. Rep. Korea", "North Korea")
countries$NAME_LONG <- str_replace(countries$NAME_LONG, "Lao PDR", "Laos")
countries$NAME_LONG <- str_replace(countries$NAME_LONG, "Brunei Darussalam", "Brunei")
data$place <- str_replace(data$place, "nigeria", "Nigeria")
data$place <- str_replace(data$place, "East Timor", "Timor-Leste")
data$place <- str_replace(data$place, "Balarus", "Belarus")
data$place <- str_replace(data$place, "Gambia", "The Gambia")
data <- data %>% mutate(place = case_when(place == "New Guinea" ~ "Papua New Guinea", TRUE ~ place))
data$place <- str_replace(data$place, "Macedonia", "North Macedonia")
data$place <- str_replace(data$place, "Burma", "Myanmar")
data$place <- str_replace(data$place, "Swaziland", "eSwatini")
data$place <- str_replace(data$place, "india", "India")
data$place <- str_replace(data$place, "Cote d'Ivoire", "Côte d'Ivoire")
data$place <- str_replace(data$place, "United States of America", "United States")
data$place <- str_replace(data$place, "Costa rica", "Costa Rica")
data$place <- str_replace(data$place, "People's Republic of China", "China")
data$place <- str_replace(data$place, "Sao Tome and Principe", "São Tomé and Principe")


sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Dashboard",
    tabName = "dashboard",
    icon = icon("dashboard")
  ),
  menuItem("Write-Up", icon = icon("pencil"), tabName = "write-up")
))

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "dashboard", fluidRow(
        column(
          width = 12,
          box(
            width = NULL,
            value_box(
              title = "",
              value = uiOutput("removed"),
              tags$p(
                "items have been removed from the Smithsonian Libraries datasets",
                style = "font-size: 20px;"
              ),
              tags$p("As of April 14, 2025", style = "font-size: 15px;"),
              theme = "teal",
              width = 10
            )
          ),
          box(width = NULL, tableOutput("removed_items"))
        ),
        column(
          width = 3,
          box(
            width = NULL,
            sliderInput(
              "range",
              "Range:",
              min = min(data$cleandate, na.rm = TRUE),
              max = max(data$cleandate, na.rm = TRUE),
              # value set for all dates
              value = c(
                min(data$cleandate, na.rm = TRUE),
                max(data$cleandate, na.rm = TRUE)
              ),
              step = 1
            )
          ),
          selectInput(
            "sort_method",
            "Sort Countries By:",
            choices = c(
              "Item Count (Descending)" = "count",
              "Country Name (A-Z)" = "alpha"
            )
          ),
          box(
            width = NULL,
            uiOutput("country_list", style = "height: 600px; overflow-y: scroll")
          )
        ),
        column(
          width = 9,
          box(width = NULL, leafletOutput("map")),
          box(
            width = NULL,
            h4("Items and Titles"),
            uiOutput("item_list"),
            style = "height: 400px; overflow-y: scroll; border: 1px solid #ddd; padding: 10px;"
          )
        )
      )
    ),
    tabItem(tabName = "write-up",
            h2("Write-up"),
            withMathJax(includeMarkdown("writeup.md"))
            )
    ))


ui <- dashboardPage(skin = "black", dashboardHeader(disable = TRUE), sidebar, body)

server <- function(input, output) {
  output$removed <- renderText({
    # we call the number we produced at the start
    glue("{number}")
  })
  
  # render the table with items that were removed, using the comp(arison) table
  output$removed_items <- renderTable({
    comp2 <- comp %>% select(c("date", "description", "name", "notes", "place", "title"))
    relocate(comp2,
             c("title", "name", "date", "description", "notes", "place"))
  })
  
  # create the base map
  output$map <- renderLeaflet({
    # count number of items per country, drop any items that don't have a country
    count_by <- data %>%
      count(place, sort = TRUE) %>%
      drop_na()
    
    # filter to countries, joining place to NAME_LONG
    mapdata <- countries %>%
      group_by(NAME_LONG) %>%
      left_join(count_by, by = c("NAME_LONG" = "place")) %>%
      # instead of there being null values added if a country did not have items, we have it set to 0 instead
      mutate(n = as.numeric(ifelse(is.na(n), 0, n)))
    
    # create initial bins
    bins <- c(0, 50, 100, 250, 500, 1000, 5000, 10000, Inf)
    # create initial colors
    cols <- colorBin("Blues", domain = mapdata$n, bins = bins)
    
    # create initial hover label per country
    labels <- sprintf("<strong>%s</strong><br/>%g items",
                      mapdata$NAME_LONG,
                      mapdata$n) %>% lapply(htmltools::HTML)
    
    # render the map
    leaflet(data = mapdata) %>%
      addProviderTiles("MapBox", options = providerTileOptions(id = "mapbox.light")) %>%
      addPolygons(
        fillColor = ~ cols(n),
        color = "black",
        weight = 0.5,
        opacity = 1,
        fillOpacity = 0.7,
        label = labels,
        layerId = ~ NAME_LONG
      ) %>%
      addLegend(
        pal = cols,
        values = ~ n,
        opacity = 0.7,
        title = "Number",
        position = "bottomright"
      ) %>%
      setView(lng = 0,
              lat = 0,
              zoom = 1)
  })
  # produce reactive variable for when we select a country on the map
  selected_country <- reactiveVal(NULL)
  
  # produce reactive variable based on the range slider, so that it filters the data columns
  filtered_data_by_date <- reactive({
    data %>%
      filter(cleandate >= input$range[1], cleandate <= input$range[2])
  })
  
  
  # observe event for when we change the date range
  observeEvent(input$range, {
    # new variable based on the one we just made which then counts the country results based on the range selected
    filtered_data <- filtered_data_by_date() %>%
      count(place, sort = TRUE) %>%
      drop_na()
    
    # we do what we did above again but this time with the filtered data of the date ranges
    mapdata_filtered <- countries %>%
      group_by(NAME_LONG) %>%
      left_join(filtered_data, by = c("NAME_LONG" = "place")) %>%
      # instead of there being null values added if a country did not have items, we have it set to 0 instead
      mutate(n = as.numeric(ifelse(is.na(n), 0, n)))
    
    # create a list of the total country item count!
    n_values <- mapdata_filtered$n
    # output the maximum number of items
    max_n <- max(n_values, na.rm = TRUE)
    
    # dynamic bins so that the chloropleth map will be responsive
    if (max_n <= 10) {
      bins <- c(1, 2, 5, 10)
    } else if (max_n <= 50) {
      bins <- c(1, 5, 10, 25, 50)
    } else if (max_n <= 100) {
      bins <- c(1, 10, 25, 50, 100)
    } else if (max_n <= 1000) {
      bins <- c(1, 50, 100, 250, 500, 1000)
    } else if (max_n <= 10000) {
      bins <- c(1, 100, 250, 500, 1000, 2500, 5000, 10000)
    } else {
      bins <- c(1, 100, 500, 1000, 5000, 10000, 50000, 100000)
    }
    
    # create colors for the bins
    cols <- colorBin("Blues", domain = n_values, bins = bins)
    
    # create label for when user hovers on the map
    labels_filtered <- sprintf(
      "<strong>%s</strong><br/>%g items",
      mapdata_filtered$NAME_LONG,
      mapdata_filtered$n
    ) %>% lapply(htmltools::HTML)
    
    # create the leaflet
    leafletProxy("map", data = mapdata_filtered) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~ cols(n),
        color = "black",
        weight = 0.5,
        opacity = 1,
        fillOpacity = 0.7,
        label = labels_filtered,
        # the layer id is the country names
        layerId = ~ NAME_LONG
      ) %>%
      # allow user to click others
      clearControls() %>%
      # add a legend
      addLegend(
        pal = cols,
        values = n_values,
        opacity = 0.7,
        title = "Number",
        position = "bottomright"
      )
  })
  
  # Render filtered item list
  render_filtered_items <- function(country) {
    # calling back to filtered_data_by_date for items based on date range
    filtered_items <- filtered_data_by_date() %>%
      filter(str_detect(place, fixed(country, ignore_case = TRUE)))
    
    # render the item table
    output$item_list <- renderTable({
      if (nrow(filtered_items) > 0) {
        filtered <- filtered_items %>% select(c("date", "description", "name", "notes", "title"))
        relocate(filtered,
                 c("title", "name", "date", "description", "notes"))
      } else {
        "No items available for this country."
      }
    })
  }
  
  # Now we go to dealing with clicking the map
  # I've got little clue how map_shape_click works, but it is based on the geojson
  observeEvent(input$map_shape_click, {
    clicked_country <- input$map_shape_click$id
    selected_country(clicked_country)
    # based on the click of the country we then render as we wanted to above
    render_filtered_items(clicked_country)
  })
  
  # Because some countries are small, we have a sidebar country click
  observeEvent(selected_country(), {
    req(selected_country())
    render_filtered_items(selected_country())
  })
  
  # And now we actually render the country list in sidebar
  output$country_list <- renderUI({
    count_by_filtered <- filtered_data_by_date() %>%
      count(place, sort = TRUE) %>%
      drop_na()
    
    # We then make a sorting method
    sorted_data <- switch(
      input$sort_method,
      # we do str_to_lower to avoid issues with eSwatini sorting
      "alpha" = count_by_filtered %>% arrange(str_to_lower(place)),
      "count" = count_by_filtered %>% arrange(desc(n)),
      count_by_filtered
    )
    
    if (nrow(sorted_data) == 0) {
      return("No countries found in this date range.")
    }
    
    tagList(h4("Countries with Items"), lapply(1:nrow(sorted_data), function(i) {
      actionLink(
        inputId = paste0("country_", i),
        label = paste0(sorted_data$place[i], " (", sorted_data$n[i], ")"),
        style = "display: block; margin-bottom: 5px;"
      )
    }))
  })
  
  
  # Observe clicks on each country while also observing date range changes, so that if we change the sorting it still updates correctly
  observe({
    count_by_filtered <- filtered_data_by_date() %>%
      count(place, sort = TRUE) %>%
      drop_na()
    
    sorted_data <- switch(
      input$sort_method,
      "alpha" = count_by_filtered %>% arrange(str_to_lower(place)),
      "count" = count_by_filtered %>% arrange(desc(n)),
      count_by_filtered
    )
    
    lapply(1:nrow(sorted_data), function(i) {
      observeEvent(input[[paste0("country_", i)]], {
        selected_country(sorted_data$place[i])
      }, ignoreInit = TRUE)
    })
  })
}

shinyApp(ui = ui, server = server)