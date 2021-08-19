library(shiny)
library(leaflet)
library(dplyr)

suburb_data_comb <- read.csv('suburb_data_comb.csv')
unique_breed <- 
    suburb_data_comb %>% arrange(desc(n)) %>% pull(breed) %>% unique()

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(
        top = 10, right = 10,
        selectInput(
            inputId = 'in_breed', 'Choose Breed', 
            choices = unique_breed, selected = 'Maltese', 
            selectize = TRUE
        )
    )
)

server <- function(input, output, session) {
    
    # Reactive expression for the data filtered to what the user selected
    filteredData <- reactive({
        suburb_data_comb %>% 
            filter(breed == input$in_breed)
    })
    
    output$map <- renderLeaflet({
        leaflet() %>% addTiles() %>% 
            fitBounds(
                min(suburb_data_comb$longitude), min(suburb_data_comb$latitude), 
                max(suburb_data_comb$longitude), max(suburb_data_comb$latitude))
    })
    
    colourpal <- reactive({
        colorNumeric(palette = 'Reds', domain = filteredData()$n)
    })
    
    observe({
        pal <- colourpal()
        
        leafletProxy("map", data = filteredData()) %>%
            clearMarkers() %>% clearControls() %>% 
            addCircleMarkers(
                ~longitude, ~latitude, 
                popup = ~paste0(suburb,': ',n), 
                label = ~paste0(suburb,': ',n),
                stroke = F, color = ~pal(n), fillOpacity = 0.5
            ) %>%
            addLegend(
                position = "bottomright", pal = pal, values = ~n,
                title = "No. of Dogs",
                opacity = 1
            )
    })
}

shinyApp(ui, server)
