library(dplyr)
library(tidyr)
library(stringr)
library(httr)

suburb_data_import <- readRDS('where-are-the-dogs/animal_permits.rds') %>% 
    as_tibble()
suburb_data <- suburb_data_import %>% 
    select(suburb) %>% distinct() %>% 
    mutate(suburb_tidy = paste(suburb, 'Brisbane'))

#### testing ####
url <- "https://places.nbnco.net.au/places/v1/autocomplete"

response <- GET(
    url, 
    add_headers(referer = 'https://www.nbnco.com.au/learn/rollout-map'), 
    query = list(query = 'Acacia Ridge Brisbane', timestamp = as.integer(Sys.time()))
)

content_get <- content(response)
resp_content <- unlist(content_get$suggestions[1])



# url_loc <- "https://places.nbnco.net.au/places/v2/location/ChIJjTecptlD1moRq-m5hk7gKkQ?source=website_rollout_map"
url_loc <- "https://places.nbnco.net.au/places/v2/location/"
response_loc  <- GET(
    paste0(url_loc, resp_content[1]), 
    add_headers(referer = 'https://www.nbnco.com.au/learn/rollout-map'), 
    query = list(source = "website_rollout_map")
)
content_get_loc <- content(response_loc)

#### for loop implementation ####

content_loc <- NULL
url <- "https://places.nbnco.net.au/places/v1/autocomplete"
url_loc <- "https://places.nbnco.net.au/places/v2/location/"

for (i in 1:length(suburb_data$suburb_tidy)) {
    response <- GET(
        url, 
        add_headers(referer = 'https://www.nbnco.com.au/learn/rollout-map'), 
        query = list(query = suburb_data$suburb_tidy[i], timestamp = as.integer(Sys.time()))
    )
    
    content_get <- content(response)
    resp_content<- unlist(content_get$suggestions[1])
    
    response_loc  <- GET(
        paste0(url_loc, resp_content[1]), 
        add_headers(referer = 'https://www.nbnco.com.au/learn/rollout-map'), 
        query = list(source = "website_rollout_map")
    )
    
    if(response_loc$status_code == 200){
        content_get_loc <- as.data.frame(content(response_loc))
        content_get_loc$suburb <- suburb_data$suburb[i]
        content_loc <- rbind(content_loc, content_get_loc)
    } else {
        print(i)
        print(suburb_data$suburb[i])
    }
}

suburb_locations <- as_tibble(content_loc) %>% 
    mutate(id = row_number()) #%>% 
# filter(!(suburb %in% c('Kooringal','Cowan Cowan','Bulwer','Moreton Island')))

# View(suburb_locations)
# plotly::plot_ly(
#     suburb_locations, x = ~longitude, y = ~latitude,
#     type = 'scatter', mode = 'markers',
#     hoverinfo = 'text', text = ~suburb
# )

suburb_data_comb <- suburb_data_import %>% 
    group_by(suburb, breed) %>% count() %>% 
    inner_join(
        suburb_locations %>% select(suburb, latitude, longitude),
        by = 'suburb'
    )

write.csv(suburb_data_comb, file = 'where-are-the-dogs-map/suburb_data_comb.csv')

library(leaflet)

#create shiny app
pal <- colorNumeric(palette = 'Reds', domain = suburb_data_comb$n)

leaflet(data = suburb_data_comb) %>% addTiles() %>%
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


