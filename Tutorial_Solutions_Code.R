
#PRACTICE: https://rstudio.github.io/leaflet/popups.html
  
# General example
content <- paste(sep = "<br/>",
  "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
  "606 5th Ave. S",
  "Seattle, WA 98138"
)

leaflet() %>% addTiles() %>%
  addPopups(-122.327298, 47.597131, content,
    options = popupOptions(closeButton = FALSE)
  )  
  
  
## Create and Customize the Base Map and the Initial View
myMap_1 <- leaflet() %>%
  setView(lng = 13.3892, lat = 52.5128, zoom = 12) %>% 
  addTiles()
print(myMap_1)


## Add data layers, Customize Popups
myMap_2 <- myMap_1 %>% addCircles(
  lng = 13.3892,   # Longitude of the circle's center
  lat = 52.5128,   # Latitude of the circle's center
  radius = 300,   # Radius of the circle in meters
  color = "blue",   # Color of the circle
)
print(myMap_2)

# But, we can create popups as well
content <- paste(sep = "<br/>",
                 "<b><a href='https://www.hertie-school.org/en/'>Hertie School</a></b>",
                 "Friedrichstraße 180",
                 "10117 Berlin"
)
  
leaflet() %>% addTiles() %>%
  addPopups(13.389223597194448, 52.51298665365591, content,
            options = popupOptions(closeButton = FALSE)
  )  


### Practical example
# Importing the data
deforestation <- read_xlsx("COL.xlsx", 
                           sheet = "Subnational 2 tree cover loss") %>%
  distinct(subnational2, .keep_all = TRUE) %>%
  mutate(subnational2 = toupper(subnational2),
         subnational1 = toupper(subnational1),
         pct_gain = `gain_2000-2020_ha`/area_ha) %>%
  rename(nombre_mpi = subnational2,
         nombre_dpt = subnational1) %>%
  arrange(nombre_mpi)

map <- sf::st_read("shapes/shapes.shp", 
                   stringsAsFactors = FALSE) %>%
  arrange(nombre_mpi)

# Function to remove accents
remove_accents <- function(input_string) {
  paste(sapply(strsplit(input_string, "")[[1]], function(x) ifelse(x %in% c("Ñ", "Ü"), x, stringi::stri_trans_general(x, "Latin-ASCII"))), collapse = "")
}

# Apply the function to the vector
deforestation$nombre_mpi <- sapply(deforestation$nombre_mpi, remove_accents)
deforestation$nombre_dpt <- sapply(deforestation$nombre_dpt, remove_accents)

# Apply the function to the vector
deforestation$nombre_mpi <- sapply(deforestation$nombre_mpi, remove_accents)
deforestation$nombre_dpt <- sapply(deforestation$nombre_dpt, remove_accents)  

#Merging data
deforestation_map <- full_join(map, deforestation, by = c("nombre_mpi", "nombre_dpt"))

#No matches
not_m <- map %>%
  anti_join(deforestation, by = c("nombre_mpi", "nombre_dpt")) %>%
  arrange(nombre_mpi)


#The palette of colors is based on a scale of greens that changes based on the gained ha of trees between 2002 and 2020 by municipality
palette <- colorNumeric(palette = "Greens", domain = deforestation_map$`gain_2000-2020_ha`)

#We create some tags by municipality using the glue and the htmltools packages
def_popup <- glue("<strong>{deforestation_map$nombre_mpi}</strong><br />
                    <strong>Department: {deforestation_map$nombre_dpt}</strong><br />
                    Total area (ha): {deforestation_map$area_ha}<br /> 
                    Gained ha of trees (2002-2020): {scales::comma(deforestation_map$`gain_2000-2020_ha`, accuracy = 1)}<br />
                    Tree cover loss 2022 (ha): {scales::comma(deforestation_map$tc_loss_ha_2022, accuracy = 1)}<br />")  %>%   
  lapply(htmltools::HTML)

#Final map
leaflet() %>%
  #addTiles() %>% #For the default map
  addProviderTiles("CartoDB.Positron") %>% #For one of the options included in the addProviderTiles() function
  #addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}") %>% #For an example of third party tiles
  addPolygons(
    data = deforestation_map,
    fillColor = ~palette(deforestation_map$`gain_2000-2020_ha`),
    label = def_popup,
    fillOpacity = 1,
    color = "grey",
    weight = 1
  )