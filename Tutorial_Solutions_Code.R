
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
  addProviderTiles("OpenStreetMap") %>%
  addPolygons(
    data = deforestation_map,
    fillColor = ~palette(deforestation_map$`gain_2000-2020_ha`),
    label = def_popup,
    fillOpacity = 1,
    color = "grey",
    weight = 1
  )


#Final Map Version 2
leaflet() %>%
  addProviderTiles(providers$Stadia.AlidadeSmooth) %>%
  addPolygons(
    data = deforestation_map,
    fillColor = ~palette(deforestation_map$`gain_2000-2020_ha`),
    label = def_popup,
    fillOpacity = 1,
    color = "grey",
    weight = 1
  )