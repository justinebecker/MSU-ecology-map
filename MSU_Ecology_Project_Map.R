library(leaflet)
library(sf)
# library(leaflegend) # currently not used
library(fontawesome)
library(tigris)
library(dplyr)
library(leafpop)
library(jpeg)

if(Sys.getenv("USER")=="t46t676"){
  setwd("/Users/t46t676/OneDrive - Montana State University/MSU/Dept_admin/MappingProject/")
}else{
  setwd("C:/Users/t46t676/OneDrive - Montana State University/MSU/Dept_admin/MappingProject/")
}

ecol_data <- read.csv("EcolProjs.csv")

ecol_data$Label <- paste0(ecol_data$Title, 
                            ". PI: ", ecol_data$PI, 
                            ". Collaborators/Funders: ", ecol_data$Collaborators_funders,
                            ". Other personnel: ", ecol_data$Other_personnel,
                            ". Image credit: ", ecol_data$Image_credit)

ecol_data$lat <- jitter(ecol_data$lat, factor = 2)
ecol_data$lon <- jitter(ecol_data$lon, factor = 2)

ecol_data <- st_as_sf(ecol_data,coords = c("lon", "lat"), crs = 4326)

# Make set of icons for groups of interest ####
iconSet <- awesomeIconList(
  bird = makeAwesomeIcon(
    text = fa('crow'),
    library = 'fa',
    iconColor = 'goldenrod',
    markerColor = 'darkblue',
    squareMarker = FALSE
  ),
  fish = makeAwesomeIcon(
    text = fa('fish'),
    library = 'fa',
    iconColor = 'goldenrod',
    markerColor = 'darkblue',
    squareMarker = FALSE
  ),
  mammal = makeAwesomeIcon(
    text = fa('paw'),
    library = 'fa',
    iconColor = 'goldenrod',
    markerColor = 'darkblue',
    #spin = TRUE,
    squareMarker = FALSE
  ),
  plant = makeAwesomeIcon(
    text = fa('leaf'),
    library = 'fa',
    iconColor = 'goldenrod',
    markerColor = 'darkblue',
    #spin = TRUE,
    squareMarker = FALSE
  ),
  invertebrate = makeAwesomeIcon(
    text = fa('bug'),
    library = 'fa',
    iconColor = 'goldenrod',
    markerColor = 'darkblue',
    #spin = TRUE,
    squareMarker = FALSE
  ),
  herpetofauna = makeAwesomeIcon(
    text = fa('frog'),
    library = 'fa',
    iconColor = 'goldenrod',
    markerColor = 'darkblue',
    #spin = TRUE,
    squareMarker = FALSE
  )
)

########################
# Global studies map ###
########################
ecol_data_mammal <- ecol_data %>%
  filter(Taxon == "Mammal")

ecol_data_bird <- ecol_data %>%
  filter(Taxon == "Bird")

ecol_data_fish <- ecol_data %>%
  filter(Taxon == "Fish")

ecol_data_plant <- ecol_data %>%
  filter(Taxon == "Plant")

ecol_data_invert <- ecol_data %>%
  filter(Taxon == "Invertebrate")

ecol_data_herp <- ecol_data %>%
  filter(Taxon == "Herpetofauna")

# Create a map widget centered on Equitorial Guinea 
# Approximate center coordinates for Equitorial Guinea are around lat 1.62, lng 10.35
map_global <- leaflet() %>% 
  # Use Esri basemap instead of addTiles()
  addProviderTiles(providers$Esri.WorldTopoMap) %>% 
  setView(lng = 10.35, lat = 1.62, zoom = 2) %>% # Set initial view and zoom level
  
  # add home button that zooms to Montana = home
  leaflet::addEasyButton(leaflet::easyButton(icon = "fa-home", 
                                             title = "Zoom to Montana", 
                                             onClick = htmlwidgets::JS("function(btn, map){ map.setView([47, -109.5], 6); }")))

# Add some more items to the map
ecol_proj_map <- map_global %>% 
  # addPolygons(data = USA_sites, 
  #             weight = 0.4, 
  #             fill = TRUE,
  #             fillOpacity = 0.05,
  #             label = ~NAME,         # Label shown on hover
  #             popup = ~NAME,         # Popup shown on click
  #             highlightOptions = highlightOptions(
  #               weight = 3,
  #               color = "black",
  #               fillOpacity = 0.1,
  #               bringToFront = FALSE)) %>%
  
  # Add all the markers for the bird studies
  addAwesomeMarkers(data = ecol_data_bird, group = "birds",
                    icon = iconSet$bird,
                    popup = ecol_data_bird$Label) %>%
  addPopupImages(ecol_data_bird$imgs, group = "birds", width = 300, maxWidth = 300) %>%
  
  # Add all the markers for the mammal studies
  addAwesomeMarkers(data = ecol_data_mammal, group = "mammals",
                    icon = iconSet$mammal,
                    popup = ecol_data_mammal$Label) %>%
  addPopupImages(ecol_data_mammal$imgs, group = "mammals", width = 300, maxWidth = 300) %>%
  
  # Add all the markers for the fish studies
  addAwesomeMarkers(data = ecol_data_fish, group = "fish",
                    icon = iconSet$fish,
                    popup = ecol_data_fish$Label) %>%
  addPopupImages(ecol_data_fish$imgs, group = "fish", width = 300, maxWidth = 300) %>%
  
  # Add all the markers for the plant studies
  addAwesomeMarkers(data = ecol_data_plant, group = "plants",
                    icon = iconSet$plant,
                    popup = ecol_data_plant$Label) %>%
  addPopupImages(ecol_data_plant$imgs, group = "plants", width = 300, maxWidth = 300) %>%
  
  # Add all the markers for the invert studies
  addAwesomeMarkers(data = ecol_data_invert, group = "invertebrates",
                    icon = iconSet$invertebrate,
                    popup = ecol_data_invert$Label) %>%
  addPopupImages(ecol_data_invert$imgs, group = "invertebrates", width = 300, maxWidth = 300) %>%
  
  # Add all the markers for the plant studies
  addAwesomeMarkers(data = ecol_data_herp, group = "herpetofauna",
                    icon = iconSet$herpetofauna,
                    popup = ecol_data_herp$Label) %>%
  addPopupImages(ecol_data_herp$imgs, group = "herpetofauna", width = 300, maxWidth = 300) %>%
  
  # Layers control box with all groups on at the start
  addLayersControl(
    overlayGroups = c("birds", "fish", "invertebrates", "herpetofauna", "mammals", "plants"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

ecol_proj_map

# Save the map as html
htmlwidgets::saveWidget(ecol_proj_map, file = "index.html", selfcontained = TRUE)



