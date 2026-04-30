# (Run install.packages(c("sf", "tigris", "dplyr", "ggplot2", "rio")) if needed)
library(sf)         # The modern standard for spatial data in R
library(tigris)     # Downloads US Census shapefiles directly (no manual unzipping!)
library(dplyr)      # Data manipulation
library(ggplot2)    # We use the same ggplot2 grammar for maps via geom_sf()
library(rio)        # For importing CSVs
library(tidygeocoder)
texascounties <- counties(state = "TX", cb = TRUE)
ggplot(data = texascounties) + 
  geom_sf() #this will create the map

FQHC_url <- 'https://data.hrsa.gov/DataDownload/DD_Files/Health_Center_Service_Delivery_and_LookAlike_Sites.xlsx'
FQHC <- import(FQHC_url)
FQHC_SF <- subset(FQHC, `Site State Abbreviation`== "TX") %>%  # ` this is used when a variable name has space in it (eg. "Site Space") or if the data have illegal variable name (like numbers)
st_as_sf(coords = c("Geocoding Artifact Address Primary X Coordinate" , "Geocoding Artifact Address Primary Y Coordinate"), crs = 4326)
ggplot(data = texascounties) + 
  geom_sf()+
  geom_sf(data = FQHC_SF, color="navy", size=1.5, alpha=.3)

  
FQHC_counties <- (st_transform (FQHC_SF, crs = st_crs(texascounties)))
FQHC_C2 <- st_join(FQHC_counties, texascounties) %>% 
  group_by(NAME) %>% 
  summarise(clinic_count=n()) %>% 
  st_drop_geometry() %>% 
  left_join(texascounties,.,by="NAME") %>% 
  mutate(clinic_count = coalesce(log(clinic_count), 0))

ggplot(data = FQHC_C2) + 
  geom_sf(aes(fill=clinic_count))+
  geom_sf(data = FQHC_SF, color="pink", size=1.5, alpha=.3)+
  scale_fill_viridis_c()

#Creating Synthetic Data of Street Addresses
street_address<-data.frame(full_address=c("7703 Floyd Curl Dr, San Antonio, TX 78258","1 Haven for Hope Way, San Antonio, TX 78207", "300 Alamo Plaza, San Antonio, TX 78205", "1402 Broadway St, Galveston, TX 77550", "411 Elm St, Dallas, TX 75202", "604 Brazos St, Austin, TX 78701", "2515 W 5th St, Irving, TX 75060", "100 Lady Bird Lane, Johnson City, TX 78636", "1412 W Ohio Ave, Midland, TX 79701", "1217 W Sam Rayburn Dr, Bonham, TX 75418", "3700 Hogge Dr, Parker, TX 75002", "2101 Ross Ave, Dallas, TX 75201", "2414 Rosedale St, Houston, TX 77004"))
#Obtain cordinantes for the addressses
geocoded_street_address<-geocode(street_address, address=full_address, method="osm")
#Converting coordinates to shape file
shape_file_street_address<-st_as_sf(geocoded_street_address,coords=c("long","lat"),crs=4326)

ggplot(data=FQHC_C2)+
  geom_sf(fill="purple")+
  geom_sf(data=FQHC_SF, color="pink", size=1.5, alpha=0.3)+
  geom_sf(data=shape_file_street_address, color="blue")

geom_sf(data=shape_file_street_address, color='green')
#Interactive Mapping
tmap_mode("view")
tm_shape(FQHC_C2)+
  tm_polygons(fill="clinic_count")+
  tm_shape(shape_file_street_address)+
  tm_dots(fill="darkgreen", fill_alpha=0.5, size=0.2)

