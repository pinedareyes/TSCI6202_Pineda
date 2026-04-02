# (Run install.packages(c("sf", "tigris", "dplyr", "ggplot2", "rio")) if needed)
library(sf)         # The modern standard for spatial data in R
library(tigris)     # Downloads US Census shapefiles directly (no manual unzipping!)
library(dplyr)      # Data manipulation
library(ggplot2)    # We use the same ggplot2 grammar for maps via geom_sf()
library(rio)        # For importing CSVs
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

