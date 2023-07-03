library(geojsonsf)
library(sf)

# Import prion polygons sourced from the Homeland Infrastructure Foundation Level Data (HIFLD) database
# https://hifld-geoplatform.opendata.arcgis.com/datasets/geoplatform::prison-boundaries/about
file<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/HIFLD Data/Prison_Boundaries.geojson'

prisons<-geojson_sf(file)

# Count prisons and capacity* by county
# *Capacity is missing many local and county jails so use with caution

prisons_agg <- prisons %>%
  st_drop_geometry() %>%
  filter(STATUS != 'CLOSED') %>%
  mutate(CAPACITY = ifelse(CAPACITY == -999, NA, CAPACITY)) %>%
  group_by(County=COUNTYFIPS) %>%
  summarise(Prisons=n(), Capacity = sum(CAPACITY, na.rm=T))
