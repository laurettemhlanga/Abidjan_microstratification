## Water bodies

###OCHA file
library(sf)
library(ggplot2)

# Read the shapefile
shapefile <- st_read("C:/Users/hp/Documents/LUC UMP/OCHA Cote d'ivoire/PLAN_D'EAU.shp")

# Plot the shapefile
ggplot() +
  geom_sf(data = shapefile) +
  theme_minimal()
