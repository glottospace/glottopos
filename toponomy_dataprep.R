# Overview ----------------------------------------------------------------
# Script associated with the publication: Integrating Linguistic and Geographic Methods in Toponymic Analysis: The Case of Puquina (Central Andes), Diachronica
# Authors: Sietze Norder and Nick Emlen 
# Aim of this script: preparation of geonames data

library(sf)
library(readxl)

# Data preparation --------------------------------------------------------

# # Read geonames data
countrylist_txt <- list.files("data/", pattern = "\\.txt$", full.names = TRUE)
countrylist_sf <- lapply(countrylist_txt, geonamestxt2sf)
geonamesdata <- do.call(rbind,countrylist_sf)
st_write(geonamesdata, "data/geonames_SA.gpkg", append = F)
# Read naturalearth data
boundarypol <- ne_download(scale = 110, type = "map_units") %>%
  filter(CONTINENT == "South America") %>%
  st_transform(crs = 'ESRI:54012')# alternatively: ne_countries(country = c("Peru", "Bolivia", "Chile"))
geonamesdata <- glottoclean_geonames(geonamesdata = geonamesdata, boundarypol = boundarypol)
st_write(geonamesdata, "data/geonames_SA.gpkg")

