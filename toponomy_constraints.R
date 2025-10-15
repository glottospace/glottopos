# Overview ----------------------------------------------------------------
# Script associated with the publication: Integrating Linguistic and Geographic Methods in Toponymic Analysis: The Case of Puquina (Central Andes), Diachronica
# Authors: Sietze Norder and Nick Emlen
# Aim of this script: apply constraints for toponymic analysis

library(sf)
library(readxl)

# Data preparation --------------------------------------------------------

# Read pre-processed geonames data (aggregated to South America)
geonamesdata <- st_read("data/geonames_SA.gpkg")

# Read textmatch 
textmatch <- read_xlsx("data/aymara-toponymy-text-matches.xlsx")

# Create empty data structure to store results
toponymsresdf <- glottocreate_toponymresdf(geonames = geonamesdata, textmatch = textmatch)

# No constraint. All topomatches ------------------------------------------
topomatch <- glottoconstraint_topomatch(topomatch = toponymsresdf, textmatch = textmatch, position = F)
st_write(topomatch, "output/topomatch_tf_all_ay.gpkg", append = F)
topomatch <- topomatch[topomatch$topomatch == TRUE, ]
st_write(topomatch, "output/topomatch_all_ay.gpkg", append = F)

# Constraint 1. Morphological position ------------------------------------
topomatch <- glottoconstraint_topomatch(topomatch = toponymsresdf, textmatch = textmatch, position = T)
st_write(topomatch, "output/topomatch_tf_position_ay.gpkg", append = F)
topomatch <- topomatch[topomatch$topomatch == TRUE, ]
st_write(topomatch, "output/topomatch_position_ay.gpkg", append = F)

# # Constraint 2. Length of match -------------------------------------------
topomatch <- st_read("output/topomatch_all_ay.gpkg")
topomatch <- glottoconstraint_length(topomatch = topomatch, minlength = 4)
st_write(topomatch, "output/topomatch_tf_length4_ay.gpkg", append = F)
topomatch <- topomatch[topomatch$topomatch == TRUE, ]
st_write(topomatch, "output/topomatch_length4_ay.gpkg", append = F)

topomatch <- st_read("output/topomatch_all_ay.gpkg")
topomatch <- glottoconstraint_length(topomatch = topomatch, minlength = 5)
st_write(topomatch, "output/topomatch_tf_length5_ay.gpkg", append = F)
topomatch <- topomatch[topomatch$topomatch == TRUE, ]
st_write(topomatch, "output/topomatch_length5_ay.gpkg", append = F)

topomatch <- st_read("output/topomatch_all_ay.gpkg")
topomatch <- glottoconstraint_length(topomatch = topomatch, minlength = 6)
st_write(topomatch, "output/topomatch_tf_length6_ay.gpkg", append = F)
topomatch <- topomatch[topomatch$topomatch == TRUE, ]
st_write(topomatch, "output/topomatch_length6_ay.gpkg", append = F)

# Constraint 3. Geographic feature ----------------------------------------
topomatch <- st_read("output/topomatch_all_ay.gpkg")
topomatch <- glottoconstraint_geofeature(topomatch = topomatch, textmatch = textmatch)
st_write(topomatch, "output/topomatch_tf_geofeature_ay.gpkg", append = F)
topomatch <- topomatch[topomatch$topomatch == TRUE, ]
st_write(topomatch, "output/topomatch_geofeature_ay.gpkg", append = F)

# Constraint 4. Mutual proximity ------------------------------------------
topomatch <- st_read("output/topomatch_tf_all_ay.gpkg")
topomatch <- glottoconstraint_proximity(topomatch = topomatch, nn = 100) 
st_write(topomatch, "output/topomatch_tf_proximity100_ay.gpkg", append = F)
topomatch <- topomatch[topomatch$topomatch == TRUE, ]
st_write(topomatch, "output/topomatch_proximity100_ay.gpkg", append = F)

topomatch <- st_read("output/topomatch_tf_all_ay.gpkg")
topomatch <- glottoconstraint_proximity(topomatch = topomatch, nn = 75) 
st_write(topomatch, "output/topomatch_tf_proximity75_ay.gpkg", append = F)
topomatch <- topomatch[topomatch$topomatch == TRUE, ]
st_write(topomatch, "output/topomatch_proximity75_ay.gpkg", append = F)

topomatch <- glottoconstraint_proximity(topomatch = topomatch, nn = 50) 
st_write(topomatch, "output/topomatch_tf_proximity50_ay.gpkg", append = F)
topomatch <- topomatch[topomatch$topomatch == TRUE, ]
st_write(topomatch, "output/topomatch_proximity50_ay.gpkg", append = F)

topomatch <- st_read("output/topomatch_tf_all_ay.gpkg")
topomatch <- glottoconstraint_proximity(topomatch = topomatch, nn = 25) 
st_write(topomatch, "output/topomatch_tf_proximity25_ay.gpkg", append = F)
topomatch <- topomatch[topomatch$topomatch == TRUE, ]
st_write(topomatch, "output/topomatch_proximity25_ay.gpkg", append = F)


