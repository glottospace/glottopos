
# Overview ----------------------------------------------------------------
# Script associated with the publication: Integrating Linguistic and Geographic Methods in Toponymic Analysis: The Case of Puquina (Central Andes), Diachronica
# Authors: Sietze Norder and Nick Emlen
# Aim of this script: define functions for toponymic analysis

# Required packages -------------------------------------------------------
library(sf)
library(rnaturalearth)
library(spatstat)
library(raster)
library(terra)
library(tmap)
library(readxl)
library(dplyr)
library(RANN)
library(digest)
library(Matrix)
`%nin%` <- Negate('%in%')


# Data preparation --------------------------------------------------------
# Convert geonames text files to sf objects
geonamestxt2sf <- function(file) {
  df <- read.delim(file, sep = "\t", header = T)
  sf_obj <- st_as_sf(df, coords = c("long_dd", "lat_dd"), crs = 4326)
  sf_obj <- sf_obj[,c("full_nm_nd","desig_cd")]
  colnames(sf_obj)[c(1,2)] <- c("toponym", "geofeature")
  sf_obj$toponym <- tolower(sf_obj$toponym)
  return(sf_obj)
}

# Create empty structure to store results
glottocreate_toponymresdf <- function(geonames, textmatch){
  txtm <- textmatch
  txtm[[1]] <- gsub("#", "", txtm[[1]])
  resdf <- data.frame(matrix(ncol = nrow(txtm), nrow = nrow(geonames) ))
  colnames(resdf) <- as.vector(unlist(txtm[,1]))
  geonames <- cbind(geonames, resdf)
  colnames(geonames) <- sub("\\.", "", colnames(geonames))
  return(geonames)
}

glottoclean_geonames <- function(geonamesdata, boundarypol){
  geonamesdata <- st_transform(geonamesdata, st_crs(boundarypol))
  within_indices <- st_within(geonamesdata, boundarypol) 
  geonamesdata_within <- geonamesdata[lengths(within_indices) > 0, ]
  return(geonamesdata_within)
}

# Constraint 1: Morphological position ------------------------------------
glottoconstraint_topomatch <- function(topomatch, textmatch, position = FALSE){
  # Check for each toponym whether there is a match with textmatch. Either anywhere in the name or taking into account position.
  # This function only identifies matches, but does not subset them. 

  txtm <- textmatch
  txtm[[1]] <- gsub("#", "", txtm[[1]])
  colnames(txtm)[1] <- "root"
  
  topocols <- names(topomatch)[names(topomatch) %nin% c("toponym", "geofeature", "geom", "toposcore", "topomatch")]
  for (i in topocols){
    ovc <- unlist(strsplit(txtm$orthog_variants_normal_char[txtm$root == i], ","))
    if(position == FALSE){
      pattern <- paste(ovc, collapse = "|")
    } else{
      if(txtm$initial[txtm$root == i] == 1 & txtm$final[txtm$root == i] == 0){
        pattern <- paste0("^(", paste(ovc, collapse="|"), ")")
      }
      if(txtm$initial[txtm$root == i] == 0 & txtm$final[txtm$root == i] == 1){
        pattern <- paste0("(", paste(ovc, collapse="|"), ")$")
      }
      if(txtm$initial[txtm$root == i] == 1 & txtm$final[txtm$root == i] == 1){
        pattern <- paste0("^(", paste(ovc, collapse="|"), ")|(", paste(ovc, collapse="|"), ")$")
      }
    }
    topomatch[,i] <- ifelse(grepl(pattern, topomatch$toponym, ignore.case = TRUE), txtm$phonemes[txtm$root == i], 0)
  }
  topomatch$topomatch <- rowSums(st_drop_geometry(topomatch[, topocols, drop = FALSE]), na.rm = TRUE) > 0
  cat("Identified", sum(topomatch$topomatch), "topomatches. \n") 
  
return(topomatch)
}

# Constraint 2. Length ----------------------------------------------------
# Check for each toponym whether the length of textmatch is larger or equal than minlength. 
# This function only identifies matches, but does not subset them. 
glottoconstraint_length <- function(topomatch, minlength){
  topocols <- names(topomatch)[names(topomatch) %nin% c("toponym", "geofeature", "geom", "toposcore", "topomatch")]
  topomatch$toposcore <- rowSums(st_drop_geometry(topomatch[,topocols]))
  topomatch$topomatch <- ifelse(topomatch$toposcore >= minlength, TRUE, FALSE) 
  cat("Topomatches of the following lenghts are present: ", sort(unique(topomatch$toposcore), decreasing = T) , "\n")
  cat("Identified", sum(topomatch$topomatch), "topomatches with at least", minlength, "matching characters", "\n") 
  return(topomatch)
}

# Constraint 3: Geographic feature ----------------------------------------
glottoconstraint_geofeature <- function(topomatch, textmatch){
  topomatch <- topomatch[topomatch$topomatch == TRUE,]
  topocols <- names(topomatch)[names(topomatch) %nin% c("toponym", "geofeature", "geom", "toposcore", "topomatch")]

  # Subset textmatches where geofeature is known
  txtm <- textmatch
  txtm[[1]] <- gsub("#", "", txtm[[1]])
  colnames(txtm)[1] <- "root"
  txtm <- txtm %>% filter(!is.na(geofeature) & geofeature != "NA")

  # Create temporary data.frame to store the results per geofeature
  geofeaturesdf <- data.frame(matrix(FALSE, nrow = nrow(topomatch), ncol = length(txtm$root)))
  names(geofeaturesdf) <- paste0("geofeature_", txtm$root)
  
  # Check match separately for each root
  for(r in txtm$root){
    txtm_r <- txtm %>% filter(root == r)
    geofeatures <- unlist(strsplit(txtm_r$geofeature, ","))
    tfr <- st_drop_geometry(topomatch[,r]) > 0
    geofeaturesdf[tfr,paste0("geofeature_", r)] <- topomatch$geofeature[tfr] %in% geofeatures
  }

  topomatch$topomatch <- rowSums(geofeaturesdf) > 0

  cat("Identified", sum(topomatch$topomatch), "topomatches. \n")
  return(topomatch)

}


# Constraint 4: Mutual proximity ------------------------------------------
glottocreate_nn <- function(topomatch, nn){
  coords <- st_coordinates(topomatch)
  knn_res <- nn2(coords, k = nn+1) 
  # Extract neighbor
  nni <- knn_res$nn.idx[, -1]  # Remove the first column (the point itself)
  nni <- as.data.frame(nni)
  colnames(nni) <- paste0("nn", 1:ncol(nni))
  nnsf <- cbind(topomatch, nni)
  return(nnsf)
}

glottoconstraint_proximity <- function(topomatch = NULL, nn = NULL){
  # This function is applied on a subset of data points and includes only those that have already been identified as topomatch. The reason for this is to reduce computation time. 
  topomatch_nn <- glottocreate_nn(topomatch = topomatch, nn = nn)
  # Subset topomatches 
  topomatches <- rownames(topomatch_nn[topomatch_nn$topomatch == TRUE,])
  tm_id <- sort(unique(as.numeric(topomatches)))
  topomatch_nnsel <- topomatch_nn[tm_id,] # Subset topomatches
  # Identify nearest neighbors (within nn) of topomatches that are also a topomatch themselves
  nncols <- grep("nn", colnames(topomatch_nnsel))
  nncolsdf <- st_drop_geometry(topomatch_nnsel[,nncols])
  # Select only neighbors that are also a topomatch (to save computation time)
  topomatch_nnsel[,nncols] <- nncolsdf %>%
    mutate(across(everything(), ~ ifelse(. %in% tm_id, ., 0)))
  
  # Delete nearest neighbors that are fully identical --------------------------------------------------
  topocols <- names(topomatch)[names(topomatch) %nin% c("toponym", "geofeature", "geom", "toposcore", "topomatch")]
  topocolsdf <- st_drop_geometry(topomatch_nnsel[,topocols])
  # Create unique hash for each row to speed up computation time
  df <- topocolsdf %>%
    mutate(row_hash = apply(., 1, digest))
  # Create a Sparse logical matrix indicating row equality (FALSE when rows are identical)
  # identical_matrix <- outer(df$row_hash, df$row_hash, `==`)
  # # Alternatively, sparse matrix to reduce memory usage
  identical_matrix <- Matrix(outer(df$row_hash, df$row_hash, `!=`), sparse = TRUE)
  
  # Convert to list showing only those IDs that are different (i.e. FALSE). 
  # Matrix shows TRUE if identical, FALSE if different
  # colnames(identical_matrix) <- rownames(topocolsdf)
  # difftopo_ls <- apply(identical_matrix, 1, function(row) {
  #   colnames(identical_matrix)[!row]
  # })
  # Unquote in case of logical matrix:
  difftopo_ls <- apply(identical_matrix, 1, function(row) {
    colnames(identical_matrix)[row]
  })
  
  # List shows for each row, the other rows that differ.
  if(!is.null(names(difftopo_ls))){
  names(difftopo_ls) <- rownames(topocolsdf)
  } else{
    stop("No topomatches that meet this criterion (i.e. no other different toponym with this number of neighbors")
  }

  
  nrtm <- nrow(topomatch_nnsel)
  for (i in 1:nrow(topomatch_nnsel)) {
    rn <- rownames(topomatch_nnsel)[i]
    ri <- st_drop_geometry(topomatch_nnsel[i,nncols])
    if(sum(ri > 0) > 0){ # Only do rows with nn that are also a topomatch
      # If a neighbor is within difftopo, it is different, and we therefore want to keep that nearest neighbor. Otherwise we replace it with 0.
      diffnn <- unlist(ri) %in% as.numeric(difftopo_ls[rn][[1]])
      # ifelse is slow:
      # topomatch_nnsel[i,nncols] <- ifelse(diffnn, unlist(ri), 0)
      # Faster alternative:
      ri_vec <- as.numeric(ri)  # or unlist(ri)
      ri_vec[!diffnn] <- 0
      topomatch_nnsel[i, nncols] <- ri_vec
      
      if (i %% 100 == 0) {
        cat(round((i/nrtm*100), 1), "%\n", sep = "")
      }
    }
  }
  topomatch_nnsel$topomatch <- rowSums(st_drop_geometry(topomatch_nnsel[,nncols]) ) > 0
  cat(" \n \n \n \n Identified", sum(topomatch_nnsel$topomatch), "topomatches. \n \n \n \n") 
  return(topomatch_nnsel)
}


