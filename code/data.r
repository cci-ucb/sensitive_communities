# ==========================================================================
# Develop data for displacement and vulnerability measures
# Initial Author: Tim Thomas
# Created: 2019.10.13
# ==========================================================================

#
# Tasks
# 1. Create measure extraction functions
# 	* get county medians
# --------------------------------------------------------------------------

# ==========================================================================
# Libraries
# ==========================================================================

library(data.table)
library(tidyverse)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
library(tidycensus)
library(tmap)

# ==========================================================================
# Data
# ==========================================================================

#
# State tract data
# --------------------------------------------------------------------------

# Pull in variables 
	source("~/git/sensitive_communities/code/vars.r")

# Download CA tracts as ESRI shapefiles
	caltracts <- 
		tracts(state = "CA")

# Download CA tract data for select variables
	caldata <-
		get_acs(
			geography = "tract",
			variables = dis_var,
			state = "CA",
			county = NULL,
			geometry = FALSE,
			cache_table = TRUE,  
			output = "wide") %>%
		select(-ends_with("M")) 

caltracts@data <- 
	left_join(caltracts@data, 
			  caldata, 
			  by = "GEOID")


#
# Create Lag Variables
# --------------------------------------------------------------------------

	coords <- coordinates(caltracts)
	IDs <- row.names(as(caltracts, "data.frame"))
	caltracts_nb <- poly2nb(caltracts) # nb
	lw_bin <- nb2listw(caltracts_nb, style = "B",zero.policy = TRUE)
	kern1 <- knn2nb(knearneigh(coords, k = 1), row.names=IDs)
	dist <- unlist(nbdists(kern1, coords)); summary(dist)
	max_1nn <- max(dist)
	dist_nb <- dnearneigh(coords, d1=0, d2 = .75*max_1nn, row.names = IDs)
	### listw object
	set.ZeroPolicyOption(TRUE)
	set.ZeroPolicyOption(TRUE)
	dists <- nbdists(dist_nb, coordinates(caltracts))
	idw <- lapply(dists, function(x) 1/(x^2))
	lw_dist_idwW <- nb2listw(dist_nb, glist = idw, style = "W")

### lag values
	caltracts$pED15.lag <- lag.listw(lw_dist_idwW,caltracts$pED15)
	caltracts$pPOV00.lag <- lag.listw(lw_dist_idwW,caltracts$pPOV00)
	caltracts$pPOV15.lag <- lag.listw(lw_dist_idwW,caltracts$pPOV15)
	caltracts$pRB30Plus_15.lag <- lag.listw(lw_dist_idwW,caltracts$pRB30Plus_15)
	caltracts$pRB50Plus_15.lag <- lag.listw(lw_dist_idwW,caltracts$pRB50Plus_15)

	glimpse(wa_tracts@data)