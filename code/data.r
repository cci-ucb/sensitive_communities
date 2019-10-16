# ==========================================================================
# Develop data for displacement and vulnerability measures
# Initial Author: Tim Thomas
# Created: 2019.10.13
# ==========================================================================

#
# Tasks
# 1. Create measure extraction functions
# 	* need to get the tracts that fall within various pumas. 
# --------------------------------------------------------------------------

# ==========================================================================
# Libraries
# ==========================================================================

library(data.table)
library(spdep)
library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)
library(tidycensus)
library(tmap)

# ==========================================================================
# Data
# ==========================================================================

	rm(list = ls())
	options(scipen = 10) # avoid scientific notation


#
# State tract data
# --------------------------------------------------------------------------

# Pull in variables 
	source("~/git/sensitive_communities/code/vars.r")

# Download CA tracts as ESRI shapefiles

	cp <- 
		pumas(state = "CA")

# Download CA tract data for select variables
	ct_data <-
		get_acs(
			geography = "tract",
			variables = dis_var,
			state = "CA",
			county = NULL,
			geometry = FALSE,
			cache_table = TRUE,  
			output = "wide") %>%
		select(-ends_with("M")) 

	cp_data <- 
		get_acs(
			geography = "public use microdata area", 
			variables = dis_var, 
			state = "CA", 
			county = NULL, 
			geometry = FALSE, 
			cache_table = TRUE, 
			output = "wide"
			) %>%
		select(-ends_with("M")) 

	cc_data <- 
		get_acs(
			geography = "county", 
			variables = dis_var, 
			state = "CA", 
			county = NULL, 
			geometry = FALSE, 
			cache_table = TRUE, 
			output = "wide"
			) %>%
		group_by(GEOID) %>% 
		summarise(
			   co_medinc = mhhincE, 
			   co_li = co_medinc*.8, 
			   co_vli = co_medinc*.5, 
			   co_eli = co_medinc*.3, 
			   co_rentprop = totrentE/tottenE, 
			   co_rb30 = (rb_34.9E+rb_39.9E+rb_49.9E+rb_55E)/rb_totE, 
			   co_rb50 = rb_55E/rb_totE, 
			   co_medrent = medrentE)

ct <- 
	tracts(state = "CA")

ct@data <- 
	left_join(ct@data, 
			  ct_data, 
			  by = "GEOID")

ct@data[is.na(ct@data)] <- 0

# ct_data <- 
# 	left_join(ct_data, 
# 			  ct@data,
# 			  by = c("GEOID" = "COUNTYFP")) %>% 
# 	left_join(., 
# 			  cc_data)
	


#
# Create share of low income households: 50% ami > county median
# --------------------------------------------------------------------------

# County 


#
# Create Lag Variables
# --------------------------------------------------------------------------

	coords <- coordinates(ct)
	IDs <- row.names(as(ct, "data.frame"))
	ct_nb <- poly2nb(ct) # nb
	lw_bin <- nb2listw(ct_nb, style = "B", zero.policy = TRUE)
	kern1 <- knn2nb(knearneigh(coords, k = 1), row.names=IDs)
	dist <- unlist(nbdists(kern1, coords)); summary(dist)
	max_1nn <- max(dist)
	dist_nb <- dnearneigh(coords, d1=0, d2 = .1*max_1nn, row.names = IDs)
	### listw object
	set.ZeroPolicyOption(TRUE)
	set.ZeroPolicyOption(TRUE)
	dists <- nbdists(dist_nb, coordinates(ct))
	idw <- lapply(dists, function(x) 1/(x^2))
	lw_dist_idwW <- nb2listw(dist_nb, glist = idw, style = "W")

### lag values
	ct$medrent.lag <- lag.listw(lw_dist_idwW,ct$medrentE)
	# ct$pPOV00.lag <- lag.listw(lw_dist_idwW,ct$pPOV00)
	# ct$pPOV15.lag <- lag.listw(lw_dist_idwW,ct$pPOV15)
	# ct$pRB30Plus_15.lag <- lag.listw(lw_dist_idwW,ct$pRB30Plus_15)
	# ct$pRB50Plus_15.lag <- lag.listw(lw_dist_idwW,ct$pRB50Plus_15)

	# glimpse(wa_tracts@data)

	tmap_mode("view")
	tm_shape(ct) + 
		tm_polygons(c("medrentE", "medrent.lag"), alphs = .5) + 
		tm_facets(sync = TRUE, ncol = 2) + 
		tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 5))
