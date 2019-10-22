# ==========================================================================
# Develop data for displacement and vulnerability measures
# Initial Author: Tim Thomas
# Created: 2019.10.13
# ==========================================================================

#
# Tasks
# 1. Create measure extraction functions
# 	* need to get the tracts that fall within various pumas.
# See: https://docs.google.com/document/d/1JvqZLnh_sZIgfrxaXPtBSJRbTEPcbx-jl_tvf5JlqiU/edit
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

# Download CA tract data for select variables
	ct_data17 <-
		get_acs(
			geography = "tract",
			variables = dis_var,
			state = "CA",
			county = NULL,
			geometry = FALSE,
			cache_table = TRUE,
			output = "wide",
			year = 2017) %>%
		select(-ends_with("M"))

	ct_data12 <-
		get_acs(
			geography = "tract",
			variables = dis_var,
			state = "CA",
			county = NULL,
			geometry = FALSE,
			cache_table = TRUE,
			output = "wide",
			year = 2012) %>%
		select(-ends_with("M"))

# Download CA block group data for select variables
	cbg_data17 <-
		get_acs(
			geography = "block group",
			variables = dis_var,
			state = "CA",
			county = NULL,
			geometry = FALSE,
			cache_table = TRUE,
			output = "wide",
			year = 2017) %>%
		select(-ends_with("M"))

### Block groups for 2008 to 2012 are not downloading
	cbg_data12 <-
		get_acs(
			geography = "block group",
			variables = dis_var,
			state = "CA",
			county = NULL,
			geometry = FALSE,
			cache_table = TRUE,
			output = "wide",
			year = 2012) %>%
		select(-ends_with("M"))

#
# PUMA data - (ON HOLD)
# --------------------------------------------------------------------------
	# cp_data <-
	# 	get_acs(
	# 		geography = "public use microdata area",
	# 		variables = dis_var,
	# 		state = "CA",
	# 		county = NULL,
	# 		geometry = FALSE,
	# 		cache_table = TRUE,
	# 		output = "wide"
	# 		) %>%
	# 	select(-ends_with("M"))


#
# County data
# --------------------------------------------------------------------------
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

#
# Download CA tracts as ESRI shapefiles - no data (will join below)
# --------------------------------------------------------------------------

# Download shapefile
	ct <-
		tracts(state = "CA")	

	cbg <-
		block_groups(state = "CA")

# create county join ID
	ct@data <-
		ct@data %>%
		mutate(county = paste0(STATEFP, COUNTYFP))

# create county join ID
	cbg@data <-
		cbg@data %>%
		mutate(county = paste0(STATEFP, COUNTYFP))

# Create new Cal object for posterity of OG ct object
	cal_tracts <- ct
	cal_bg <- cbg

# Join County and ACS tract data to cal_tracts shapefile
	cal_tracts@data <-
		left_join(cal_tracts@data,
				  cc_data,
				  by = c("county" = "GEOID")) %>%
		left_join(.,
				  ct_data12,
				  by = "GEOID") %>%
		left_join(.,
				  ct_data17,
				  by = "GEOID")


#
# Create household income breaks (how many households fall within certain
# 	thresholds)
# --------------------------------------------------------------------------

newnames <-
	c('HHInc_10E.x' = 10000,'HHInc_15E.x' = 14999,'HHInc_20E.x' = 19999,'HHInc_25E.x' = 24999,'HHInc_30E.x' = 29999,'HHInc_35E.x' = 34999,'HHInc_40E.x' = 39999,'HHInc_45E.x' = 44999,'HHInc_50E.x' = 49999,'HHInc_60E.x' = 59999,'HHInc_75E.x' = 74999,'HHInc_100E.x' = 99999,'HHInc_125E.x' = 124999,'HHInc_150E.x' = 149999,'HHInc_200E.x' = 199999,'HHInc_250E.x' = 200000)

	hhinc12 <-
		cal_tracts@data %>%
		select(GEOID, county, co_medinc, HHInc_TotalE.x:HHInc_250E.x) %>%
		group_by(GEOID) %>%
		mutate(LI_val = co_medinc*.8,
			   VLI_val = co_medinc*.5,
			   ELI_val = co_medinc*.3) %>%
		gather(medinc_cat, medinc_cat_count, HHInc_10E.x:HHInc_250E.x) %>%
		mutate_at(vars(medinc_cat), ~newnames) %>%
		mutate(LI = if_else(ELI_val >= medinc_cat, 1, 0),
			   VLI = if_else(VLI_val >= medinc_cat, 1, 0),
			   ELI = if_else(ELI_val >= medinc_cat, 1, 0)) %>%


	setnames(hhinc12, old = )


		%>%
		rename_at(vars)
		rename_at(vars(HHInc_10E.x:HHInc_250E.x), list(renames))

### LEFT OFF ###
# Note, EL Measures
# Rent gap
# Change in rent
# Educational attainment
# Bachelors
# Income segregation by race
# Rent burden (high local, low nearby)
###



#
# Mutate select tract variables
# --------------------------------------------------------------------------

	cal@data <-
		cal@data %>%
		mutate(
			   pRenters12 = totrentE.x/tottenE.x,
			   pRenters17 = totrentE.y/tottenE.y,
			   )

# ==========================================================================
# Create lag variables
# ==========================================================================

#
# Create neighbor matrix
# --------------------------------------------------------------------------
	coords <- coordinates(cal)
	IDs <- row.names(as(cal, "data.frame"))
	cal_nb <- poly2nb(cal) # nb
	lw_bin <- nb2listw(cal_nb, style = "B", zero.policy = TRUE)
	kern1 <- knn2nb(knearneigh(coords, k = 1), row.names=IDs)
	dist <- unlist(nbdists(kern1, coords)); summary(dist)
	max_1nn <- max(dist)
	dist_nb <- dnearneigh(coords, d1=0, d2 = .1*max_1nn, row.names = IDs)
	### listw object
	set.ZeroPolicyOption(TRUE)
	set.ZeroPolicyOption(TRUE)
	dists <- nbdists(dist_nb, coordinates(cal))
	idw <- lapply(dists, function(x) 1/(x^2))
	lw_dist_idwW <- nb2listw(dist_nb, glist = idw, style = "W")

#
# Create select lag variables
# --------------------------------------------------------------------------

	ct$medrent.lag <- lag.listw(lw_dist_idwW,ct$medrentE)
	# ct$pPOV00.lag <- lag.listw(lw_dist_idwW,ct$pPOV00)
	# ct$pPOV15.lag <- lag.listw(lw_dist_idwW,ct$pPOV15)
	# ct$pRB30Plus_15.lag <- lag.listw(lw_dist_idwW,ct$pRB30Plus_15)
	# ct$pRB50Plus_15.lag <- lag.listw(lw_dist_idwW,ct$pRB50Plus_15)

alameda <-
	tracts(state = "CA",
		   county = "Alameda")

alameda@data <-
	left_join(alameda@data,
			  ct@data) %>%
	mutate(medrentE = ifelse(medrentE == 0, NA, medrentE)) %>%
	group_by(COUNTYFP) %>%
	mutate(co_medrent.lag = median(medrent.lag),
		   lag_rent_scale = scale(medrent.lag),
		   tract_diffrent_loc.lag = medrentE - co_medrent.lag,
		   tr_rentdiff_loc.lagscale = scale(tract_diffrent_loc.lag))

	# glimpse(wa_tracts@data)

	tmap_mode("view")

alamedamap <-
	tm_shape(alameda) +
		# tm_polygons(c("medrentE", "medrent.lag", "tr_rentdiff_loc.lagscale"), breaks = c(-3, -1, 1, 4)) +
		# tm_facets(sync = TRUE, ncol = 2, nrow = 2) +
		tm_polygons("tr_rentdiff_loc.lagscale",
				breaks = c(-Inf,-2, -1, 1, Inf),
				title = "Local and Lag Median Rent\nDifference in\nStandard Deviations\n(2017)",
				palette="RdBu",
				id = "NAMELSAD",
				popup.vars = c("Median Rent" = "medrentE",
							   "Lag Med Rent" = "medrent.lag",
				 			   "SD Diff" = "tr_rentdiff_loc.lagscale"),
				popup.format = list(digits=2)) +
		tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 12), alpha = .5)

alamedamap

cal <- ct

cal@data <-
	cal@data %>%
	mutate(medrentE = ifelse(medrentE == 0, NA, medrentE)) %>%
	group_by(COUNTYFP) %>%
	mutate(co_medrent.lag = median(medrent.lag),
		   lag_rent_scale = scale(medrent.lag),
		   tract_diffrent_loc.lag = medrentE - co_medrent.lag,
		   tr_rentdiff_loc.lagscale = scale(tract_diffrent_loc.lag))

calmap <-
	tm_shape(cal) +
		# tm_polygons(c("medrentE", "medrent.lag", "tr_rentdiff_loc.lagscale"), breaks = c(-3, -1, 1, 4)) +
		# tm_facets(sync = TRUE, ncol = 2, nrow = 2) +
		tm_fill("tr_rentdiff_loc.lagscale",
				breaks = c(-Inf,-2, -1, 1, Inf),
				title = "Local and Lag Median Rent\nDifference in\nStandard Deviations\n(2017)",
				palette="RdBu",
				id = "NAMELSAD",
				popup.vars = c("Median Rent" = "medrentE",
							   "Lag Med Rent" = "medrent.lag",
				 			   "SD Diff" = "tr_rentdiff_loc.lagscale"),
				popup.format = list(digits=2)) +
		tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 8), alpha = .5)

calmap

#
# tm_fill() = polygon fill
# tm_border() = add borders
# tm_polygons() = add both borders and fill
# --------------------------------------------------------------------------

# ==========================================================================
# Check with washington
# ==========================================================================

	wt_data <-
		get_acs(
			geography = "tract",
			variables = dis_var,
			state = "WA",
			county = NULL,
			geometry = FALSE,
			cache_table = TRUE,
			output = "wide") %>%
		select(-ends_with("M"))

	wp_data <-
		get_acs(
			geography = "public use microdata area",
			variables = dis_var,
			state = "WA",
			county = NULL,
			geometry = FALSE,
			cache_table = TRUE,
			output = "wide"
			) %>%
		select(-ends_with("M"))

	wc_data <-
		get_acs(
			geography = "county",
			variables = dis_var,
			state = "WA",
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

wt <-
	tracts(state = "WA")

wt@data <-
	left_join(wt@data,
			  wt_data,
			  by = "GEOID")

wt@data[is.na(wt@data)] <- 0

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
# Create Lag Variables for California
# --------------------------------------------------------------------------

	coords <- coordinates(wt)
	IDs <- row.names(as(wt, "data.frame"))
	wt_nb <- poly2nb(wt) # nb
	lw_bin <- nb2listw(wt_nb, style = "B", zero.policy = TRUE)
	kern1 <- knn2nb(knearneigh(coords, k = 1), row.names=IDs)
	dist <- unlist(nbdists(kern1, coords)); summary(dist)
	max_1nn <- max(dist)
	dist_nb <- dnearneigh(coords, d1=0, d2 = .1*max_1nn, row.names = IDs)
	### listw object
	set.ZeroPolicyOption(TRUE)
	set.ZeroPolicyOption(TRUE)
	dists <- nbdists(dist_nb, coordinates(wt))
	idw <- lapply(dists, function(x) 1/(x^2))
	lw_dist_idwW <- nb2listw(dist_nb, glist = idw, style = "W")

### lag values
	wt$medrent.lag <- lag.listw(lw_dist_idwW,wt$medrentE)
	# wt$pPOV00.lag <- lag.listw(lw_dist_idwW,wt$pPOV00)
	# wt$pPOV15.lag <- lag.listw(lw_dist_idwW,wt$pPOV15)
	# wt$pRB30Plus_15.lag <- lag.listw(lw_dist_idwW,wt$pRB30Plus_15)
	# wt$pRB50Plus_15.lag <- lag.listw(lw_dist_idwW,wt$pRB50Plus_15)

king <-
	tracts(state = "WA",
		   county = "King")

king@data <-
	left_join(king@data,
			  wt@data) %>%
	mutate(medrentE = ifelse(medrentE == 0, NA, medrentE)) %>%
	group_by(COUNTYFP) %>%
	mutate(co_medrent.lag = median(medrent.lag),
		   lag_rent_scale = scale(medrent.lag),
		   tract_diffrent_loc.lag = medrentE - co_medrent.lag,
		   tr_rentdiff_loc.lagscale = scale(tract_diffrent_loc.lag))

	# glimpse(wa_tracts@data)

	tmap_mode("view")
	tm_shape(king) +
		# tm_polygons(c("medrentE", "medrent.lag", "tr_rentdiff_loc.lagscale"), breaks = c(-3, -1, 1, 4)) +
		# tm_facets(sync = TRUE, ncol = 2, nrow = 2) +
		tm_polygons("tr_rentdiff_loc.lagscale",
				breaks = c(-Inf,-2, -1, 1, Inf),
				title = "Local and Lag Median Rent\nDifference in\nStandard Deviations\n(2017)",
				palette="RdBu",
				id = "NAMELSAD",
				popup.vars = c("Median Rent" = "medrentE",
							   "Lag Med Rent" = "medrent.lag",
				 			   "SD Diff" = "tr_rentdiff_loc.lagscale"),
				popup.format = list(digits=2)) +
		tm_view(set.view = c(lon = -122.3321, lat = 47.6062, zoom = 12), alpha = .5)

# ==========================================================================
# Demo other measures
# ==========================================================================

#
# Use PUMS data for denominator
# --------------------------------------------------------------------------

	test <- pumas(state = "CA")
