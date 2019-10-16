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
# Create Lag Variables for California
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

Alamedamap <-
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
