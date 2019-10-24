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

# Pull in variables list
	source("~/git/sensitive_communities/code/vars.r")

# Download CA tract data for select variables
	tr_data17 <-
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

	tr_data12 <-
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

#
# County data
# --------------------------------------------------------------------------
	county_acsdata <- function(year)
		get_acs(
			geography = "county",
			variables = dis_var,
			state = "CA",
			county = NULL,
			geometry = FALSE,
			cache_table = TRUE,
			output = "wide",
			year = year
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
			   co_medrent = medrentE,
			   co_toted = totedE,
			   co_bachplus = sum(bachE, masE, proE, docE),
			   co_meded = median(co_bachplus/co_toted))

	co_data <-
		left_join(county_acsdata(2012), county_acsdata(2017), by = "GEOID")

#
# Download CA tracts as ESRI shapefiles with no data (will join below)
# --------------------------------------------------------------------------

# Download shapefile
	ct <-
		tracts(state = "CA")


# create county join ID
	ct@data <-
		ct@data %>%
		mutate(county = paste0(STATEFP, COUNTYFP))

# Create new Cal object for posterity of OG ct object
	cal_tracts <- ct

# Join County and ACS tract data to cal_tracts shapefile
	cal_tracts@data <-
		left_join(cal_tracts@data,
				  co_data,
				  by = c("county" = "GEOID")) %>%
		left_join(.,
				  tr_data12,
				  by = "GEOID") %>%
		left_join(.,
				  tr_data17,
				  by = "GEOID")

# ==========================================================================
# Create variables
# ==========================================================================

#
# County Share of ELI households -
#	Create household income breaks (how many households fall within certain
# 	thresholds)
# --------------------------------------------------------------------------

newnames12 <-
	c('HHInc_10E.x' = 9999,'HHInc_15E.x' = 14999,'HHInc_20E.x' = 19999,'HHInc_25E.x' = 24999,'HHInc_30E.x' = 29999,'HHInc_35E.x' = 34999,'HHInc_40E.x' = 39999,'HHInc_45E.x' = 44999,'HHInc_50E.x' = 49999,'HHInc_60E.x' = 59999,'HHInc_75E.x' = 74999,'HHInc_100E.x' = 99999,'HHInc_125E.x' = 124999,'HHInc_150E.x' = 149999,'HHInc_200E.x' = 199999,'HHInc_250E.x' = 199999)

	trli12 <-
		cal_tracts@data %>%
		select(GEOID, COUNTYFP, co_medinc.x, HHInc_TotalE.x:HHInc_250E.x) %>%
		group_by(GEOID) %>%
		mutate(LI_val = co_medinc.x*.8,
			   VLI_val = co_medinc.x*.5,
			   ELI_val = co_medinc.x*.3) %>%
		gather(medinc_cat, medinc_cat_count, HHInc_10E.x:HHInc_250E.x) %>%
		mutate_at(vars(medinc_cat), ~newnames12) %>%
		mutate(bottom_inccat = if_else(medinc_cat <= 9999, medinc_cat - 9999,
							   if_else(medinc_cat <= 49999 &
							   		   medinc_cat >= 10000, medinc_cat - 4999,
								if_else(medinc_cat >= 50000 &
										medinc_cat <= 99999, medinc_cat -24999,
								if_else(medinc_cat >= 100000 &
										medinc_cat <= 199999, medinc_cat - 49999,
								NA_real_)))),
			   top_inccat = medinc_cat) %>%
		mutate(LI = if_else(LI_val >= top_inccat, 1,
					if_else(LI_val <= top_inccat &
			   					LI_val >= bottom_inccat,
			   					(LI_val - bottom_inccat)/(top_inccat - bottom_inccat), 0)),
			   VLI = if_else(VLI_val >= top_inccat, 1,
					 if_else(VLI_val <= top_inccat &
			   					VLI_val >= bottom_inccat,
			   					(VLI_val - bottom_inccat)/(top_inccat - bottom_inccat), 0)),
			   ELI = if_else(ELI_val >= top_inccat, 1,
					 if_else(ELI_val <= top_inccat &
			   					ELI_val >= bottom_inccat,
			   					(ELI_val - bottom_inccat)/(top_inccat - bottom_inccat), 0)),
			   tr_totalinc12 = sum(medinc_cat_count, na.rm = TRUE),
			   tr_LI_count12 = sum(LI*medinc_cat_count, na.rm = TRUE),
			   tr_VLI_count12 = sum(VLI*medinc_cat_count, na.rm = TRUE),
			   tr_ELI_count12 = sum(ELI*medinc_cat_count, na.rm = TRUE),
			   tr_LI_prop12 = tr_LI_count12/tr_totalinc12,
			   tr_VLI_prop12 = tr_VLI_count12/tr_totalinc12,
			   tr_ELI_prop12 = tr_ELI_count12/tr_totalinc12) %>%
		select(GEOID, COUNTYFP, LI_val:ELI_val, tr_totalinc12:tr_ELI_prop12) %>%
		distinct() %>%
		ungroup()

newnames17 <-
	c('HHInc_10E.y' = 9999,'HHInc_15E.y' = 14999,'HHInc_20E.y' = 19999,'HHInc_25E.y' = 24999,'HHInc_30E.y' = 29999,'HHInc_35E.y' = 34999,'HHInc_40E.y' = 39999,'HHInc_45E.y' = 44999,'HHInc_50E.y' = 49999,'HHInc_60E.y' = 59999,'HHInc_75E.y' = 74999,'HHInc_100E.y' = 99999,'HHInc_125E.y' = 124999,'HHInc_150E.y' = 149999,'HHInc_200E.y' = 199999,'HHInc_250E.y' = 199999)

	trli17 <-
		cal_tracts@data %>%
		select(GEOID, COUNTYFP, co_medinc.y, HHInc_TotalE.y:HHInc_250E.y) %>%
		group_by(GEOID) %>%
		mutate(LI_val = co_medinc.y*.8,
			   VLI_val = co_medinc.y*.5,
			   ELI_val = co_medinc.y*.3) %>%
		gather(medinc_cat, medinc_cat_count, HHInc_10E.y:HHInc_250E.y) %>%
		mutate_at(vars(medinc_cat), ~newnames17) %>%
		mutate(bottom_inccat = if_else(medinc_cat <= 9999, medinc_cat - 9999,
							   if_else(medinc_cat <= 49999 &
							   		   medinc_cat >= 10000, medinc_cat - 4999,
								if_else(medinc_cat >= 50000 &
										medinc_cat <= 99999, medinc_cat -24999,
								if_else(medinc_cat >= 100000 &
										medinc_cat <= 199999, medinc_cat - 49999,
								NA_real_)))),
			   top_inccat = medinc_cat) %>%
		mutate(LI = if_else(LI_val >= top_inccat, 1,
					if_else(LI_val <= top_inccat &
			   					LI_val >= bottom_inccat,
			   					(LI_val - bottom_inccat)/(top_inccat - bottom_inccat), 0)),
			   VLI = if_else(VLI_val >= top_inccat, 1,
					 if_else(VLI_val <= top_inccat &
			   					VLI_val >= bottom_inccat,
			   					(VLI_val - bottom_inccat)/(top_inccat - bottom_inccat), 0)),
			   ELI = if_else(ELI_val >= top_inccat, 1,
					 if_else(ELI_val <= top_inccat &
			   					ELI_val >= bottom_inccat,
			   					(ELI_val - bottom_inccat)/(top_inccat - bottom_inccat), 0)),
			   tr_totalinc17 = sum(medinc_cat_count, na.rm = TRUE),
			   tr_LI_count17 = sum(LI*medinc_cat_count, na.rm = TRUE),
			   tr_VLI_count17 = sum(VLI*medinc_cat_count, na.rm = TRUE),
			   tr_ELI_count17 = sum(ELI*medinc_cat_count, na.rm = TRUE),
			   tr_LI_prop17 = tr_LI_count17/tr_totalinc17,
			   tr_VLI_prop17 = tr_VLI_count17/tr_totalinc17,
			   tr_ELI_prop17 = tr_ELI_count17/tr_totalinc17) %>%
		select(GEOID, COUNTYFP, LI_val:ELI_val,tr_totalinc17:tr_ELI_prop17) %>%
		distinct() %>%
		ungroup()

lidata <-
	left_join(trli12, trli17, by = c("GEOID", "COUNTYFP")) %>%
	group_by(COUNTYFP) %>%
	mutate(co_totalinc12 = sum(tr_totalinc12, na.rm = TRUE),
		   co_LI_count12 = sum(tr_LI_count12, na.rm = TRUE),
		   co_VLI_count12 = sum(tr_VLI_count12, na.rm = TRUE),
		   co_ELI_count12 = sum(tr_ELI_count12, na.rm = TRUE),
		   co_LI_prop12 = co_LI_count12/co_totalinc12,
		   co_VLI_prop12 = co_VLI_count12/co_totalinc12,
		   co_ELI_prop12 = co_ELI_count12/co_totalinc12,
		   co_LI_propmed12 = median(tr_LI_prop12, na.rm = TRUE),
		   co_VLI_propmed12 = median(tr_VLI_prop12, na.rm = TRUE),
		   co_ELI_propmed12 = median(tr_ELI_prop12, na.rm = TRUE),
		   co_totalinc17 = sum(tr_totalinc17, na.rm = TRUE),
		   co_LI_count17 = sum(tr_LI_count17, na.rm = TRUE),
		   co_VLI_count17 = sum(tr_VLI_count17, na.rm = TRUE),
		   co_ELI_count17 = sum(tr_ELI_count17, na.rm = TRUE),
		   co_LI_prop17 = co_LI_count17/co_totalinc17,
		   co_VLI_prop17 = co_VLI_count17/co_totalinc17,
		   co_ELI_prop17 = co_ELI_count17/co_totalinc17,
		   co_LI_propmed17 = median(tr_LI_prop17, na.rm = TRUE),
		   co_VLI_propmed17 = median(tr_VLI_prop17, na.rm = TRUE),
		   co_ELI_propmed17 = median(tr_ELI_prop17, na.rm = TRUE))

# glimpse(lidata)

#
# Change
# --------------------------------------------------------------------------


cal_tracts@data <-
	left_join(cal_tracts@data, lidata, by = c("GEOID", "COUNTYFP")) %>%
	group_by(GEOID) %>%
	mutate(tr_propstudent17 = sum(colenrollE.y, proenrollE.y)/totenrollE.y,
		   tr_rentprop12 = totrentE.x/tottenE.x,
		   tr_rentprop17 = totrentE.y/tottenE.y,
		   tr_rbprop12 = sum(rb_55E.x, na.rm = TRUE)/rb_totE.x,
		   tr_rbprop17 = sum(rb_55E.y, na.rm = TRUE)/rb_totE.y,
		   tr_medrent12 = medrentE.x*1.07,
		   tr_medrent17 = medrentE.y,
		   tr_chrent = tr_medrent17-tr_medrent12,
		   tr_propchrent = (tr_medrent17-tr_medrent12)/tr_medrent12,
		   tr_bachplus12 = sum(bachE.x, masE.x, proE.x, docE.x, na.rm = TRUE),
		   tr_bachplus17 = sum(bachE.y, masE.y, proE.y, docE.y, na.rm = TRUE),
		   tr_proped12 = if_else(tr_bachplus12 == 0 & totedE.x == 0,
		   						 0,
		   						 tr_bachplus12/totedE.x),
		   tr_proped17 = if_else(tr_bachplus17 == 0 & totedE.y == 0,
		   						 0,
		   						 tr_bachplus17/totedE.y),
		   tr_propched = if_else(tr_proped17 == 0 & tr_proped12 == 0,
		   						 0,
		   						 (tr_proped17 - tr_proped12)/(tr_proped12))) %>%
	group_by(COUNTYFP) %>%
	mutate(co_medrentprop12 = median(tr_rentprop12, na.rm = TRUE),
		   co_medrentprop17 = median(tr_rentprop17, na.rm = TRUE),
		   co_medrbprop12 = median(tr_rbprop12, na.rm = TRUE),
		   co_medrbprop17 = median(tr_rbprop17, na.rm = TRUE),
		   co_medrent12 = median(tr_medrent12, na.rm = TRUE),
		   co_medrent17 = median(tr_medrent17, na.rm = TRUE),
		   co_medchrent = median(tr_chrent, na.rm = TRUE),
		   co_medproped12 = median(tr_proped12, na.rm = TRUE),
		   co_medproped17 = median(tr_proped17, na.rm = TRUE),
		   co_medched = median(tr_propched, na.rm = TRUE)) %>%
	group_by(GEOID) %>%
	mutate(tr_medrent12 = if_else(is.na(tr_medrent12),
								  co_medrent12,
								  tr_medrent12),
		   tr_medrent17 = if_else(is.na(tr_medrent17),
		   						  co_medrent17,
		   						  tr_medrent17),
		   tr_chrent = if_else(is.na(tr_chrent),
		   						   co_medchrent,
		   						   tr_chrent),
		   tr_propchrent = (tr_medrent17-tr_medrent12)/tr_medrent12,
		   co_medpropchrent = median(tr_propchrent),
		   tr_propched = if_else(tr_propched == Inf,
		   						 (tr_proped17-tr_proped12)/mean(tr_proped17,tr_proped12),
		   						 tr_propched)) %>%
	ungroup()

# cal_tracts@data %>% filter(tr_propched >= 20) %>% glimpse()
# cal_tracts@data %>% summary()
# ==========================================================================
# Create lag variables
# ==========================================================================

#
# Create neighbor matrix
# --------------------------------------------------------------------------
	coords <- coordinates(cal_tracts)
	IDs <- row.names(as(cal_tracts, "data.frame"))
	cal_tracts_nb <- poly2nb(cal_tracts) # nb
	lw_bin <- nb2listw(cal_tracts_nb, style = "B", zero.policy = TRUE)
	kern1 <- knn2nb(knearneigh(coords, k = 1), row.names=IDs)
	dist <- unlist(nbdists(kern1, coords)); summary(dist)
	max_1nn <- max(dist)
	dist_nb <- dnearneigh(coords, d1=0, d2 = .1*max_1nn, row.names = IDs)
	### listw object
	spdep::set.ZeroPolicyOption(TRUE)
	spdep::set.ZeroPolicyOption(TRUE)
	dists <- nbdists(dist_nb, coordinates(cal_tracts))
	idw <- lapply(dists, function(x) 1/(x^2))
	lw_dist_idwW <- nb2listw(dist_nb, glist = idw, style = "W")

#
# Create select lag variables
# --------------------------------------------------------------------------

	cal_tracts$tr_chrent.lag <- lag.listw(lw_dist_idwW,cal_tracts$tr_chrent)
	cal_tracts$tr_propchrent.lag <- lag.listw(lw_dist_idwW,cal_tracts$tr_propchrent)
	cal_tracts$tr_propched.lag <- lag.listw(lw_dist_idwW,cal_tracts$tr_propched)
	cal_tracts$tr_medrent17.lag <- lag.listw(lw_dist_idwW,cal_tracts$tr_medrent17)

# ==========================================================================
# Categorize tracts
# ==========================================================================

cal_tracts_co <- cal_tracts

cal_tracts_co@data <-
	cal_tracts_co@data %>%
	group_by(COUNTYFP) %>%
	mutate(tr_rentgap = tr_medrent17.lag - tr_medrent17,
		   tr_rentgapsc = scale(tr_rentgap),
		   tr_rentgappropdiff = (tr_medrent17.lag - tr_medrent17)/((tr_medrent17 + tr_medrent17.lag)/2),
		   co_rentgap = median(tr_rentgap),
		   v_renters = if_else(tr_rentprop17 > co_medrentprop17, 1, 0),
		   v_ELI = if_else(tr_propstudent17 < .25 & tr_ELI_prop17 > co_ELI_prop17, 1, 0),
		   v_rb = if_else(tr_rbprop17 > co_medrbprop17, 1, 0),
		   dp_chrent_comed = if_else(tr_propchrent > co_medpropchrent | tr_propchrent.lag > co_medpropchrent, 1, 0),
		   dp_chrent_10 = if_else(tr_propchrent > .1 | tr_propchrent.lag > .1, 1, 0),
		   dp_ched = if_else(tr_propched > co_medched | tr_propched.lag > co_medched, 1, 0),
		   dp_rentgap_comed = if_else(tr_rentgap > co_rentgap, 1, 0),
		   dp_rentgap_10 = ifelse(tr_rentgappropdiff > .1, 1, 0)) %>%
	group_by(GEOID) %>%
	mutate(v_count = sum(v_ELI, v_rb),
		   dp_count_co = sum(dp_chrent_comed,
		   					 # dp_ched,
		   					 dp_rentgap_comed, na.rm = TRUE),
		   dp_count_10 = sum(dp_chrent_10,
		   					 # dp_ched,
		   					 dp_rentgap_10, na.rm = TRUE),
		   vulnerable = if_else(v_renters >= 1 & v_count >=1, 1, 0),
		   dis_press_co = if_else(dp_count_co >= 1, 1, 0),
		   dis_press_10 = if_else(dp_count_10 >= 1, 1, 0),
		   risk_co = if_else(vulnerable + dis_press_co >= 2, 1, 0),
		   risk_10 = if_else(vulnerable + dis_press_10 >= 2, 1, 0)) %>%
	ungroup()

cal_tracts_co@data %>%
	summarise(risk_co = sum(risk_co, na.rm = TRUE), risk_10 = sum(risk_10, na.rm = TRUE))

# ==========================================================================
# Map
# ==========================================================================

cal_tracts_10 <- cal_tracts_co

tmap_mode("view")

sc_map_co <-
tm_shape(cal_tracts_co) +
	tm_fill("risk_co",
			n = 2,
			title = "Tracts at Risk",
			id = "GEOID",
			popup.vars = c("Prop. Renters" = "tr_rentprop17",
						   "Rent" = "tr_medrent17",
						   "Rent Lag" = "tr_medrent17.lag",
						   "Ch. Rent" = "tr_chrent",
						   "Ch. Rent Lag" = "tr_chrent.lag",
						   "Rent Gap" = "tr_rentgap",
						   "Rent Burden" = "tr_rbprop17",
						   "Prop. ELI" = "tr_ELI_prop17",
						   "Prop. Students" = "tr_propstudent17",
						   "v_renters",
						   "v_ELI",
						   "v_rb",
						   "dp_chrent_comed",
						   "dp_rentgap_comed"),
			popup.format = list(digits=2)) +
	tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 12), alpha = .5)

sc_map_10 <-
tm_shape(cal_tracts_10) +
	tm_fill("risk_10",
			n = 2,
			title = "Tracts at Risk",
			id = "GEOID",
			popup.vars = c("Prop. Renters" = "tr_rentprop17",
						   "Rent" = "tr_medrent17",
						   "Rent Lag" = "tr_medrent17.lag",
						   "Ch. Rent" = "tr_chrent",
						   "Ch. Rent Lag" = "tr_chrent.lag",
						   "Rent Gap" = "tr_rentgap",
						   "Rent Burden" = "tr_rbprop17",
						   "Prop. ELI" = "tr_ELI_prop17",
						   "Prop. Students" = "tr_propstudent17",
						   "v_renters",
						   "v_ELI",
						   "v_rb",
						   "dp_chrent_10",
						   "dp_rentgap_10"),
			popup.format = list(digits=2)) +
	tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 12), alpha = .5)

tmap_save(sc_map_co, "~/git/sensitive_communities/docs/sc_map_cov1.html")
tmap_save(sc_map_10, "~/git/sensitive_communities/docs/sc_map_10v1.html")

# ==========================================================================
# End Code
# ==========================================================================


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

# tr_data <-
# 	left_join(tr_data,
# 			  ct@data,
# 			  by = c("GEOID" = "COUNTYFP")) %>%
# 	left_join(.,
# 			  co_data)



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

#
# block group data
# --------------------------------------------------------------------------

	# cbg <-
	# 	block_groups(state = "CA")

# create county join ID
	# cbg@data <-
	# 	cbg@data %>%
	# 	mutate(county = paste0(STATEFP, COUNTYFP))
	# cal_bg <- cbg

# Download CA block group data for select variables
	# cbg_data17 <-
	# 	get_acs(
	# 		geography = "block group",
	# 		variables = dis_var,
	# 		state = "CA",
	# 		county = NULL,
	# 		geometry = FALSE,
	# 		cache_table = TRUE,
	# 		output = "wide",
	# 		year = 2017) %>%
	# 	select(-ends_with("M"))

### Block groups for 2008 to 2012 are not downloading
	# cbg_data12 <-
	# 	get_acs(
	# 		geography = "block group",
	# 		variables = dis_var,
	# 		state = "CA",
	# 		county = NULL,
	# 		geometry = FALSE,
	# 		cache_table = TRUE,
	# 		output = "wide",
	# 		year = 2012) %>%
	# 	select(-ends_with("M"))

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

df %>%
	# ungroup() %>%
	# select(starts_with("tr_rentgap")) %>%
	filter(tr_rentgapsc >= 0.5 & tr_rentgapsc <= 1) %>%
	summarise(min = min(tr_rentgappropdiff), max = max(tr_rentgappropdiff)) %>%
	arrange(min) %>%
	summary()
	data.frame()


# ==========================================================================
# END CODE
# ==========================================================================
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
