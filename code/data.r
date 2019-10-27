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

#
# Load packages and install them if they're not installed.
# --------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(rmapshaper, data.table, spdep, tidyverse, tigris, tidycensus, tmap)

options(tigris_use_cache = TRUE)

# ==========================================================================
# Data
# ==========================================================================

rm(list = ls())
options(scipen = 10) # avoid scientific notation

#
# State tract data
# --------------------------------------------------------------------------

# Load variables list
source("~/git/sensitive_communities/code/vars.r")

# Download CA tract data for select variables
tr_data <- function(year)
	get_acs(
		geography = "tract",
		variables = dis_var,
		state = "CA",
		county = NULL,
		geometry = FALSE,
		cache_table = TRUE,
		output = "wide",
		year = year
		) %>%
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
		co_meded = median(co_bachplus/co_toted)
		)

co_data <-
	left_join(county_acsdata(2012), county_acsdata(2017), by = "GEOID")

#
# Download CA tracts as ESRI shapefiles with no data (will join below)
# --------------------------------------------------------------------------

# Download shapefile
ct <- tracts(state = "CA", cb = TRUE)

# create county join ID
ct@data <-
	ct@data %>%
	mutate(county = paste0(STATEFP, COUNTYFP))

# Join County and ACS tract data to ct shapefile
ct@data <-
	left_join(ct@data, co_data, by = c("county" = "GEOID")) %>%
	left_join(.,tr_data(2012), by = "GEOID") %>%
	left_join(.,tr_data(2017), by = "GEOID")

#
# Transit Rich Areas
# --------------------------------------------------------------------------

unzip("~/git/sensitive_communities/data/TransitRichAreas4326.zip")
transit <- st_read("~/git/sensitive_communities/data/TransitRichAreas4326/Transit Rich Areas 4326.shp")

# ==========================================================================
# Create variables
# ==========================================================================

#
# County Share of ELI households -
#	Create household income breaks (i.e. how many households fall within
# 	certain thresholds)
# --------------------------------------------------------------------------

# List to convert income values to numeric values.
newnames12 <-
	c('HHInc_10E.x' = 9999,'HHInc_15E.x' = 14999,'HHInc_20E.x' = 19999,'HHInc_25E.x' = 24999,'HHInc_30E.x' = 29999,'HHInc_35E.x' = 34999,'HHInc_40E.x' = 39999,'HHInc_45E.x' = 44999,'HHInc_50E.x' = 49999,'HHInc_60E.x' = 59999,'HHInc_75E.x' = 74999,'HHInc_100E.x' = 99999,'HHInc_125E.x' = 124999,'HHInc_150E.x' = 149999,'HHInc_200E.x' = 199999,'HHInc_250E.x' = 199999)

newnames17 <-
	c('HHInc_10E.y' = 9999,'HHInc_15E.y' = 14999,'HHInc_20E.y' = 19999,'HHInc_25E.y' = 24999,'HHInc_30E.y' = 29999,'HHInc_35E.y' = 34999,'HHInc_40E.y' = 39999,'HHInc_45E.y' = 44999,'HHInc_50E.y' = 49999,'HHInc_60E.y' = 59999,'HHInc_75E.y' = 74999,'HHInc_100E.y' = 99999,'HHInc_125E.y' = 124999,'HHInc_150E.y' = 149999,'HHInc_200E.y' = 199999,'HHInc_250E.y' = 199999)

trli12 <-
	ct@data %>%
	select(GEOID, COUNTYFP, co_medinc.x, HHInc_TotalE.x:HHInc_250E.x) %>%
	group_by(GEOID) %>%
	mutate(
		LI_val = co_medinc.x*.8,
		VLI_val = co_medinc.x*.5,
		ELI_val = co_medinc.x*.3
		) %>%
	gather(
		medinc_cat,
		medinc_cat_count,
		HHInc_10E.x:HHInc_250E.x
		) %>%
	mutate_at(vars(medinc_cat), ~newnames12
		) %>%
	mutate(
		bottom_inccat = case_when(medinc_cat <= 9999 ~ medinc_cat - 9999,
					   			  medinc_cat <= 49999 &
								  medinc_cat >= 10000 ~ medinc_cat - 4999,
								  medinc_cat >= 50000 &
								  medinc_cat <= 99999 ~ medinc_cat -24999,
								  medinc_cat >= 100000 &
								  medinc_cat <= 199999 ~ medinc_cat - 49999,
								  TRUE ~ NA_real_),
		top_inccat = medinc_cat
		) %>%
	mutate(
		LI = case_when(LI_val >= top_inccat ~ 1,
					   LI_val <= top_inccat &
		   			   LI_val >= bottom_inccat ~
	   				   (LI_val - bottom_inccat)/(top_inccat - bottom_inccat),
		   			   TRUE ~ 0),
		VLI = case_when(VLI_val >= top_inccat ~ 1,
					 	VLI_val <= top_inccat &
			   			VLI_val >= bottom_inccat ~
						(VLI_val - bottom_inccat)/(top_inccat - bottom_inccat),
						TRUE ~ 0),
		ELI = case_when(ELI_val >= top_inccat ~ 1,
			 		    ELI_val <= top_inccat &
						ELI_val >= bottom_inccat ~
						(ELI_val - bottom_inccat)/(top_inccat - bottom_inccat),
						TRUE ~ 0),
		tr_totalinc12 = sum(medinc_cat_count, na.rm = TRUE),
		tr_LI_count12 = sum(LI*medinc_cat_count, na.rm = TRUE),
		tr_VLI_count12 = sum(VLI*medinc_cat_count, na.rm = TRUE),
		tr_ELI_count12 = sum(ELI*medinc_cat_count, na.rm = TRUE),
		tr_LI_prop12 = tr_LI_count12/tr_totalinc12,
		tr_VLI_prop12 = tr_VLI_count12/tr_totalinc12,
		tr_ELI_prop12 = tr_ELI_count12/tr_totalinc12
		) %>%
	select(GEOID, COUNTYFP, LI_val:ELI_val, tr_totalinc12:tr_ELI_prop12) %>%
	distinct() %>%
	ungroup()


trli17 <-
	ct@data %>%
	select(GEOID, COUNTYFP, co_medinc.y, HHInc_TotalE.y:HHInc_250E.y) %>%
	group_by(GEOID) %>%
	mutate(
		LI_val = co_medinc.y*.8,
		VLI_val = co_medinc.y*.5,
		ELI_val = co_medinc.y*.3
		) %>%
	gather(medinc_cat, medinc_cat_count, HHInc_10E.y:HHInc_250E.y) %>%
	mutate_at(vars(medinc_cat), ~newnames17) %>%
	mutate(
		bottom_inccat = case_when(medinc_cat <= 9999 ~ medinc_cat - 9999,
								  medinc_cat <= 49999 &
								  medinc_cat >= 10000 ~ medinc_cat - 4999,
								  medinc_cat >= 50000 &
								  medinc_cat <= 99999 ~ medinc_cat -24999,
								  medinc_cat >= 100000 &
								  medinc_cat <= 199999 ~ medinc_cat - 49999,
								  TRUE ~ NA_real_),
		top_inccat = medinc_cat
		) %>%
	mutate(
		LI = case_when(LI_val >= top_inccat ~ 1,
					   LI_val <= top_inccat &
					   LI_val >= bottom_inccat ~
			   		   (LI_val - bottom_inccat)/(top_inccat - bottom_inccat),
			   		   TRUE ~ 0),
		VLI = case_when(VLI_val >= top_inccat ~ 1,
					    VLI_val <= top_inccat &
			   			VLI_val >= bottom_inccat ~
			   			(VLI_val - bottom_inccat)/(top_inccat - bottom_inccat),
			   			TRUE ~ 0),
		ELI = case_when(ELI_val >= top_inccat ~ 1,
						ELI_val <= top_inccat &
						ELI_val >= bottom_inccat ~
						(ELI_val - bottom_inccat)/(top_inccat - bottom_inccat),
						TRUE ~ 0),
		tr_totalinc17 = sum(medinc_cat_count, na.rm = TRUE),
		tr_LI_count17 = sum(LI*medinc_cat_count, na.rm = TRUE),
		tr_VLI_count17 = sum(VLI*medinc_cat_count, na.rm = TRUE),
		tr_ELI_count17 = sum(ELI*medinc_cat_count, na.rm = TRUE),
		tr_LI_prop17 = tr_LI_count17/tr_totalinc17,
		tr_VLI_prop17 = tr_VLI_count17/tr_totalinc17,
		tr_ELI_prop17 = tr_ELI_count17/tr_totalinc17
		) %>%
	select(GEOID, COUNTYFP, LI_val:ELI_val,tr_totalinc17:tr_ELI_prop17) %>%
	distinct() %>%
	ungroup()

lidata <-
	left_join(trli12, trli17, by = c("GEOID", "COUNTYFP")) %>%
	group_by(COUNTYFP) %>%
	mutate(
		co_totalinc12 = sum(tr_totalinc12, na.rm = TRUE),
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
		co_ELI_propmed17 = median(tr_ELI_prop17, na.rm = TRUE)
		)

#
# Change
# --------------------------------------------------------------------------


ct@data <-
	left_join(ct@data, lidata, by = c("GEOID", "COUNTYFP")) %>%
	group_by(GEOID) %>%
	mutate(
		tr_propstudent17 = sum(colenrollE.y, proenrollE.y)/totenrollE.y,
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
		tr_proped12 = case_when(tr_bachplus12 == 0 & totedE.x == 0 ~ 0,
								TRUE ~ tr_bachplus12/totedE.x),
		tr_proped17 = case_when(tr_bachplus17 == 0 & totedE.y == 0 ~ 0,
								TRUE ~ tr_bachplus17/totedE.y),
		tr_propched = case_when(tr_proped17 == 0 & tr_proped12 == 0 ~ 0,
								TRUE ~ (tr_proped17 - tr_proped12)/(tr_proped12))
		) %>%
	group_by(COUNTYFP) %>%
	mutate(
		co_medrentprop12 = median(tr_rentprop12, na.rm = TRUE),
		co_medrentprop17 = median(tr_rentprop17, na.rm = TRUE),
		co_medrbprop12 = median(tr_rbprop12, na.rm = TRUE),
		co_medrbprop17 = median(tr_rbprop17, na.rm = TRUE),
		co_medrent12 = median(tr_medrent12, na.rm = TRUE),
		co_medrent17 = median(tr_medrent17, na.rm = TRUE),
		co_medchrent = median(tr_chrent, na.rm = TRUE),
		co_medproped12 = median(tr_proped12, na.rm = TRUE),
		co_medproped17 = median(tr_proped17, na.rm = TRUE),
		co_medched = median(tr_propched, na.rm = TRUE)
		) %>%
	group_by(GEOID) %>%
	mutate( ## special data features ##
		tr_medrent12 = case_when(is.na(tr_medrent12) ~ co_medrent12,
								 TRUE ~ tr_medrent12),
		tr_medrent17 = case_when(is.na(tr_medrent17) ~ co_medrent17,
		   						 TRUE ~ tr_medrent17),
		tr_chrent = case_when(is.na(tr_chrent) ~ co_medchrent,
		   					  TRUE ~ tr_chrent),
		tr_propchrent = (tr_medrent17-tr_medrent12)/tr_medrent12,
		tr_propched = case_when(tr_propched == Inf ~
							    (tr_proped17-tr_proped12)/mean(tr_proped17,tr_proped12),
		   						TRUE ~ tr_propched)
		) %>%
	group_by(COUNTYFP) %>%
	mutate(co_medpropchrent = median(tr_propchrent)) %>%
	ungroup()

# ==========================================================================
# Create lag variables
# ==========================================================================

#
# Create neighbor matrix
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
	spdep::set.ZeroPolicyOption(TRUE)
	spdep::set.ZeroPolicyOption(TRUE)
	dists <- nbdists(dist_nb, coordinates(ct))
	idw <- lapply(dists, function(x) 1/(x^2))
	lw_dist_idwW <- nb2listw(dist_nb, glist = idw, style = "W")

#
# Create select lag variables
# --------------------------------------------------------------------------

	ct$tr_chrent.lag <- lag.listw(lw_dist_idwW,ct$tr_chrent)
	# ct$tr_propchrent.lag <- lag.listw(lw_dist_idwW,ct$tr_propchrent)
	# ct$tr_propched.lag <- lag.listw(lw_dist_idwW,ct$tr_propched)
	ct$tr_medrent17.lag <- lag.listw(lw_dist_idwW,ct$tr_medrent17)
	ct$tr_medrent12.lag <- lag.listw(lw_dist_idwW,ct$tr_medrent12)

ct_sf <-
	ct %>%
	st_as_sf() %>%
	group_by(COUNTYFP) %>%
	mutate(
		tr_propchrent.lag = (tr_medrent17.lag - tr_medrent12.lag)/( tr_medrent12.lag),
		tr_rentgap = tr_medrent17.lag - tr_medrent17,
		tr_rentgapprop = (tr_medrent17.lag - tr_medrent17)/((tr_medrent17 +  tr_medrent17.lag)/2),
		co_rentgap = median(tr_rentgap),
		co_rentgapprop = median(tr_rentgapprop),
		rent_prank = rank(tr_rentprop17)/length(tr_rentprop17)
		) %>%
	ungroup()

# ==========================================================================
# Categorize tracts
# ==========================================================================

final_df <-
	ct_sf %>%
	group_by(GEOID) %>%
	mutate(
## Vulnerable Population Variables ##
		v_renters_co = case_when(tr_rentprop17 > co_medrentprop17 ~ 1,
								 TRUE ~ 0),
		v_renters_60th = case_when(rent_prank <= .6 ~ 1,
								   TRUE ~ 0),
		v_renters_50p = case_when(tr_rentprop17 >= .5 ~ 1,
								  TRUE ~ 0),
		v_ELI = case_when(tr_propstudent17 < .25 &
		   				tr_ELI_prop17 > co_ELI_prop17 ~ 1,
		   				TRUE ~ 0),
		v_rb = case_when(tr_rbprop17 > co_medrbprop17 ~ 1,
						 TRUE ~ 0),
## Displacement Pressure Variables ##
		dp_chrent_co = case_when(tr_propchrent > co_medpropchrent ~ 1,
		   						 tr_propchrent.lag > co_medpropchrent ~ 1,
		   						 TRUE ~ 0),
		dp_chrent_10 = case_when(tr_propchrent >= .1 ~ 1,
		   						 tr_propchrent.lag >= .1 ~ 1,
		   						 TRUE ~ 0),
		dp_rentgap_co = case_when(tr_rentgapprop > co_rentgapprop ~ 1,
								  TRUE ~ 0),
		dp_rentgap_10 = case_when(tr_rentgapprop > .1 ~ 1,
								  TRUE ~ 0),
		`Scenario 01` = case_when(sum(v_renters_co, v_ELI, na.rm = TRUE) == 2 &
						   sum(dp_chrent_co, dp_rentgap_10) >=1 ~ TRUE),
		`Scenario 02` = case_when(sum(v_renters_co, v_ELI, na.rm = TRUE) == 2 &
						   sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
		`Scenario 03` = case_when(sum(v_renters_co, v_ELI, na.rm = TRUE) == 2 &
						   sum(dp_chrent_10, dp_rentgap_10) >=1 ~ TRUE),
		`Scenario 04` = case_when(sum(v_renters_co, v_ELI, na.rm = TRUE) == 2 &
						   sum(dp_chrent_10, dp_rentgap_co) >=1 ~ TRUE),

		`Scenario 05` = case_when(sum(v_renters_co, v_ELI, v_rb, na.rm = TRUE) == 3 &
						   sum(dp_chrent_co, dp_rentgap_10) >=1 ~ TRUE),
		`Scenario 06` = case_when(sum(v_renters_co, v_ELI, v_rb, na.rm = TRUE) == 3 &
						   sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
		`Scenario 07` = case_when(sum(v_renters_co, v_ELI, v_rb, na.rm = TRUE) == 3 &
						   sum(dp_chrent_10, dp_rentgap_10) >=1 ~ TRUE),
		`Scenario 08` = case_when(sum(v_renters_co, v_ELI, v_rb, na.rm = TRUE) == 3 &
						   sum(dp_chrent_10, dp_rentgap_co) >=1 ~ TRUE),

		`Scenario 09` = case_when(sum(v_renters_60th, v_ELI, na.rm = TRUE) == 2 &
						   sum(dp_chrent_co, dp_rentgap_10) >=1 ~ TRUE),
		`Scenario 10` = case_when(sum(v_renters_60th, v_ELI, na.rm = TRUE) == 2 &
						   sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
		`Scenario 11` = case_when(sum(v_renters_60th, v_ELI, na.rm = TRUE) == 2 &
						   sum(dp_chrent_10, dp_rentgap_10) >=1 ~ TRUE),
		`Scenario 12` = case_when(sum(v_renters_60th, v_ELI, na.rm = TRUE) == 2 &
						   sum(dp_chrent_10, dp_rentgap_co) >=1 ~ TRUE),

		`Scenario 13` = case_when(sum(v_renters_60th, v_ELI, v_rb, na.rm = TRUE) == 3 &
						   sum(dp_chrent_co, dp_rentgap_10) >=1 ~ TRUE),
		`Scenario 14` = case_when(sum(v_renters_60th, v_ELI, v_rb, na.rm = TRUE) == 3 &
						   sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
		`Scenario 15` = case_when(sum(v_renters_60th, v_ELI, v_rb, na.rm = TRUE) == 3 &
						   sum(dp_chrent_10, dp_rentgap_10) >=1 ~ TRUE),
		`Scenario 16` = case_when(sum(v_renters_60th, v_ELI, v_rb, na.rm = TRUE) == 3 &
						   sum(dp_chrent_10, dp_rentgap_co) >=1 ~ TRUE),

		`Scenario 17` = case_when(sum(v_renters_50p, v_ELI, na.rm = TRUE) == 2 &
						   sum(dp_chrent_co, dp_rentgap_10) >=1 ~ TRUE),
		`Scenario 18` = case_when(sum(v_renters_50p, v_ELI, na.rm = TRUE) == 2 &
						   sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
		`Scenario 19` = case_when(sum(v_renters_50p, v_ELI, na.rm = TRUE) == 2 &
						   sum(dp_chrent_10, dp_rentgap_10) >=1 ~ TRUE),
		`Scenario 20` = case_when(sum(v_renters_50p, v_ELI, na.rm = TRUE) == 2 &
						   sum(dp_chrent_10, dp_rentgap_co) >=1 ~ TRUE),

		`Scenario 21` = case_when(sum(v_renters_50p, v_ELI, v_rb, na.rm = TRUE) == 3 &
						   sum(dp_chrent_co, dp_rentgap_10) >=1 ~ TRUE),
		`Scenario 22` = case_when(sum(v_renters_50p, v_ELI, v_rb, na.rm = TRUE) == 3 &
						   sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
		`Scenario 23` = case_when(sum(v_renters_50p, v_ELI, v_rb, na.rm = TRUE) == 3 &
						   sum(dp_chrent_10, dp_rentgap_10) >=1 ~ TRUE),
		`Scenario 24` = case_when(sum(v_renters_50p, v_ELI, v_rb, na.rm = TRUE) == 3 &
						   sum(dp_chrent_10, dp_rentgap_co) >=1 ~ TRUE),
		text = "",
		pwhite = WhiteE.y/totraceE.y,
		pblack = BlackE.y/totraceE.y,
		pasian = AsianE.y/totraceE.y,
		platinx = LatinxE.y/totraceE.y,
		pother = (totraceE.y - sum(WhiteE.y,BlackE.y,AsianE.y,LatinxE.y, na.rm = TRUE))/totraceE.y,
		pwelfare = welfE.y/totwelfE.y,
		ppoverty = sum(povfamhE.y, povnonfamhE.y, na.rm = TRUE)/totpovE.y,
		unemp = unempE.y/totunempE.y,
		pfemhhch = sum(femfamheadchE.y, femnonfamheadchE.y, na.rm = TRUE)/totfhcE.y
		)

glimpse(final_df)

# ==========================================================================
# Map
# ==========================================================================

tmap_mode("view")

sen_map1 <- function(scen, renters, eli, rb, chrent, rentgap)
tm_basemap(leaflet::providers$CartoDB.Positron) + # http://leaflet-extras.github.io/leaflet-providers/preview/
tm_shape(transit) +
	tm_polygons("MAP_COLORS", palette="Greys", alpha = .5) +
tm_shape(final_df) +
	tm_fill(scen,
			palette = c("#FF6633","#FF6633"),
			alpha = .5,
			colorNA = NULL,
			title = "Sensitive Communities",
			id = "GEOID",
			popup.vars = c("Renters" = renters,
						   "ELI" = eli,
						   "RB" = rb,
						   "Ch Rent" = chrent,
						   "Rent Gap" = rentgap,
						   "---" = "text",
						   "% Rent" = "tr_rentprop17",
						   "$ Rent" = "tr_medrent17",
						   "Rent Lag" = "tr_medrent17.lag",
						   "Ch Rent" = "tr_chrent",
						   "Ch R Lag" = "tr_chrent.lag",
						   "Rent Gap" = "tr_rentgap",
						   "RB" = "tr_rbprop17",
						   "% ELI" = "tr_ELI_prop17",
						   "% Stud." = "tr_propstudent17",
						   "---" = "text",
						   "% White" = "pwhite",
						   "% Black" = "pblack",
						   "% Asian" = "pasian",
						   "% Lat" = "platinx",
						   "% Other" = "pother",
						   "% Welf." = "pwelfare",
						   "% Pov." = "ppoverty",
						   "% Unemp." = "unemp",
						   "% FHHw/C"= "pfemhhch"
						   ),
			popup.format = list(digits=2)) +
	tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 12), alpha = .5) +
	tm_layout(title = paste0(scen, ": ",renters,", ", eli, ", ", chrent, ", ", rentgap))

sen_map2 <- function(scen, renters, eli, rb, chrent, rentgap)
	sen_map1(scen, renters, eli, rb, chrent, rentgap) +
	tm_layout(title = paste0(scen, ": ",renters,", ", eli, ", ", rb, ", ", chrent, ", ", rentgap))

save_map <- function(x,y)
	tmap_save(x, paste0("~/git/sensitive_communities/docs/", y, ".html"))


scen01 <- sen_map1("Scenario 01", "v_renters_co", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_10")
scen02 <- sen_map1("Scenario 02", "v_renters_co", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
scen03 <- sen_map1("Scenario 03", "v_renters_co", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_10")
scen04 <- sen_map1("Scenario 04", "v_renters_co", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_co")

scen05 <- sen_map2("Scenario 05", "v_renters_co", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_10")
scen06 <- sen_map2("Scenario 06", "v_renters_co", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
scen07 <- sen_map2("Scenario 07", "v_renters_co", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_10")
scen08 <- sen_map2("Scenario 08", "v_renters_co", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_co")

scen09 <- sen_map1("Scenario 09", "v_renters_60th", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_10")
scen10 <- sen_map1("Scenario 10", "v_renters_60th", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
scen11 <- sen_map1("Scenario 11", "v_renters_60th", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_10")
scen12 <- sen_map1("Scenario 12", "v_renters_60th", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_co")

scen13 <- sen_map2("Scenario 13", "v_renters_60th", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_10")
scen14 <- sen_map2("Scenario 14", "v_renters_60th", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
scen15 <- sen_map2("Scenario 15", "v_renters_60th", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_10")
scen16 <- sen_map2("Scenario 16", "v_renters_60th", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_co")

scen17 <- sen_map1("Scenario 17", "v_renters_50p", "v_ELI", "v_rb","dp_chrent_co", "dp_rentgap_10")
scen18 <- sen_map1("Scenario 18", "v_renters_50p", "v_ELI", "v_rb","dp_chrent_co", "dp_rentgap_co")
scen19 <- sen_map1("Scenario 19", "v_renters_50p", "v_ELI", "v_rb","dp_chrent_10", "dp_rentgap_10")
scen20 <- sen_map1("Scenario 20", "v_renters_50p", "v_ELI", "v_rb","dp_chrent_10", "dp_rentgap_co")

scen21 <- sen_map2("Scenario 21", "v_renters_50p", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_10")
scen22 <- sen_map2("Scenario 22", "v_renters_50p", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
scen23 <- sen_map2("Scenario 23", "v_renters_50p", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_10")
scen24 <- sen_map2("Scenario 24", "v_renters_50p", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_co")

save_map(scen01, "scen01"); save_map(scen02, "scen02"); save_map(scen03, "scen03"); save_map(scen04, "scen04"); save_map(scen05, "scen05"); save_map(scen06, "scen06"); save_map(scen07, "scen07"); save_map(scen08, "scen08"); save_map(scen09, "scen09"); save_map(scen10, "scen10"); save_map(scen11, "scen11"); save_map(scen12, "scen12"); save_map(scen13, "scen13"); save_map(scen14, "scen14"); save_map(scen15, "scen15"); save_map(scen16, "scen16"); save_map(scen17, "scen17"); save_map(scen18, "scen18"); save_map(scen19, "scen19"); save_map(scen20, "scen20"); save_map(scen21, "scen21"); save_map(scen22, "scen22"); save_map(scen23, "scen23"); save_map(scen24, "scen24");

# ==========================================================================
# Get tract counts for each scenario
# ==========================================================================

	final_df %>%
	ungroup() %>%
	select(GEOID, COUNTYFP, `Scenario 01`:`Scenario 24`) %>%
	gather(scenario, value, `Scenario 01`:`Scenario 24`) %>%
	group_by(scenario) %>%
	summarise(count = sum(case_when(value == TRUE ~ 1, TRUE ~ 0), na.rm = TRUE)) %>%
	data.frame()

# ==========================================================================
# TESTBED
test <-
	ct_sf %>%
	st_set_geometry(NULL) %>%
	group_by(COUNTYFP) %>%
	select(tottenE.y, tr_rentprop17, v_renterssd, co_medrentprop17, co_ELI_prop17,co_medrbprop17) %>%
	mutate(meanprop = mean(tr_rentprop17, na.rm = TRUE), co_rentcount = sum(tottenE.y, na.rm = TRUE),
		percrank=rank(tr_rentprop17)/length(tr_rentprop17)) %>%
	arrange(COUNTYFP,v_renterssd)

test %>%
	tbl_df() %>%
	nest(-COUNTYFP) %>%
  	mutate(Quantiles = map(data, ~ enframe(quantile(.$tr_rentprop17), "quantile")))

hist(test$co_rentcount, breaks = 100)

test %>%
	select(COUNTYFP, co_rentcount) %>%
	data.frame() %>% arrange(co_rentcount) %>%
	distinct() %>%
	ggplot() +
		geom_bar(aes(x = reorder(COUNTYFP,co_rentcount), y = co_rentcount), stat = "identity")

test %>%
	select(COUNTYFP, co_medrentprop17) %>%
	data.frame() %>% arrange(co_medrentprop17) %>%
	distinct() %>%
	ggplot() +
		geom_bar(aes(x = reorder(COUNTYFP,co_medrentprop17), y = co_medrentprop17), stat = "identity")

test %>%
	select(COUNTYFP, v_renterssd, ) %>%
	data.frame() %>% arrange(co_medrentprop17) %>%
	distinct() %>%
	ggplot() +
		geom_bar(aes(x = reorder(COUNTYFP,co_medrentprop17), y = co_medrentprop17), stat = "identity")

test %>%
	select(COUNTYFP, co_ELI_prop17) %>%
	data.frame() %>% arrange(co_ELI_prop17) %>%
	distinct() %>%
	ggplot() +
		geom_bar(aes(x = reorder(COUNTYFP,co_ELI_prop17), y = co_ELI_prop17), stat = "identity")
test %>%
	select(COUNTYFP, co_medrbprop17) %>%
	data.frame() %>% arrange(co_medrbprop17) %>%
	distinct() %>%
	ggplot() +
		geom_bar(aes(x = reorder(COUNTYFP,co_medrbprop17), y = co_medrbprop17), stat = "identity")


# ==========================================================================

sc_map_10 <-
tm_basemap(leaflet::providers$CartoDB.Positron) +
tm_shape(transit) +
	tm_polygons("MAP_COLORS", palette="Greys", alpha = .25) +
tm_shape(ct_10) +
	tm_fill("sens_10",
			palette = c("#FF6633","#FF6633"),
			colorNA = NULL,
			title = "Sensitive Communities",
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
						   "Criteria Met:" = "Criteria",
						   "Renters" = "v_renters",
						   "ELI" = "v_ELI",
						   "Rent Burden" = "v_rb",
						   "Ch. Rent" = "dp_chrent_co",
						   "Rent Gap" = "dp_rentgapprop"),
			popup.format = list(digits=2)) +
	tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 12), alpha = .5)

sc_maprg <-
tm_basemap(leaflet::providers$CartoDB.Positron) +
tm_shape(transit) +
	tm_polygons("MAP_COLORS", palette="Greys", alpha = .25) +
tm_shape(ct_sf) +
	tm_fill("sensrg",
			palette = c("#FF6633","#FF6633"),
			colorNA = NULL,
			title = "Sensitive Communities",
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
						   "Criteria Met:" = "Criteria",
						   "Renters" = "v_renters",
						   "ELI" = "v_ELI",
						   "Rent Burden" = "v_rb",
						   "Ch. Rent" = "dp_chrent_co",
						   "Rent Gap" = "dp_rentgapprop"),
			popup.format = list(digits=2)) +
	tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 12), alpha = .5)

tmap_save(sc_maprg, "~/git/sensitive_communities/docs/sc_maprgv1.html")
tmap_save(sc_map_co, "~/git/sensitive_communities/docs/sc_map_cov1.html")
tmap_save(sc_map_10, "~/git/sensitive_communities/docs/sc_map_10v1.html")

# ==========================================================================
# Validation
# ==========================================================================

#
# Check on how many tracts are High Rent, High RB, low ELI
# --------------------------------------------------------------------------

eli_check <- ct_10

eli_check@data %>%
	group_by(risk_co, v_renters, v_rb,v_ELI,v_count) %>%
	count()
	# There are 352 tracts that fit this Scenario

eli_check@data <-
	eli_check@data %>%
	mutate(eli_check = if_else(v_renters == 1 & v_rb == 1 & v_ELI == 0 & risk_co == 1, 1, NA_real_))
eli_map <-
	tm_shape(eli_check) +
	tm_fill("eli_check") +
	tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 12), alpha = .5)

tmap_save(eli_map, "~/git/sensitive_communities/docs/hirentpop_hirbpop_loeli_map.html")

# ==========================================================================
# End Code
# ==========================================================================
