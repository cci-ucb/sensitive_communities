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
pacman::p_load(data.table, plotly, spdep, tidyverse, tigris, tidycensus, tmap)

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
		) 

#
# Block group data - NOT WORKING
# --------------------------------------------------------------------------

# ctys <- counties(state = "CA", cb = TRUE, class = "sf") %>%
# pull(NAME) %>% unique()

# bgs12 <- map_df(ctys, function(cty) {
# 	get_acs(
# 		geography = "block group",
# 		variables = dis_var,
# 		state = "CA",
# 		county = "Alameda",
# 		geometry = FALSE,
# 		cache_table = TRUE,
# 		output = "wide",
# 		year = 2012
# 		) 
# })

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

# ==========================================================================
# Change income to renter income
# ==========================================================================

ri_names12 <- c(
	'HHIncTenRent_5E.x' = 4999, # Renter occupied!!Less than $5,000
	'HHIncTenRent_10E.x' = 9999, # Renter occupied!!$5,000 to $9,999
	'HHIncTenRent_15E.x' = 14999, # Renter occupied!!$10,000 to $14,999
	'HHIncTenRent_20E.x' = 19999, # Renter occupied!!$15,000 to $19,999
	'HHIncTenRent_25E.x' = 24999, # Renter occupied!!$20,000 to $24,999
	'HHIncTenRent_35E.x' = 34999, # Renter occupied!!$25,000 to $34,999
	'HHIncTenRent_50E.x' = 49999, # Renter occupied!!$35,000 to $49,999
	'HHIncTenRent_75E.x' = 74999, # Renter occupied!!$50,000 to $74,999
	'HHIncTenRent_100E.x' = 99999, # Renter occupied!!$75,000 to $99,999
	'HHIncTenRent_150E.x' = 149999, # Renter occupied!!$100,000 to $149,999
	'HHIncTenRent_151E.x' = 150000 # Renter occupied!!$150,000 or more
	)

ri_names17 <- c(
	'HHIncTenRent_5E.y' = 4999, # Renter occupied!!Less than $5,000
	'HHIncTenRent_10E.y' = 9999, # Renter occupied!!$5,000 to $9,999
	'HHIncTenRent_15E.y' = 14999, # Renter occupied!!$10,000 to $14,999
	'HHIncTenRent_20E.y' = 19999, # Renter occupied!!$15,000 to $19,999
	'HHIncTenRent_25E.y' = 24999, # Renter occupied!!$20,000 to $24,999 #
	'HHIncTenRent_35E.y' = 34999, # Renter occupied!!$25,000 to $34,999
	'HHIncTenRent_50E.y' = 49999, # Renter occupied!!$35,000 to $49,999
	'HHIncTenRent_75E.y' = 74999, # Renter occupied!!$50,000 to $74,999
	'HHIncTenRent_100E.y' = 99999, # Renter occupied!!$75,000 to $99,999
	'HHIncTenRent_150E.y' = 149999, # Renter occupied!!$100,000 to $149,999
	'HHIncTenRent_151E.y' = 150000 # Renter occupied!!$150,000 or more
	)


trli12 <-
	ct@data %>%
	select(GEOID, COUNTYFP, co_medinc.x, HHInc_TotalE.x, HHIncTenRentE.x:HHIncTenRent_151E.x) %>%
	group_by(GEOID) %>%
	mutate(
		LI_val = co_medinc.x*.8,
		VLI_val = co_medinc.x*.5,
		ELI_val = co_medinc.x*.3
		) %>%
	gather(
		r_medinc_cat,
		r_medinc_cat_count,
		HHIncTenRent_5E.x:HHIncTenRent_151E.x
		) %>%
	mutate_at(vars(r_medinc_cat), ~ri_names12
		) %>%
	mutate(
		bottom_inccat = case_when(r_medinc_cat <= 4999 ~ r_medinc_cat - 4999,
					   			  r_medinc_cat >= 9999 &
								  r_medinc_cat <= 24999 ~ r_medinc_cat - 4999,
								  r_medinc_cat == 34999 ~ r_medinc_cat - 9999,
								  r_medinc_cat == 49999 ~ r_medinc_cat - 14999, 
								  r_medinc_cat == 74999 ~ r_medinc_cat - 24999, 
								  r_medinc_cat == 99999 ~ r_medinc_cat - 24999, 
								  r_medinc_cat == 149999 ~ r_medinc_cat - 49999, 
								  r_medinc_cat == 150000 ~ 150000, 
								  TRUE ~ NA_real_),
		top_inccat = r_medinc_cat
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
		tr_totalinc12 = sum(r_medinc_cat_count, na.rm = TRUE),
		tr_LI_count12 = sum(LI*r_medinc_cat_count, na.rm = TRUE),
		tr_VLI_count12 = sum(VLI*r_medinc_cat_count, na.rm = TRUE),
		tr_ELI_count12 = sum(ELI*r_medinc_cat_count, na.rm = TRUE),
		tr_LI_prop12 = tr_LI_count12/tr_totalinc12,
		tr_VLI_prop12 = tr_VLI_count12/tr_totalinc12,
		tr_ELI_prop12 = tr_ELI_count12/tr_totalinc12
		) %>%
	select(GEOID, COUNTYFP, LI_val:ELI_val, tr_totalinc12:tr_ELI_prop12) %>%
	distinct() %>%
	ungroup()


trli17 <-
	ct@data %>%
	select(GEOID, COUNTYFP, co_medinc.y, HHInc_TotalE.y, HHIncTenRentE.y:HHIncTenRent_151E.y) %>%
	group_by(GEOID) %>%
	mutate(
		LI_val = co_medinc.y*.8,
		VLI_val = co_medinc.y*.5,
		ELI_val = co_medinc.y*.3
		) %>%
	gather(
		r_medinc_cat,
		r_medinc_cat_count,
		HHIncTenRent_5E.y:HHIncTenRent_151E.y
		) %>%
	mutate_at(vars(r_medinc_cat), ~ri_names17
		) %>%
	mutate(
		bottom_inccat = case_when(r_medinc_cat <= 4999 ~ r_medinc_cat - 4999,
					   			  r_medinc_cat >= 9999 &
								  r_medinc_cat <= 24999 ~ r_medinc_cat - 4999,
								  r_medinc_cat == 34999 ~ r_medinc_cat - 9999,
								  r_medinc_cat == 49999 ~ r_medinc_cat - 14999, 
								  r_medinc_cat == 74999 ~ r_medinc_cat - 24999, 
								  r_medinc_cat == 99999 ~ r_medinc_cat - 24999, 
								  r_medinc_cat == 149999 ~ r_medinc_cat - 49999, 
								  r_medinc_cat == 150000 ~ 150000, 
								  TRUE ~ NA_real_),
		top_inccat = r_medinc_cat
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
		tr_totalinc17 = sum(r_medinc_cat_count, na.rm = TRUE),
		tr_LI_count17 = sum(LI*r_medinc_cat_count, na.rm = TRUE),
		tr_VLI_count17 = sum(VLI*r_medinc_cat_count, na.rm = TRUE),
		tr_ELI_count17 = sum(ELI*r_medinc_cat_count, na.rm = TRUE),
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
# Rent Burden
# Testing: 
# 	1. Rent burden capped at $35k
# 	2. Rent burden by 80%, 50%, and 30% AMI
# 	3. by rent burden at 30% and 50%
# --------------------------------------------------------------------------



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

# ==========================================================================
# Create Neighborhood Typologies
# ==========================================================================

source("~/git/Functions/NeighType_Fun.R")

cal_nt <-
	ntdf(state = "CA") %>% 
	select(GEOID,NeighType) %>% 
	mutate(v_poc = case_when(NeighType == "White-Asian" ~ 0,
							 NeighType == "All White" ~ 0,
							 NeighType == "White-Shared" ~ 0,
							 TRUE ~ 1))	

ntcheck(cal_nt)

final_df <-
	left_join(ct_sf, cal_nt) %>%
	group_by(GEOID) %>%
	mutate(
## Vulnerable Population Variables ##
		v_renters_co = case_when(tr_rentprop17 > co_medrentprop17 ~ 1,
								 TRUE ~ 0),
		v_renters_60th = case_when(rent_prank >= .6 ~ 1,
								   TRUE ~ 0),
		v_renters_50p = case_when(tr_rentprop17 >= .5 ~ 1,
								  TRUE ~ 0),		
		v_renters_40p = case_when(tr_rentprop17 >= .4 ~ 1, # v2
								  TRUE ~ 0),
		v_renters_30p = case_when(tr_rentprop17 >= .3 ~ 1, # v2
								  TRUE ~ 0),
		v_ELI = case_when(tr_propstudent17 < .20 & # v2
		   				tr_ELI_prop17 > co_ELI_prop17 ~ 1,
		   				TRUE ~ 0),		
		v_VLI = case_when(tr_propstudent17 < .20 & # v2
		   				tr_VLI_prop17 > co_VLI_prop17 ~ 1, # v2
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
		text = "",
		popup_text = paste0("Tract: ", GEOID),
		pwhite = WhiteE.y/totraceE.y,
		pblack = BlackE.y/totraceE.y,
		pasian = AsianE.y/totraceE.y,
		platinx = LatinxE.y/totraceE.y,
		pother = (totraceE.y - sum(WhiteE.y,BlackE.y,AsianE.y,LatinxE.y, na.rm = TRUE))/totraceE.y,
		pwelfare = welfE.y/totwelfE.y,
		ppoverty = sum(povfamhE.y, povnonfamhE.y, na.rm = TRUE)/totpovE.y,
		unemp = unempE.y/totunempE.y,
		pfemhhch = sum(femfamheadchE.y, femnonfamheadchE.y, na.rm = TRUE)/totfhcE.y,
		pPOC = (totraceE.y - WhiteE.y)/totraceE.y,
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
		`Scenario 25` = case_when(sum(v_renters_30p, 
									  v_ELI, 
									  v_rb, na.rm = TRUE) == 3 &
						   		  sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
		`Scenario 26` = case_when(sum(v_renters_30p, 
									  v_VLI, 
									  v_rb, na.rm = TRUE) == 3 &
						   		  sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
		`Scenario 27` = case_when(sum(v_poc, 
									  v_renters_30p, 
									  v_ELI, 
									  v_rb, na.rm = TRUE) >= 3 &
						   		  sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
		`Scenario 28` = case_when(sum(v_poc, 
									  v_renters_30p, 
									  v_VLI, 
									  v_rb, na.rm = TRUE) >= 3 &
						   		  sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
		`Scenario 29` = case_when(sum(v_poc, 
									  v_renters_30p, 
									  v_ELI, 
									  v_rb, na.rm = TRUE) >= 3 &
						   		  sum(dp_chrent_10, dp_rentgap_co) >=1 ~ TRUE),
		`Scenario 30` = case_when(sum(v_poc, 
									  v_renters_30p, 
									  v_ELI, 
									  v_rb, na.rm = TRUE) >= 3 &
						   		  sum(dp_chrent_co, dp_rentgap_10) >=1 ~ TRUE),
		`Scenario 31` = case_when(sum(v_poc, 
									  v_renters_30p, 
									  v_ELI, 
									  v_rb, na.rm = TRUE) >= 3 &
						   		  sum(dp_chrent_10, dp_rentgap_10) >=1 ~ TRUE),		
		`Scenario 32` = case_when(sum(v_poc, 
									  v_renters_30p, 
									  v_VLI, 
									  v_rb, na.rm = TRUE) >= 3 &
						   		  sum(dp_chrent_10, dp_rentgap_co) >=1 ~ TRUE),
		`Scenario 33` = case_when(sum(v_poc, 
									  v_renters_30p, 
									  v_VLI, 
									  v_rb, na.rm = TRUE) >= 3 &
						   		  sum(dp_chrent_co, dp_rentgap_10) >=1 ~ TRUE),
		`Scenario 34` = case_when(sum(v_poc, 
									  v_renters_30p, 
									  v_VLI, 
									  v_rb, na.rm = TRUE) >= 3 &
						   		  sum(dp_chrent_10, dp_rentgap_10) >=1 ~ TRUE),		
		`Scenario 35` = case_when(tr_propstudent17 < .20 &
								  v_poc == 1 & 
								  v_renters_30p == 1 & 
								  sum(v_VLI, 
									  v_rb, na.rm = TRUE) >= 1 ~ TRUE, 
						   		  sum(dp_chrent_co, dp_rentgap_co) >= 1 ~ TRUE), 
# Version 2
		`Scenario 36` = case_when(sum(v_poc == 1,
								  	  v_renters_40p == 1,
								  	  v_VLI, 
									  v_rb, na.rm = TRUE) >= 3 & 
						   		  sum(dp_chrent_co, 
						   		  	  dp_rentgap_co) >= 1 ~ TRUE, 
						   		  totraceE.y >= 500 & 
						   		  pPOC >= .3 & 
						   		  tr_propstudent17 < .20, 
						   		  ), 
		) %>%
ungroup()

glimpse(final_df)
# st_write(final_df, "~/data/sensitive_communities/final_df_v1.shp")

# ==========================================================================
# Map
# ==========================================================================

#
# Mapping function (transit on hold till update)
# --------------------------------------------------------------------------

tmap_mode("view")

sen_map1 <- function(scen, renters, eli, rb, chrent, rentgap)
tm_basemap(leaflet::providers$CartoDB.Positron) + # http://leaflet-extras.github.io/leaflet-providers/preview/
# tm_shape(transit,
# 		 name = "Transit Rich Areas Layer") +
# 	tm_polygons("FID",
# 				palette= "Greys",
# 				alpha = .5,
# 				label = "Transit Rich Areas",
# 				title = "") +
tm_shape(final_df, name = "Sensitive Communities Layer") +
	tm_polygons(scen,
			palette = c("#FF6633","#FF6633"),
			label = "Sensitive Communities",
			alpha = .5,
			border.alpha = .15,
			border.color = "gray",
			colorNA = NULL,
			title = "",
			id = "popup_text",
			popup.vars = c("Tot HH" = "tottenE.y",
						   "% Rent" = "tr_rentprop17",
						   "$ Rent" = "tr_medrent17",
						   "$ R Lag" = "tr_medrent17.lag",
						   "$ R Gap" = "tr_rentgap",
						   "Ch Rent" = "tr_chrent",
						   "Ch R Lag" = "tr_chrent.lag",
						   "% RB" = "tr_rbprop17",
						   "% ELI" = "tr_ELI_prop17",
						   "% VLI" = "tr_VLI_prop17",
						   "% Stud." = "tr_propstudent17",
						   "----------" = "text",
						   "Neigh." = "NeighType",
						   "% White" = "pwhite",
						   "% Black" = "pblack",
						   "% Asian" = "pasian",
						   "% Lat" = "platinx",
						   "% Other" = "pother",
						   "% POC" = "pPOC",
						   "% Welf" = "pwelfare",
						   "% Pov" = "ppoverty",
						   "% Unemp" = "unemp",
						   "%FHHw/C"= "pfemhhch",
						   "----------" = "text",
						   "SC Criteria" = "text",
						   "----------" = "text",
						   "POC" = "v_poc", 
						   "Renters" = renters,
						   "LI_Cat" = eli,
						   "RB" = rb,
						   "Ch Rent" = chrent,
						   "Rent Gap" = rentgap
						   ),
			popup.format = list(digits=2)) +
	tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 9), alpha = .5) +
	tm_layout(title = paste0(scen, ": ",renters,", ", eli, ", ", chrent, ", ", rentgap))

sen_map2 <- function(scen, renters, eli, rb, chrent, rentgap)
	sen_map1(scen, renters, eli, rb, chrent, rentgap) +
	tm_layout(title = paste0(scen, ": ",renters,", ", eli, ", ", rb, ", ", chrent, ", ", rentgap))

sen_map3 <- function(scen, renters, eli, rb, chrent, rentgap)
	sen_map1(scen, renters, eli, rb, chrent, rentgap) +
	tm_layout(title = paste0(scen, ": v_POC, ",renters,", ", eli, ", ", rb, ", ", chrent, ", ", rentgap))

save_map <- function(x,y)
	tmap_save(x, paste0("~/git/sensitive_communities/docs/", y, ".html"))

#
# Make maps
# --------------------------------------------------------------------------

# scen01 <- sen_map1("Scenario 01", "v_renters_co", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_10")
# scen02 <- sen_map1("Scenario 02", "v_renters_co", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
# scen03 <- sen_map1("Scenario 03", "v_renters_co", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_10")
# scen04 <- sen_map1("Scenario 04", "v_renters_co", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_co")

# scen05 <- sen_map2("Scenario 05", "v_renters_co", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_10")
# scen06 <- sen_map2("Scenario 06", "v_renters_co", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
# scen07 <- sen_map2("Scenario 07", "v_renters_co", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_10")
# scen08 <- sen_map2("Scenario 08", "v_renters_co", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_co")

# scen09 <- sen_map1("Scenario 09", "v_renters_60th", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_10")
# scen10 <- sen_map1("Scenario 10", "v_renters_60th", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
# scen11 <- sen_map1("Scenario 11", "v_renters_60th", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_10")
# scen12 <- sen_map1("Scenario 12", "v_renters_60th", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_co")

# scen13 <- sen_map2("Scenario 13", "v_renters_60th", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_10")
# scen14 <- sen_map2("Scenario 14", "v_renters_60th", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
# scen15 <- sen_map2("Scenario 15", "v_renters_60th", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_10")
# scen16 <- sen_map2("Scenario 16", "v_renters_60th", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_co")

# scen17 <- sen_map1("Scenario 17", "v_renters_50p", "v_ELI", "v_rb","dp_chrent_co", "dp_rentgap_10")
# scen18 <- sen_map1("Scenario 18", "v_renters_50p", "v_ELI", "v_rb","dp_chrent_co", "dp_rentgap_co")
# scen19 <- sen_map1("Scenario 19", "v_renters_50p", "v_ELI", "v_rb","dp_chrent_10", "dp_rentgap_10")
# scen20 <- sen_map1("Scenario 20", "v_renters_50p", "v_ELI", "v_rb","dp_chrent_10", "dp_rentgap_co")

# scen21 <- sen_map2("Scenario 21", "v_renters_50p", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_10")
# scen22 <- sen_map2("Scenario 22", "v_renters_50p", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
# scen23 <- sen_map2("Scenario 23", "v_renters_50p", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_10")
# scen24 <- sen_map2("Scenario 24", "v_renters_50p", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_co")
scen25 <- sen_map2("Scenario 25", "v_renters_30p", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
scen26 <- sen_map2("Scenario 26", "v_renters_30p", "v_VLI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
scen27 <- sen_map3("Scenario 27", "v_renters_30p", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
scen28 <- sen_map3("Scenario 28", "v_renters_30p", "v_VLI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
scen29 <- sen_map3("Scenario 29", "v_renters_30p", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_co")
scen30 <- sen_map3("Scenario 30", "v_renters_30p", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_10")
scen31 <- sen_map3("Scenario 31", "v_renters_30p", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_10")
scen32 <- sen_map3("Scenario 32", "v_renters_30p", "v_VLI", "v_rb", "dp_chrent_10", "dp_rentgap_co")
scen33 <- sen_map3("Scenario 33", "v_renters_30p", "v_VLI", "v_rb", "dp_chrent_co", "dp_rentgap_10")
scen34 <- sen_map3("Scenario 34", "v_renters_30p", "v_VLI", "v_rb", "dp_chrent_10", "dp_rentgap_10")
scen35 <- sen_map3("Scenario 35", "v_renters_30p", "v_VLI", "v_rb", "dp_chrent_co", "dp_rentgap_co")

final_df %>% 
	pull(`Scenario 25`) %>% 
	sum(na.rm = TRUE)
final_df %>% 
	pull(`Scenario 26`) %>% 
	sum(na.rm = TRUE)
final_df %>% 
	pull(`Scenario 27`) %>% 
	sum(na.rm = TRUE)
final_df %>% 
	pull(`Scenario 28`) %>% 
	sum(na.rm = TRUE)
final_df %>% 
	pull(`Scenario 29`) %>% 
	sum(na.rm = TRUE)
final_df %>% 
	pull(`Scenario 30`) %>% 
	sum(na.rm = TRUE)
final_df %>% 
	pull(`Scenario 31`) %>% 
	sum(na.rm = TRUE)
final_df %>% 
	pull(`Scenario 32`) %>% 
	sum(na.rm = TRUE)
final_df %>% 
	pull(`Scenario 33`) %>% 
	sum(na.rm = TRUE)
final_df %>% 
	pull(`Scenario 34`) %>% 
	sum(na.rm = TRUE)


final_df
#
# Save maps
# --------------------------------------------------------------------------

# save_map(scen01, "scen01")
# save_map(scen02, "scen02")
# save_map(scen03, "scen03")
# save_map(scen04, "scen04")
# save_map(scen05, "scen05")
# save_map(scen06, "scen06")
# save_map(scen07, "scen07")
# save_map(scen08, "scen08")
# save_map(scen09, "scen09")
# save_map(scen10, "scen10")
# save_map(scen11, "scen11")
# save_map(scen12, "scen12")
# save_map(scen13, "scen13")
# save_map(scen14, "scen14")
# save_map(scen15, "scen15")
# save_map(scen16, "scen16")
# save_map(scen17, "scen17")
# save_map(scen18, "scen18")
# save_map(scen19, "scen19")
# save_map(scen20, "scen20")
# save_map(scen21, "scen21")
# save_map(scen22, "scen22")
# save_map(scen23, "scen23")
# save_map(scen24, "scen24")
save_map(scen25, "scen25")
save_map(scen26, "scen26")
save_map(scen27, "scen27")
save_map(scen28, "scen28")
save_map(scen29, "scen29")
save_map(scen30, "scen30")
save_map(scen31, "scen31")
save_map(scen32, "scen32")
save_map(scen33, "scen33")
save_map(scen34, "scen34")

# ==========================================================================
# Get tract counts for each scenario
# ==========================================================================

	final_df %>%
	st_set_geometry(NULL) %>%
	ungroup() %>%
	select(GEOID, COUNTYFP, `Scenario 01`:`Scenario 24`) %>%
	gather(scenario, value, `Scenario 01`:`Scenario 24`) %>%
	group_by(scenario) %>%
	summarise(count = sum(case_when(value == TRUE ~ 1, TRUE ~ 0), na.rm = TRUE)) %>%
	data.frame()

#
# Bay area sen count
# --------------------------------------------------------------------------
bay_co <-
c("Alameda","Contra Costa","Marin","Napa","San Francisco","Santa Clara","San Mateo","Solano","Sonoma")

bay_co = c(001,013,041,055,075,081,085,095,097)

	final_df %>%
	st_set_geometry(NULL) %>%
	filter(as.numeric(COUNTYFP) %in% bay_co) %>%
	group_by(COUNTYFP) %>%
	summarise(n_sen = sum(`Scenario 22`, na.rm = TRUE),
			  t_count = n(),
			  p_sen = n_sen/t_count) %>%
	ungroup() %>%
	mutate(total_sen = sum(t_count),
		   p_sen_of_area = n_sen/total_sen)

#
# All county counts
# --------------------------------------------------------------------------


	final_df %>%
	st_set_geometry(NULL) %>%
	group_by(COUNTYFP) %>%
	summarise(n_sen = sum(`Scenario 22`, na.rm = TRUE),
			  t_count = n(),
			  p_sen = n_sen/t_count) %>%
	ungroup() %>%
	mutate(total_sen = sum(t_count),
		   p_sen_of_area = n_sen/total_sen) %>%
	data.frame()

# ==========================================================================
# Get proportion POC for SC and Counties
# ==========================================================================

#
# for sc 22
# --------------------------------------------------------------------------

	final_df %>%
	st_set_geometry(NULL) %>%
	group_by(COUNTYFP) %>%
	summarise(n_sen = sum(`Scenario 22`, na.rm = TRUE),
			  t_count = n(),
			  p_sen = n_sen/t_count) %>%
	ungroup() %>%
	mutate(total)



# ==========================================================================
# ==========================================================================
# ==========================================================================
# Export shapefile
# ==========================================================================
# ==========================================================================
# ==========================================================================

	final_df %>%
	ungroup() %>%
	mutate(
		tract_FIPS = GEOID,
		sens_com = `Scenario 22`,
		meets_renter_crit = v_renters_50p,
		meets_ELI_crit = v_ELI,
		meets_rentburden_crit = v_rb,
		meets_ch_Rent_crit = dp_chrent_co,
		meets_rentgap_crit = dp_rentgap_co,
		proportion_renters = tr_rentprop17,
		tract_median_rent = tr_medrent17,
		nearby_med_rent = tr_medrent17.lag,
		rent_gap = tr_rentgap,
		Rent_change = tr_chrent,
		nearby_rent_change = tr_chrent.lag,
		prop_rent_burden = tr_rbprop17,
		prop_ELI = tr_ELI_prop17,
		prop_students = tr_propstudent17,
		prop_white = pwhite,
		prop_Black = pblack,
		prop_Asian = pasian,
		prop_Lat = platinx,
		prop_Other = pother,
		prop_on_welfare = pwelfare,
		prop_poverty = ppoverty,
		prop_unemployed = unemp
		) %>%
	select(tract_FIPS:prop_unemployed) %>%
	# st_write("~/git/sensitive_communities/data/output/UDP_SCMap.shp")


# ==========================================================================
# Get demographics
# ==========================================================================

#
# For SC's
# --------------------------------------------------------------------------

senmed <-
	final_df %>%
	st_set_geometry(NULL) %>%
	filter(`Scenario 22` == 1) %>%
	group_by(COUNTYFP) %>%
	summarise(
		sentrmedWhite = median(round(pwhite, 3),na.rm = TRUE),
		sentrmedPOC = median(round(pPOC, 3),na.rm = TRUE),
		sentrmedBlack = median(round(pblack, 3),na.rm = TRUE),
		sentrmedAsian = median(round(pasian, 3),na.rm = TRUE),
		sentrmedLat = median(round(platinx, 3),na.rm = TRUE),
		sentrmedOther = median(round(pother, 3),na.rm = TRUE),
		sentrmedRB = median(round(tr_rbprop17, 3),na.rm = TRUE),
		sentrmedELI = median(round(tr_ELI_prop17, 3),na.rm = TRUE),
		sentrmedRent = median(round(tr_medrent17, 3),na.rm = TRUE),
		sentrmedrent_Gap = median(round(tr_rentgap, 3),na.rm = TRUE),
		sentrmedCh_Rent = median(round(tr_chrent, 3),na.rm = TRUE)) %>%
	data.frame()

final_df %>%
st_set_geometry(NULL) %>%
group_by(COUNTYFP) %>%
	summarise(
		cotrmedWhite = median(round(pwhite, 3),na.rm = TRUE),
		cotrmedPOC = median(round(pPOC, 3),na.rm = TRUE),
		cotrmedBlack = median(round(pblack, 3),na.rm = TRUE),
		cotrmedAsian = median(round(pasian, 3),na.rm = TRUE),
		cotrmedLat = median(round(platinx, 3),na.rm = TRUE),
		cotrmedOther = median(round(pother, 3),na.rm = TRUE),
		cotrmedRB = median(round(tr_rbprop17, 3),na.rm = TRUE),
		cotrmedELI = median(round(tr_ELI_prop17, 3),na.rm = TRUE),
		cotrmedRent = median(round(tr_medrent17, 3),na.rm = TRUE),
		cotrmedrent_Gap = median(round(tr_rentgap, 3),na.rm = TRUE),
		cotrmedCh_Rent = median(round(tr_chrent, 3),na.rm = TRUE)) %>%
	left_join(., senmed) %>%
	data.frame




	# pRent = paste0(min(round(tr_rentprop17, 3)*100), " / ",median(round(tr_rentprop17, 3)*100), " / ", max(round(tr_rentprop17, 3)*100)),
	# rent_Lag = paste0(min(round(tr_medrent17.lag, 3)*100), " / ",median(round(tr_medrent17.lag, 3)*100), " / ", max(round(tr_medrent17.lag, 3)*100)),
	# Ch_rent_Lag = paste0(min(round(tr_chrent.lag, 3)*100), " / ",median(round(tr_chrent.lag, 3)*100), " / ", max(round(tr_chrent.lag, 3)*100)),
	# students = paste0(min(round(tr_propstudent17, 3)*100), " / ",median(round(tr_propstudent17, 3)*100), " / ", max(round(tr_propstudent17, 3)*100)),
	# Welf = paste0(min(round(pwelfare, 3)*100), " / ",median(round(pwelfare, 3)*100), " / ", max(round(pwelfare, 3)*100)),
	# Pov = paste0(min(round(ppoverty, 3)*100), " / ",median(round(ppoverty, 3)*100), " / ", max(round(ppoverty, 3)*100)),
	# Unemp = paste0(min(round(unemp, 3)*100), " / ",median(round(unemp, 3)*100), " / ", max(round(unemp, 3)*100)),
	# FemHHwCh = paste0(min(round(pfemhhch, 3)*100), " / ",median(round(pfemhhch, 3)*100), " / ", max(round(pfemhhch, 3)*100))) %>%
data.frame




# ==========================================================================
# ==========================================================================
# Version 2
# ==========================================================================
# ==========================================================================

# ==========================================================================
# ==========================================================================
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
