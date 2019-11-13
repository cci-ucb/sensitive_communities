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
# https://docs.google.com/document/d/15G-sDDMfl1eFpouwWzfMhhORYGzj9uUTsxnteoOxMHo/edit
# https://docs.google.com/document/d/14RpROPfs4F65GwmOn3r3RR9_W40CRpQa79hCjCQTGrE/edit
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
# Block group data - no income table so foregoing BG's now.
# --------------------------------------------------------------------------

# unzip("~/git/sensitive_communities/data/ca_tracts_bgs.zip")
# unzip("~/git/sensitive_communities/data/rent12_bgs.csv.zip")

# bg <-
# 	st_read("~/git/sensitive_communities/data/ca_tracts_bgs/ca_tracts_bgs.shp") %>%
# 	st_as_sf() %>%
# 	st_set_geometry(NULL)

# bgdf12 <- fread("~/git/sensitive_communities/data/rent12_bgs.csv")

# bg12 <-
# 	get_acs(
# 		geography = "block group",
# 		variables = dis_var,
# 		state = "CA",
# 		county = NULL,
# 		geometry = TRUE,
# 		cache_table = TRUE,
# 		output = "wide",
# 		year = 2017
# 		)



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
	select(GEOID,
		   COUNTYFP,
		   co_medinc.x,
		   HHInc_TotalE.x,
		   HHIncTenRentE.x:HHIncTenRent_151E.x,
		   -ends_with("M.x")) %>%
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
	select(GEOID,
		   COUNTYFP,
		   co_medinc.y,
		   HHInc_TotalE.y,
		   HHIncTenRentE.y:HHIncTenRent_151E.y,
		   -ends_with("M.y")) %>%
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
	dist_nb <- dnearneigh(coords, d1=0, d2 = .5*max_1nn, row.names = IDs)
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

#
# Rent Burden by income
# Testing:
# 	1. Rent burden capped at $35k
# 	2. Rent burden by 80%, 50%, and 30% AMI
# 	3. by rent burden at 30% and 50%
# --------------------------------------------------------------------------

lirb17 <-
	left_join(
		get_acs(
			geography = "tract",
			variables = ir_var,
			state = "CA",
			county = NULL,
			geometry = FALSE,
			cache_table = TRUE,
			# output = "wide",
			year = 2017
			),
		ct_sf %>%
			st_set_geometry(NULL) %>%
			select(GEOID, medinc = co_medinc.y)) %>%
	separate(variable, c("ir_type", "rb", "income")) %>%
	left_join(.,ct_sf %>% select(GEOID, COUNTYFP))


tot_ir <-
	lirb17 %>%
	filter(rb == "tot", income == "tot") %>%
	group_by(GEOID, COUNTYFP) %>%
	summarise(tot_ir = sum(estimate, na.rm = TRUE)) %>%
	ungroup()

irli <-
	lirb17 %>%
	filter(income != "tot") %>%
	select(-ir_type) %>%
	mutate(income = as.numeric(income),
		   bottom_inccat = case_when(
								income == 9999 ~ income - 9999,
								income == 19999 ~ income - 9999,
								income == 34999 ~ income - 14999,
								income == 49999 ~ income - 14999,
								income == 74999 ~ income - 24999,
								income == 99999 ~ income - 24999,
								income == 100000 ~ 100000,
								TRUE ~ NA_real_
								),
		   top_inccat = case_when(income == 100000 ~ 200000,
		   						 TRUE ~ income),
		   LI_val = medinc*.8,
		   VLI_val = medinc*.5,
		   ELI_val = medinc*.3,
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
		   LI_ir_count = LI*estimate,
		   VLI_ir_count = VLI*estimate,
		   ELI_ir_count = ELI*estimate)

#
# Create ir tables
# --------------------------------------------------------------------------

rb30 <- c("349","399","499", "5plus")

ir35k_30rb <-
	irli %>%
	filter(income <= 35000,
		   rb %in% rb30) %>%
	group_by(GEOID) %>%
	summarise(ir35_30rb = sum(estimate, na.rm = TRUE)) %>%
	left_join(tot_ir,.) %>%
	mutate(ir35k_30rbp = ir35_30rb/tot_ir) %>%
	group_by(COUNTYFP) %>%
	mutate(ir35_30_coavg = mean(ir35k_30rbp, na.rm = TRUE),
		   ir35_30_comed = median(ir35k_30rbp, na.rm = TRUE),
		   v_rb35k_30rb = case_when(ir35k_30rbp > ir35_30_comed ~ 1)) %>%
	ungroup()

ir35k_50rb <-
	irli %>%
	filter(income <= 35000,
		   rb == "5plus") %>%
	group_by(GEOID) %>%
	summarise(ir35_50rb = sum(estimate, na.rm = TRUE)) %>%
	left_join(tot_ir,.) %>%
	mutate(ir35_50rbp = ir35_50rb/tot_ir) %>%
	group_by(COUNTYFP) %>%
	mutate(ir35_50_coavg = mean(ir35_50rbp, na.rm = TRUE),
		   ir35_50_comed = median(ir35_50rbp, na.rm = TRUE),
		   v_rb35k_50rb = case_when(ir35_50rbp > ir35_50_comed ~ 1)) %>%
	ungroup()

irLI_30rb <-
	irli %>%
	filter(LI > 0,
		   rb %in% rb30) %>%
	group_by(GEOID) %>%
	summarise(irLI_30rb = sum(LI_ir_count, na.rm = TRUE)) %>%
	left_join(tot_ir,.) %>%
	mutate(irLI_30p = irLI_30rb/tot_ir) %>%
	group_by(COUNTYFP) %>%
	mutate(irLI_30_coavg = mean(irLI_30p, na.rm = TRUE),
		   irLI_30_comed = median(irLI_30p, na.rm = TRUE),
		   v_rbLI_30rb = case_when(irLI_30p > irLI_30_comed ~ 1)) %>%
	ungroup()

irLI_50rb <-
	irli %>%
	filter(LI > 0,
		   rb == "5plus") %>%
	group_by(GEOID) %>%
	summarise(irLI_50rb = sum(LI_ir_count, na.rm = TRUE)) %>%
	left_join(tot_ir,.) %>%
	mutate(irLI_50p = irLI_50rb/tot_ir) %>%
	group_by(COUNTYFP) %>%
	mutate(irLI_50_coavg = mean(irLI_50p, na.rm = TRUE),
		   irLI_50_comed = median(irLI_50p, na.rm = TRUE),
		   v_rbLI_50rb = case_when(irLI_50p > irLI_50_comed ~ 1)) %>%
	ungroup()

irVLI_30rb <-
	irli %>%
	filter(VLI > 0,
		   rb %in% rb30) %>%
	group_by(GEOID) %>%
	summarise(irVLI_30rb = sum(LI_ir_count, na.rm = TRUE)) %>%
	left_join(tot_ir,.) %>%
	mutate(irVLI_30p = irVLI_30rb/tot_ir) %>%
	group_by(COUNTYFP) %>%
	mutate(irVLI_30_coavg = mean(irVLI_30p, na.rm = TRUE),
		   irVLI_30_comed = median(irVLI_30p, na.rm = TRUE),
		   v_rbVLI_30rb = case_when(irVLI_30p > irVLI_30_comed ~ 1)) %>%
	ungroup()

irVLI_50rb <-
	irli %>%
	filter(VLI > 0,
		   rb == "5plus") %>%
	group_by(GEOID) %>%
	summarise(irVLI_50rb = sum(LI_ir_count, na.rm = TRUE)) %>%
	left_join(tot_ir,.) %>%
	mutate(irVLI_50p = irVLI_50rb/tot_ir) %>%
	group_by(COUNTYFP) %>%
	mutate(irVLI_50_coavg = mean(irVLI_50p, na.rm = TRUE),
		   irVLI_50_comed = median(irVLI_50p, na.rm = TRUE),
		   v_rbVLI_50rb = case_when(irVLI_50p > irVLI_50_comed ~ 1)) %>%
	ungroup()

irELI_30rb <-
	irli %>%
	filter(ELI > 0,
		   rb %in% rb30) %>%
	group_by(GEOID) %>%
	summarise(irELI_30rb = sum(LI_ir_count, na.rm = TRUE)) %>%
	left_join(tot_ir,.) %>%
	mutate(irELI_30p = irELI_30rb/tot_ir) %>%
	group_by(COUNTYFP) %>%
	mutate(irELI_30_coavg = mean(irELI_30p, na.rm = TRUE),
		   irELI_30_comed = median(irELI_30p, na.rm = TRUE),
		   v_rbELI_30rb = case_when(irELI_30p > irELI_30_comed ~ 1)) %>%
	ungroup()

irELI_50rb <-
	irli %>%
	filter(ELI > 0,
		   rb == "5plus") %>%
	group_by(GEOID) %>%
	summarise(irELI_50rb = sum(LI_ir_count, na.rm = TRUE)) %>%
	left_join(tot_ir,.) %>%
	mutate(irELI_50p = irELI_50rb/tot_ir) %>%
	group_by(COUNTYFP) %>%
	mutate(irELI_50_coavg = mean(irELI_50p, na.rm = TRUE),
		   irELI_50_comed = median(irELI_50p, na.rm = TRUE),
		   v_rbELI_50rb = case_when(irELI_50p > irELI_50_comed ~ 1)) %>%
	ungroup()


ir_df <-
	left_join(ir35k_30rb %>% select(GEOID, ends_with("p"), starts_with("v_rb")),
			  ir35k_50rb %>% select(GEOID, ends_with("p"), starts_with("v_rb"))) %>%
	left_join(., irLI_30rb %>% select(GEOID, ends_with("p"), starts_with("v_rb"))) %>%
	left_join(., irLI_50rb %>% select(GEOID, ends_with("p"), starts_with("v_rb"))) %>%
	left_join(., irVLI_30rb %>% select(GEOID, ends_with("p"), starts_with("v_rb"))) %>%
	left_join(., irVLI_50rb %>% select(GEOID, ends_with("p"), starts_with("v_rb"))) %>%
	left_join(., irELI_30rb %>% select(GEOID, ends_with("p"), starts_with("v_rb"))) %>%
	left_join(., irELI_50rb %>% select(GEOID, ends_with("p"), starts_with("v_rb"))) %>%
	replace(is.na(.), 0)

# ==========================================================================
# Create Final DF
# ==========================================================================

final_df <-
	left_join(ct_sf, cal_nt) %>%
	left_join(., ir_df) %>%
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
		v_VLI_med = case_when(tr_propstudent17 < .20 & # v2
		   				tr_VLI_prop17 > co_VLI_propmed17 ~ 1, # v2
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
# 		`Scenario 01` = case_when(sum(v_renters_co, v_ELI, na.rm = TRUE) == 2 &
# 						   sum(dp_chrent_co, dp_rentgap_10) >=1 ~ TRUE),
# 		`Scenario 02` = case_when(sum(v_renters_co, v_ELI, na.rm = TRUE) == 2 &
# 						   sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
# 		`Scenario 03` = case_when(sum(v_renters_co, v_ELI, na.rm = TRUE) == 2 &
# 						   sum(dp_chrent_10, dp_rentgap_10) >=1 ~ TRUE),
# 		`Scenario 04` = case_when(sum(v_renters_co, v_ELI, na.rm = TRUE) == 2 &
# 						   sum(dp_chrent_10, dp_rentgap_co) >=1 ~ TRUE),

# 		`Scenario 05` = case_when(sum(v_renters_co, v_ELI, v_rb, na.rm = TRUE) == 3 &
# 						   sum(dp_chrent_co, dp_rentgap_10) >=1 ~ TRUE),
# 		`Scenario 06` = case_when(sum(v_renters_co, v_ELI, v_rb, na.rm = TRUE) == 3 &
# 						   sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
# 		`Scenario 07` = case_when(sum(v_renters_co, v_ELI, v_rb, na.rm = TRUE) == 3 &
# 						   sum(dp_chrent_10, dp_rentgap_10) >=1 ~ TRUE),
# 		`Scenario 08` = case_when(sum(v_renters_co, v_ELI, v_rb, na.rm = TRUE) == 3 &
# 						   sum(dp_chrent_10, dp_rentgap_co) >=1 ~ TRUE),

# 		`Scenario 09` = case_when(sum(v_renters_60th, v_ELI, na.rm = TRUE) == 2 &
# 						   sum(dp_chrent_co, dp_rentgap_10) >=1 ~ TRUE),
# 		`Scenario 10` = case_when(sum(v_renters_60th, v_ELI, na.rm = TRUE) == 2 &
# 						   sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
# 		`Scenario 11` = case_when(sum(v_renters_60th, v_ELI, na.rm = TRUE) == 2 &
# 						   sum(dp_chrent_10, dp_rentgap_10) >=1 ~ TRUE),
# 		`Scenario 12` = case_when(sum(v_renters_60th, v_ELI, na.rm = TRUE) == 2 &
# 						   sum(dp_chrent_10, dp_rentgap_co) >=1 ~ TRUE),

# 		`Scenario 13` = case_when(sum(v_renters_60th, v_ELI, v_rb, na.rm = TRUE) == 3 &
# 						   sum(dp_chrent_co, dp_rentgap_10) >=1 ~ TRUE),
# 		`Scenario 14` = case_when(sum(v_renters_60th, v_ELI, v_rb, na.rm = TRUE) == 3 &
# 						   sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
# 		`Scenario 15` = case_when(sum(v_renters_60th, v_ELI, v_rb, na.rm = TRUE) == 3 &
# 						   sum(dp_chrent_10, dp_rentgap_10) >=1 ~ TRUE),
# 		`Scenario 16` = case_when(sum(v_renters_60th, v_ELI, v_rb, na.rm = TRUE) == 3 &
# 						   sum(dp_chrent_10, dp_rentgap_co) >=1 ~ TRUE),

# 		`Scenario 17` = case_when(sum(v_renters_50p, v_ELI, na.rm = TRUE) == 2 &
# 						   sum(dp_chrent_co, dp_rentgap_10) >=1 ~ TRUE),
# 		`Scenario 18` = case_when(sum(v_renters_50p, v_ELI, na.rm = TRUE) == 2 &
# 						   sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
# 		`Scenario 19` = case_when(sum(v_renters_50p, v_ELI, na.rm = TRUE) == 2 &
# 						   sum(dp_chrent_10, dp_rentgap_10) >=1 ~ TRUE),
# 		`Scenario 20` = case_when(sum(v_renters_50p, v_ELI, na.rm = TRUE) == 2 &
# 						   sum(dp_chrent_10, dp_rentgap_co) >=1 ~ TRUE),

# 		`Scenario 21` = case_when(sum(v_renters_50p, v_ELI, v_rb, na.rm = TRUE) == 3 &
# 						   sum(dp_chrent_co, dp_rentgap_10) >=1 ~ TRUE),
# 		`Scenario 22` = case_when(sum(v_renters_50p,
# 						`			  v_ELI,
# 									  v_rb, na.rm = TRUE) == 3 &
# 						   sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
# 		`Scenario 23` = case_when(sum(v_renters_50p, v_ELI, v_rb, na.rm = TRUE) == 3 &
# 						   sum(dp_chrent_10, dp_rentgap_10) >=1 ~ TRUE),
# 		`Scenario 24` = case_when(sum(v_renters_50p, v_ELI, v_rb, na.rm = TRUE) == 3 &
# 						   sum(dp_chrent_10, dp_rentgap_co) >=1 ~ TRUE),
# 		`Scenario 25` = case_when(sum(v_renters_30p,
# 									  v_ELI,
# 									  v_rb, na.rm = TRUE) == 3 &
# 						   		  sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
# 		`Scenario 26` = case_when(sum(v_renters_30p,
# 									  v_VLI,
# 									  v_rb, na.rm = TRUE) == 3 &
# 						   		  sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
# 		`Scenario 27` = case_when(sum(v_poc,
# 									  v_renters_30p,
# 									  v_ELI,
# 									  v_rb, na.rm = TRUE) >= 3 &
# 						   		  sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
# 		`Scenario 28` = case_when(sum(v_poc,
# 									  v_renters_30p,
# 									  v_VLI,
# 									  v_rb, na.rm = TRUE) >= 3 &
# 						   		  sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
# 		`Scenario 29` = case_when(sum(v_poc,
# 									  v_renters_30p,
# 									  v_ELI,
# 									  v_rb, na.rm = TRUE) >= 3 &
# 						   		  sum(dp_chrent_10, dp_rentgap_co) >=1 ~ TRUE),
# 		`Scenario 30` = case_when(sum(v_poc,
# 									  v_renters_30p,
# 									  v_ELI,
# 									  v_rb, na.rm = TRUE) >= 3 &
# 						   		  sum(dp_chrent_co, dp_rentgap_10) >=1 ~ TRUE),
# 		`Scenario 31` = case_when(sum(v_poc,
# 									  v_renters_30p,
# 									  v_ELI,
# 									  v_rb, na.rm = TRUE) >= 3 &
# 						   		  sum(dp_chrent_10, dp_rentgap_10) >=1 ~ TRUE),
# 		`Scenario 32` = case_when(sum(v_poc,
# 									  v_renters_30p,
# 									  v_VLI,
# 									  v_rb, na.rm = TRUE) >= 3 &
# 						   		  sum(dp_chrent_10, dp_rentgap_co) >=1 ~ TRUE),
# 		`Scenario 33` = case_when(sum(v_poc,
# 									  v_renters_30p,
# 									  v_VLI,
# 									  v_rb, na.rm = TRUE) >= 3 &
# 						   		  sum(dp_chrent_co, dp_rentgap_10) >=1 ~ TRUE),
# 		`Scenario 34` = case_when(sum(v_poc,
# 									  v_renters_30p,
# 									  v_VLI,
# 									  v_rb, na.rm = TRUE) >= 3 &
# 						   		  sum(dp_chrent_10, dp_rentgap_10) >=1 ~ TRUE),
# 		`Scenario 35` = case_when(tr_propstudent17 < .20 &
# 								  v_poc == 1 &
# 								  v_renters_30p == 1 &
# 								  sum(v_VLI,
# 									  v_rb, na.rm = TRUE) >= 1 ~ TRUE,
# 						   		  sum(dp_chrent_co, dp_rentgap_co) >= 1 ~ TRUE),
# # Version 2
# 		`Scenario 36` = case_when(sum(v_poc,
# 								  	  v_renters_40p,
# 								  	  v_VLI,
# 									  v_rb35k_30rb, na.rm = TRUE) >= 3 &
# 						   		  sum(dp_chrent_co,
# 						   		  	  dp_rentgap_co) >= 1 &
# 						   		  totraceE.y >= 500 &
# 						   		  pPOC >= .3 &
# 						   		  tr_propstudent17 < .20 ~ TRUE
# 						   		  ),
# 		`Scenario 37` = case_when(sum(v_poc,
# 						  	  v_renters_40p,
# 						  	  v_VLI,
# 							  v_rb35k_50rb, na.rm = TRUE) >= 3 &
# 				   		  sum(dp_chrent_co,
# 				   		  	  dp_rentgap_co) >= 1 &
# 				   		  totraceE.y >= 500 &
# 				   		  pPOC >= .3 &
# 				   		  tr_propstudent17 < .20 ~ TRUE
# 				   		  ),
# 		`Scenario 38` = case_when(sum(v_poc,
# 						  	  v_renters_40p,
# 						  	  v_VLI,
# 							  v_rbLI_30rb, na.rm = TRUE) >= 3 &
# 				   		  sum(dp_chrent_co,
# 				   		  	  dp_rentgap_co) >= 1 &
# 				   		  totraceE.y >= 500 &
# 				   		  pPOC >= .3 &
# 				   		  tr_propstudent17 < .20 ~ TRUE
# 				   		  ),
# 		`Scenario 39` = case_when(sum(v_poc,
# 						  	  v_renters_40p,
# 						  	  v_VLI,
# 							  v_rbLI_50rb, na.rm = TRUE) >= 3 &
# 				   		  sum(dp_chrent_co,
# 				   		  	  dp_rentgap_co) >= 1 &
# 				   		  totraceE.y >= 500 &
# 				   		  pPOC >= .3 &
# 				   		  tr_propstudent17 < .20 ~ TRUE
# 				   		  ),
# 		`Scenario 40` = case_when(sum(v_poc,
# 						  	  v_renters_40p,
# 						  	  v_VLI,
# 							  v_rbVLI_30rb, na.rm = TRUE) >= 3 &
# 				   		  sum(dp_chrent_co,
# 				   		  	  dp_rentgap_co) >= 1 &
# 				   		  totraceE.y >= 500 &
# 				   		  pPOC >= .3 &
# 				   		  tr_propstudent17 < .20 ~ TRUE
# 				   		  ),
# 		`Scenario 41` = case_when(sum(v_poc,
# 						  	  v_renters_40p,
# 						  	  v_VLI,
# 							  v_rbVLI_50rb, na.rm = TRUE) >= 3 &
# 				   		  sum(dp_chrent_co,
# 				   		  	  dp_rentgap_co) >= 1 &
# 				   		  totraceE.y >= 500 &
# 				   		  pPOC >= .3 &
# 				   		  tr_propstudent17 < .20 ~ TRUE
# 				   		  ),
# 		`Scenario 42` = case_when(sum(v_poc,
# 						  	  v_renters_40p,
# 						  	  v_VLI,
# 							  v_rbELI_30rb, na.rm = TRUE) >= 3 &
# 				   		  sum(dp_chrent_co,
# 				   		  	  dp_rentgap_co) >= 1 &
# 				   		  totraceE.y >= 500 &
# 				   		  pPOC >= .3 &
# 				   		  tr_propstudent17 < .20 ~ TRUE
# 				   		  ),
# 		`Scenario 43` = case_when(sum(v_poc,
# 						  	  v_renters_40p,
# 						  	  v_VLI,
# 							  v_rbELI_50rb, na.rm = TRUE) >= 3 &
# 				   		  sum(dp_chrent_co,
# 				   		  	  dp_rentgap_co) >= 1 &
# 				   		  totraceE.y >= 500 &
# 				   		  pPOC >= .3 &
# 				   		  tr_propstudent17 < .20 ~ TRUE
# 				   		  ),
# # With VLI must
# 		`Scenario 44` = case_when(v_VLI == 1 &
# 								  sum(v_poc,
# 								  	  v_renters_40p,
# 									  v_rb35k_30rb, na.rm = TRUE) >= 2 &
# 						   		  sum(dp_chrent_co,
# 						   		  	  dp_rentgap_co) >= 1 &
# 						   		  totraceE.y >= 500 &
# 						   		  pPOC >= .3 &
# 						   		  tr_propstudent17 < .20 ~ TRUE
# 						   		  ),
# 		`Scenario 45` = case_when(v_VLI == 1 &
# 								  sum(v_poc,
# 						  	  v_renters_40p,
# 							  v_rb35k_50rb, na.rm = TRUE) >= 2 &
# 				   		  sum(dp_chrent_co,
# 				   		  	  dp_rentgap_co) >= 1 &
# 				   		  totraceE.y >= 500 &
# 				   		  pPOC >= .3 &
# 				   		  tr_propstudent17 < .20 ~ TRUE
# 				   		  ),
# 		`Scenario 46` = case_when(v_VLI == 1 &
# 								  sum(v_poc,
# 						  	  v_renters_40p,
# 							  v_rbLI_30rb, na.rm = TRUE) >= 2 &
# 				   		  sum(dp_chrent_co,
# 				   		  	  dp_rentgap_co) >= 1 &
# 				   		  totraceE.y >= 500 &
# 				   		  pPOC >= .3 &
# 				   		  tr_propstudent17 < .20 ~ TRUE
# 				   		  ),
# 		`Scenario 47` = case_when(v_VLI == 1 &
# 								  sum(v_poc,
# 						  	  v_renters_40p,
# 							  v_rbLI_50rb, na.rm = TRUE) >= 2 &
# 				   		  sum(dp_chrent_co,
# 				   		  	  dp_rentgap_co) >= 1 &
# 				   		  totraceE.y >= 500 &
# 				   		  pPOC >= .3 &
# 				   		  tr_propstudent17 < .20 ~ TRUE
# 				   		  ),
# 		`Scenario 48` = case_when(v_VLI == 1 &
# 								  sum(v_poc,
# 						  	  v_renters_40p,
# 							  v_rbVLI_30rb, na.rm = TRUE) >= 2 &
# 				   		  sum(dp_chrent_co,
# 				   		  	  dp_rentgap_co) >= 1 &
# 				   		  totraceE.y >= 500 &
# 				   		  pPOC >= .3 &
# 				   		  tr_propstudent17 < .20 ~ TRUE
# 				   		  ),
# 		`Scenario 49` = case_when(v_VLI == 1 &
# 								  sum(v_poc,
# 						  	  v_renters_40p,
# 							  v_rbVLI_50rb, na.rm = TRUE) >= 2 &
# 				   		  sum(dp_chrent_co,
# 				   		  	  dp_rentgap_co) >= 1 &
# 				   		  totraceE.y >= 500 &
# 				   		  pPOC >= .3 &
# 				   		  tr_propstudent17 < .20 ~ TRUE
# 				   		  ),
# 		`Scenario 50` = case_when(v_VLI == 1 &
# 								  sum(v_poc,
# 						  	  v_renters_40p,
# 							  v_rbELI_30rb, na.rm = TRUE) >= 2 &
# 				   		  sum(dp_chrent_co,
# 				   		  	  dp_rentgap_co) >= 1 &
# 				   		  totraceE.y >= 500 &
# 				   		  pPOC >= .3 &
# 				   		  tr_propstudent17 < .20 ~ TRUE
# 				   		  ),
		`Scenario 51` = case_when(v_VLI == 1 &
								  sum(v_poc,
						  	  v_renters_40p,
							  v_rbELI_50rb, na.rm = TRUE) >= 2 &
				   		  sum(dp_chrent_co,
				   		  	  dp_rentgap_co) >= 1 &
				   		  totraceE.y >= 500 &
				   		  pPOC >= .3 &
				   		  tr_propstudent17 < .20 ~ TRUE
				   		  ),
		`Scenario 52` = case_when(v_VLI_med == 1 &
								  sum(v_poc,
						  	  v_renters_40p,
							  v_rbVLI_50rb, na.rm = TRUE) >= 2 &
				   		  sum(dp_chrent_co,
				   		  	  dp_rentgap_co) >= 1 &
				   		  totraceE.y >= 500 &
				   		  pPOC >= .3 &
				   		  tr_propstudent17 < .20 ~ TRUE
				   		  ),
		`Scenario 53` = case_when(v_VLI_med == 1 &
								  sum(v_poc,
						  	  v_renters_50p,
							  v_rbVLI_50rb, na.rm = TRUE) >= 2 &
				   		  sum(dp_chrent_co,
				   		  	  dp_rentgap_co) >= 1 &
				   		  totraceE.y >= 500 &
				   		  pPOC >= .3 &
				   		  tr_propstudent17 < .20 ~ TRUE
				   		  ),
		`sc52_tier1` = case_when(v_VLI_med == 1 &
								 sum(v_poc,
									 v_renters_40p,
									 v_rbLI_50rb, na.rm = TRUE) >= 2 &
								 sum(dp_chrent_co,
									 dp_rentgap_co) >= 1 &
								 totraceE.y >= 500 &
								 pPOC >= .3 &
								 tr_propstudent17 < .20 ~ "Heightened Sensitivity"),
		`sc52_tier2` = case_when(v_VLI_med == 1 &
								 sum(v_poc,
										v_renters_40p,
										v_rbLI_50rb, na.rm = TRUE) >= 2 &
									totraceE.y >= 500 &
									pPOC >= .3 &
									tr_propstudent17 < .20 ~ "Vulnerable"),
		`sc52_tier3PV` = case_when(v_VLI_med == 1 &
								   v_poc == 1 &
								   totraceE.y >= 500 &
								   tr_propstudent17 < .20 ~ "Some Vulnerability - POC+VLI"),
		`sc52_tier3PRB` = case_when(v_poc == 1 &
								   v_rbVLI_50rb == 1 &
								   totraceE.y >= 500 &
								   tr_propstudent17 < .20 ~ "Some Vulnerability - POC+RB"),
		`sc52_tier3PR` = case_when(v_poc == 1 &
								   v_renters_40p == 1,
								   totraceE.y >= 500 &
								   tr_propstudent17 < .20 ~ "Some Vulnerability - POC+Renters"),
		`sc52_tier3VLI` = case_when(v_VLI_med == 1 &
								    totraceE.y >= 500 &
								    tr_propstudent17 < .20 ~ "Some Vulnerability - VLI Alone"),
		`sc52_tier3RBV` = case_when(v_VLI_med == 1 &
									v_rbVLI_50rb == 1 &
								    totraceE.y >= 500 &
								    tr_propstudent17 < .20 ~ "Some Vulnerability - VLI+RB"),
		`sc52_tier3RV` = case_when(v_VLI_med == 1 &
								   v_rbVLI_50rb == 1 &
								   totraceE.y >= 500 &
								   tr_propstudent17 < .20 ~ "Some Vulnerability - Renter+VLI")
		) %>%
ungroup()


# Issues:
# 	1. should we use median of medians for county medians?
#

# POC+Renter
# VLI alone
# POC+Renter
# RB+VLI
# Renter+VLI






#
# Tiers
# --------------------------------------------------------------------------



#
# Advocate highlights
# --------------------------------------------------------------------------

adv_ssc <- c("06001422300","06001441923","06001403300","06041125000","06013339002","06013340001","06075015600","06075045200","06075042601","06075047701","06075042700","06085511301","06097152203","06097152000","06097153001","06097153104","06097151401","06097153200","06097151308","06041112100","06041111000","06037265700","06037265202","06037700801","06037461502","06037302401","06037302505","06037302506","06037302302","06037302201","06037302004","06037301802","06037301702","06037301900","06037301100","06037302002","06037302003","06037301206","06037301601","06037310703","06037310702","06037310701","06083000301","06083001102","06037701702","06059042313","06037701100","06037264103","06037221710","06037222200","06037221601","06037222100","06037222500","06037231300","06037231210","06037231220","06037231100","06037224600","06037224420","06037189800","06037189904","06037190100","06037190700","06037190802","06065044807","06065044509","06067006900","06067006800","06067006702","06067006701","06067006202","06067005505","06067005506","06067005510","06067005601","06067005602","06067006002","06067007413","06067007504","06067007301","06067007601","06067006003","06067007414","06067007423","06067007501","06067008133","06067007701","06067005903","06067007019","06067007015")
adv_notsc <- c("06075023400","06075020900","06081611800","06081611900","06001406000","06075026003","06075010600","06075011700","06075011800","06075012301","06075012501","06075012502","06075015900","06075023001","06075023102","06075023103","06075023200","06075023300","06075023400","06075025701","06075025702","06075025800","06075026001","06075026003","06075026301","06075026303","06075033203","06075033204","06075035202","06075047701","06075047902","06075060502","06019000300","06047000304","06047000301","06099002503","06037462100","06037234600","06037234700","06037234800","06037234300","06047000203","06047000201","06089012101","06063000501","06023001000","06029004101","06029006304","06029006202","06037219500","06037234200","06037234300","06037234000","06037231400","06001407300","06001409000","06001401700","06013376000","06095250801","06055201006","06055201005","06055201007","06001408500","06001408400","06037209401","06037208902","06037208801","06037224320","06037224310","06037212202","06037206031","06037204700","06037204120","06037203710","06037203800","06037203600","06037204920","06037203900","06037532603","06037532604","06037532606","06037533202","06037534501","06037534502","06037535607","06037540300","06037540501","06037535605","06037535803","06037535802","06037535901","06037535902","06037536104","06037540101","06037540102","06037541700","06037542000","06037541802","06037540000","06037542104","06037540600","06037542401","06037542200","06037570404","06037543201","06037543100","06037543000","06037542900","06037542800","06037541002","06037291220","06037542602","06037541001","06037291210","06037541100","06037541200","06037541300","06037553200","06037553300","06037554103","06037553100","06037554105","06037554002","06037554001","06037554101","06037554301","06037554201","06037188300","06037188100","06019002602","06019003600","06019002100","06019004207","06019004404","06065045604")

glimpse(final_df)
# st_write(final_df, "~/data/sensitive_communities/final_df_v1.shp")

####
# Things to consider
# 	- some other threshold of VLI other than the median
# 	-

# ==========================================================================
# Map
# ==========================================================================

#
# Mapping function (transit on hold till update)
# --------------------------------------------------------------------------

tmap_mode("view")

sen_map1 <- function(scen, renters, li, lirb, rb, chrent, rentgap)
tm_basemap(leaflet::providers$CartoDB.Positron) + # http://leaflet-extras.github.io/leaflet-providers/preview/
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
			popup.vars = c("Tot Pop" = "totraceE.y"
						   "Tot HH" = "tottenE.y",
						   "% Rent" = "tr_rentprop17",
						   "$ Rent" = "tr_medrent17",
						   "$ R Lag" = "tr_medrent17.lag",
						   "$ R Gap" = "tr_rentgap",
						   "Ch Rent" = "tr_chrent",
						   "Ch R Lag" = "tr_chrent.lag",
						   "% RB" = "tr_rbprop17",
						   "% inc x rb " = lirb,
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
						   "LI_Cat" = li,
						   "RB" = rb,
						   "Ch Rent" = chrent,
						   "Rent Gap" = rentgap
						   ),
			popup.format = list(digits=2)) +
	tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 9), alpha = .5) +
	tm_layout(title = paste0(scen, ": ",renters,", ", li, ", ", chrent, ", ", rentgap))

sen_map2 <- function(scen, renters, li, lirb, rb, chrent, rentgap)
	sen_map1(scen, renters, li, lirb, rb, chrent, rentgap) +
	tm_layout(title = paste0(scen, ": ",renters,", ", li, ", ", rb, ", ", chrent, ", ", rentgap))

sen_map3 <- function(scen, renters, li, lirb, rb, chrent, rentgap)
	sen_map1(scen, renters, li, lirb, rb, chrent, rentgap) +
	tm_layout(title = paste0(scen, ": v_POC, ",renters,", ", li, ", ", rb, ", ", chrent, ", ", rentgap))

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
# scen25 <- sen_map2("Scenario 25", "v_renters_30p", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
# scen26 <- sen_map2("Scenario 26", "v_renters_30p", "v_VLI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
# scen27 <- sen_map3("Scenario 27", "v_renters_30p", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
# scen28 <- sen_map3("Scenario 28", "v_renters_30p", "v_VLI", "v_rb", "dp_chrent_co", "dp_rentgap_co")
# scen29 <- sen_map3("Scenario 29", "v_renters_30p", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_co")
# scen30 <- sen_map3("Scenario 30", "v_renters_30p", "v_ELI", "v_rb", "dp_chrent_co", "dp_rentgap_10")
# scen31 <- sen_map3("Scenario 31", "v_renters_30p", "v_ELI", "v_rb", "dp_chrent_10", "dp_rentgap_10")
# scen32 <- sen_map3("Scenario 32", "v_renters_30p", "v_VLI", "v_rb", "dp_chrent_10", "dp_rentgap_co")
# scen33 <- sen_map3("Scenario 33", "v_renters_30p", "v_VLI", "v_rb", "dp_chrent_co", "dp_rentgap_10")
# scen34 <- sen_map3("Scenario 34", "v_renters_30p", "v_VLI", "v_rb", "dp_chrent_10", "dp_rentgap_10")
# scen35 <- sen_map3("Scenario 35", "v_renters_30p", "v_VLI", "v_rb", "dp_chrent_co", "dp_rentgap_co")

# V2
scen36 <- sen_map3("Scenario 36", "v_renters_40p", "v_VLI", "ir35k_30rbp", "v_rb35k_30rb", "dp_chrent_co", "dp_rentgap_co")
scen37 <- sen_map3("Scenario 37", "v_renters_40p", "v_VLI", "ir35_50rbp", "v_rb35k_50rb", "dp_chrent_co", "dp_rentgap_co")
scen38 <- sen_map3("Scenario 38", "v_renters_40p", "v_VLI", "irLI_30p", "v_rbLI_30rb", "dp_chrent_co", "dp_rentgap_co")
scen39 <- sen_map3("Scenario 39", "v_renters_40p", "v_VLI", "irLI_50p", "v_rbLI_50rb", "dp_chrent_co", "dp_rentgap_co")
scen40 <- sen_map3("Scenario 40", "v_renters_40p", "v_VLI", "irVLI_30p", "v_rbVLI_30rb", "dp_chrent_co", "dp_rentgap_co")
scen41 <- sen_map3("Scenario 41", "v_renters_40p", "v_VLI", "irVLI_50p", "v_rbVLI_50rb", "dp_chrent_co", "dp_rentgap_co")
scen42 <- sen_map3("Scenario 42", "v_renters_40p", "v_VLI", "irELI_30p", "v_rbELI_30rb", "dp_chrent_co", "dp_rentgap_co")
scen43 <- sen_map3("Scenario 43", "v_renters_40p", "v_VLI", "irELI_50p", "v_rbELI_50rb", "dp_chrent_co", "dp_rentgap_co")
scen44 <- sen_map3("Scenario 44", "v_renters_40p", "v_VLI", "ir35k_30rbp", "v_rb35k_30rb", "dp_chrent_co", "dp_rentgap_co")
scen45 <- sen_map3("Scenario 45", "v_renters_40p", "v_VLI", "ir35_50rbp", "v_rb35k_50rb", "dp_chrent_co", "dp_rentgap_co")
scen46 <- sen_map3("Scenario 46", "v_renters_40p", "v_VLI", "irLI_30p", "v_rbLI_30rb", "dp_chrent_co", "dp_rentgap_co")
scen47 <- sen_map3("Scenario 47", "v_renters_40p", "v_VLI", "irLI_50p", "v_rbLI_50rb", "dp_chrent_co", "dp_rentgap_co")
scen48 <- sen_map3("Scenario 48", "v_renters_40p", "v_VLI", "irVLI_30p", "v_rbVLI_30rb", "dp_chrent_co", "dp_rentgap_co")
scen49 <- sen_map3("Scenario 49", "v_renters_40p", "v_VLI", "irVLI_50p", "v_rbVLI_50rb", "dp_chrent_co", "dp_rentgap_co")
scen50 <- sen_map3("Scenario 50", "v_renters_40p", "v_VLI", "irELI_30p", "v_rbELI_30rb", "dp_chrent_co", "dp_rentgap_co")
scen51 <- sen_map3("Scenario 51", "v_renters_40p", "v_VLI", "irELI_50p", "v_rbELI_50rb", "dp_chrent_co", "dp_rentgap_co")
scen52 <- sen_map3("Scenario 52", "v_renters_40p", "v_VLI_med", "irVLI_50p", "v_rbVLI_50rb", "dp_chrent_co", "dp_rentgap_co")
scen53 <- sen_map3("Scenario 53", "v_renters_50p", "v_VLI_med", "irVLI_50p", "v_rbVLI_50rb", "dp_chrent_co", "dp_rentgap_co")


final_df %>%
	pull(`Scenario 36`) %>%
	sum(na.rm = TRUE)
final_df %>%
	pull(`Scenario 37`) %>%
	sum(na.rm = TRUE)
final_df %>%
	pull(`Scenario 38`) %>%
	sum(na.rm = TRUE)
final_df %>%
	pull(`Scenario 39`) %>%
	sum(na.rm = TRUE)
final_df %>%
	pull(`Scenario 40`) %>%
	sum(na.rm = TRUE)
final_df %>%
	pull(`Scenario 41`) %>%
	sum(na.rm = TRUE)
final_df %>%
	pull(`Scenario 42`) %>%
	sum(na.rm = TRUE)
final_df %>%
	pull(`Scenario 43`) %>%
	sum(na.rm = TRUE)
final_df %>%
	pull(`Scenario 44`) %>%
	sum(na.rm = TRUE)
final_df %>%
	pull(`Scenario 45`) %>%
	sum(na.rm = TRUE)
final_df %>%
	pull(`Scenario 46`) %>%
	sum(na.rm = TRUE)
final_df %>%
	pull(`Scenario 47`) %>%
	sum(na.rm = TRUE)
final_df %>%
	pull(`Scenario 48`) %>%
	sum(na.rm = TRUE)
final_df %>%
	pull(`Scenario 49`) %>%
	sum(na.rm = TRUE)
final_df %>%
	pull(`Scenario 50`) %>%
	sum(na.rm = TRUE)
final_df %>%
	pull(`Scenario 51`) %>%
	sum(na.rm = TRUE)
final_df %>%
	pull(`Scenario 53`) %>%
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
# save_map(scen25, "scen25")
# save_map(scen26, "scen26")
# save_map(scen27, "scen27")
# save_map(scen28, "scen28")
# save_map(scen29, "scen29")
# save_map(scen30, "scen30")
# save_map(scen31, "scen31")
# save_map(scen32, "scen32")
# save_map(scen33, "scen33")
# save_map(scen34, "scen34")
save_map(scen36, "scen36")
save_map(scen37, "scen37")
save_map(scen38, "scen38")
save_map(scen39, "scen39")
save_map(scen40, "scen40")
save_map(scen41, "scen41")
save_map(scen42, "scen42")
save_map(scen43, "scen43")

save_map(scen44, "scen44")
save_map(scen45, "scen45")
save_map(scen46, "scen46")
save_map(scen47, "scen47")
save_map(scen48, "scen48")
save_map(scen49, "scen49")
save_map(scen50, "scen50")
save_map(scen51, "scen51")
save_map(scen52, "scen52")

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
