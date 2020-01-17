# ==========================================================================
# Develop data for displacement and vulnerability measures
# Author: Tim Thomas - timthomas@berkeley.edu
# Created: 2019.10.13
# 1.0 code: 2019.12.1
# ==========================================================================

# Clear the session
rm(list = ls())
options(scipen = 10) # avoid scientific notation

# ==========================================================================
# Libraries
# ==========================================================================

#
# Load packages and install them if they're not installed.
# --------------------------------------------------------------------------

# load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rmapshaper, scales, data.table, plotly, spdep, tidyverse, tigris, tidycensus, tmap, leaflet)

# Cache downloaded tiger files
options(tigris_use_cache = TRUE)

# ==========================================================================
# Data
# ==========================================================================

#
# Transit Rich Areas
# --------------------------------------------------------------------------

unzip("~/git/sensitive_communities/data/transit_buffers_sc.zip")
transit <- st_read("~/git/sensitive_communities/data/transit_buffers_sc/transit_buffers_sc.shp") %>% st_as_sf()

#
# Census Variables
# --------------------------------------------------------------------------

source("~/git/sensitive_communities/code/vars.r")

#
# Tract data
# --------------------------------------------------------------------------

ct <- tracts(state = "CA", cb = TRUE) # cb option downloads a smaller shapefile

### Tract data extraction function
tr_data <- function(year, vars, output = "tidy")
	get_acs(
		geography = "tract",
		variables = vars,
		state = "CA",
		county = NULL,
		geometry = FALSE,
		cache_table = TRUE,
		output = output,
		year = year,
		keep_geo_vars = TRUE
		)

# 2017 data
tr_df17 <-
    tr_data(2017, sc_vars) %>%
    mutate(COUNTY = substr(GEOID, 1, 5))

# 2012 data
tr_df12 <-
	tr_data(2012, c('medrent12' = 'B25064_001')) %>%
    mutate(COUNTY = substr(GEOID, 1, 5),
    	   estimate = estimate*1.07)

# merge dataframes
df <-
	left_join(tr_df17 %>%
				select(-moe) %>%
				spread(variable, estimate),
			  tr_df12 %>%
			  	select(-moe) %>%
			  	spread(variable, estimate))

# identify low data quality tracts using `totrent` variable
low_dq <-
	tr_df17 %>%
	filter(variable == "totrent") %>%
	group_by(GEOID) %>%
	mutate(quality = case_when((moe/estimate) >= .6 ~ NA_real_,
							   TRUE ~ estimate)) %>%
	filter(is.na(quality)) %>%
	select(GEOID) %>%
	distinct() %>%
	pull()

#
# County subdivision tracts and place tracts for "Big City" definitions
# --------------------------------------------------------------------------

# Early versions considered larger cities but we only use county subdivision for Fresno
csd <-
	county_subdivisions("CA", cb = TRUE, class = "sf") %>%
	filter(NAME %in% c("Fresno"#,
					   # "Los Angeles",
					   # "South Gate-East Los Angeles",
					   # "Santa Monica",
					   # "South Bay Cities",
					   # "Torrance",
					   # "Palos Verdes",
					   # "Inglewood",
					   # "Long Beach-Lakewood",
					   # "Compton",
					   # "Downey-Norwalk",
					   # "Southwest San Gabriel Valley",
					   # "San Francisco",
					   # "Oakland"
					   ))

csd_tracts <-
	sf::st_centroid(sf::st_as_sf(ct)) %>%
	sf::st_set_crs(4269) %>%
	.[csd, ] %>%
	st_set_geometry(NULL) %>%
	select(GEOID) %>%
	pull()

# We use places for the following cities. 
place <-
	places("CA", cb = TRUE, class = "sf") %>%
	filter(NAME %in% c("Los Angeles",
					   "San Francisco",
					   "San Diego",
					   "San Jose",
					   "Sacramento",
					   "Oakland",
					   "San Francisco"))
place_tracts <-
	sf::st_centroid(sf::st_as_sf(ct)) %>%
	sf::st_set_crs(4269) %>%
	.[place, ] %>%
	st_set_geometry(NULL) %>%
	select(GEOID) %>%
	pull()


big_city_tracts <- c(csd_tracts, place_tracts)

	tm_shape(place) +
		tm_polygons("NAME") +
	tm_shape(csd) +
		tm_polygons("NAME")

#
# VLI data
# --------------------------------------------------------------------------

# rename income categories
inc_names17 <- c(
	'HHInc_10' = 9999, # Less than $10,000 HOUSEHOLD INCOME 9999
	'HHInc_15' = 14999, # $10,000 to $14,999 HOUSEHOLD INCOME
	'HHInc_20' = 19999, # $15,000 to $19,999 HOUSEHOLD INCOME
	'HHInc_25' = 24999, # $20,000 to $24,999 HOUSEHOLD INCOME
	'HHInc_30' = 29999, # $25,000 to $29,999 HOUSEHOLD INCOME
	'HHInc_35' = 34999, # $30,000 to $34,999 HOUSEHOLD INCOME
	'HHInc_40' = 39999, # $35,000 to $39,999 HOUSEHOLD INCOME
	'HHInc_45' = 44999, # $40,000 to $44,999 HOUSEHOLD INCOME
	'HHInc_50' = 49999, # $45,000 to $49,999 HOUSEHOLD INCOME 4999
	'HHInc_60' = 59999, # $50,000 to $59,999 HOUSEHOLD INCOME 9999
	'HHInc_75' = 74999, # $60,000 to $74,999 HOUSEHOLD INCOME
	'HHInc_100' = 99999, # $75,000 to $99,999 HOUSEHOLD INCOME
	'HHInc_125' = 124999, # $100,000 to $124,999 HOUSEHOLD INCOME
	'HHInc_150' = 149999, # $125,000 to $149,999 HOUSEHOLD INCOME 24999
	'HHInc_200' = 199999, # $150,000 to $199,999 HOUSEHOLD INCOME
	'HHInc_250' = 249999 # $200,000 or more HOUSEHOLD INCOME 49999
	)

# create dataframe
df_vli <-
	df %>%
	select(GEOID,
		   COUNTY,
		   mhhinc,
		   HHInc_10:HHInc_75) %>%
	group_by(COUNTY) %>%
	mutate(co_mhhinc = median(mhhinc, na.rm = TRUE),
		   co_LI_val = .8*co_mhhinc,
		   co_VLI_val = .5*co_mhhinc,
		   co_ELI_val = .3*co_mhhinc) %>%
	ungroup() %>%
	gather(medinc_cat, medinc_count, HHInc_10:HHInc_75) %>%
	mutate(medinc_cat = recode(medinc_cat, !!!inc_names17)) %>%
	mutate(bottom_inccat = case_when(medinc_cat == 9999 ~ medinc_cat - 9999,
									 medinc_cat > 9999 &
									 medinc_cat <= 49999 ~ medinc_cat - 4999,
									 medinc_cat == 59999 ~ medinc_cat - 9999,
									 medinc_cat > 59999 &
									 medinc_cat <= 149999 ~ medinc_cat - 24999,
									 medinc_cat >= 199999 ~ medinc_cat - 49999,
								  TRUE ~ NA_real_),
		top_inccat = medinc_cat,
		LI = case_when(co_LI_val >= top_inccat ~ 1,
					   co_LI_val <= top_inccat &
					   co_LI_val >= bottom_inccat ~
			   		   (co_LI_val - bottom_inccat)/(top_inccat - bottom_inccat),
			   		   TRUE ~ 0),
		VLI = case_when(co_VLI_val >= top_inccat ~ 1,
					    co_VLI_val <= top_inccat &
			   			co_VLI_val >= bottom_inccat ~
			   			(co_VLI_val - bottom_inccat)/(top_inccat - bottom_inccat),
			   			TRUE ~ 0),
		ELI = case_when(co_ELI_val >= top_inccat ~ 1,
						co_ELI_val <= top_inccat &
						co_ELI_val >= bottom_inccat ~
						(co_ELI_val - bottom_inccat)/(top_inccat - bottom_inccat),
						TRUE ~ 0)) %>%
	group_by(GEOID) %>%
	mutate(tr_totinc_count = sum(medinc_count, na.rm = TRUE),
		   tr_LI_count = sum(LI*medinc_count, na.rm = TRUE),
		   tr_VLI_count = sum(VLI*medinc_count, na.rm = TRUE),
		   tr_ELI_count = sum(ELI*medinc_count, na.rm = TRUE),
		   tr_LI_prop = tr_LI_count/tr_totinc_count,
		   tr_VLI_prop = tr_VLI_count/tr_totinc_count,
		   tr_ELI_prop = tr_ELI_count/tr_totinc_count) %>%
	select(GEOID:co_ELI_val, tr_totinc_count:tr_ELI_prop) %>%
	distinct()

df_vli %>%
	filter(GEOID == "06001401600")

#
# Rent Burden by income for renters
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
		df_vli %>%
			select(GEOID, COUNTY, co_mhhinc)) %>%
	separate(variable, c("ir_type", "rb", "income"))

tot_ir <-
	lirb17 %>%
	filter(rb == "tot", income == "tot") %>%
	group_by(GEOID, COUNTY) %>%
	summarise(tot_ir = sum(estimate, na.rm = TRUE)) %>%
	ungroup()

df_irli <-
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
		   co_LI_val = co_mhhinc*.8,
		   co_VLI_val = co_mhhinc*.5,
		   co_ELI_val = co_mhhinc*.3,
		   LI = case_when(co_LI_val >= top_inccat ~ 1,
						  co_LI_val <= top_inccat &
						  co_LI_val >= bottom_inccat ~
						  (co_LI_val - bottom_inccat)/(top_inccat - bottom_inccat),
						  TRUE ~ 0),
		   VLI = case_when(co_VLI_val >= top_inccat ~ 1,
						   co_VLI_val <= top_inccat &
						   co_VLI_val >= bottom_inccat ~
						   (co_VLI_val - bottom_inccat)/(top_inccat - bottom_inccat),
						   TRUE ~ 0),
		   ELI = case_when(co_ELI_val >= top_inccat ~ 1,
						   co_ELI_val <= top_inccat &
						   co_ELI_val >= bottom_inccat ~
						   (co_ELI_val - bottom_inccat)/(top_inccat - bottom_inccat),
						   TRUE ~ 0),
		   tr_LI_ir_count = LI*estimate,
		   tr_VLI_ir_count = VLI*estimate,
		   tr_ELI_ir_count = ELI*estimate)


df_LI30RB <-
	df_irli %>%
	filter(LI > 0,
		   rb %in% c("349","399","499", "5plus")) %>%
	group_by(GEOID) %>%
	summarise(tr_irLI_30rb_ct = sum(tr_LI_ir_count, na.rm = TRUE)) %>%
	left_join(tot_ir,.) %>%
	mutate(tr_irLI_30p = tr_irLI_30rb_ct/tot_ir) %>%
	group_by(COUNTY) %>%
	mutate(co_irLI_30_med = median(tr_irLI_30p, na.rm = TRUE),
		   v_rbLI_30rb = case_when(tr_irLI_30p > co_irLI_30_med ~ 1,
		   						   TRUE ~ 0)) %>%
	ungroup()

df_LI50RB <-
	df_irli %>%
	filter(VLI > 0,
		   rb == "5plus") %>%
	group_by(GEOID) %>%
	summarise(tr_irLI_50rb_ct = sum(tr_LI_ir_count, na.rm = TRUE)) %>%
	left_join(tot_ir,.) %>%
	mutate(tr_irLI_50p = tr_irLI_50rb_ct/tot_ir) %>%
	group_by(COUNTY) %>%
	mutate(co_irLI_50_med = median(tr_irLI_50p, na.rm = TRUE),
		   v_rbLI_50rb = case_when(tr_irLI_50p > co_irLI_50_med ~ 1,
		   						   TRUE ~ 0)) %>%
	ungroup()

df_VLI30RB <-
	df_irli %>%
	filter(VLI > 0,
		   rb %in% c("349","399","499", "5plus")) %>%
	group_by(GEOID) %>%
	summarise(tr_irVLI_30rb_ct = sum(tr_VLI_ir_count, na.rm = TRUE)) %>%
	left_join(tot_ir,.) %>%
	mutate(tr_irVLI_30p = tr_irVLI_30rb_ct/tot_ir) %>%
	group_by(COUNTY) %>%
	mutate(co_irVLI_30_med = median(tr_irVLI_30p, na.rm = TRUE),
		   v_rbVLI_30rb = case_when(tr_irVLI_30p > co_irVLI_30_med ~ 1,
		   						   TRUE ~ 0)) %>%
	ungroup()

df_VLI50RB <-
	df_irli %>%
	filter(LI > 0,
		   rb == "5plus") %>%
	group_by(GEOID) %>%
	summarise(tr_irVLI_50rb_ct = sum(tr_VLI_ir_count, na.rm = TRUE)) %>%
	left_join(tot_ir,.) %>%
	mutate(tr_irVLI_50p = tr_irVLI_50rb_ct/tot_ir) %>%
	group_by(COUNTY) %>%
	mutate(co_irVLI_50_med = median(tr_irVLI_50p, na.rm = TRUE),
		   v_rbVLI_50rb = case_when(tr_irVLI_50p > co_irVLI_50_med ~ 1,
		   						   TRUE ~ 0)) %>%
	ungroup()

df_irli_f <-
	left_join(df_LI30RB, df_LI50RB) %>%
	left_join(., df_VLI30RB) %>%
	left_join(., df_VLI50RB)

#
# Neighborhood Typologies
# --------------------------------------------------------------------------

source("~/git/Functions/NeighType_Fun.R")
# get code from https://gitlab.com/timathomas/Functions/blob/master/NeighType_Fun.R

df_nt <-
	ntdf(state = "CA") %>%
	select(GEOID,NeighType) 

ntcheck(df_nt)

# ==========================================================================
# Create lag variables
# ==========================================================================

#
# Fill in NA's with county medians for lag development
# --------------------------------------------------------------------------

df_rent <-
	df %>%
	group_by(COUNTY) %>%
	mutate(medrent = case_when(is.na(medrent) ~ median(medrent, na.rm = TRUE),
							   TRUE ~ medrent),
		   tr_medrent12 = case_when(is.na(medrent12) ~ median(medrent12, na.rm = TRUE),
		   					   TRUE ~ medrent12),
		   tr_chrent = medrent - tr_medrent12,
		   tr_pchrent = (medrent - tr_medrent12)/tr_medrent12, 
		   co_medrent12 = median(tr_medrent12, na.rm = TRUE))

ct <- tracts("CA", cb = TRUE)

ct@data <-
	left_join(ct@data, df_rent, by = "GEOID") %>%
	left_join(., df_vli) %>%
	left_join(., df_irli_f) %>%
	left_join(., df_nt)

#
# Create neighbor matrix
# --------------------------------------------------------------------------
	coords <- coordinates(ct)
	IDs <- row.names(as(ct, "data.frame"))
	ct_nb <- poly2nb(ct) # nb
	lw_bin <- nb2listw(ct_nb, style = "W", zero.policy = TRUE)

	kern1 <- knn2nb(knearneigh(coords, k = 1), row.names=IDs)
	dist <- unlist(nbdists(kern1, coords)); summary(dist)
	max_1nn <- max(dist)
	dist_nb <- dnearneigh(coords, d1=0, d2 = .1*max_1nn, row.names = IDs)
	spdep::set.ZeroPolicyOption(TRUE)
	spdep::set.ZeroPolicyOption(TRUE)
	dists <- nbdists(dist_nb, coordinates(ct))
	idw <- lapply(dists, function(x) 1/(x^2))
	lw_dist_idwW <- nb2listw(dist_nb, glist = idw, style = "W")

#
# Create select lag variables
# --------------------------------------------------------------------------

	ct$tr_pchrent.lag <- lag.listw(lw_dist_idwW,ct$tr_pchrent)
	ct$tr_chrent.lag <- lag.listw(lw_dist_idwW,ct$tr_chrent)
	ct$tr_medrent.lag <- lag.listw(lw_dist_idwW,ct$medrent)

#
# Develop variables
# --------------------------------------------------------------------------

ct_new <- ct

ct_new@data <-
	ct_new@data %>%
	group_by(GEOID) %>%
	mutate(
		tr_population = race_tot,
		tr_dq = case_when(GEOID %in% low_dq ~ 0,
						  TRUE ~ 1),
		tr_rentgap = tr_medrent.lag - medrent,
		tr_rentgapprop = tr_rentgap/((medrent + tr_medrent.lag)/2),
		tr_pstudents = sum(st_colenroll, st_proenroll, na.rm = TRUE)/st_totenroll,
		tr_povstudents = sum(st_pov_under, st_pov_grad, na.rm = TRUE), 
		tr_medrent = medrent,
		tr_prenters = totrent/totten,
		tr_rb = sum(rb_34.9, rb_39.9, rb_49.9, rb_55, na.rm = TRUE)/rb_tot,
		tr_households = totten,
		tr_pWhite = race_White/race_tot,
		tr_pBlack = race_Black/race_tot,
		tr_pAsian = race_Asian/race_tot,
		tr_pLatinx = race_Latinx/race_tot,
		tr_pOther = (race_tot - race_White - race_Black - race_Asian - race_Latinx)/race_tot,
		tr_pPOC = 1-(race_White/race_tot),
		tr_pwelf = welf/welf_tot,
		tr_ppoverty = sum(pov_famh, pov_nonfamh, na.rm = TRUE)/pov_tot,
		tr_punemp = unemp/unemp_tot,
		tr_pfemhhch = sum(fhh_famheadch, fhh_nonfamheadch, na.rm = TRUE)/fhh_tot
		) %>%
	group_by(COUNTYFP) %>%
	mutate(tr_POC_rank = rank(tr_pPOC)/length(tr_pPOC),
		   co_rentgap = median(tr_rentgap, na.rm = TRUE),
		   co_rentgapprop = median(tr_rentgapprop, na.rm = TRUE),
		   co_pstudents = median(tr_pstudents, na.rm = TRUE),
		   co_irVLI_50p = median(tr_irVLI_50p, na.rm = TRUE),
		   # co_pchrent = median(case_when(tr_pchrent >= 0 ~ tr_pchrent), na.rm = TRUE),
		   co_pchrent = median(tr_pchrent, na.rm = TRUE),
		   co_prenters = median(tr_prenters, na.rm = TRUE),
		   co_pPOC = median(tr_pPOC, na.rm = TRUE),
		   co_LI_prop = median(tr_LI_prop, na.rm = TRUE),
		   co_VLI_prop = median(tr_VLI_prop, na.rm = TRUE),
		   co_ELI_prop = median(tr_ELI_prop, na.rm = TRUE)) %>%
	## Scenario Criteria
	group_by(GEOID) %>%
	mutate(
		big_city = case_when(GEOID %in% big_city_tracts ~ 1,
							 TRUE ~ 0),
		v_VLI = case_when(tr_pstudents < .2 & 
						  tr_VLI_prop > .2 ~ 1,
						  # tr_pstudents >= .2 & (tr_VLI_count - tr_povstudents)/tr_totinc_count > co_VLI_prop ~ 1,
						  # tr_VLI_prop > co_VLI_prop ~ 1,
						  TRUE ~ 0),
		v_Renters = case_when(tr_prenters > .4 ~ 1,
							  TRUE ~ 0),
		v_RB30LI = case_when(tr_irLI_30p > co_irLI_30_med ~ 1,
						   TRUE ~ 0),
		v_RB50LI = case_when(tr_irLI_50p > co_irLI_50_med ~ 1,
						   TRUE ~ 0),
		v_RB30VLI = case_when(tr_irVLI_30p > co_irVLI_30_med ~ 1,
						   TRUE ~ 0),
		v_RB50VLI = case_when(tr_irVLI_50p > co_irVLI_50_med ~ 1,
						   TRUE ~ 0),
		v_POC = case_when(tr_pPOC > .5 ~ 1, # changed to 50%
						  TRUE ~ 0),
		dp_PChRent = case_when(#(tr_medrent12/co_medrent12) < 1.5 & 
							   tr_pchrent > 0 & 
							   tr_pchrent > co_pchrent ~ 1,
							   # (tr_medrent12/co_medrent12) < 1.5 & 
		   					   tr_pchrent.lag > co_pchrent ~ 1,
						  	   TRUE ~ 0),
		dp_RentGap = case_when(tr_rentgapprop > 0 & tr_rentgapprop > co_rentgapprop ~ 1,
						  	   TRUE ~ 0),
	## Scenarios
		scen1.VLI50RB = case_when(v_VLI == 1 &
							  tr_pstudents < .2 &
							  tr_population >= 500 &
							  sum(v_Renters,
							  	  v_RB50VLI,
							  	  v_POC, na.rm = TRUE) >= 2 &
							  sum(dp_PChRent,
							  	  dp_RentGap, na.rm = TRUE) >= 1 ~ 1,
						  tr_POC_rank >= .95 &
							  tr_pstudents < .2 &
							  tr_population >= 500 &
							  sum(v_VLI,
							  	  v_Renters,
							  	  v_RB50VLI,
							  	  v_POC, na.rm = TRUE) >= 3 &
							  sum(dp_PChRent,
							  	  dp_RentGap, na.rm = TRUE) >= 1 ~ 1,
						  tr_pPOC >= .9 &
							  tr_pstudents < .2 &
							  tr_population >= 500 &
							  sum(v_VLI,
							  	  v_Renters,
							  	  v_RB50VLI,
							  	  v_POC, na.rm = TRUE) >= 3 &
							  sum(dp_PChRent,
							  	  dp_RentGap, na.rm = TRUE) >= 1 ~ 1,
						  TRUE ~ 0),
		scen3.VLI50RB = case_when(big_city == 1 ~ scen1.VLI50RB,
						  big_city == 0 &
						  	tr_pstudents < .2 &
							tr_population >= 500 &
							v_VLI == 1 &
							sum(v_Renters, v_RB50VLI, v_POC, na.rm = TRUE) >= 2 &
							sum(dp_PChRent, dp_RentGap, na.rm = TRUE) == 2 ~ 1,
						  TRUE ~ 0)) %>%
	ungroup()

#
# Find surrounding sensitive communities
# --------------------------------------------------------------------------
	ct_new$tr_sc_VLI50RB.lag <- lag.listw(lw_bin,ct_new$scen1.VLI50RB)

#
# Curate final dataset
# --------------------------------------------------------------------------
df_final.RB50VLI <-
	ct_new %>%
	st_as_sf() %>%
	select(GEOID,
		   COUNTY,
		   NeighType,
		   big_city,
		   starts_with("tr_"),
		   starts_with("co_"),
		   starts_with("v_"),
		   starts_with("dp_"),
		   starts_with("scen")) %>%
	group_by(GEOID) %>%
	mutate(
		tier2 = case_when(tr_dq == 0 ~ NA_character_,
						  tr_pstudents < .2 &
							tr_population >= 500 &
						  	v_VLI == 1 &
							  sum(v_Renters,
							  	  v_RB50VLI,
							  	  v_POC, na.rm = TRUE) >= 2 ~ "Vulnerable"),
		tier3 = case_when(tr_dq == 0 ~ NA_character_,
						  sum(v_POC, v_VLI, na.rm = TRUE) == 2 |
						  sum(v_POC, v_RB50VLI, na.rm = TRUE) == 2|
						  sum(v_POC, v_Renters, na.rm = TRUE) == 2|
						  v_VLI == 1 |
						  sum(v_VLI, v_RB50VLI, na.rm = TRUE) == 2|
						  sum(v_VLI, v_Renters, na.rm = TRUE) == 2 ~ "Tier 3: Some Vulnerability"),
		tier1 = case_when(tr_dq == 0 ~ "Poor Data Quality",
						    scen3.VLI50RB == 1 ~ "Heightened Sensitivity",
						    big_city == 1 &
							tr_sc_VLI50RB.lag >= .6 &
							tr_pstudents < .2 &
							tr_population >= 500 &
						  	v_VLI == 1 &
							sum(v_Renters,
								v_RB50VLI,
								v_POC, na.rm = TRUE) >= 2 ~ "Heightened Sensitivity"),
		text = "",
		popup_text = paste0("Tract: ", GEOID)) %>% 
	ungroup()

write_csv(df_final.RB50VLI %>% st_set_geometry(NULL), "~/git/sensitive_communities/data/final_df.csv.bz2")
st_write(df_final.RB50VLI, "~/git/sensitive_communities/data/final_df.shp", delete_layer = TRUE)

#
# Descriptive statistics
# --------------------------------------------------------------------------

# Medians and means
df_final.RB50VLI %>% 
st_set_geometry(NULL) %>% 
ungroup() %>% 
filter(tier1 == "Heightened Sensitivity") %>% 
summarise(
		  pPOC = mean(tr_pPOC), 
		  mPOC = median(tr_pPOC), 
		  pRenter = mean(tr_prenters), 
		  mRenter = median(tr_prenters), 
		  pVLI = mean(tr_VLI_prop), 
		  mVLI = median(tr_VLI_prop), 
		  pVLI_50RB = mean(tr_irVLI_50p), 
		  mVLI_50RB = median(tr_irVLI_50p),
		  ppChRent = mean(tr_pchrent), 
		  mpChRent = median(tr_pchrent)
		  ) %>% 
data.frame()

# Neighborhood type
df_final.RB50VLI %>% 
st_set_geometry(NULL) %>% 
group_by(NeighType) %>% 
summarise(count_ca = n()) %>% 
left_join(. , 
	df_final.RB50VLI %>% 
	st_set_geometry(NULL) %>% 
	filter(tier1 == "Heightened Sensitivity") %>% 
	group_by(NeighType) %>% 
	summarise(count_sc = n())) %>% 
data.frame() %>% arrange(desc(count_ca)) %>% 
mutate(p = percent(count_sc/count_ca, accuracy = .1))


#
# Race concatenation
# --------------------------------------------------------------------------

data <- 
	fread("~/git/sensitive_communities/data/final_df.csv.bz2")

data %>% 
	group_by(tier1) %>% 
	summarise(poc = median(tr_pPOC, na.rm = TRUE))


#                   NeighType count_ca count_sc      p
# 1              White-Latinx     2562      553  21.6%
# 2        White-Asian-Latinx     1922      474  24.7%
# 3               White-Asian      509       63  12.4%
# 4              White-Shared      449       14   3.1% # Mostly white
# 13                All White       56       NA   <NA> # Mostly white
# 5        White-Black-Latinx      433      145  33.5%
# 6              Asian-Latinx      376      172  45.7%
# 7  White-Black-Asian-Latinx      348      132  37.9%
# 8                All Latinx      335      143  42.7%
# 9              Black-Latinx      304      185  60.9%
# 10            Latinx-Shared      293      153  52.2%
# 11       Black-Asian-Latinx       94       52  55.3%
# 12       White-Latinx-Other       90       18  20.0%
# 21             Asian-Shared       13        3  23.1% # Mostly POC
# 22 Black-Asian-Latinx-Other       10        7  70.0% # Mostly POC
# 24       Black-Latinx-Other        7        5  71.4% # Mostly POC
# 25             Black-Shared        7        3  42.9% # Mostly POC
# 26       Asian-Latinx-Other        5        3  60.0% # Mostly POC
# 27              Asian-Black        3        2  66.7% # Mostly POC
# 28                All Other        2       NA   <NA> # Mostly POC
# 30                All Asian        1        1 100.0% # Mostly POC
# 31              Black-Other        1       NA   <NA> # Mostly POC
# 32             Latinx-Other        1       NA   <NA> # Mostly POC
# 33             Other-Shared        1       NA   <NA> # Mostly POC
# 14 White-Asian-Latinx-Other       55        9  16.4% # Diverse
# 15               Integrated       44       18  40.9% # Diverse
# 17 White-Black-Latinx-Other       26       16  61.5% # Diverse
# 18              White-Other       20        1   5.0% # Diverse
# 19        White-Asian-Other       18        4  22.2% # Diverse
# 20        White-Black-Asian       16        5  31.2% # Diverse
# 23              White-Black        8        3  37.5% # Diverse
# 29        White-Black-Other        2        1  50.0% # Diverse
# 34  White-Black-Asian-Other        1       NA   <NA> # Diverse
# 16        unpopulated tract       29       NA   <NA>

# Descriptive stats
check <- 
	df_final.RB50VLI %>% 
	st_set_geometry(NULL) %>% 
	mutate(nt2 = case_when(
		NeighType == "White-Shared" | 
		NeighType == "All White" ~ "Mostly White", 
		NeighType == "Asian-Shared" |
		NeighType == "Black-Asian-Latinx-Other" |
		NeighType == "Black-Latinx-Other" |
		NeighType == "Black-Shared" |
		NeighType == "Asian-Latinx-Other" |
		NeighType == "Asian-Black" |
		NeighType == "All Other" |
		NeighType == "All Asian" |
		NeighType == "Black-Other" |
		NeighType == "Latinx-Other" |
		NeighType == "Other-Shared" ~ "Mostly POC", 
		NeighType == "White-Asian-Latinx-Other" |
		NeighType == "Integrated" |
		NeighType == "White-Black-Latinx-Other" |
		NeighType == "White-Other" |
		NeighType == "White-Asian-Other" |
		NeighType == "White-Black-Asian" |
		NeighType == "White-Black" |
		NeighType == "White-Black-Other" |
		NeighType == "White-Black-Asian-Other" ~ "Diverse", 
		TRUE ~ NeighType)) 

check %>% 
	group_by(nt2) %>% 
	summarise(total_tracts = n()) %>% 
	left_join(., check %>% 
					filter(tier1 == "Heightened Sensitivity") %>% 
					group_by(nt2) %>% 
					summarise(sc_tracts = n())) %>% 
	mutate(p_sc_tracts = percent(sc_tracts/total_tracts))


# Number of tracts with more than 500 people
df_final.RB50VLI %>% 
	filter(tr_population < 500) %>% 
	count()

# Number of Heightened Sensitivity, poor data quality, and NA tracts
df_final.RB50VLI %>% 
	st_set_geometry(NULL) %>% 
	filter(tr_population >= 500) %>% 
	group_by(tier1) %>% 
	count()

#
# transit layer
# --------------------------------------------------------------------------

Bus <-
	transit %>%
	filter(label == "High-Quality Bus Corridor Buffer") %>%
	mutate(label = as.character(label))

Rail <-
	transit %>%
	filter(label == "Fixed Transit Stop Buffer") %>%
	mutate(label = as.character(label))

#
# Map functions
# --------------------------------------------------------------------------
# save_map <- function(x,y) tmap_save(x, paste0("~/git/sensitive_communities/docs/", y, ".html"))

tmap_mode("view")

# p_text <- function(x, y){
# 	paste0('<span class="right">', x, '</span><span class="left">', y, '</span>'​)}

# popup = paste0("<b>Total Population</b><br>", tr_population),
# 		str_c("Tot HH: ", tr_households),
# 		str_c("% Rent: ", tr_prenters),
# 		str_c("$ Rent: ", tr_medrent),
# 		str_c("$ R Lag: ", tr_medrent.lag),
# 		str_c("$ R Gap: ", tr_rentgap),
# 		str_c("Ch Rent: ", tr_chrent),
# 		str_c("Ch R Lag: ", tr_chrent.lag),
# 		str_c("% RB: ", tr_rb),
# 		str_c("% VLI x RB: ", tr_irVLI_50p),
# 		str_c("% ELI: ", tr_ELI_prop),
# 		str_c("% VLI: ", tr_VLI_prop),
# 		str_c("% Stud.: ", tr_pstudents),
# 		str_c("----------: ", text),
# 		str_c("Neigh.: ", NeighType),
# 		str_c("% White: ", tr_pWhite),
# 		str_c("% Black: ", tr_pBlack),
# 		str_c("% Asian: ", tr_pAsian),
# 		str_c("% Lat: ", tr_pLatinx),
# 		str_c("% Other: ", tr_pOther),
# 		str_c("% POC: ", tr_pPOC),
# 		str_c("% Welf: ", tr_pwelf),
# 		str_c("% Pov: ", tr_ppoverty),
# 		str_c("% Unemp: ", tr_punemp),
# 		str_c("%FHHw/C"= "tr_pfemhhch"),
# 		str_c("----------: ", text),
# 		str_c("SC Criteria: ", text),
# 		str_c("----------: ", text),
# 		str_c("VLI: ", v_VLI),
# 		str_c("POC: ", v_POC),
# 		str_c("Renters: ", v_Renters),
# 		str_c("RB: ", v_RB50VLI),
# 		str_c("Ch Rent: ", dp_PChRent),
# 		str_c("Rent Gap: ", dp_RentGap), 
# 		sep = "<br/>"
# 						   )

df_tiers <- 
	df_final.RB50VLI %>%
	select(GEOID, tr_population, tr_households, v_VLI, tr_VLI_prop, co_VLI_prop, tr_pstudents, v_POC, tr_pPOC, co_pPOC, tr_POC_rank, v_Renters, tr_prenters, co_prenters, v_RB50VLI, tr_irVLI_50p, co_irVLI_50p, dp_PChRent, tr_pchrent, tr_pchrent.lag, co_pchrent, dp_RentGap, tr_rentgap, co_rentgap, tr_medrent, tr_medrent.lag, NeighType, tr_pWhite, tr_pBlack, tr_pAsian, tr_pLatinx, tr_pOther, tier1, tier2) %>% 
	mutate(popup = 
		str_c(
			"<h3>Tract: ", GEOID, "</h3>", 

			"<b>Total population</b><br>", 
				comma(tr_population), 
				"<br>", 
			 		    
			"<b>Total households</b><br>", 
				comma(tr_households),
				"<br>", 
				"<br>",		

			"<b><i><u>Vulnerable Population Measures Met</b></i></u>", 
				"<br>", 
			
			"<b>Very low income</b><br>", 
				case_when(v_VLI == 1 ~ "Yes", TRUE ~ "No"), 
			 	"<br>(<i>",
			 		percent(tr_VLI_prop, accuracy = .1), " tract VLI, ",
			 		percent(co_VLI_prop, accuracy = .1), " county VLI, & ",
			 		percent(tr_pstudents, accuracy = .1), " students</i>)", 
			 	"<br>",

			"<b>Persons of color</b><br>", 
			  	case_when(v_POC == 1 ~ "Yes", TRUE ~ "No"), 
			 	" (<i>",
			 		percent(tr_pPOC, accuracy = .1), " tract & ",
			 		percent(co_pPOC, accuracy = .1), " county</i>)", 
			 	"<br>",

			"<b>Renting household percentage</b><br>    ", 
			  	case_when(v_Renters == 1 ~ "Yes", TRUE ~ "No"), 
			 	" (<i>",
			 		percent(tr_prenters, accuracy = .1), " tract & ",
			 		percent(co_prenters, accuracy = .1), " county</i>)", 
				"<br>", 

			"<b>Very low income renters paying<br>over 50% of income to rent</b><br>    ", 
			  	case_when(v_RB50VLI == 1 ~ "Yes", TRUE ~ "No"), 
			 	" (<i>",
			 		percent(tr_irVLI_50p, accuracy = .1), " tract & ",
			 		percent(co_irVLI_50p, accuracy = .1), " county</i>)", 
				"<br>", 			  
				"<br>",

			"<b><i><u>Displacement Pressures Met</b></i></u>", 
			  "<br>", 
			  "<b>Change in rent</b><br>    ", 
			  	case_when(dp_PChRent == 1 ~ "Yes", TRUE ~ "No"), 
			 	" (<i>",
			 		percent(tr_pchrent, accuracy = .1), " tract, ",
			 		percent(tr_pchrent.lag, accuracy = .1), " nearby, & ",
			 		percent(co_pchrent, accuracy = .1), " county</i>)", 
			 	"<br>",
	  
			"<b>Rent gap</b><br>     ", 
			  	case_when(dp_RentGap == 1 ~ "Yes", TRUE ~ "No"), 
			 	" (<i>",
			 		dollar(tr_rentgap), " tract & ",
			 		dollar(co_rentgap), " county</i>)", 
				"<br>", 			  
				"<br>",

			"<b><i><u>Rent</b></i></u>", 
				"<br>", 
					"<b>Local</b>","<br>", 
					dollar(tr_medrent), "<br>", 
					"<b>Nearby</b>", "<br>", 
					dollar(tr_medrent.lag), "<br>", 
				"<br>", 

			"<b><i><u>Racial composition</b></i></u>", "<br>", 
				"<b>Neighborhood Type</b>", "<br>", 
				NeighType, "<br>", 
				"<b>White alone</b>", "<br>",  
				percent(tr_pWhite, accuracy = .1), "<br>", 
				"<b>Black or African American alone</b>", "<br>", 
				percent(tr_pBlack, accuracy = .1), "<br>", 
				"<b>Asian alone</b>", "<br>", 
				percent(tr_pAsian, accuracy = .1), "<br>", 
				"<b>Latinx</b>", "<br>", 
				percent(tr_pLatinx, accuracy = .1), "<br>", 
				"<b>Other</b>", "<br>", 
				percent(tr_pOther, accuracy = .1), "<br>"
			  )) # %>% ms_simplify(.) # prefer the detail 

df_tier2 <- 
	df_tiers %>% 
	filter(!is.na(tier2))

# m <- 
# 	leaflet(df_tiers, df_tier2) %>% 
# 	addProviderTiles(providers$CartoDB.Positron) %>% 
# 	addMiniMap(tiles = providers$CartoDB.Positron, 
# 			   toggleDisplay = TRUE) %>% 
# 	addEasyButton(
# 		easyButton(
# 		    icon="fa-crosshairs", 
# 		    title="My Location",
# 		    onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
# 	setView(-122.2712, 37.8044, zoom = 10)


	# addPolygons(data = df_tiers, 
	# 			group = "tier2", 
	# 			# name = "Vulnerable", 
	# 			fillColor = c("#6699FF", "#6699FF"),
	# 			weight = .5, 
	# 			opacity = .45, 
	# 			fillOpacity = .1, 
	# 			stroke = TRUE, 
	# 			popup = ~popup, 
	# 			popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)) %>% 


pal1 <- 
	colorFactor(
		c("#FF6633", "#CCCCCC"), 
		domain = df_tiers$tier1, 
		na.color = "transparent"
	)

pal2 <- 
	colorFactor(
		c("#6699FF", "#CCCCCC"), 
		domain = df_tiers$tier2, 
		na.color = "transparent"
	)

map <- 
	leaflet(data = c(df_tiers, df_tier2)) %>% 
	addProviderTiles(providers$CartoDB.Positron) %>% 
	addMiniMap(tiles = providers$CartoDB.Positron, 
			   toggleDisplay = TRUE) %>% 
	addEasyButton(
		easyButton(
		    icon="fa-crosshairs", 
		    title="My Location",
		    onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
	setView(-122.2712, 37.8044, zoom = 10) %>% 
# Bus layer
	addPolygons(data = Bus, 
				label = "label", 
				color = "#000000", 
				fillColor="#CCCCCC", 
				weight = .5, 
				opacity = .45, 
				fillOpacity = .1, 
				stroke = TRUE, 
				group = "Bus") %>% 	
	addLegend(
		color = "#CCCCCC", 
		labels = Bus$label, 
		group = "Bus"
	) %>% 
# Rail layer
	addPolygons(data = Rail, 
				layerId = "label", 
				color = "#000000", 
				fillColor="#CCCCCC", 
				weight = .5, 
				opacity = .45, 
				fillOpacity = .1, 
				stroke = TRUE, 
				group = "Rail"
	) %>% 
	addLegend(
		color = "#CCCCCC", 
		labels = Rail$label, 
		group = "Rail"
	) %>% 
# Vulnerable layer
	addPolygons(
		data = df_tier2, 
		group = "Vulnerable",
		fillOpacity = .5, 
		color = ~pal2(tier2),
		stroke = TRUE, 
		weight = .5, # border thickness
		opacity = .45, 
		highlightOptions = highlightOptions(
							color = "#ff4a4a", 
							weight = 5,
      						bringToFront = TRUE
      						), 
		popup = ~popup, 
		popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
	) %>% 
	addLegend(
		pal = pal2, 
		values = ~tier2, 
		group = "Vulnerable", 
		title = ""
	) %>% 
# Heightened Sensitivity layer
	addPolygons(
		data = df_tiers, 
		group = "Heightened Sensitivity",
		fillOpacity = .5, 
		color = ~pal1(tier1),
		stroke = TRUE, 
		weight = .5, # border thickness
		opacity = .45, 
		highlightOptions = highlightOptions(
							color = "#ff4a4a", 
							weight = 5,
      						bringToFront = TRUE
      						), 
		popup = ~popup, 
		popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
	) %>% 
	addLegend(
		pal = pal1, 
		values = ~tier1, 
		title = ""
	) %>% 
	addLayersControl(overlayGroups = c("Heightened Sensitivity", "Vulnerable", "Bus", "Rail"),
					 options = layersControlOptions(collapsed = TRUE)) %>% 
	hideGroup(c("Bus", "Vulnerable"))

htmlwidgets::saveWidget(map, file="~/git/sensitive_communities/docs/map.html")
# run in terminal, not in rstudio
object.size(map)

# ==========================================================================
# ==========================================================================
# ==========================================================================
# Excess code
# ==========================================================================
# ==========================================================================
# ==========================================================================







# object.size()

# m %>% 
# 	addPolygons(
# 		data = df_tiers, 
# 		group = "tier1", 
# 		fillColor = c("#CCCCCC", "red"), # polygon colors per value
# 		weight = .5, # border thickness
# 		color = "transparent",
# 		opacity = .45, 
# 		fillOpacity = .1, 
# 		stroke = TRUE, 
# 		popup = ~popup, 
# 		popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
# 	) %>% 
# 	addLegend(values = values(tier1)) %>%
# 	addLayersControl(#baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
# 					 overlayGroups = c("tier1", "teir2"),
# 					 options = layersControlOptions(collapsed = FALSE)) %>% 
# 	hideGroup(c("Bus",
# 						 "tier2"#,
# 						 # "adv_surprisedissc",
# 						 # "adv_shouldbe"
# 						 ))
# ## 
# # Issues: 
# # 	What is it coloring? 

# df_tiers %>% 
# 	filter(GEOID == "06001981900") %>% 
# 	glimpse()










# # map <-
# tm_basemap(leaflet::providers$CartoDB.Positron) + 
# 	# For other basemaps see: http://leaflet-extras.github.io/leaflet-providers/preview/
# tm_shape(Bus) +
# 	tm_polygons("label",
# 				palette="Greys", alpha = .25,
# 				border.color = "gray",
# 				border.alpha = .5,
# 				id = "label",
# 				popup.vars = c("Type: " = "id"),
# 				title = "") +
# tm_shape(Rail) +
# 	tm_polygons("label",
# 				palette="Greys",
# 				alpha = .25,
# 				border.alpha = .5,
# 				id = "label",
# 				popup.vars = c("Type: " = "id"),
# 				title = "") +
# tm_shape(df_tier2, name = "Vulnerable") +
# 	tm_polygons("tier2",
# 			palette = c("#6699FF", "#6699FF"),
# 			# label = "Heightened Sensitivity",
# 			alpha = .5,
# 			border.alpha = .05,
# 			border.color = "gray",
# 			colorNA = NULL,
# 			title = "",
# 			id = "popup_text",
# 			popup.vars = c("Tot Pop" = "tr_population",
# 						   "Tot HH" = "tr_households",
# 						   "% Rent" = "tr_prenters",
# 						   "$ Rent" = "tr_medrent",
# 						   "$ R Lag" = "tr_medrent.lag",
# 						   "$ R Gap" = "tr_rentgap",
# 						   "Ch Rent" = "tr_chrent",
# 						   "Ch R Lag" = "tr_chrent.lag",
# 						   "% RB" = "tr_rb",
# 						   "% VLI x RB" = "tr_irVLI_50p",
# 						   "% ELI" = "tr_ELI_prop",
# 						   "% VLI" = "tr_VLI_prop",
# 						   "% Stud." = "tr_pstudents",
# 						   "----------" = "text",
# 						   "Neigh." = "NeighType",
# 						   "% White" = "tr_pWhite",
# 						   "% Black" = "tr_pBlack",
# 						   "% Asian" = "tr_pAsian",
# 						   "% Lat" = "tr_pLatinx",
# 						   "% Other" = "tr_pOther",
# 						   "% POC" = "tr_pPOC",
# 						   "----------" = "text",
# 						   "SC Criteria" = "text",
# 						   "----------" = "text",
# 						   "VLI" = "v_VLI",
# 						   "POC" = "v_POC",
# 						   "Renters" = "v_Renters",
# 						   "RB" = "v_RB50VLI",
# 						   "Ch Rent" = "dp_PChRent",
# 						   "Rent Gap" = "dp_RentGap"
# 						   ),
# 			popup.format = list(digits=2)) +
# tm_shape(df_final.RB50VLI, name = "Heightened Sensitivity") +
# 	tm_polygons("tier1",
# 			palette = c("#CCCCCC", "#FF6633"),
# 			# label = "Heightened Sensitivity",
# 			alpha = .5,
# 			border.alpha = .05,
# 			border.color = "gray",
# 			colorNA = NULL,
# 			title = "",
# 			id = "popup_text",
# 			popup.vars = c("Tot Pop" = "tr_population",
# 						   "Tot HH" = "tr_households",
# 						   "% Rent" = "tr_prenters",
# 						   "$ Rent" = "tr_medrent",
# 						   "$ R Lag" = "tr_medrent.lag",
# 						   "$ R Gap" = "tr_rentgap",
# 						   "Ch Rent" = "tr_chrent",
# 						   "Ch R Lag" = "tr_chrent.lag",
# 						   "% RB" = "tr_rb",
# 						   "% VLI x RB" = "tr_irVLI_50p",
# 						   "% ELI" = "tr_ELI_prop",
# 						   "% VLI" = "tr_VLI_prop",
# 						   "% Stud." = "tr_pstudents",
# 						   "----------" = "text",
# 						   "Neigh." = "NeighType",
# 						   "% White" = "tr_pWhite",
# 						   "% Black" = "tr_pBlack",
# 						   "% Asian" = "tr_pAsian",
# 						   "% Lat" = "tr_pLatinx",
# 						   "% Other" = "tr_pOther",
# 						   "% POC" = "tr_pPOC",
# 						   "% Welf" = "tr_pwelf",
# 						   "% Pov" = "tr_ppoverty",
# 						   "% Unemp" = "tr_punemp",
# 						   "%FHHw/C"= "tr_pfemhhch",
# 						   "----------" = "text",
# 						   "SC Criteria" = "text",
# 						   "----------" = "text",
# 						   "VLI" = "v_VLI",
# 						   "POC" = "v_POC",
# 						   "Renters" = "v_Renters",
# 						   "RB" = "v_RB50VLI",
# 						   "Ch Rent" = "dp_PChRent",
# 						   "Rent Gap" = "dp_RentGap"
# 						   ),
# 			popup.format = list(digits=2)) +
# tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 9), alpha = .9)

# map <-
# 	tmap_leaflet(map) %>%
# 	addPopups(map, options = popupOptions(minWidth = 300,
# 									 maxWidth = 300)) %>% 
# 	leaflet::hideGroup(c("Bus",
# 						 "Vulnerable"#,
# 						 # "adv_surprisedissc",
# 						 # "adv_shouldbe"
# 						 )) %>% 
# 	addMiniMap(tiles = providers$CartoDB.Positron, 
# 			   toggleDisplay = TRUE) %>% 
# 	addEasyButton(easyButton(
#     icon="fa-crosshairs", title="My Location",
#     onClick=JS("function(btn, map){ map.locate({setView: true}); }")))

# # Create html 
# htmlwidgets::saveWidget(map, file="~/git/sensitive_communities/docs/map.html")
