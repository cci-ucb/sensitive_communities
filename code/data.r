# ==========================================================================
# Develop data for displacement and vulnerability measures
# Initial Author: Tim Thomas
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
pacman::p_load(data.table, plotly, spdep, tidyverse, tigris, tidycensus, tmap, leaflet)

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

sc_vars <-
c(
### AMI Variables
	'HHInc_Total' = 'B19001_001', # Total HOUSEHOLD INCOME
	'HHInc_10' = 'B19001_002', # Less than $10,000 HOUSEHOLD INCOME
	'HHInc_15' = 'B19001_003', # $10,000 to $14,999 HOUSEHOLD INCOME
	'HHInc_20' = 'B19001_004', # $15,000 to $19,999 HOUSEHOLD INCOME
	'HHInc_25' = 'B19001_005', # $20,000 to $24,999 HOUSEHOLD INCOME
	'HHInc_30' = 'B19001_006', # $25,000 to $29,999 HOUSEHOLD INCOME
	'HHInc_35' = 'B19001_007', # $30,000 to $34,999 HOUSEHOLD INCOME
	'HHInc_40' = 'B19001_008', # $35,000 to $39,999 HOUSEHOLD INCOME
	'HHInc_45' = 'B19001_009', # $40,000 to $44,999 HOUSEHOLD INCOME
	'HHInc_50' = 'B19001_010', # $45,000 to $49,999 HOUSEHOLD INCOME
	'HHInc_60' = 'B19001_011', # $50,000 to $59,999 HOUSEHOLD INCOME
	'HHInc_75' = 'B19001_012', # $60,000 to $74,999 HOUSEHOLD INCOME
	'HHInc_100' = 'B19001_013', # $75,000 to $99,999 HOUSEHOLD INCOME
	'HHInc_125' = 'B19001_014', # $100,000 to $124,999 HOUSEHOLD INCOME
	'HHInc_150' = 'B19001_015', # $125,000 to $149,999 HOUSEHOLD INCOME
	'HHInc_200' = 'B19001_016', # $150,000 to $199,999 HOUSEHOLD INCOME
	'HHInc_250' = 'B19001_017', # $200,000 or more HOUSEHOLD INCOME
### Renting household variables
	'totten' = 'B25003_001', # Total # households
	'totrent' = 'B25003_003', # total # renting households
### POC variables
	'race_tot' = 'B03002_001',
	'race_White' = 'B03002_003',
	'race_Black' = 'B03002_004',
	'race_Asian' = 'B03002_006',
	'race_Latinx' = 'B03002_012',
### Median Rent
	'medrent' = 'B25064_001',
### Students
	'st_totenroll' = 'B14007_001',
	'st_colenroll' = 'B14007_017',
	'st_proenroll' = 'B14007_018',
	'st_pov_under' = 'B14006_009', 
	'st_pov_grad' = 'B14006_010', 
### Additional Pop-up Variables
	# Overall rent-burden
	'rb_tot' = 'B25070_001',
	'rb_34.9' = 'B25070_007',
	'rb_39.9' = 'B25070_008',
	'rb_49.9' = 'B25070_009',
	'rb_55' = 'B25070_010',
	# On public assistance
	'welf_tot' = 'B19057_001',
	'welf' = 'B19057_002',
	# Poverty
	'pov_tot' = 'B17017_001',
	'pov_famh' = 'B17017_003',
	'pov_nonfamh' = 'B17017_020',
	# Unemployed
	'unemp_tot' = 'B23025_001',
	'unemp' = 'B23025_005',
	# female headed households
	'fhh_tot' = 'B11005_001',
	'fhh_famheadch' = 'B11005_007',
	'fhh_nonfamheadch' = 'B11005_010',
	# Median household income
	'mhhinc' = 'B19013_001')

# Rent burden by LI status
ir_var <- c(
	'ir_tot_tot' = 'B25074_001',# Estimate!!Total
	'ir_tot_9999' = 'B25074_002', # Estimate!!Total!!Less than $10 000
	'ir_19_9999' = 'B25074_003', # Estimate!!Total!!Less than $10 000!!Less than 20.0 percent
	'ir_249_9999' = 'B25074_004', # Estimate!!Total!!Less than $10 000!!20.0 to 24.9 percent
	'ir_299_9999' = 'B25074_005', # Estimate!!Total!!Less than $10 000!!25.0 to 29.9 percent
	'ir_349_9999' = 'B25074_006', # Estimate!!Total!!Less than $10 000!!30.0 to 34.9 percent
	'ir_399_9999' = 'B25074_007', # Estimate!!Total!!Less than $10 000!!35.0 to 39.9 percent
	'ir_499_9999' = 'B25074_008', # Estimate!!Total!!Less than $10 000!!40.0 to 49.9 percent
	'ir_5plus_9999' = 'B25074_009', # Estimate!!Total!!Less than $10 000!!50.0 percent or more
	'ir_x_9999' = 'B25074_010', # Estimate!!Total!!Less than $10 000!!Not computed
	'ir_tot_19999' = 'B25074_011', # Estimate!!Total!!$10 000 to $19 999
	'ir_19_19999' = 'B25074_012', # Estimate!!Total!!$10 000 to $19 999!!Less than 20.0 percent
	'ir_249_19999' = 'B25074_013', # Estimate!!Total!!$10 000 to $19 999!!20.0 to 24.9 percent
	'ir_299_19999' = 'B25074_014', # Estimate!!Total!!$10 000 to $19 999!!25.0 to 29.9 percent
	'ir_349_19999' = 'B25074_015', # Estimate!!Total!!$10 000 to $19 999!!30.0 to 34.9 percent
	'ir_399_19999' = 'B25074_016', # Estimate!!Total!!$10 000 to $19 999!!35.0 to 39.9 percent
	'ir_499_19999' = 'B25074_017', # Estimate!!Total!!$10 000 to $19 999!!40.0 to 49.9 percent
	'ir_5plus_19999' = 'B25074_018', # Estimate!!Total!!$10 000 to $19 999!!50.0 percent or more
	'ir_x_19999' = 'B25074_019', # Estimate!!Total!!$10 000 to $19 999!!Not computed
	'ir_tot_34999' = 'B25074_020', # Estimate!!Total!!$20 000 to $34 999
	'ir_19_34999' = 'B25074_021', # Estimate!!Total!!$20 000 to $34 999!!Less than 20.0 percent
	'ir_249_34999' = 'B25074_022', # Estimate!!Total!!$20 000 to $34 999!!20.0 to 24.9 percent
	'ir_299_34999' = 'B25074_023', # Estimate!!Total!!$20 000 to $34 999!!25.0 to 29.9 percent
	'ir_349_34999' = 'B25074_024', # Estimate!!Total!!$20 000 to $34 999!!30.0 to 34.9 percent
	'ir_399_34999' = 'B25074_025', # Estimate!!Total!!$20 000 to $34 999!!35.0 to 39.9 percent
	'ir_499_34999' = 'B25074_026', # Estimate!!Total!!$20 000 to $34 999!!40.0 to 49.9 percent
	'ir_5plus_34999' = 'B25074_027', # Estimate!!Total!!$20 000 to $34 999!!50.0 percent or more
	'ir_x_34999' = 'B25074_028', # Estimate!!Total!!$20 000 to $34 999!!Not computed
	'ir_tot_49999' = 'B25074_029', # Estimate!!Total!!$35 000 to $49 999
	'ir_19_49999' = 'B25074_030', # Estimate!!Total!!$35 000 to $49 999!!Less than 20.0 percent
	'ir_249_49999' = 'B25074_031', # Estimate!!Total!!$35 000 to $49 999!!20.0 to 24.9 percent
	'ir_299_49999' = 'B25074_032', # Estimate!!Total!!$35 000 to $49 999!!25.0 to 29.9 percent
	'ir_349_49999' = 'B25074_033', # Estimate!!Total!!$35 000 to $49 999!!30.0 to 34.9 percent
	'ir_399_49999' = 'B25074_034', # Estimate!!Total!!$35 000 to $49 999!!35.0 to 39.9 percent
	'ir_499_49999' = 'B25074_035', # Estimate!!Total!!$35 000 to $49 999!!40.0 to 49.9 percent
	'ir_5plus_49999' = 'B25074_036', # Estimate!!Total!!$35 000 to $49 999!!50.0 percent or more
	'ir_x_49999' = 'B25074_037', # Estimate!!Total!!$35 000 to $49 999!!Not computed
	'ir_tot_74999' = 'B25074_038', # Estimate!!Total!!$50 000 to $74 999
	'ir_19_74999' = 'B25074_039', # Estimate!!Total!!$50 000 to $74 999!!Less than 20.0 percent
	'ir_249_74999' = 'B25074_040', # Estimate!!Total!!$50 000 to $74 999!!20.0 to 24.9 percent
	'ir_299_74999' = 'B25074_041', # Estimate!!Total!!$50 000 to $74 999!!25.0 to 29.9 percent
	'ir_349_74999' = 'B25074_042', # Estimate!!Total!!$50 000 to $74 999!!30.0 to 34.9 percent
	'ir_399_74999' = 'B25074_043', # Estimate!!Total!!$50 000 to $74 999!!35.0 to 39.9 percent
	'ir_499_74999' = 'B25074_044', # Estimate!!Total!!$50 000 to $74 999!!40.0 to 49.9 percent
	'ir_5plus_74999' = 'B25074_045', # Estimate!!Total!!$50 000 to $74 999!!50.0 percent or more
	'ir_x_74999' = 'B25074_046', # Estimate!!Total!!$50 000 to $74 999!!Not computed
	'ir_tot_99999' = 'B25074_047', # Estimate!!Total!!$75 000 to $99 999
	'ir_19_99999' = 'B25074_048', # Estimate!!Total!!$75 000 to $99 999!!Less than 20.0 percent
	'ir_249_99999' = 'B25074_049', # Estimate!!Total!!$75 000 to $99 999!!20.0 to 24.9 percent
	'ir_299_99999' = 'B25074_050', # Estimate!!Total!!$75 000 to $99 999!!25.0 to 29.9 percent
	'ir_349_99999' = 'B25074_051', # Estimate!!Total!!$75 000 to $99 999!!30.0 to 34.9 percent
	'ir_399_99999' = 'B25074_052', # Estimate!!Total!!$75 000 to $99 999!!35.0 to 39.9 percent
	'ir_499_99999' = 'B25074_053', # Estimate!!Total!!$75 000 to $99 999!!40.0 to 49.9 percent
	'ir_5plus_99999' = 'B25074_054', # Estimate!!Total!!$75 000 to $99 999!!50.0 percent or more
	'ir_x_99999' = 'B25074_055', # Estimate!!Total!!$75 000 to $99 999!!Not computed
	'ir_tot_100000' = 'B25074_056', # Estimate!!Total!!$100 000 or more
	'ir_19_100000' = 'B25074_057', # Estimate!!Total!!$100 000 or more!!Less than 20.0 percent
	'ir_249_100000' = 'B25074_058', # Estimate!!Total!!$100 000 or more!!20.0 to 24.9 percent
	'ir_299_100000' = 'B25074_059', # Estimate!!Total!!$100 000 or more!!25.0 to 29.9 percent
	'ir_349_100000' = 'B25074_060', # Estimate!!Total!!$100 000 or more!!30.0 to 34.9 percent
	'ir_399_100000' = 'B25074_061', # Estimate!!Total!!$100 000 or more!!35.0 to 39.9 percent
	'ir_499_100000' = 'B25074_062', # Estimate!!Total!!$100 000 or more!!40.0 to 49.9 percent
	'ir_5plus_100000' = 'B25074_063', # Estimate!!Total!!$100 000 or more!!50.0 percent or more
	'ir_x_100000' = 'B25074_064' # Estimate!!Total!!$100 000 or more!!Not computed
	)

#
# Tract data
# --------------------------------------------------------------------------

ct <- tracts(state = "CA", cb = TRUE) # cb option downloads a smaller shapefile

### Tract data extraction
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

tr_df17 <-
    tr_data(2017, sc_vars) %>%
    mutate(COUNTY = substr(GEOID, 1, 5))

tr_df12 <-
	tr_data(2012, c('medrent12' = 'B25064_001')) %>%
    mutate(COUNTY = substr(GEOID, 1, 5),
    	   estimate = estimate*1.07)

df <-
	left_join(tr_df17 %>%
				select(-moe) %>%
				spread(variable, estimate),
			  tr_df12 %>%
			  	select(-moe) %>%
			  	spread(variable, estimate))

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
# https://gitlab.com/timathomas/Functions/blob/master/NeighType_Fun.R

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
							  	  v_POC, na.rm = TRUE) >= 2 ~ "Tier 2: Vulnerable"),
		tier3 = case_when(tr_dq == 0 ~ NA_character_,
						  sum(v_POC, v_VLI, na.rm = TRUE) == 2 |
						  sum(v_POC, v_RB50VLI, na.rm = TRUE) == 2|
						  sum(v_POC, v_Renters, na.rm = TRUE) == 2|
						  v_VLI == 1 |
						  sum(v_VLI, v_RB50VLI, na.rm = TRUE) == 2|
						  sum(v_VLI, v_Renters, na.rm = TRUE) == 2 ~ "Tier 3: Some Vulnerability"),
		tier1 = case_when(tr_dq == 0 ~ "Poor Data Quality",
						    scen3.VLI50RB == 1 ~ "Tier 1: Heightened Sensitivity",
						    big_city == 1 &
							tr_sc_VLI50RB.lag >= .6 &
							tr_pstudents < .2 &
							tr_population >= 500 &
						  	v_VLI == 1 &
							sum(v_Renters,
								v_RB50VLI,
								v_POC, na.rm = TRUE) >= 2 ~ "Tier 1: Heightened Sensitivity"),
		text = "",
		popup_text = paste0("Tract: ", GEOID)) %>% 
	ungroup()

fwrite(df_final.RB50VLI %>% st_set_geometry(NULL), file = "~/git/sensitive_communities/data/final_df_191216.csv")
st_write(df_final.RB50VLI, "~/git/sensitive_communities/data/final_df_191216.shp", delete_layer = TRUE)

df_final.RB50VLI %>% filter(GEOID == "06095252702") %>% glimpse()

#
# Descritpive statistics
# --------------------------------------------------------------------------

df_final.RB50VLI %>% 
st_set_geometry(NULL) %>% 
ungroup() %>% 
filter(tier1 == "Tier 1: Heightened Sensitivity") %>% 
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

df_final.RB50VLI %>% 
	filter(tr_population < 500) %>% 
	count()

df_final.RB50VLI %>% st_set_geometry(NULL) %>% filter(tr_population >= 500) %>% group_by(tier1) %>% count()

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
save_map <- function(x,y)
	tmap_save(x, paste0("~/git/sensitive_communities/docs/", y, ".html"))

tmap_mode("view")

p_text <- function(x, y){
	paste0('<span class="right">', x, '</span><span class="left">', y, '</span>'​)}

popup = paste0("<b>Total Population</b><br>", tr_population),
		str_c("Tot HH: ", tr_households),
		str_c("% Rent: ", tr_prenters),
		str_c("$ Rent: ", tr_medrent),
		str_c("$ R Lag: ", tr_medrent.lag),
		str_c("$ R Gap: ", tr_rentgap),
		str_c("Ch Rent: ", tr_chrent),
		str_c("Ch R Lag: ", tr_chrent.lag),
		str_c("% RB: ", tr_rb),
		str_c("% VLI x RB: ", tr_irVLI_50p),
		str_c("% ELI: ", tr_ELI_prop),
		str_c("% VLI: ", tr_VLI_prop),
		str_c("% Stud.: ", tr_pstudents),
		str_c("----------: ", text),
		str_c("Neigh.: ", NeighType),
		str_c("% White: ", tr_pWhite),
		str_c("% Black: ", tr_pBlack),
		str_c("% Asian: ", tr_pAsian),
		str_c("% Lat: ", tr_pLatinx),
		str_c("% Other: ", tr_pOther),
		str_c("% POC: ", tr_pPOC),
		str_c("% Welf: ", tr_pwelf),
		str_c("% Pov: ", tr_ppoverty),
		str_c("% Unemp: ", tr_punemp),
		str_c("%FHHw/C"= "tr_pfemhhch"),
		str_c("----------: ", text),
		str_c("SC Criteria: ", text),
		str_c("----------: ", text),
		str_c("VLI: ", v_VLI),
		str_c("POC: ", v_POC),
		str_c("Renters: ", v_Renters),
		str_c("RB: ", v_RB50VLI),
		str_c("Ch Rent: ", dp_PChRent),
		str_c("Rent Gap: ", dp_RentGap), 
		sep = "<br/>"
						   )
library(scales)

df_tiers <- df_final.RB50VLI %>%
	# filter(!is.na(tier2)) %>% 
	mutate(popup = 
		str_c("<h4>Tract: ", GEOID, "</h4>", 
			  '<b>Total population</b><br>    ', comma(tr_population), 
			  			"<br>", 
			 		    # "<br>", 
			  '<b>Total households</b><br>    ', comma(tr_households),
			  			"<br>", 
			 		    "<br>",		
			  "<b><i><u>Vulnerable Population Measures Met</b></i></u>", 
					  # "<br>", 
					  "<br>", 
			  "<b>Very low income</b><br>    ", 
			  	case_when(v_VLI == 1 ~ "Yes", TRUE ~ "No"), 
			 	" (<i>VLI: ",
			 		percent(tr_VLI_prop, accuracy = .1),
			 		" & students: ", 
			 		percent(tr_pstudents, accuracy = .1), "</i>)", 
			 				"<br>", 			 				  
			  "<b>Persons of color</b><br>    ", 
			  	case_when(v_POC == 1 ~ "Yes", TRUE ~ "No"), 
			 	" (<i>",
			 		percent(tr_pPOC, accuracy = .1),
			 		"</i>)", 
			 				"<br>", 
			   "<b>Renting household percentage</b><br>    ", 
			  	case_when(v_Renters == 1 ~ "Yes", TRUE ~ "No"), 
			 	" (<i>",
			 		percent(tr_prenters, accuracy = .1),
			 		"</i>)", 
						 	"<br>", 
			   "<b>Very low income renters paying<br>over 50% of income to rent</b><br>    ", 
			  	case_when(v_RB50VLI == 1 ~ "Yes", TRUE ~ "No"), 
			 	" (<i>",
			 		percent(tr_irVLI_50p, accuracy = .1),
			 		"</i>)", 
			  "<br>",
			  "<br>",
			  "<b><i><u>Displacement Pressures Met</b></i></u>", 
			  # "<br>", 
			  "<br>", 
			  "<b>Change in rent</b><br>    ", 
			  	case_when(dp_PChRent == 1 ~ "Yes", TRUE ~ "No"), 
			 	" (<i>Local: ",
			 		percent(tr_pchrent, accuracy = .1),
			 		" & nearby: ",
			 		percent(tr_pchrent.lag, accuracy = .1),
			 		"</i>)", 
			 				"<br>", 
			 				# "<br>", 			  
			  "<b>Rent gap</b><br>     ", 
			  	case_when(dp_RentGap == 1 ~ "Yes", TRUE ~ "No"), 
			 	" (<i>",
			 		percent(tr_rentgapprop, accuracy = .1),
			 		"</i>)", 
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
			  ))

m <- leaflet() %>% 
	addProviderTiles(providers$CartoDB.Positron) %>% 
	addMiniMap(tiles = providers$CartoDB.Positron, 
			   toggleDisplay = TRUE) %>% 
	addEasyButton(easyButton(
    icon="fa-crosshairs", title="My Location",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
	setView(-122.2712, 37.8044, zoom = 9)


m %>%
	addPolygons(data = Bus, 
				label = "label", 
				color = "#000000", 
				fillColor="#CCCCCC", 
				weight = .5, 
				opacity = .45, 
				fillOpacity = .1, 
				stroke = TRUE) %>% 	
	addPolygons(data = Rail, 
				layerId = "label", 
				color = "#000000", 
				fillColor="#CCCCCC", 
				weight = .5, 
				opacity = .45, 
				fillOpacity = .1, 
				stroke = TRUE) %>% 
	addPolygons(data = df_tiers, 
				group = "tier2", 
				weight = .5, 
				opacity = .45, 
				fillOpacity = .1, 
				stroke = TRUE, 
				popup = ~popup, 
				popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)) %>% 
	addPolygons(data = df_tiers, 
				group = "tier1", 
				weight = .5, 
				opacity = .45, 
				fillOpacity = .1, 
				stroke = TRUE, 
				popup = ~popup, 
				popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)) %>% 
	addLayersControl(#baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
					 overlayGroups = c("tier1", "teir2"),
					 options = layersControlOptions(collapsed = FALSE)) %>% 
	hideGroup(c("Bus",
						 "Tier 2: Vulnerable"#,
						 # "adv_surprisedissc",
						 # "adv_shouldbe"
						 ))


map <-
tm_basemap(leaflet::providers$CartoDB.Positron) + 
	# For other basemaps see: http://leaflet-extras.github.io/leaflet-providers/preview/
tm_shape(Bus) +
	tm_polygons("label",
				palette="Greys", alpha = .25,
				border.color = "gray",
				border.alpha = .5,
				id = "label",
				popup.vars = c("Type: " = "id"),
				title = "") +
tm_shape(Rail) +
	tm_polygons("label",
				palette="Greys",
				alpha = .25,
				border.alpha = .5,
				id = "label",
				popup.vars = c("Type: " = "id"),
				title = "") +
tm_shape(df_tier2, name = "Tier 2: Vulnerable") +
	tm_polygons("tier2",
			palette = c("#6699FF", "#6699FF"),
			# label = "Heightened Sensitivity",
			alpha = .5,
			border.alpha = .05,
			border.color = "gray",
			colorNA = NULL,
			title = "",
			id = "popup_text",
			popup.vars = c("Tot Pop" = "tr_population",
						   "Tot HH" = "tr_households",
						   "% Rent" = "tr_prenters",
						   "$ Rent" = "tr_medrent",
						   "$ R Lag" = "tr_medrent.lag",
						   "$ R Gap" = "tr_rentgap",
						   "Ch Rent" = "tr_chrent",
						   "Ch R Lag" = "tr_chrent.lag",
						   "% RB" = "tr_rb",
						   "% VLI x RB" = "tr_irVLI_50p",
						   "% ELI" = "tr_ELI_prop",
						   "% VLI" = "tr_VLI_prop",
						   "% Stud." = "tr_pstudents",
						   "----------" = "text",
						   "Neigh." = "NeighType",
						   "% White" = "tr_pWhite",
						   "% Black" = "tr_pBlack",
						   "% Asian" = "tr_pAsian",
						   "% Lat" = "tr_pLatinx",
						   "% Other" = "tr_pOther",
						   "% POC" = "tr_pPOC",
						   "----------" = "text",
						   "SC Criteria" = "text",
						   "----------" = "text",
						   "VLI" = "v_VLI",
						   "POC" = "v_POC",
						   "Renters" = "v_Renters",
						   "RB" = "v_RB50VLI",
						   "Ch Rent" = "dp_PChRent",
						   "Rent Gap" = "dp_RentGap"
						   ),
			popup.format = list(digits=2)) +
tm_shape(df_final.RB50VLI, name = "Tier 1: Heightened Sensitivity") +
	tm_polygons("tier1",
			palette = c("#CCCCCC", "#FF6633"),
			# label = "Heightened Sensitivity",
			alpha = .5,
			border.alpha = .05,
			border.color = "gray",
			colorNA = NULL,
			title = "",
			id = "popup_text",
			popup.vars = c("Tot Pop" = "tr_population",
						   "Tot HH" = "tr_households",
						   "% Rent" = "tr_prenters",
						   "$ Rent" = "tr_medrent",
						   "$ R Lag" = "tr_medrent.lag",
						   "$ R Gap" = "tr_rentgap",
						   "Ch Rent" = "tr_chrent",
						   "Ch R Lag" = "tr_chrent.lag",
						   "% RB" = "tr_rb",
						   "% VLI x RB" = "tr_irVLI_50p",
						   "% ELI" = "tr_ELI_prop",
						   "% VLI" = "tr_VLI_prop",
						   "% Stud." = "tr_pstudents",
						   "----------" = "text",
						   "Neigh." = "NeighType",
						   "% White" = "tr_pWhite",
						   "% Black" = "tr_pBlack",
						   "% Asian" = "tr_pAsian",
						   "% Lat" = "tr_pLatinx",
						   "% Other" = "tr_pOther",
						   "% POC" = "tr_pPOC",
						   "% Welf" = "tr_pwelf",
						   "% Pov" = "tr_ppoverty",
						   "% Unemp" = "tr_punemp",
						   "%FHHw/C"= "tr_pfemhhch",
						   "----------" = "text",
						   "SC Criteria" = "text",
						   "----------" = "text",
						   "VLI" = "v_VLI",
						   "POC" = "v_POC",
						   "Renters" = "v_Renters",
						   "RB" = "v_RB50VLI",
						   "Ch Rent" = "dp_PChRent",
						   "Rent Gap" = "dp_RentGap"
						   ),
			popup.format = list(digits=2)) +
tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 9), alpha = .9)

map <-
	tmap_leaflet(map) %>%
	addPopups(map, options = popupOptions(minWidth = 300,
									 maxWidth = 300)) %>% 
	leaflet::hideGroup(c("Bus",
						 "Tier 2: Vulnerable"#,
						 # "adv_surprisedissc",
						 # "adv_shouldbe"
						 )) %>% 
	addMiniMap(tiles = providers$CartoDB.Positron, 
			   toggleDisplay = TRUE) %>% 
	addEasyButton(easyButton(
    icon="fa-crosshairs", title="My Location",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }")))

# Create html 
htmlwidgets::saveWidget(map, file="~/git/sensitive_communities/docs/map.html")
