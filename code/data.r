# ==========================================================================
# Develop data for displacement and vulnerability measures
# Initial Author: Tim Thomas
# Created: 2019.10.13
# 1.0 code: 2019.11.14
# ==========================================================================

# ==========================================================================
# Libraries
# ==========================================================================

#
# Load packages and install them if they're not installed.
# --------------------------------------------------------------------------

# Clear the session
rm(list = ls())
options(scipen = 10) # avoid scientific notation

# load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, plotly, spdep, tidyverse, tigris, tidycensus, tmap)

# Cache downloaded tiger files
options(tigris_use_cache = TRUE)

# ==========================================================================
# Data
# ==========================================================================

###
# Measures
# 	% VLI (<50% AMI) > county median for % VLI; and < 20% student
# 	% renter occupied units > 40%
# 	% rent burdened LI renters (<80% AMI) (above 30% of income spent on rent) > county median
# 	>30% POC, and POC neighborhood type (not All White, White Shared, or White-Asian)
# 	∆ rent: % Change in median rent > median for county or change in extra-local rent > county median
# 	rent gap: Difference > county median difference
###

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
# County subdivision tracts and place tracts
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
	select(GEOID,NeighType) #%>%
	# mutate(v_poc = case_when(# NeighType == "White-Asian" ~ 0,
	# 						 NeighType == "All White" ~ 0,
	# 						 NeighType == "White-Shared" ~ 0,
	# 						 TRUE ~ 1))

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
		   medrent12 = case_when(is.na(medrent12) ~ median(medrent12, na.rm = TRUE),
		   					   TRUE ~ medrent12),
		   tr_chrent = medrent - medrent12,
		   tr_pchrent = (medrent - medrent12)/medrent12)

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
#
# ## California places
# 	big_city <-
# 		fread("~/git/sensitive_communities/data/ca_place.csv") %>%
# 		mutate(city = case_when(cplace %in% c("Los Angeles",
# 											  "San Francisco",
# 											  "Oakland",
# 											  "San Jose",
# 											  "Fresno",
# 											  "San Diego",
# 											  "Sacramento",
# 											  "Long Beach") ~ 1,
# 								TRUE ~ 0),
# 			   GEOID = paste0("0", geoid)) %>%
# 		filter(city == 1) %>%
# 		select(GEOID) %>%
# 		pull()

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
		v_POC = case_when(tr_pPOC > .4 ~ 1,
						  TRUE ~ 0),
		dp_PChRent = case_when(tr_pchrent > co_pchrent ~ 1,
		   					   tr_pchrent.lag > co_pchrent ~ 1,
						  	   TRUE ~ 0),
		dp_RentGap = case_when(tr_rentgapprop > co_rentgapprop ~ 1,
						  	   TRUE ~ 0),
	## Scenarios
		scen1.LI30RB = case_when(v_VLI == 1 &
							  tr_pstudents < .2 &
							  tr_population >= 500 &
							  sum(v_Renters,
							  	  v_RB30LI,
							  	  v_POC, na.rm = TRUE) >= 2 &
							  sum(dp_PChRent,
							  	  dp_RentGap, na.rm = TRUE) >= 1 ~ 1,
						  tr_POC_rank >= .95 &
							  tr_pstudents < .2 &
							  tr_population >= 500 &
							  sum(v_VLI,
							  	  v_Renters,
							  	  v_RB30LI,
							  	  v_POC, na.rm = TRUE) >= 3 &
							  sum(dp_PChRent,
							  	  dp_RentGap, na.rm = TRUE) >= 1 ~ 1,
						  tr_pPOC >= .9 &
							  tr_pstudents < .2 &
							  tr_population >= 500 &
							  sum(v_VLI,
							  	  v_Renters,
							  	  v_RB30LI,
							  	  v_POC, na.rm = TRUE) >= 3 &
							  sum(dp_PChRent,
							  	  dp_RentGap, na.rm = TRUE) >= 1 ~ 1,
						  TRUE ~ 0),
		scen3.LI30RB = case_when(big_city == 1 ~ scen1.LI30RB,
						  big_city == 0 &
						  	tr_pstudents < .2 &
							tr_population >= 500 &
							v_VLI == 1 &
							sum(v_Renters, v_RB30LI, v_POC, na.rm = TRUE) >= 2 &
							sum(dp_PChRent, dp_RentGap, na.rm = TRUE) == 2 ~ 1,
						  TRUE ~ 0),
		scen1.LI50RB = case_when(v_VLI == 1 &
							  tr_pstudents < .2 &
							  tr_population >= 500 &
							  sum(v_Renters,
							  	  v_RB50LI,
							  	  v_POC, na.rm = TRUE) >= 2 &
							  sum(dp_PChRent,
							  	  dp_RentGap, na.rm = TRUE) >= 1 ~ 1,
						  tr_POC_rank >= .95 &
							  tr_pstudents < .2 &
							  tr_population >= 500 &
							  sum(v_VLI,
							  	  v_Renters,
							  	  v_RB50LI,
							  	  v_POC, na.rm = TRUE) >= 3 &
							  sum(dp_PChRent,
							  	  dp_RentGap, na.rm = TRUE) >= 1 ~ 1,
						  tr_pPOC >= .9 &
							  tr_pstudents < .2 &
							  tr_population >= 500 &
							  sum(v_VLI,
							  	  v_Renters,
							  	  v_RB50LI,
							  	  v_POC, na.rm = TRUE) >= 3 &
							  sum(dp_PChRent,
							  	  dp_RentGap, na.rm = TRUE) >= 1 ~ 1,
						  TRUE ~ 0),
		scen3.LI50RB = case_when(big_city == 1 ~ scen1.LI50RB,
						  big_city == 0 &
						  	tr_pstudents < .2 &
							tr_population >= 500 &
							v_VLI == 1 &
							sum(v_Renters, v_RB50LI, v_POC, na.rm = TRUE) >= 2 &
							sum(dp_PChRent, dp_RentGap, na.rm = TRUE) == 2 ~ 1,
						  TRUE ~ 0),
		scen1.VLI30RB = case_when(v_VLI == 1 &
							  tr_pstudents < .2 &
							  tr_population >= 500 &
							  sum(v_Renters,
							  	  v_RB30VLI,
							  	  v_POC, na.rm = TRUE) >= 2 &
							  sum(dp_PChRent,
							  	  dp_RentGap, na.rm = TRUE) >= 1 ~ 1,
						  tr_POC_rank >= .95 &
							  tr_pstudents < .2 &
							  tr_population >= 500 &
							  sum(v_VLI,
							  	  v_Renters,
							  	  v_RB30VLI,
							  	  v_POC, na.rm = TRUE) >= 3 &
							  sum(dp_PChRent,
							  	  dp_RentGap, na.rm = TRUE) >= 1 ~ 1,
						  tr_pPOC >= .9 &
							  tr_pstudents < .2 &
							  tr_population >= 500 &
							  sum(v_VLI,
							  	  v_Renters,
							  	  v_RB30VLI,
							  	  v_POC, na.rm = TRUE) >= 3 &
							  sum(dp_PChRent,
							  	  dp_RentGap, na.rm = TRUE) >= 1 ~ 1,
						  TRUE ~ 0),
		scen3.VLI30RB = case_when(big_city == 1 ~ scen1.VLI30RB,
						  big_city == 0 &
						  	tr_pstudents < .2 &
							tr_population >= 500 &
							v_VLI == 1 &
							sum(v_Renters, v_RB30VLI, v_POC, na.rm = TRUE) >= 2 &
							sum(dp_PChRent, dp_RentGap, na.rm = TRUE) == 2 ~ 1,
						  TRUE ~ 0),
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

	ct_new$tr_sc_LI30RB.lag <- lag.listw(lw_bin,ct_new$scen1.LI30RB)
	ct_new$tr_sc_LI50RB.lag <- lag.listw(lw_bin,ct_new$scen1.LI50RB)
	ct_new$tr_sc_VLI30RB.lag <- lag.listw(lw_bin,ct_new$scen1.VLI30RB)
	ct_new$tr_sc_VLI50RB.lag <- lag.listw(lw_bin,ct_new$scen1.VLI50RB)

# glimpse(ct@data %>% filter(GEOID == "06037228500"))
# glimpse(ct@data %>% filter(GEOID == "06037700700"))
# glimpse(ct@data %>% filter(GEOID == "06001401600"))
# glimpse(ct@data %>% filter(GEOID == "06075060400"))
# glimpse(ct@data %>% filter(GEOID == "06037700802"))
# glimpse(ct@data %>% filter(GEOID == "06081611800"))
# glimpse(ct@data %>% filter(GEOID == "06081611800"))

# ct@data %>% group_by(scen1, big_city) %>% count()
# ct@data %>% group_by(scen2, big_city) %>% count()
# ct@data %>% group_by(scen3, big_city) %>% count()
# ct@data %>% group_by(scen4, big_city) %>% count()

# ==========================================================================
# Final dataframe
# ==========================================================================

df_final.RB30LI <-
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
							  	  v_RB30LI,
							  	  v_POC, na.rm = TRUE) >= 2 ~ "Tier 2: Vulnerable"),
		tier3 = case_when(tr_dq == 0 ~ NA_character_,
						  sum(v_POC, v_VLI, na.rm = TRUE) == 2 |
						  sum(v_POC, v_RB30LI, na.rm = TRUE) == 2|
						  sum(v_POC, v_Renters, na.rm = TRUE) == 2|
						  v_VLI == 1 |
						  sum(v_VLI, v_RB30LI, na.rm = TRUE) == 2|
						  sum(v_VLI, v_Renters, na.rm = TRUE) == 2 ~ "Tier 3: Some Vulnerability"),
		tier1 = case_when(tr_dq == 0 ~ "Poor Data Quality",
						    scen3.LI30RB == 1 ~ "Tier 1: Heightened Sensitivity",
						    big_city == 1 &
							tr_sc_LI30RB.lag >= .6 &
							tr_pstudents < .2 &
							tr_population >= 500 &
						  	v_VLI == 1 &
							sum(v_Renters,
								v_RB30LI,
								v_POC, na.rm = TRUE) >= 2 ~ "Tier 1: Heightened Sensitivity"),
		text = "",
		popup_text = paste0("Tract: ", GEOID))

# df_final.RB50LI <-
# 	ct_new %>%
# 	st_as_sf() %>%
# 	select(GEOID,
# 		   COUNTY,
# 		   NeighType,
# 		   big_city,
# 		   starts_with("tr_"),
# 		   starts_with("co_"),
# 		   starts_with("v_"),
# 		   starts_with("dp_"),
# 		   starts_with("scen")) %>%
# 	group_by(GEOID) %>%
# 	mutate(
# 		tier2 = case_when(tr_dq == 0 ~ NA_character_,
# 						  tr_pstudents < .2 &
# 							tr_population >= 500 &
# 						  	v_VLI == 1 &
# 							  sum(v_Renters,
# 							  	  v_RB50LI,
# 							  	  v_POC, na.rm = TRUE) >= 2 ~ "Tier 2: Vulnerable"),
# 		tier3 = case_when(tr_dq == 0 ~ NA_character_,
# 						  sum(v_POC, v_VLI, na.rm = TRUE) == 2 |
# 						  sum(v_POC, v_RB50LI, na.rm = TRUE) == 2|
# 						  sum(v_POC, v_Renters, na.rm = TRUE) == 2|
# 						  v_VLI == 1 |
# 						  sum(v_VLI, v_RB50LI, na.rm = TRUE) == 2|
# 						  sum(v_VLI, v_Renters, na.rm = TRUE) == 2 ~ "Tier 3: Some Vulnerability"),
# 		tier1 = case_when(tr_dq == 0 ~ "Poor Data Quality",
# 						    scen3.LI50RB == 1 ~ "Tier 1: Heightened Sensitivity",
# 						    big_city == 1 &
# 							tr_sc_LI50RB.lag >= .6 &
# 							tr_pstudents < .2 &
# 							tr_population >= 500 &
# 						  	v_VLI == 1 &
# 							sum(v_Renters,
# 								v_RB50LI,
# 								v_POC, na.rm = TRUE) >= 2 ~ "Tier 1: Heightened Sensitivity"),
# 		text = "",
# 		popup_text = paste0("Tract: ", GEOID))

# df_final.RB30VLI <-
# 	ct_new %>%
# 	st_as_sf() %>%
# 	select(GEOID,
# 		   COUNTY,
# 		   NeighType,
# 		   big_city,
# 		   starts_with("tr_"),
# 		   starts_with("co_"),
# 		   starts_with("v_"),
# 		   starts_with("dp_"),
# 		   starts_with("scen")) %>%
# 	group_by(GEOID) %>%
# 	mutate(
# 		tier2 = case_when(tr_dq == 0 ~ NA_character_,
# 						  tr_pstudents < .2 &
# 							tr_population >= 500 &
# 						  	v_VLI == 1 &
# 							  sum(v_Renters,
# 							  	  v_RB30VLI,
# 							  	  v_POC, na.rm = TRUE) >= 2 ~ "Tier 2: Vulnerable"),
# 		tier3 = case_when(tr_dq == 0 ~ NA_character_,
# 						  sum(v_POC, v_VLI, na.rm = TRUE) == 2 |
# 						  sum(v_POC, v_RB30VLI, na.rm = TRUE) == 2|
# 						  sum(v_POC, v_Renters, na.rm = TRUE) == 2|
# 						  v_VLI == 1 |
# 						  sum(v_VLI, v_RB30VLI, na.rm = TRUE) == 2|
# 						  sum(v_VLI, v_Renters, na.rm = TRUE) == 2 ~ "Tier 3: Some Vulnerability"),
# 		tier1 = case_when(tr_dq == 0 ~ "Poor Data Quality",
# 						    scen3.VLI30RB == 1 ~ "Tier 1: Heightened Sensitivity",
# 						    big_city == 1 &
# 							tr_sc_VLI30RB.lag >= .6 &
# 							tr_pstudents < .2 &
# 							tr_population >= 500 &
# 						  	v_VLI == 1 &
# 							sum(v_Renters,
# 								v_RB30VLI,
# 								v_POC, na.rm = TRUE) >= 2 ~ "Tier 1: Heightened Sensitivity"),
# 		text = "",
# 		popup_text = paste0("Tract: ", GEOID))

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
		popup_text = paste0("Tract: ", GEOID))

# df_final %>% st_set_geometry(NULL) %>% group_by(tier1) %>% count()
# df_final %>% st_set_geometry(NULL) %>% group_by(tier1.2) %>% count()
df_final.RB30LI %>% st_set_geometry(NULL) %>% group_by(tier1) %>% count()
df_final.RB50LI %>% st_set_geometry(NULL) %>% group_by(tier1) %>% count()
df_final.RB30VLI %>% st_set_geometry(NULL) %>% group_by(tier1) %>% count()
df_final.RB50VLI %>% st_set_geometry(NULL) %>% group_by(tier1) %>% count()

# df_final %>% st_set_geometry(NULL) %>% group_by(tier1.4) %>% count()
# df_final %>% st_set_geometry(NULL) %>% group_by(scen1) %>% count()
# df_final %>% st_set_geometry(NULL) %>% group_by(tier1, scen1, tier2, tier3) %>% count()


# df_final.RB30LI %>% pull(tr_pPOC) %>% unique() %>% summary()
# df_final.RB50VLI %>% pull(co_pchrent) %>% unique() %>% summary()

# ggplot(df_final.RB30LI) +
# 	geom_point(aes(x = reorder(GEOID, co_pchrent), y = co_pchrent))

# glimpse(df_final %>% filter(GEOID == "06081613800"))
# glimpse(df_final %>% filter(GEOID == "06055201800"))
# glimpse(df_final %>% filter(GEOID == "06037222001"))
# glimpse(df_final %>% filter(GEOID == "06037222001"))
# glimpse(df_final %>% filter(GEOID == "06075017700"))
# glimpse(df_final %>% filter(GEOID == "06001426100"))
# glimpse(df_final %>% filter(GEOID == "06037531101"))
# glimpse(df_final %>% filter(GEOID == "06037531504"))
# glimpse(df_final %>% filter(GEOID == "06037532700"))
# glimpse(df_final %>% filter(GEOID == "06075015900"))
# glimpse(df_final %>% filter(GEOID == "06037228720"))
# glimpse(df_final %>% filter(GEOID == "06081611800"))
# glimpse(df_final %>% filter(GEOID == "06037228720"))
# glimpse(df_final %>% filter(GEOID == "06075017700"))

# glimpse(df_final %>% filter(GEOID == "06037531101"))
# glimpse(df_final %>% filter(GEOID == "06037531504"))
# glimpse(df_final %>% filter(GEOID == "06037532700"))
# glimpse(df_final %>% filter(GEOID == "06037203900"))
# glimpse(df_final %>% filter(GEOID == "06075980501"))
# glimpse(df_final %>% filter(GEOID == "06019002701"))
# glimpse(df_final %>% filter(GEOID == "06037228720"))
glimpse(df_final %>% filter(GEOID == "06081610400"))
glimpse(df_final %>% filter(GEOID == "06081610400"))

# st_write(df_final, "~/git/sensitive_communities/data/df_final.shp", delete_layer = TRUE)
# fwrite(df_final %>% st_set_geometry(NULL), "~/git/sensitive_communities/data/df_final.csv")

df_final %>% st_set_geometry(NULL) %>% select(tr_VLI_prop, co_VLI_prop) %>% distinct() %>% ungroup() %>% summary()

ggplot(df_final %>% filter(COUNTY == "06037")) +
	geom_point(aes(x = reorder(GEOID, -tr_irLI_30p), y = tr_irLI_30p))

ggplot(ct@data) +
	geom_point(aes(x = reorder(ALAND, -totrent), y = totrent))

# st_erase <- function(x, y) {
#   st_difference(x, st_union(st_combine(y)))
# }

# map_df
# ca_water <- area_water("NY", class = "sf")

# ca_erase <- st_erase(df_final, ca_water)

# ==========================================================================
# Add advocates layer
# ==========================================================================

adv_surprisedissc <- c("06001422300", "06001441923", "06001403300", "06041125000", "06013339002", "06013340001", "06075015600", "06075045200", "06075042601", "06075047701", "06075042700", "06085511301", "06097152203", "06097152000", "06097153001", "06097153104", "06097151401", "06097153200", "06097151308", "06041112100", "06041111000", "06037265700", "06037265202", "06037700801", "06037461502", "06037302401", "06037302505", "06037302506", "06037302302", "06037302201", "06037302004", "06037301802", "06037301702", "06037301900", "06037301100", "06037302002", "06037302003", "06037301206", "06037301601", "06037310703", "06037310702", "06037310701", "06083000301", "06083001102", "06037701702", "06059042313", "06037701100", "06037264103", "06037221710", "06037222200", "06037221601", "06037222100", "06037222500", "06037231300", "06037231210", "06037231220", "06037231100", "06037224600", "06037224420", "06037189800", "06037189904", "06037190100", "06037190700", "06037190802", "06065044807", "06065044509", "06067006900", "06067006800", "06067006702", "06067006701", "06067006202", "06067005505", "06067005506", "06067005510", "06067005601", "06067005602", "06067006002", "06067007413", "06067007504", "06067007301", "06067007601", "06067006003", "06067007414", "06067007423", "06067007501", "06067008133", "06067007701", "06067005903", "06067007019", "06067007015", "06019005510", "06019004505", "06053010101", "06037462400", "06037462700", "06037701100", "06077001300", "0601093003", "06037540901", "06041112202", "06041112100", "06041111000")

adv_shouldbe <- c("06075023400", "06075020900", "06081611800", "06081611900", "06001406000", "06075026003", "06075010600", "06075011700", "06075011800", "06075012301", "06075012501", "06075012502", "06075015900", "06075023001", "06075023102", "06075023103", "06075023200", "06075023300", "06075023400", "06075025701", "06075025702", "06075025800", "06075026001", "06075026003", "06075026301", "06075026303", "06075033203", "06075033204", "06075035202", "06075047701", "06075047902", "06075060502", "06019002602", "06019003600", "06019002100", "06019004207", "06019004404", "06065045604", "06019000300", "06047000304", "06047000301", "06099002503", "06047000203", "06047000201", "06089012101", "06063000501", "06023001000", "06029004101", "06029006304", "06029006202", "06037462100", "06037234600", "06037234700", "06037234800", "06037234300", "06037219500", "06037234200", "06037234300", "06037234000", "06037231400", "06001407300", "06001409000", "06001401700", "06013376000", "06095250801", "06055201006", "06055201005", "06055201007", "06001408500", "06001408400", "06037209401", "06037208902", "06037208801", "06037224320", "06037224310", "06037212202", "06037206031", "06037204700", "06037204120", "06037203710", "06037203800", "06037203600", "06037204920", "06037203900", "06037532603", "06037532604", "06037532606", "06037533202", "06037534501", "06037534502", "06037535607", "06037540300", "06037540501", "06037535605", "06037535803", "06037535802", "06037535901", "06037535902", "06037536104", "06037540101", "06037540102", "06037541700", "06037542000", "06037541802", "06037540000", "06037542104", "06037540600", "06037542401", "06037542200", "06037570404", "06037543201", "06037543100", "06037543000", "06037542900", "06037542800", "06037541002", "06037291220", "06037542602", "06037541001", "06037291210", "06037541100", "06037541200", "06037541300", "06037553200", "06037553300", "06037554103", "06037553100", "06037554105", "06037554002", "06037554001", "06037554101", "06037554301", "06037554201", "06037188300", "06037188100", "06047000701", "06047000601", "06047000902", "06047001502", "06047001402", "06019000100", "06019000300", "06019000700", "06019001303", "06019001301", "06019002602", "06037224410", "06037221710", "06037221810", "06037221900", "06037224700", "06037222700", "06037222600", "06001442400", "06001442301", "6001409000", "6075017700", "6075022801", "6075022803", "6075022903", "6075022902", "6075025401", "6075025200", "6075033203", "6075033204", "6075033201", "6075030102", "6075042602", "6075047600", "6075061500", "6075018000", "6075060700", "6075015900", "6075016801")

advocate_tracts <-
	df_final %>%
	ungroup() %>%
	mutate(adv_surprisedissc = case_when(GEOID %in% adv_surprisedissc ~ TRUE),
		   adv_shouldbe = case_when(GEOID %in% adv_shouldbe ~ TRUE))

adv_surprisedissc <-
	advocate_tracts%>%
	filter(adv_surprisedissc == TRUE)

adv_shouldbe <-
	advocate_tracts%>%
	filter(adv_shouldbe == TRUE)

adv_shouldbe <-
	advocate_tracts%>%
	filter(adv_shouldbe == TRUE)

# fwrite(advocate_tracts %>% st_set_geometry(NULL), "~/git/sensitive_communities/data/191118_sc_advocate.csv")
# st_write(advocate_tracts, "~/git/sensitive_communities/data/191118_sc_advocate.shp")


# ==========================================================================
# Maps
# ==========================================================================

# tm_basemap(leaflet::providers$CartoDB.Positron) +
# 	tm_shape(df_final, name = "Tier 1 - Tier 1: Heightened Sensitivity") +
# 	tm_polygons("tier1",
# 			# palette = c("#FF6633","#FF6633"),
# 			palette = c("#FF6633","#CCCCCC"),
# 			# label = "Tier 1: Heightened Sensitivity",
# 			alpha = .7,
# 			border.alpha = .15,
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
# 						   "% inc x rb " = "tr_irLI_30p",
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
# 						   "POC" = "v_POC",
# 						   "Renters" = "v_Renters",
# 						   "VLI" = "v_VLI",
# 						   "RB" = "v_RBLI",
# 						   "Ch Rent" = "dp_PChRent",
# 						   "Rent Gap" = "dp_RentGap"
# 						   ),
# 			popup.format = list(digits=2)) +
# 	# tm_layout(title = paste0("Scenario: v_POC, ",renters,", ", vli, ", ", rb, ", ", chrent, ", ", rentgap)) +
# 	tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 9), alpha = .9)

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

df_tier2 <- df_final.RB50VLI
bc_tract <-
	df_final.RB50VLI %>%
	filter(big_city == 1)

# sen_map4 <- function(t1,
# 					 renters,
# 					 vli,
# 					 rb,
# 					 chrent,
# 					 rentgap,
# 					 title = paste0("Scenario: v_POC, ",renters,", ", vli, ", ", rb, ", ", chrent, ", ", rentgap))

map.RB50VLI <-
tm_basemap(leaflet::providers$CartoDB.Positron) + # http://leaflet-extras.github.io/leaflet-providers/preview/
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
						   "% LI x RB" = "tr_irLI_30p",
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
						   "% LI x RB" = "tr_irLI_30p",
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
	# tm_shape(place, name = "places") +
	# 	tm_polygons(border.col = "black", lwd = 3, alpha = 0) +
	# tm_shape(csd, name = "county sub division") +
	# 	tm_polygons(border.col = "grey", lwd = 3, alpha = 0) +
	# tm_shape(adv_surprisedissc, name = "Surprised It's Sensitive") +
	# 	tm_polygons(border.col = "blue", lwd = 3, alpha = 0) +
	# tm_shape(adv_shouldbe, name = "Should Be Sensitive") +
	# 	tm_polygons(border.col = "red", lwd = 3, alpha = 0) +
# tm_layout(title = "Scenario: v_POC, v_Renters, v_VLI, v_RBLI, dp_PChRent, dp_RentGap") +
tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 9), alpha = .9)

map.RB50VLI <-
	tmap_leaflet(map.RB50VLI) %>%
	leaflet::hideGroup(c("Bus",
						 "Tier 2: Vulnerable"#,
						 # "adv_surprisedissc",
						 # "adv_shouldbe"
						 ))

# save_map(v2map, "v2map")
htmlwidgets::saveWidget(map.RB50VLI, file="~/git/sensitive_communities/docs/map.RB50VLI.html")




# ==========================================================================
#
# ==========================================================================





mapb <-
tm_basemap(leaflet::providers$CartoDB.Positron) + # http://leaflet-extras.github.io/leaflet-providers/preview/
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
tm_shape(df_final, name = "Sensitive Community") +
	tm_polygons("tier1.4",
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
						   "% LI x RB" = "tr_irLI_30p",
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
						   "RB" = "v_RBLI",
						   "Ch Rent" = "dp_PChRent",
						   "Rent Gap" = "dp_RentGap"
						   ),
			popup.format = list(digits=2)) +
tm_layout(title = "Scenario: v_POC, v_Renters, v_VLI, v_RBLI, dp_PChRent, dp_RentGap") +
tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 9), alpha = .9)

v2mapb <-
	tmap_leaflet(mapb) %>%
	leaflet::hideGroup("Bus")

# save_map(v2map, "v2map")
library(htmlwidgets)
saveWidget(v2mapb, file="~/git/sensitive_communities/docs/v2mapb.html")


















sen_map5 <- function(t1,
					 t2_df,
					 t2,
					 t3_df,
					 t3,
					 renters,
					 vli,
					 rb,
					 chrent,
					 rentgap,
					 title = paste0("Scenario: v_POC, ",renters,", ", vli, ", ", rb, ", ", chrent, ", ", rentgap))
tm_basemap(leaflet::providers$CartoDB.Positron) + # http://leaflet-extras.github.io/leaflet-providers/preview/
			tm_shape(t3_df, name = "Tier 3 - Some Vulnerability") +
			tm_polygons(t3,
			palette = "#4daf4a",
			# label = "Tier 3 - Some Vulnerability",
			alpha = .7,
			border.alpha = .15,
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
						   "% LI x RB" = "tr_irLI_30p",
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
						   "POC" = "v_POC",
						   "Renters" = renters,
						   "VLI" = vli,
						   "RB" = rb,
						   "Ch Rent" = chrent,
						   "Rent Gap" = rentgap
						   ),
			popup.format = list(digits=2)) +
			tm_shape(t2_df, name = "Tier 2 - Vulnerable Communities") +
			tm_polygons(t2,
			palette = "#377eb8",
			# label = "Tier 2 - Vulnerable Communities",
			alpha = .7,
			border.alpha = .15,
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
						   "% inc x rb " = "tr_irLI_30p",
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
						   "POC" = "v_POC",
						   "Renters" = renters,
						   "VLI" = vli,
						   "RB" = rb,
						   "Ch Rent" = chrent,
						   "Rent Gap" = rentgap
						   ),
			popup.format = list(digits=2)) +
	tm_shape(df_final, name = "Tier 1 - Heightened Sensitivity") +
	tm_polygons(t1,
			# palette = c("#FF6633","#FF6633"),
			palette = c("#CCCCCC", "#FF6633"),
			# label = "Heightened Sensitivity",
			alpha = .7,
			border.alpha = .15,
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
						   "% inc x rb " = "tr_irLI_30p",
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
						   "POC" = "v_POC",
						   "Renters" = renters,
						   "VLI" = vli,
						   "RB" = rb,
						   "Ch Rent" = chrent,
						   "Rent Gap" = rentgap
						   ),
			popup.format = list(digits=2)) +
	tm_shape(adv_surprisedissc, name = "Surprised It's Sensitive") +
		tm_polygons(border.col = "blue", lwd = 3, alpha = 0) +
	tm_shape(adv_shouldbe, name = "Should Be Sensitive") +
		tm_polygons(border.col = "red", lwd = 3, alpha = 0) +
	tm_layout(title = title) +
	tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 9), alpha = .9)



#
# Make maps
# --------------------------------------------------------------------------

t2_df <-
	df_final %>%
	filter(tier2 == "Tier 2: Vulnerable")

t3_df <-
	df_final %>%
	filter(tier3 == "Tier 3: Some Vulnerability")


# tiermap1 <-
# 	sen_map4(t1 = "tier1",
# 			 t2_df = t2_df,
# 			 t2 = "tier2",
# 			 t3_df = t3_df,
# 			 t3 = "tier3",
# 			 renters = "v_Renters",
# 			 vli = "v_VLI",
# 			 rb = "v_RBLI",
# 			 chrent = "dp_PChRent",
# 			 rentgap = "dp_RentGap",
# 			 "Scenario: No City Differentiation")

# save_map(tiermap1, "tiermap1")

# tiermap2 <-
# 	sen_map4(t1 = "tier1.2",
# 			 t2_df = t2_df,
# 			 t2 = "tier2",
# 			 t3_df = t3_df,
# 			 t3 = "tier3",
# 			 renters = "v_Renters",
# 			 vli = "v_VLI",
# 			 rb = "v_RBLI",
# 			 chrent = "dp_PChRent",
# 			 rentgap = "dp_RentGap",
# 			 title = "Scenario: City Differentiation == 4:4 Vuln. & 2:2 DP")

# save_map(tiermap2, "tiermap2")

tiermap3 <-
	sen_map4(t1 = "tier1",
			 t2_df = t2_df,
			 t2 = "tier2",
			 t3_df = t3_df,
			 t3 = "tier3",
			 renters = "v_Renters",
			 vli = "v_VLI",
			 rb = "v_RBLI",
			 chrent = "dp_PChRent",
			 rentgap = "dp_RentGap",
			 title = "Scenario: City Differentiation == v_VLI, 2:3 Vuln., & 2:2 DP")

save_map(tiermap3, "tiermap3")

# tiermap4 <-
# 	sen_map4(t1 = "tier1.2",
# 			 t2_df = t2_df,
# 			 t2 = "tier2",
# 			 t3_df = t3_df,
# 			 t3 = "tier3",
# 			 renters = "v_Renters",
# 			 vli = "v_VLI",
# 			 rb = "v_RBLI",
# 			 chrent = "dp_PChRent",
# 			 rentgap = "dp_RentGap",
# 			 title = "Scenario: City Differentiation == 4:4 Vuln. OR 2:2 DP")

# save_map(tiermap4, "tiermap4")

tiermap5 <-
	sen_map5(t1 = "tier1",
			 t2_df = t2_df,
			 t2 = "tier2",
			 t3_df = t3_df,
			 t3 = "tier3",
			 renters = "v_Renters",
			 vli = "v_VLI",
			 rb = "v_RBLI",
			 chrent = "dp_PChRent",
			 rentgap = "dp_RentGap",
			 title = "Scenario: City Differentiation == v_VLI, 2:3 Vuln., & 2:2 DP & Advocate Layers")

save_map(tiermap5, "tiermap5")

# ==========================================================================
# EXCESS CODE - to be erased
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
		# v_VLI = case_when(tr_propstudent17 < .20 & # v2
		#    				tr_VLI_prop17 > co_VLI_prop17 ~ 1, # v2
		#    				TRUE ~ 0),
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
		`Scenario 22` = case_when(sum(v_renters_50p,
									  v_ELI,
									  v_rb, na.rm = TRUE) == 3 &
						   sum(dp_chrent_co, dp_rentgap_co) >=1 ~ TRUE),
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
		`Scenario 52b` = case_when(v_VLI_med == 1 &
								  sum(v_renters_40p,
							  v_rbVLI_50rb, na.rm = TRUE) >= 1 &
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
		tier1 = case_when(v_VLI_med == 1 &
						  sum(v_poc,
							  v_renters_40p,
							  v_rbLI_50rb, na.rm = TRUE) >= 2 &
						  sum(dp_chrent_co,
							  dp_rentgap_co) >= 1 &
						  totraceE.y >= 500 &
						  pPOC >= .3 &
						  tr_propstudent17 < .20 ~ "Heightened Sensitivity"),
		tier2 = case_when(v_VLI_med == 1 &
						  sum(v_poc,
							  v_renters_40p,
							  v_rbLI_50rb, na.rm = TRUE) >= 2 &
						  totraceE.y >= 500 &
						  pPOC >= .3 &
						  tr_propstudent17 < .20 ~ "Vulnerable"),
		tier3 = case_when(sum(v_poc, v_VLI_med, na.rm = TRUE) == 2 |
						  sum(v_poc, v_rbLI_50rb, na.rm = TRUE) == 2|
						  sum(v_poc, v_renters_40p, na.rm = TRUE) == 2|
						  v_VLI_med == 1 |
						  sum(v_VLI_med, v_rbLI_50rb, na.rm = TRUE) == 2|
						  sum(v_VLI_med, v_renters_40p, na.rm = TRUE) == 2 ~ "Some Vulnerability")
		) %>%
ungroup()

# fwrite(final_df %>% st_set_geometry(NULL), file = "~/data/temp/191114_scdata.csv")

final_df %>%
	select(pPOC) %>% hist(pPOC)
hist(final_df$pPOC)

ggplot(data = final_df, aes(pPOC, tr_VLI_prop17)) +
	geom_point() +
	geom_smooth()

#
# Advocate highlights
# --------------------------------------------------------------------------

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

sen_map1 <- function(scen, renters, vli, rb, chrent, rentgap)
tm_basemap(leaflet::providers$CartoDB.Positron) + # http://leaflet-extras.github.io/leaflet-providers/preview/
tm_shape(final_df, name = "Sensitive Communities Layer") +
	tm_polygons(scen,
			# palette = c("#FF6633","#FF6633"),
			palette = c("#e41a1c","#e41a1c"),
			label = "Sensitive Communities",
			alpha = .9,
			border.alpha = .15,
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
						   "% inc x rb " = "tr_irLI_30p",
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
						   "POC" = "v_POC",
						   "Renters" = renters,
						   "VLI" = vli,
						   "RB" = rb,
						   "Ch Rent" = chrent,
						   "Rent Gap" = rentgap
						   ),
			popup.format = list(digits=2)) +
	tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 9), alpha = .9) +
	tm_layout(title = paste0(scen, ": ",renters,", ", li, ", ", chrent, ", ", rentgap))

sen_map2 <- function(scen, renters, li, lirb, rb, chrent, rentgap)
	sen_map1(scen, renters, li, lirb, rb, chrent, rentgap) +
	tm_layout(title = paste0(scen, ": ",renters,", ", li, ", ", rb, ", ", chrent, ", ", rentgap))

sen_map3 <- function(scen, renters, li, lirb, rb, chrent, rentgap)
	sen_map1(scen, renters, li, lirb, rb, chrent, rentgap) +
	tm_layout(title = paste0(scen, ": v_POC, ",renters,", ", li, ", ", rb, ", ", chrent, ", ", rentgap))

sen_map4 <- function(t1,
					 t2_df,
					 t2,
					 t3_df,
					 t3,
					 renters,
					 vli,
					 rb,
					 chrent,
					 rentgap)
tm_basemap(leaflet::providers$CartoDB.Positron) + # http://leaflet-extras.github.io/leaflet-providers/preview/
			tm_shape(t3_df, name = "Tier 3 - Some Vulnerability") +
			tm_polygons(t3,
			palette = c("#4daf4a","#4daf4a"),
			label = "Tier 3 - Some Vulnerability",
			alpha = .9,
			border.alpha = .15,
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
						   "% inc x rb " = "tr_irLI_30p",
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
						   "POC" = "v_POC",
						   "Renters" = renters,
						   "VLI" = vli,
						   "RB" = rb,
						   "Ch Rent" = chrent,
						   "Rent Gap" = rentgap
						   )) +
			tm_shape(t2_df, name = "Tier 2 - Vulnerable Communities") +
			tm_polygons(t2,
			palette = c("#377eb8","#377eb8"),
			label = "Tier 2 - Vulnerable Communities",
			alpha = .9,
			border.alpha = .15,
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
						   "% inc x rb " = "tr_irLI_30p",
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
						   "POC" = "v_POC",
						   "Renters" = renters,
						   "VLI" = vli,
						   "RB" = rb,
						   "Ch Rent" = chrent,
						   "Rent Gap" = rentgap
						   ),
			popup.format = list(digits=2)) +
	tm_shape(df_final, name = "Sensitive Communities Layer") +
	tm_polygons(t1,
			# palette = c("#FF6633","#FF6633"),
			palette = c("#e41a1c","#e41a1c"),
			label = "Sensitive Communities",
			alpha = .9,
			border.alpha = .15,
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
						   "% inc x rb " = "tr_irLI_30p",
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
						   "POC" = "v_POC",
						   "Renters" = renters,
						   "VLI" = vli,
						   "RB" = rb,
						   "Ch Rent" = chrent,
						   "Rent Gap" = rentgap
						   ),
			popup.format = list(digits=2)) +
	tm_layout(title = paste0("Scenario 52: v_POC, ",renters,", ", vli, ", ", rb, ", ", chrent, ", ", rentgap)) +
	tm_view(set.view = c(lon = -122.2712, lat = 37.8044, zoom = 9), alpha = .9)

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



t2_df <-
	df_final %>%
	filter(tier2 == "Vulnerable")

t3_df <-
	df_final %>%
	filter(tier3 == "Some Vulnerability")


tiermap1 <-
	sen_map4(t1 = "tier1",
			 t2_df = t2_df,
			 t2 = "tier2",
			 t3_df = t3_df,
			 t3 = "tier3",
			 renters = "v_Renters",
			 vli = "v_VLI",
			 rb = "v_RBLI",
			 chrent = "dp_PChRent",
			 rentgap = "dp_RentGap")



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
	pull(`Scenario 52`) %>%
	sum(na.rm = TRUE)
final_df %>%
	pull(`Scenario 52b`) %>%
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
# save_map(scen36, "scen36")
# save_map(scen37, "scen37")
# save_map(scen38, "scen38")
# save_map(scen39, "scen39")
# save_map(scen40, "scen40")
# save_map(scen41, "scen41")
# save_map(scen42, "scen42")
# save_map(scen43, "scen43")

# save_map(scen44, "scen44")
# save_map(scen45, "scen45")
# save_map(scen46, "scen46")
# save_map(scen47, "scen47")
# save_map(scen48, "scen48")
# save_map(scen49, "scen49")
# save_map(scen50, "scen50")
# save_map(scen51, "scen51")
# save_map(scen52, "scen52")

# Saving tier maps
save_map(tsc52pv, "tsc52pv")
save_map(tsc52prb, "tsc52prb")
save_map(tsc52pr, "tsc52pr")
save_map(tsc52v, "tsc52v")
save_map(tsc52vrb, "tsc52vrb")
save_map(tsc52vr, "tsc52vr")
save_map(tiermap1, "tiermap1")

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
