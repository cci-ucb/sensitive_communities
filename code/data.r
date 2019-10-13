# ==========================================================================
# Develop data for displacement and vulnerability measures
# Initial Author: Tim Thomas
# Created: 2019.10.13
# ==========================================================================

#
# Tasks
# 1. Create measure extraction functions
# --------------------------------------------------------------------------

# ==========================================================================
# Libraries
# ==========================================================================

library(data.table)
library(tidyverse)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
library(tidycensus)

# ==========================================================================
# Data
# ==========================================================================

#
# State tract data
# --------------------------------------------------------------------------

caltr <-
	tracts("CA", class = "sf") 



source("~/git/sensitive_communities/code/vars.r")

gen_vars12 <-
	c(
	'totrace' = 'B03002_001E',
	'White' = 'B03002_003E',
	'Black' = 'B03002_004E',
	'Asian' = 'B03002_006E',
	'Latinx' = 'B03002_012E',
	'totwelf' = 'B19057_001E', #
	'welf' = 'B19057_002E',
	'totpov' = 'B17017_001E', #
	'povfamh' = 'B17017_003E',
	'povnonfamh' = 'B17017_020E',
	'totunemp' = 'B23025_001E',
	'unemp' = 'B23025_005E',
	'totfemhh' = 'B09019_001E',
	'femfamhh' = 'B09019_006E',
	'femnonfamhh' = 'B09019_029',
	'totage' = 'B01001_001E',
	'Munder5' = 'B01001_003E',
	'Mfiveto9' = 'B01001_004E',
	'Mtento14' = 'B01001_005E',
	'Mfiftto17' = 'B01001_006E',
	'Funder5' = 'B01001_027E',
	'Ffiveto9' = 'B01001_028E',
	'Ftento14' = 'B01001_029E',
	'Ffiftto17' = 'B01001_030E',
	'Mel1' = 'B01001_020E', # Male!!65 and 66 years
	'Mel2' = 'B01001_021E', # Male!!67 to 69 years
	'Mel3' = 'B01001_022E', # Male!!70 to 74 years
	'Mel4' = 'B01001_023E', # Male!!75 to 79 years
	'Mel5' = 'B01001_024E', # Male!!80 to 84 years
	'Mel6' = 'B01001_025E', # Male!!85 years and over
	'Fel1' = 'B01001_044E', # Female!!65 and 66 years
	'Fel2' = 'B01001_045E', # Female!!67 to 69 years
	'Fel3' = 'B01001_046E', # Female!!70 to 74 years
	'Fel4' = 'B01001_047E', # Female!!75 to 79 years
	'Fel5' = 'B01001_048E', # Female!!80 to 84 years
	'Fel6' = 'B01001_049E', # Female!!85 years and over
	'toted' = 'B15003_001E',
	'bach' = 'B15003_022E',
	'mas' = 'B15003_023E',
	'pro' = 'B15003_024E',
	'doc' = 'B15003_025E',
	'medhhinc' = 'B19013_001E',
	'totten' = 'B25003_001',  # Estimate!!Total TENURE
	'totown' = 'B25003_002', # Estimate!!Total!!Owner occupied TENURE
	'totrent' = 'B25003_003', # Estimate!!Total!!Renter occupied TENURE
	'tottenWHT' = 'B25003A_001', # Estimate!!Total TENURE (WHITE ALONE HOUSEHOLDER)
	'totownWHT' = 'B25003A_002', # Estimate!!Total!!Owner occupied TENURE (WHITE ALONE HOUSEHOLDER)
	'totrentWHT' = 'B25003A_003', # Estimate!!Total!!Renter occupied TENURE (WHITE ALONE HOUSEHOLDER)
	'tottenBLK' = 'B25003B_001', # Estimate!!Total TENURE (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'totownBLK' = 'B25003B_002', # Estimate!!Total!!Owner occupied TENURE (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'totrentBLK' = 'B25003B_003', # Estimate!!Total!!Renter occupied TENURE (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'tottenAIAN' = 'B25003C_001', # Estimate!!Total TENURE (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'totownAIAN' = 'B25003C_002', # Estimate!!Total!!Owner occupied TENURE (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'totrentAIAN' = 'B25003C_003', # Estimate!!Total!!Renter occupied TENURE (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'tottenASI' = 'B25003D_001', # Estimate!!Total TENURE (ASIAN ALONE HOUSEHOLDER)
	'totownASI' = 'B25003D_002', # Estimate!!Total!!Owner occupied TENURE (ASIAN ALONE HOUSEHOLDER)
	'totrentASI' = 'B25003D_003', # Estimate!!Total!!Renter occupied TENURE (ASIAN ALONE HOUSEHOLDER)
	'tottenNHOP' = 'B25003E_001', # Estimate!!Total TENURE (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'totownNHOP' = 'B25003E_002', # Estimate!!Total!!Owner occupied TENURE (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'totrentNHOP' = 'B25003E_003', # Estimate!!Total!!Renter occupied TENURE (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'tottenOTH' = 'B25003F_001', # Estimate!!Total TENURE (SOME OTHER RACE ALONE HOUSEHOLDER)
	'totownOTH' = 'B25003F_002', # Estimate!!Total!!Owner occupied TENURE (SOME OTHER RACE ALONE HOUSEHOLDER)
	'totrentOTH' = 'B25003F_003', # Estimate!!Total!!Renter occupied TENURE (SOME OTHER RACE ALONE HOUSEHOLDER)
	'tottenTWO' = 'B25003G_001', # Estimate!!Total TENURE (TWO OR MORE RACES HOUSEHOLDER)
	'totownTWO' = 'B25003G_002', # Estimate!!Total!!Owner occupied TENURE (TWO OR MORE RACES HOUSEHOLDER)
	'totrentTWO' = 'B25003G_003', # Estimate!!Total!!Renter occupied TENURE (TWO OR MORE RACES HOUSEHOLDER)
	'tottenWHTNL' = 'B25003H_001', # Estimate!!Total TENURE (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'totownWHTNL' = 'B25003H_002', # Estimate!!Total!!Owner occupied TENURE (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'totrentWHTNL' = 'B25003H_003', # Estimate!!Total!!Renter occupied TENURE (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'tottenLAT' = 'B25003I_001', # Estimate!!Total TENURE (HISPANIC OR LATINO HOUSEHOLDER)
	'totownLAT' = 'B25003I_002', # Estimate!!Total!!Owner occupied TENURE (HISPANIC OR LATINO HOUSEHOLDER)
	'totrentLAT' = 'B25003I_003', # Estimate!!Total!!Renter occupied TENURE (HISPANIC OR LATINO HOUSEHOLDER)
	'vac_tot' = 'B25004_001',
   	'vac_rent' = 'B25004_003',
	'mgrent' = 'B25064_001E',
	'totoccupation' = 'C24010_001E',
	'Mmgr' = 'C24010_003E',
	'Fmgr' = 'C24010_039E',
	'totfhc' = 'B11005_001E',
	'femfamheadch' = 'B11005_007E',
	'femnonfamheadch' = 'B11005_010E',
	'totschool' = 'B14002_001E',
	'pschlM1' = 'B14002_006E',
	'pschlM2' = 'B14002_009E',
	'pschlM3' = 'B14002_012E',
	'pschlM4' = 'B14002_015E',
	'pschlM5' = 'B14002_018E',
	'pschlF1' = 'B14002_030E',
	'pschlF2' = 'B14002_033E',
	'pschlF3' = 'B14002_036E',
	'pschlF4' = 'B14002_039E',
	'pschlF5' = 'B14002_042E'
	)


cal17 <- 
	get_acs(
		geography = "tract", 
		variables = gen_vars12, 
		state = "CA", 
		county = NULL, 
		geometry = TRUE, 
		cache_table = TRUE) %>% 
	select(-moe) %>% 
	group_by(GEOID) %>% 
	spread(variables)

