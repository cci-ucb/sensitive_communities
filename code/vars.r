# ==========================================================================
# List of variables for various functions
# Created: 1/22/2019
# Tim Thomas - timothyathomas@gmail.com
# ==========================================================================

# ==========================================================================
# View/Select variables
# ==========================================================================

#
# Online
# --------------------------------------------------------------------------

	# https://www.census.gov/data/developers/data-sets/acs-migration-flows.html
	# https://www.census.gov/data/developers/data-sets/acs-5year.html
	# https://www.census.gov/data/developers/data-sets/acs-3year.html
	# https://www.census.gov/data/developers/data-sets/ACS-supplemental-data.html
	# https://www.census.gov/data/developers/data-sets/acs-1year.html
		# https://api.census.gov/data/2012/acs1/variables.html
	# https://www.census.gov/data/developers/data-sets/decennial-census.2000.html

#
# Through Tidycensus and RStudio
# --------------------------------------------------------------------------

	# vars_acs5_2017 <- tidycensus::load_variables(2017, 'acs5', cache = TRUE)
	# vars_acs1_2017 <- tidycensus::load_variables(2017, 'acs1', cache = TRUE)
	# vars_acs1_2016 <- tidycensus::load_variables(2016, 'acs1', cache = TRUE)
	# vars_acs1_2015 <- tidycensus::load_variables(2015, 'acs1', cache = TRUE)
	# vars_acs1_2014 <- tidycensus::load_variables(2014, 'acs1', cache = TRUE)
	# vars_acs1_2013 <- tidycensus::load_variables(2013, 'acs1', cache = TRUE)
	# vars_acs1_2012 <- tidycensus::load_variables(2012, 'acs1', cache = TRUE)
	# vars_acs5_2012 <- tidycensus::load_variables(2012, 'acs5', cache = TRUE)
	# vars_acs3_2011 <- tidycensus::load_variables(2012, 'acs3', cache = TRUE)
	# vars_acs5_2011 <- tidycensus::load_variables(2011, 'acs5', cache = TRUE)
	# vars_dec10 <- tidycensus::load_variables(2010, 'sf1', cache = TRUE)
	# vars_dec10sf2 <- tidycensus::load_variables(2010, 'sf2', cache = TRUE)
	# vars_dec00sf3 <- tidycensus::load_variables(2000, 'sf3', cache = TRUE)
	# vars_dec00sf1 <- tidycensus::load_variables(2000, 'sf1', cache = TRUE)
	# View(vars_acs5_2017)
	# View(vars_acs1_2017)
	# View(vars_acs1_2016)
	# View(vars_acs1_2015)
	# View(vars_acs1_2014)
	# View(vars_acs1_2013)
	# View(vars_acs1_2012)
	# View(vars_acs5_2012)
	# View(vars_acs3_2011)
	# View(vars_acs5_2011)
	# View(vars_dec10)
	# View(vars_dec10sf2)
	# View(vars_dec00sf3)
	# View(vars_dec00sf1)


# ==========================================================================
# Migration
# ==========================================================================




# ==========================================================================
# Income Variables
# ==========================================================================

#
# CPI consumer price index
# --------------------------------------------------------------------------

# cpi calc https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=1.00&year1=200604&year2=201704 april to april 2017
cpi <-
fread(
'year	CPI
2000	1.43
2001	1.38
2002	1.36
2003	1.33
2004	1.30
2005	1.26
2006	1.21
2007	1.18
2008	1.14
2009	1.15
2010	1.12
2011	1.09
2012	1.06
2013	1.05
2014	1.03
2015	1.03
2016	1.02
2017	1
2018	.98
2019	.98')

#
# Median Income
# --------------------------------------------------------------------------
acsmedinc_vars <-
	c('mhhinc' = 'B19013_001',
	  'mhhinc_wht' = 'B19013A_001',
	  'mhhinc_blk' = 'B19013B_001',
	  'mhhinc_aian' = 'B19013C_001',
	  'mhhinc_asi' = 'B19013D_001',
	  'mhhinc_nhop' = 'B19013E_001',
	  'mhhinc_oth' = 'B19013F_001',
	  'mhhinc_two' = 'B19013G_001',
	  'mhhinc_whtnl' = 'B19013H_001',
	  'mhhinc_lat' = 'B19013I_001')

decmedinc_vars <-
	c(
	  'mhhinc' = 'P053001',
	  'mhhinc_wht' = 'P152A001',
	  'mhhinc_blk' = 'P152B001',
	  'mhhinc_aian' = 'P152C001',
	  'mhhinc_asi' = 'P152D001',
	  'mhhinc_nhop' = 'P152E001',
	  'mhhinc_oth' = 'P152F001',
	  'mhhinc_two' = 'P152G001',
	  'mhhinc_lat' = 'P152H001',
	  'mhhinc_whtnl' = 'P152I001')

#
# Household income variables by race
# --------------------------------------------------------------------------

acshhinc_vars <-
	c(
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
	'HHIncWht_Total' = 'B19001A_001', # Total HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_10' = 'B19001A_002', # Less than $10,000 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_15' = 'B19001A_003', # $10,000 to $14,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_20' = 'B19001A_004', # $15,000 to $19,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_25' = 'B19001A_005', # $20,000 to $24,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_30' = 'B19001A_006', # $25,000 to $29,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_35' = 'B19001A_007', # $30,000 to $34,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_40' = 'B19001A_008', # $35,000 to $39,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_45' = 'B19001A_009', # $40,000 to $44,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_50' = 'B19001A_010', # $45,000 to $49,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_60' = 'B19001A_011', # $50,000 to $59,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_75' = 'B19001A_012', # $60,000 to $74,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_100' = 'B19001A_013', # $75,000 to $99,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_125' = 'B19001A_014', # $100,000 to $124,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_150' = 'B19001A_015', # $125,000 to $149,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_200' = 'B19001A_016', # $150,000 to $199,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_250' = 'B19001A_017', # $200,000 or more HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncBlk_Total' = 'B19001B_001', # Total HOUSEHOLD INCOME (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'HHIncBlk_10' = 'B19001B_002', # Less than $10,000 HOUSEHOLD INCOME (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'HHIncBlk_15' = 'B19001B_003', # $10,000 to $14,999 HOUSEHOLD INCOME (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'HHIncBlk_20' = 'B19001B_004', # $15,000 to $19,999 HOUSEHOLD INCOME (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'HHIncBlk_25' = 'B19001B_005', # $20,000 to $24,999 HOUSEHOLD INCOME (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'HHIncBlk_30' = 'B19001B_006', # $25,000 to $29,999 HOUSEHOLD INCOME (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'HHIncBlk_35' = 'B19001B_007', # $30,000 to $34,999 HOUSEHOLD INCOME (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'HHIncBlk_40' = 'B19001B_008', # $35,000 to $39,999 HOUSEHOLD INCOME (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'HHIncBlk_45' = 'B19001B_009', # $40,000 to $44,999 HOUSEHOLD INCOME (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'HHIncBlk_50' = 'B19001B_010', # $45,000 to $49,999 HOUSEHOLD INCOME (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'HHIncBlk_60' = 'B19001B_011', # $50,000 to $59,999 HOUSEHOLD INCOME (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'HHIncBlk_75' = 'B19001B_012', # $60,000 to $74,999 HOUSEHOLD INCOME (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'HHIncBlk_100' = 'B19001B_013', # $75,000 to $99,999 HOUSEHOLD INCOME (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'HHIncBlk_125' = 'B19001B_014', # $100,000 to $124,999 HOUSEHOLD INCOME (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'HHIncBlk_150' = 'B19001B_015', # $125,000 to $149,999 HOUSEHOLD INCOME (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'HHIncBlk_200' = 'B19001B_016', # $150,000 to $199,999 HOUSEHOLD INCOME (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'HHIncBlk_250' = 'B19001B_017', # $200,000 or more HOUSEHOLD INCOME (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
	'HHIncAIAN_Total' = 'B19001C_001', # Total HOUSEHOLD INCOME (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'HHIncAIAN_10' = 'B19001C_002', # Less than $10,000 HOUSEHOLD INCOME (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'HHIncAIAN_15' = 'B19001C_003', # $10,000 to $14,999 HOUSEHOLD INCOME (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'HHIncAIAN_20' = 'B19001C_004', # $15,000 to $19,999 HOUSEHOLD INCOME (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'HHIncAIAN_25' = 'B19001C_005', # $20,000 to $24,999 HOUSEHOLD INCOME (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'HHIncAIAN_30' = 'B19001C_006', # $25,000 to $29,999 HOUSEHOLD INCOME (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'HHIncAIAN_35' = 'B19001C_007', # $30,000 to $34,999 HOUSEHOLD INCOME (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'HHIncAIAN_40' = 'B19001C_008', # $35,000 to $39,999 HOUSEHOLD INCOME (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'HHIncAIAN_45' = 'B19001C_009', # $40,000 to $44,999 HOUSEHOLD INCOME (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'HHIncAIAN_50' = 'B19001C_010', # $45,000 to $49,999 HOUSEHOLD INCOME (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'HHIncAIAN_60' = 'B19001C_011', # $50,000 to $59,999 HOUSEHOLD INCOME (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'HHIncAIAN_75' = 'B19001C_012', # $60,000 to $74,999 HOUSEHOLD INCOME (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'HHIncAIAN_100' = 'B19001C_013', # $75,000 to $99,999 HOUSEHOLD INCOME (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'HHIncAIAN_125' = 'B19001C_014', # $100,000 to $124,999 HOUSEHOLD INCOME (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'HHIncAIAN_150' = 'B19001C_015', # $125,000 to $149,999 HOUSEHOLD INCOME (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'HHIncAIAN_200' = 'B19001C_016', # $150,000 to $199,999 HOUSEHOLD INCOME (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'HHIncAIAN_250' = 'B19001C_017', # $200,000 or more HOUSEHOLD INCOME (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
	'HHIncAsi_Total' = 'B19001D_001', # Total HOUSEHOLD INCOME (ASIAN ALONE HOUSEHOLDER)
	'HHIncAsi_10' = 'B19001D_002', # Less than $10,000 HOUSEHOLD INCOME (ASIAN ALONE HOUSEHOLDER)
	'HHIncAsi_15' = 'B19001D_003', # $10,000 to $14,999 HOUSEHOLD INCOME (ASIAN ALONE HOUSEHOLDER)
	'HHIncAsi_20' = 'B19001D_004', # $15,000 to $19,999 HOUSEHOLD INCOME (ASIAN ALONE HOUSEHOLDER)
	'HHIncAsi_25' = 'B19001D_005', # $20,000 to $24,999 HOUSEHOLD INCOME (ASIAN ALONE HOUSEHOLDER)
	'HHIncAsi_30' = 'B19001D_006', # $25,000 to $29,999 HOUSEHOLD INCOME (ASIAN ALONE HOUSEHOLDER)
	'HHIncAsi_35' = 'B19001D_007', # $30,000 to $34,999 HOUSEHOLD INCOME (ASIAN ALONE HOUSEHOLDER)
	'HHIncAsi_40' = 'B19001D_008', # $35,000 to $39,999 HOUSEHOLD INCOME (ASIAN ALONE HOUSEHOLDER)
	'HHIncAsi_45' = 'B19001D_009', # $40,000 to $44,999 HOUSEHOLD INCOME (ASIAN ALONE HOUSEHOLDER)
	'HHIncAsi_50' = 'B19001D_010', # $45,000 to $49,999 HOUSEHOLD INCOME (ASIAN ALONE HOUSEHOLDER)
	'HHIncAsi_60' = 'B19001D_011', # $50,000 to $59,999 HOUSEHOLD INCOME (ASIAN ALONE HOUSEHOLDER)
	'HHIncAsi_75' = 'B19001D_012', # $60,000 to $74,999 HOUSEHOLD INCOME (ASIAN ALONE HOUSEHOLDER)
	'HHIncAsi_100' = 'B19001D_013', # $75,000 to $99,999 HOUSEHOLD INCOME (ASIAN ALONE HOUSEHOLDER)
	'HHIncAsi_125' = 'B19001D_014', # $100,000 to $124,999 HOUSEHOLD INCOME (ASIAN ALONE HOUSEHOLDER)
	'HHIncAsi_150' = 'B19001D_015', # $125,000 to $149,999 HOUSEHOLD INCOME (ASIAN ALONE HOUSEHOLDER)
	'HHIncAsi_200' = 'B19001D_016', # $150,000 to $199,999 HOUSEHOLD INCOME (ASIAN ALONE HOUSEHOLDER)
	'HHIncAsi_250' = 'B19001D_017', # $200,000 or more HOUSEHOLD INCOME (ASIAN ALONE HOUSEHOLDER)
	'HHIncNHPI_Total' = 'B19001E_001', # Total HOUSEHOLD INCOME (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'HHIncNHPI_10' = 'B19001E_002', # Less than $10,000 HOUSEHOLD INCOME (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'HHIncNHPI_15' = 'B19001E_003', # $10,000 to $14,999 HOUSEHOLD INCOME (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'HHIncNHPI_20' = 'B19001E_004', # $15,000 to $19,999 HOUSEHOLD INCOME (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'HHIncNHPI_25' = 'B19001E_005', # $20,000 to $24,999 HOUSEHOLD INCOME (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'HHIncNHPI_30' = 'B19001E_006', # $25,000 to $29,999 HOUSEHOLD INCOME (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'HHIncNHPI_35' = 'B19001E_007', # $30,000 to $34,999 HOUSEHOLD INCOME (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'HHIncNHPI_40' = 'B19001E_008', # $35,000 to $39,999 HOUSEHOLD INCOME (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'HHIncNHPI_45' = 'B19001E_009', # $40,000 to $44,999 HOUSEHOLD INCOME (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'HHIncNHPI_50' = 'B19001E_010', # $45,000 to $49,999 HOUSEHOLD INCOME (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'HHIncNHPI_60' = 'B19001E_011', # $50,000 to $59,999 HOUSEHOLD INCOME (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'HHIncNHPI_75' = 'B19001E_012', # $60,000 to $74,999 HOUSEHOLD INCOME (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'HHIncNHPI_100' = 'B19001E_013', # $75,000 to $99,999 HOUSEHOLD INCOME (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'HHIncNHPI_125' = 'B19001E_014', # $100,000 to $124,999 HOUSEHOLD INCOME (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'HHIncNHPI_150' = 'B19001E_015', # $125,000 to $149,999 HOUSEHOLD INCOME (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'HHIncNHPI_200' = 'B19001E_016', # $150,000 to $199,999 HOUSEHOLD INCOME (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'HHIncNHPI_250' = 'B19001E_017', # $200,000 or more HOUSEHOLD INCOME (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
	'HHIncOth_Total' = 'B19001F_001', # Total HOUSEHOLD INCOME (SOME OTHER RACE ALONE HOUSEHOLDER)
	'HHIncOth_10' = 'B19001F_002', # Less than $10,000 HOUSEHOLD INCOME (SOME OTHER RACE ALONE HOUSEHOLDER)
	'HHIncOth_15' = 'B19001F_003', # $10,000 to $14,999 HOUSEHOLD INCOME (SOME OTHER RACE ALONE HOUSEHOLDER)
	'HHIncOth_20' = 'B19001F_004', # $15,000 to $19,999 HOUSEHOLD INCOME (SOME OTHER RACE ALONE HOUSEHOLDER)
	'HHIncOth_25' = 'B19001F_005', # $20,000 to $24,999 HOUSEHOLD INCOME (SOME OTHER RACE ALONE HOUSEHOLDER)
	'HHIncOth_30' = 'B19001F_006', # $25,000 to $29,999 HOUSEHOLD INCOME (SOME OTHER RACE ALONE HOUSEHOLDER)
	'HHIncOth_35' = 'B19001F_007', # $30,000 to $34,999 HOUSEHOLD INCOME (SOME OTHER RACE ALONE HOUSEHOLDER)
	'HHIncOth_40' = 'B19001F_008', # $35,000 to $39,999 HOUSEHOLD INCOME (SOME OTHER RACE ALONE HOUSEHOLDER)
	'HHIncOth_45' = 'B19001F_009', # $40,000 to $44,999 HOUSEHOLD INCOME (SOME OTHER RACE ALONE HOUSEHOLDER)
	'HHIncOth_50' = 'B19001F_010', # $45,000 to $49,999 HOUSEHOLD INCOME (SOME OTHER RACE ALONE HOUSEHOLDER)
	'HHIncOth_60' = 'B19001F_011', # $50,000 to $59,999 HOUSEHOLD INCOME (SOME OTHER RACE ALONE HOUSEHOLDER)
	'HHIncOth_75' = 'B19001F_012', # $60,000 to $74,999 HOUSEHOLD INCOME (SOME OTHER RACE ALONE HOUSEHOLDER)
	'HHIncOth_100' = 'B19001F_013', # $75,000 to $99,999 HOUSEHOLD INCOME (SOME OTHER RACE ALONE HOUSEHOLDER)
	'HHIncOth_125' = 'B19001F_014', # $100,000 to $124,999 HOUSEHOLD INCOME (SOME OTHER RACE ALONE HOUSEHOLDER)
	'HHIncOth_150' = 'B19001F_015', # $125,000 to $149,999 HOUSEHOLD INCOME (SOME OTHER RACE ALONE HOUSEHOLDER)
	'HHIncOth_200' = 'B19001F_016', # $150,000 to $199,999 HOUSEHOLD INCOME (SOME OTHER RACE ALONE HOUSEHOLDER)
	'HHIncOth_250' = 'B19001F_017', # $200,000 or more HOUSEHOLD INCOME (SOME OTHER RACE ALONE HOUSEHOLDER)
	'HHIncTwo_Total' = 'B19001G_001', # Total HOUSEHOLD INCOME (TWO OR MORE RACES HOUSEHOLDER)
	'HHIncTwo_10' = 'B19001G_002', # Less than $10,000 HOUSEHOLD INCOME (TWO OR MORE RACES HOUSEHOLDER)
	'HHIncTwo_15' = 'B19001G_003', # $10,000 to $14,999 HOUSEHOLD INCOME (TWO OR MORE RACES HOUSEHOLDER)
	'HHIncTwo_20' = 'B19001G_004', # $15,000 to $19,999 HOUSEHOLD INCOME (TWO OR MORE RACES HOUSEHOLDER)
	'HHIncTwo_25' = 'B19001G_005', # $20,000 to $24,999 HOUSEHOLD INCOME (TWO OR MORE RACES HOUSEHOLDER)
	'HHIncTwo_30' = 'B19001G_006', # $25,000 to $29,999 HOUSEHOLD INCOME (TWO OR MORE RACES HOUSEHOLDER)
	'HHIncTwo_35' = 'B19001G_007', # $30,000 to $34,999 HOUSEHOLD INCOME (TWO OR MORE RACES HOUSEHOLDER)
	'HHIncTwo_40' = 'B19001G_008', # $35,000 to $39,999 HOUSEHOLD INCOME (TWO OR MORE RACES HOUSEHOLDER)
	'HHIncTwo_45' = 'B19001G_009', # $40,000 to $44,999 HOUSEHOLD INCOME (TWO OR MORE RACES HOUSEHOLDER)
	'HHIncTwo_50' = 'B19001G_010', # $45,000 to $49,999 HOUSEHOLD INCOME (TWO OR MORE RACES HOUSEHOLDER)
	'HHIncTwo_60' = 'B19001G_011', # $50,000 to $59,999 HOUSEHOLD INCOME (TWO OR MORE RACES HOUSEHOLDER)
	'HHIncTwo_75' = 'B19001G_012', # $60,000 to $74,999 HOUSEHOLD INCOME (TWO OR MORE RACES HOUSEHOLDER)
	'HHIncTwo_100' = 'B19001G_013', # $75,000 to $99,999 HOUSEHOLD INCOME (TWO OR MORE RACES HOUSEHOLDER)
	'HHIncTwo_125' = 'B19001G_014', # $100,000 to $124,999 HOUSEHOLD INCOME (TWO OR MORE RACES HOUSEHOLDER)
	'HHIncTwo_150' = 'B19001G_015', # $125,000 to $149,999 HOUSEHOLD INCOME (TWO OR MORE RACES HOUSEHOLDER)
	'HHIncTwo_200' = 'B19001G_016', # $150,000 to $199,999 HOUSEHOLD INCOME (TWO OR MORE RACES HOUSEHOLDER)
	'HHIncTwo_250' = 'B19001G_017', # $200,000 or more HOUSEHOLD INCOME (TWO OR MORE RACES HOUSEHOLDER)
	'HHIncWhtNL_Total' = 'B19001H_001', # Total HOUSEHOLD INCOME (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncWhtNL_10' = 'B19001H_002', # Less than $10,000 HOUSEHOLD INCOME (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncWhtNL_15' = 'B19001H_003', # $10,000 to $14,999 HOUSEHOLD INCOME (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncWhtNL_20' = 'B19001H_004', # $15,000 to $19,999 HOUSEHOLD INCOME (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncWhtNL_25' = 'B19001H_005', # $20,000 to $24,999 HOUSEHOLD INCOME (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncWhtNL_30' = 'B19001H_006', # $25,000 to $29,999 HOUSEHOLD INCOME (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncWhtNL_35' = 'B19001H_007', # $30,000 to $34,999 HOUSEHOLD INCOME (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncWhtNL_40' = 'B19001H_008', # $35,000 to $39,999 HOUSEHOLD INCOME (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncWhtNL_45' = 'B19001H_009', # $40,000 to $44,999 HOUSEHOLD INCOME (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncWhtNL_50' = 'B19001H_010', # $45,000 to $49,999 HOUSEHOLD INCOME (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncWhtNL_60' = 'B19001H_011', # $50,000 to $59,999 HOUSEHOLD INCOME (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncWhtNL_75' = 'B19001H_012', # $60,000 to $74,999 HOUSEHOLD INCOME (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncWhtNL_100' = 'B19001H_013', # $75,000 to $99,999 HOUSEHOLD INCOME (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncWhtNL_125' = 'B19001H_014', # $100,000 to $124,999 HOUSEHOLD INCOME (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncWhtNL_150' = 'B19001H_015', # $125,000 to $149,999 HOUSEHOLD INCOME (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncWhtNL_200' = 'B19001H_016', # $150,000 to $199,999 HOUSEHOLD INCOME (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncWhtNL_250' = 'B19001H_017', # $200,000 or more HOUSEHOLD INCOME (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncLat_Total' = 'B19001I_001', # Total HOUSEHOLD INCOME (HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncLat_10' = 'B19001I_002', # Less than $10,000 HOUSEHOLD INCOME (HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncLat_15' = 'B19001I_003', # $10,000 to $14,999 HOUSEHOLD INCOME (HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncLat_20' = 'B19001I_004', # $15,000 to $19,999 HOUSEHOLD INCOME (HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncLat_25' = 'B19001I_005', # $20,000 to $24,999 HOUSEHOLD INCOME (HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncLat_30' = 'B19001I_006', # $25,000 to $29,999 HOUSEHOLD INCOME (HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncLat_35' = 'B19001I_007', # $30,000 to $34,999 HOUSEHOLD INCOME (HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncLat_40' = 'B19001I_008', # $35,000 to $39,999 HOUSEHOLD INCOME (HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncLat_45' = 'B19001I_009', # $40,000 to $44,999 HOUSEHOLD INCOME (HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncLat_50' = 'B19001I_010', # $45,000 to $49,999 HOUSEHOLD INCOME (HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncLat_60' = 'B19001I_011', # $50,000 to $59,999 HOUSEHOLD INCOME (HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncLat_75' = 'B19001I_012', # $60,000 to $74,999 HOUSEHOLD INCOME (HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncLat_100' = 'B19001I_013', # $75,000 to $99,999 HOUSEHOLD INCOME (HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncLat_125' = 'B19001I_014', # $100,000 to $124,999 HOUSEHOLD INCOME (HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncLat_150' = 'B19001I_015', # $125,000 to $149,999 HOUSEHOLD INCOME (HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncLat_200' = 'B19001I_016', # $150,000 to $199,999 HOUSEHOLD INCOME (HISPANIC OR LATINO HOUSEHOLDER)
	'HHIncLat_250' = 'B19001I_017' # $200,000 or more HOUSEHOLD INCOME (HISPANIC OR LATINO HOUSEHOLDER)
)

dechhinc_vars <-
	c(
	'HHInc_Total' = 'P052001', # Total HOUSEHOLD INCOME
	'HHInc_10' = 'P052002', # Less than $10,000 HOUSEHOLD INCOME
	'HHInc_15' = 'P052003', # $10,000 to $14,999 HOUSEHOLD INCOME
	'HHInc_20' = 'P052004', # $15,000 to $19,999 HOUSEHOLD INCOME
	'HHInc_25' = 'P052005', # $20,000 to $24,999 HOUSEHOLD INCOME
	'HHInc_30' = 'P052006', # $25,000 to $29,999 HOUSEHOLD INCOME
	'HHInc_35' = 'P052007', # $30,000 to $34,999 HOUSEHOLD INCOME
	'HHInc_40' = 'P052008', # $35,000 to $39,999 HOUSEHOLD INCOME
	'HHInc_45' = 'P052009', # $40,000 to $44,999 HOUSEHOLD INCOME
	'HHInc_50' = 'P052010', # $45,000 to $49,999 HOUSEHOLD INCOME
	'HHInc_60' = 'P052011', # $50,000 to $59,999 HOUSEHOLD INCOME
	'HHInc_75' = 'P052012', # $60,000 to $74,999 HOUSEHOLD INCOME
	'HHInc_100' = 'P052013', # $75,000 to $99,999 HOUSEHOLD INCOME
	'HHInc_125' = 'P052014', # $100,000 to $124,999 HOUSEHOLD INCOME
	'HHInc_150' = 'P052015', # $125,000 to $149,999 HOUSEHOLD INCOME
	'HHInc_200' = 'P052016', # $150,000 to $199,999 HOUSEHOLD INCOME
	'HHInc_250' = 'P052017', # $200,000 or more HOUSEHOLD INCOME
	'HHIncWht_Total' = 'P151A001', # Total: Households (White Alone Householder)
	'HHIncWht_10' = 'P151A002', # Total: Less than $10,000 (White Alone Householder)
	'HHIncWht_15' = 'P151A003', # Total: $10,000 to $14,999 (White Alone Householder)
	'HHIncWht_20' = 'P151A004', # Total: $15,000 to $19,999 (White Alone Householder)
	'HHIncWht_25' = 'P151A005', # Total: $20,000 to $24,999 (White Alone Householder)
	'HHIncWht_30' = 'P151A006', # Total: $25,000 to $29,999 (White Alone Householder)
	'HHIncWht_35' = 'P151A007', # Total: $30,000 to $34,999 (White Alone Householder)
	'HHIncWht_40' = 'P151A008', # Total: $35,000 to $39,999 (White Alone Householder)
	'HHIncWht_45' = 'P151A009', # Total: $40,000 to $44,999 (White Alone Householder)
	'HHIncWht_50' = 'P151A010', # Total: $45,000 to $49,999 (White Alone Householder)
	'HHIncWht_60' = 'P151A011', # Total: $50,000 to $59,999 (White Alone Householder)
	'HHIncWht_75' = 'P151A012', # Total: $60,000 to $74,999 (White Alone Householder)
	'HHIncWht_100' = 'P151A013', # Total: $75,000 to $99,999 (White Alone Householder)
	'HHIncWht_125' = 'P151A014', # Total: $100,000 to $124,999 (White Alone Householder)
	'HHIncWht_150' = 'P151A015', # Total: $125,000 to $149,999 (White Alone Householder)
	'HHIncWht_200' = 'P151A016', # Total: $150,000 to $199,999 (White Alone Householder)
	'HHIncWht_250' = 'P151A017', # Total: $200,000 or more (White Alone Householder)
	'HHIncBlk_Total' = 'P151B001', # Total: Households (Black or African American Alone Householder)
	'HHIncBlk_10' = 'P151B002', # Total: Less than $10,000 (Black or African American Alone Householder)
	'HHIncBlk_15' = 'P151B003', # Total: $10,000 to $14,999 (Black or African American Alone Householder)
	'HHIncBlk_20' = 'P151B004', # Total: $15,000 to $19,999 (Black or African American Alone Householder)
	'HHIncBlk_25' = 'P151B005', # Total: $20,000 to $24,999 (Black or African American Alone Householder)
	'HHIncBlk_30' = 'P151B006', # Total: $25,000 to $29,999 (Black or African American Alone Householder)
	'HHIncBlk_35' = 'P151B007', # Total: $30,000 to $34,999 (Black or African American Alone Householder)
	'HHIncBlk_40' = 'P151B008', # Total: $35,000 to $39,999 (Black or African American Alone Householder)
	'HHIncBlk_45' = 'P151B009', # Total: $40,000 to $44,999 (Black or African American Alone Householder)
	'HHIncBlk_50' = 'P151B010', # Total: $45,000 to $49,999 (Black or African American Alone Householder)
	'HHIncBlk_60' = 'P151B011', # Total: $50,000 to $59,999 (Black or African American Alone Householder)
	'HHIncBlk_75' = 'P151B012', # Total: $60,000 to $74,999 (Black or African American Alone Householder)
	'HHIncBlk_100' = 'P151B013', # Total: $75,000 to $99,999 (Black or African American Alone Householder)
	'HHIncBlk_125' = 'P151B014', # Total: $100,000 to $124,999 (Black or African American Alone Householder)
	'HHIncBlk_150' = 'P151B015', # Total: $125,000 to $149,999 (Black or African American Alone Householder)
	'HHIncBlk_200' = 'P151B016', # Total: $150,000 to $199,999 (Black or African American Alone Householder)
	'HHIncBlk_250' = 'P151B017', # Total: $200,000 or more (Black or African American Alone Householder)
	'HHIncAIAN_Total' = 'P151C001', # Total: Households (American Indian and Alaska Native Alone Householder)
	'HHIncAIAN_10' = 'P151C002', # Total: Less than $10,000 (American Indian and Alaska Native Alone Householder)
	'HHIncAIAN_15' = 'P151C003', # Total: $10,000 to $14,999 (American Indian and Alaska Native Alone Householder)
	'HHIncAIAN_20' = 'P151C004', # Total: $15,000 to $19,999 (American Indian and Alaska Native Alone Householder)
	'HHIncAIAN_25' = 'P151C005', # Total: $20,000 to $24,999 (American Indian and Alaska Native Alone Householder)
	'HHIncAIAN_30' = 'P151C006', # Total: $25,000 to $29,999 (American Indian and Alaska Native Alone Householder)
	'HHIncAIAN_35' = 'P151C007', # Total: $30,000 to $34,999 (American Indian and Alaska Native Alone Householder)
	'HHIncAIAN_40' = 'P151C008', # Total: $35,000 to $39,999 (American Indian and Alaska Native Alone Householder)
	'HHIncAIAN_45' = 'P151C009', # Total: $40,000 to $44,999 (American Indian and Alaska Native Alone Householder)
	'HHIncAIAN_50' = 'P151C010', # Total: $45,000 to $49,999 (American Indian and Alaska Native Alone Householder)
	'HHIncAIAN_60' = 'P151C011', # Total: $50,000 to $59,999 (American Indian and Alaska Native Alone Householder)
	'HHIncAIAN_75' = 'P151C012', # Total: $60,000 to $74,999 (American Indian and Alaska Native Alone Householder)
	'HHIncAIAN_100' = 'P151C013', # Total: $75,000 to $99,999 (American Indian and Alaska Native Alone Householder)
	'HHIncAIAN_125' = 'P151C014', # Total: $100,000 to $124,999 (American Indian and Alaska Native Alone Householder)
	'HHIncAIAN_150' = 'P151C015', # Total: $125,000 to $149,999 (American Indian and Alaska Native Alone Householder)
	'HHIncAIAN_200' = 'P151C016', # Total: $150,000 to $199,999 (American Indian and Alaska Native Alone Householder)
	'HHIncAIAN_250' = 'P151C017', # Total: $200,000 or more (American Indian and Alaska Native Alone Householder)
	'HHIncAsi_Total' = 'P151D001', # Total: Households (Asian Alone Householder)
	'HHIncAsi_10' = 'P151D002', # Total: Less than $10,000 (Asian Alone Householder)
	'HHIncAsi_15' = 'P151D003', # Total: $10,000 to $14,999 (Asian Alone Householder)
	'HHIncAsi_20' = 'P151D004', # Total: $15,000 to $19,999 (Asian Alone Householder)
	'HHIncAsi_25' = 'P151D005', # Total: $20,000 to $24,999 (Asian Alone Householder)
	'HHIncAsi_30' = 'P151D006', # Total: $25,000 to $29,999 (Asian Alone Householder)
	'HHIncAsi_35' = 'P151D007', # Total: $30,000 to $34,999 (Asian Alone Householder)
	'HHIncAsi_40' = 'P151D008', # Total: $35,000 to $39,999 (Asian Alone Householder)
	'HHIncAsi_45' = 'P151D009', # Total: $40,000 to $44,999 (Asian Alone Householder)
	'HHIncAsi_50' = 'P151D010', # Total: $45,000 to $49,999 (Asian Alone Householder)
	'HHIncAsi_60' = 'P151D011', # Total: $50,000 to $59,999 (Asian Alone Householder)
	'HHIncAsi_75' = 'P151D012', # Total: $60,000 to $74,999 (Asian Alone Householder)
	'HHIncAsi_100' = 'P151D013', # Total: $75,000 to $99,999 (Asian Alone Householder)
	'HHIncAsi_125' = 'P151D014', # Total: $100,000 to $124,999 (Asian Alone Householder)
	'HHIncAsi_150' = 'P151D015', # Total: $125,000 to $149,999 (Asian Alone Householder)
	'HHIncAsi_200' = 'P151D016', # Total: $150,000 to $199,999 (Asian Alone Householder)
	'HHIncAsi_250' = 'P151D017', # Total: $200,000 or more (Asian Alone Householder)
	'HHIncNHPI_Total' = 'P151E001', # Total: Households (Native Hawaiian and Other Pacific Islander Alone Householder)
	'HHIncNHPI_10' = 'P151E002', # Total: Less than $10,000 (Native Hawaiian and Other Pacific Islander Alone Householder)
	'HHIncNHPI_15' = 'P151E003', # Total: $10,000 to $14,999 (Native Hawaiian and Other Pacific Islander Alone Householder)
	'HHIncNHPI_20' = 'P151E004', # Total: $15,000 to $19,999 (Native Hawaiian and Other Pacific Islander Alone Householder)
	'HHIncNHPI_25' = 'P151E005', # Total: $20,000 to $24,999 (Native Hawaiian and Other Pacific Islander Alone Householder)
	'HHIncNHPI_30' = 'P151E006', # Total: $25,000 to $29,999 (Native Hawaiian and Other Pacific Islander Alone Householder)
	'HHIncNHPI_35' = 'P151E007', # Total: $30,000 to $34,999 (Native Hawaiian and Other Pacific Islander Alone Householder)
	'HHIncNHPI_40' = 'P151E008', # Total: $35,000 to $39,999 (Native Hawaiian and Other Pacific Islander Alone Householder)
	'HHIncNHPI_45' = 'P151E009', # Total: $40,000 to $44,999 (Native Hawaiian and Other Pacific Islander Alone Householder)
	'HHIncNHPI_50' = 'P151E010', # Total: $45,000 to $49,999 (Native Hawaiian and Other Pacific Islander Alone Householder)
	'HHIncNHPI_60' = 'P151E011', # Total: $50,000 to $59,999 (Native Hawaiian and Other Pacific Islander Alone Householder)
	'HHIncNHPI_75' = 'P151E012', # Total: $60,000 to $74,999 (Native Hawaiian and Other Pacific Islander Alone Householder)
	'HHIncNHPI_100' = 'P151E013', # Total: $75,000 to $99,999 (Native Hawaiian and Other Pacific Islander Alone Householder)
	'HHIncNHPI_125' = 'P151E014', # Total: $100,000 to $124,999 (Native Hawaiian and Other Pacific Islander Alone Householder)
	'HHIncNHPI_150' = 'P151E015', # Total: $125,000 to $149,999 (Native Hawaiian and Other Pacific Islander Alone Householder)
	'HHIncNHPI_200' = 'P151E016', # Total: $150,000 to $199,999 (Native Hawaiian and Other Pacific Islander Alone Householder)
	'HHIncNHPI_250' = 'P151E017', # Total: $200,000 or more (Native Hawaiian and Other Pacific Islander Alone Householder)
	'HHIncOth_Total' = 'P151F001', # Total: Households (Some Other Race Alone Householder)
	'HHIncOth_10' = 'P151F002', # Total: Less than $10,000 (Some Other Race Alone Householder)
	'HHIncOth_15' = 'P151F003', # Total: $10,000 to $14,999 (Some Other Race Alone Householder)
	'HHIncOth_20' = 'P151F004', # Total: $15,000 to $19,999 (Some Other Race Alone Householder)
	'HHIncOth_25' = 'P151F005', # Total: $20,000 to $24,999 (Some Other Race Alone Householder)
	'HHIncOth_30' = 'P151F006', # Total: $25,000 to $29,999 (Some Other Race Alone Householder)
	'HHIncOth_35' = 'P151F007', # Total: $30,000 to $34,999 (Some Other Race Alone Householder)
	'HHIncOth_40' = 'P151F008', # Total: $35,000 to $39,999 (Some Other Race Alone Householder)
	'HHIncOth_45' = 'P151F009', # Total: $40,000 to $44,999 (Some Other Race Alone Householder)
	'HHIncOth_50' = 'P151F010', # Total: $45,000 to $49,999 (Some Other Race Alone Householder)
	'HHIncOth_60' = 'P151F011', # Total: $50,000 to $59,999 (Some Other Race Alone Householder)
	'HHIncOth_75' = 'P151F012', # Total: $60,000 to $74,999 (Some Other Race Alone Householder)
	'HHIncOth_100' = 'P151F013', # Total: $75,000 to $99,999 (Some Other Race Alone Householder)
	'HHIncOth_125' = 'P151F014', # Total: $100,000 to $124,999 (Some Other Race Alone Householder)
	'HHIncOth_150' = 'P151F015', # Total: $125,000 to $149,999 (Some Other Race Alone Householder)
	'HHIncOth_200' = 'P151F016', # Total: $150,000 to $199,999 (Some Other Race Alone Householder)
	'HHIncOth_250' = 'P151F017', # Total: $200,000 or more (Some Other Race Alone Householder)
	'HHIncTwo_Total' = 'P151G001', # Total: Households (Two or More Races Householder)
	'HHIncTwo_10' = 'P151G002', # Total: Less than $10,000 (Two or More Races Householder)
	'HHIncTwo_15' = 'P151G003', # Total: $10,000 to $14,999 (Two or More Races Householder)
	'HHIncTwo_20' = 'P151G004', # Total: $15,000 to $19,999 (Two or More Races Householder)
	'HHIncTwo_25' = 'P151G005', # Total: $20,000 to $24,999 (Two or More Races Householder)
	'HHIncTwo_30' = 'P151G006', # Total: $25,000 to $29,999 (Two or More Races Householder)
	'HHIncTwo_35' = 'P151G007', # Total: $30,000 to $34,999 (Two or More Races Householder)
	'HHIncTwo_40' = 'P151G008', # Total: $35,000 to $39,999 (Two or More Races Householder)
	'HHIncTwo_45' = 'P151G009', # Total: $40,000 to $44,999 (Two or More Races Householder)
	'HHIncTwo_50' = 'P151G010', # Total: $45,000 to $49,999 (Two or More Races Householder)
	'HHIncTwo_60' = 'P151G011', # Total: $50,000 to $59,999 (Two or More Races Householder)
	'HHIncTwo_75' = 'P151G012', # Total: $60,000 to $74,999 (Two or More Races Householder)
	'HHIncTwo_100' = 'P151G013', # Total: $75,000 to $99,999 (Two or More Races Householder)
	'HHIncTwo_125' = 'P151G014', # Total: $100,000 to $124,999 (Two or More Races Householder)
	'HHIncTwo_150' = 'P151G015', # Total: $125,000 to $149,999 (Two or More Races Householder)
	'HHIncTwo_200' = 'P151G016', # Total: $150,000 to $199,999 (Two or More Races Householder)
	'HHIncTwo_250' = 'P151G017', # Total: $200,000 or more (Two or More Races Householder)
	'HHIncWhtNL_Total' = 'P151I001', # Total: Households (White Alone, Not Hispanic or Latino Householder)
	'HHIncWhtNL_10' = 'P151I002', # Total: Less than $10,000 (White Alone, Not Hispanic or Latino Householder)
	'HHIncWhtNL_15' = 'P151I003', # Total: $10,000 to $14,999 (White Alone, Not Hispanic or Latino Householder)
	'HHIncWhtNL_20' = 'P151I004', # Total: $15,000 to $19,999 (White Alone, Not Hispanic or Latino Householder)
	'HHIncWhtNL_25' = 'P151I005', # Total: $20,000 to $24,999 (White Alone, Not Hispanic or Latino Householder)
	'HHIncWhtNL_30' = 'P151I006', # Total: $25,000 to $29,999 (White Alone, Not Hispanic or Latino Householder)
	'HHIncWhtNL_35' = 'P151I007', # Total: $30,000 to $34,999 (White Alone, Not Hispanic or Latino Householder)
	'HHIncWhtNL_40' = 'P151I008', # Total: $35,000 to $39,999 (White Alone, Not Hispanic or Latino Householder)
	'HHIncWhtNL_45' = 'P151I009', # Total: $40,000 to $44,999 (White Alone, Not Hispanic or Latino Householder)
	'HHIncWhtNL_50' = 'P151I010', # Total: $45,000 to $49,999 (White Alone, Not Hispanic or Latino Householder)
	'HHIncWhtNL_60' = 'P151I011', # Total: $50,000 to $59,999 (White Alone, Not Hispanic or Latino Householder)
	'HHIncWhtNL_75' = 'P151I012', # Total: $60,000 to $74,999 (White Alone, Not Hispanic or Latino Householder)
	'HHIncWhtNL_100' = 'P151I013', # Total: $75,000 to $99,999 (White Alone, Not Hispanic or Latino Householder)
	'HHIncWhtNL_125' = 'P151I014', # Total: $100,000 to $124,999 (White Alone, Not Hispanic or Latino Householder)
	'HHIncWhtNL_150' = 'P151I015', # Total: $125,000 to $149,999 (White Alone, Not Hispanic or Latino Householder)
	'HHIncWhtNL_200' = 'P151I016', # Total: $150,000 to $199,999 (White Alone, Not Hispanic or Latino Householder)
	'HHIncWhtNL_250' = 'P151I017', # Total: $200,000 or more (White Alone, Not Hispanic or Latino Householder)
	'HHIncLat_Total' = 'P151H001', # Total: Households (Hispanic or Latino Householder)
	'HHIncLat_10' = 'P151H002', # Total: Less than $10,000 (Hispanic or Latino Householder)
	'HHIncLat_15' = 'P151H003', # Total: $10,000 to $14,999 (Hispanic or Latino Householder)
	'HHIncLat_20' = 'P151H004', # Total: $15,000 to $19,999 (Hispanic or Latino Householder)
	'HHIncLat_25' = 'P151H005', # Total: $20,000 to $24,999 (Hispanic or Latino Householder)
	'HHIncLat_30' = 'P151H006', # Total: $25,000 to $29,999 (Hispanic or Latino Householder)
	'HHIncLat_35' = 'P151H007', # Total: $30,000 to $34,999 (Hispanic or Latino Householder)
	'HHIncLat_40' = 'P151H008', # Total: $35,000 to $39,999 (Hispanic or Latino Householder)
	'HHIncLat_45' = 'P151H009', # Total: $40,000 to $44,999 (Hispanic or Latino Householder)
	'HHIncLat_50' = 'P151H010', # Total: $45,000 to $49,999 (Hispanic or Latino Householder)
	'HHIncLat_60' = 'P151H011', # Total: $50,000 to $59,999 (Hispanic or Latino Householder)
	'HHIncLat_75' = 'P151H012', # Total: $60,000 to $74,999 (Hispanic or Latino Householder)
	'HHIncLat_100' = 'P151H013', # Total: $75,000 to $99,999 (Hispanic or Latino Householder)
	'HHIncLat_125' = 'P151H014', # Total: $100,000 to $124,999 (Hispanic or Latino Householder)
	'HHIncLat_150' = 'P151H015', # Total: $125,000 to $149,999 (Hispanic or Latino Householder)
	'HHIncLat_200' = 'P151H016', # Total: $150,000 to $199,999 (Hispanic or Latino Householder)
	'HHIncLat_250' = 'P151H017' # Total: $200,000 or more (Hispanic or Latino Householder)
	)

# ==========================================================================
# Median Gross Rent by Year Structure Built
# ==========================================================================

mgryb_acsvars <-
	c('MGRYB_Total' = 'B25111_001',# Total MEDIAN GROSS RENT BY YEAR STRUCTURE BUILT
	'MGRYB_2014' = 'B25111_002',# 2014 or later MEDIAN GROSS RENT BY YEAR STRUCTURE BUILT
	'MGRYB_2010' = 'B25111_003',# 2010 to 2013 MEDIAN GROSS RENT BY YEAR STRUCTURE BUILT
	'MGRYB_2000' = 'B25111_004',# 2000 to 2009 MEDIAN GROSS RENT BY YEAR STRUCTURE BUILT
	'MGRYB_1990' = 'B25111_005',# 1990 to 1999 MEDIAN GROSS RENT BY YEAR STRUCTURE BUILT
	'MGRYB_1980' = 'B25111_006',# 1980 to 1989 MEDIAN GROSS RENT BY YEAR STRUCTURE BUILT
	'MGRYB_1970' = 'B25111_007',# 1970 to 1979 MEDIAN GROSS RENT BY YEAR STRUCTURE BUILT
	'MGRYB_1960' = 'B25111_008',# 1960 to 1969 MEDIAN GROSS RENT BY YEAR STRUCTURE BUILT
	'MGRYB_1950' = 'B25111_009',# 1950 to 1959 MEDIAN GROSS RENT BY YEAR STRUCTURE BUILT
	'MGRYB_1940' = 'B25111_010',# 1940 to 1949 MEDIAN GROSS RENT BY YEAR STRUCTURE BUILT
	'MGRYB_1939' = 'B25111_011')# 1939 or earlier MEDIAN GROSS RENT BY YEAR STRUCTURE BUILT

# ==========================================================================
# Tenure
# ==========================================================================

#
# Census rent
# --------------------------------------------------------------------------

acsrent_vars <-
	c(
	'totten' = 'B25003_001',
	'totown' = 'B25003_002',
	'totrent' = 'B25003_003',
	'tottenWHT' = 'B25003A_001',
	'totownWHT' = 'B25003A_002',
	'totrentWHT' = 'B25003A_003',
	'tottenBLK' = 'B25003B_001',
	'totownBLK' = 'B25003B_002',
	'totrentBLK' = 'B25003B_003',
	'tottenAIAN' = 'B25003C_001',
	'totownAIAN' = 'B25003C_002',
	'totrentAIAN' = 'B25003C_003',
	'tottenASI' = 'B25003D_001',
	'totownASI' = 'B25003D_002',
	'totrentASI' = 'B25003D_003',
	'tottenNHOP' = 'B25003E_001',
	'totownNHOP' = 'B25003E_002',
	'totrentNHOP' = 'B25003E_003',
	'tottenOTH' = 'B25003F_001',
	'totownOTH' = 'B25003F_002',
	'totrentOTH' = 'B25003F_003',
	'tottenTWO' = 'B25003G_001',
	'totownTWO' = 'B25003G_002',
	'totrentTWO' = 'B25003G_003',
	'tottenWHTNL' = 'B25003H_001',
	'totownWHTNL' = 'B25003H_002',
	'totrentWHTNL' = 'B25003H_003',
	'tottenLAT' = 'B25003I_001',
	'totownLAT' = 'B25003I_002',
	'totrentLAT' = 'B25003I_003',
	'vac_tot' = 'B25004_001',
   	'vac_rent' = 'B25004_003',
	'mgrent' = 'B25064_001'
	)

decrent_vars00 <- c(
	'totten' = 'H011001',
	'totown' = 'H011002',
	'totrent' = 'H011010',
	'totownWHT' = 'H011003',
	'totrentWHT' = 'H011011',
	'totownBLK' = 'H011004',
	'totrentBLK' = 'H011012',
	'totownAIAN' = 'H011005',
	'totrentAIAN' = 'H011013',
	'totownASI' = 'H011006',
	'totrentASI' = 'H011014',
	'totownNHOP' = 'H011007',
	'totrentNHOP' = 'H011015',
	'totownOTH' = 'H011008',
	'totrentOTH' = 'H011016',
	'totownTWO' = 'H011009',
	'totrentTWO' = 'H011017',
	'tottenWHTNL' = 'H013001',
	'totownWHTNL' = 'H013002',
	'totrentWHTNL' = 'H013003',
	'tottenLAT' = 'H012001',
	'totownLAT' = 'H012002',
	'totrentLAT' = 'H012003',
	'vac_tot' = 'H008001',
	'vac_rent' = 'H008002',
	'mgrent' = 'H063001'
	)

#
# Rent burden
# --------------------------------------------------------------------------

acsrb_vars <-
	c(
	'rb_tot' = 'B25070_001', # GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME
	'rb_10.0' = 'B25070_002', # Less than 10.0 percent GROSS RENT as % hh income
	'rb_14.9' = 'B25070_003', # 10.0 to 14.9 percent GROSS RENT as % hh income
	'rb_19.9' = 'B25070_004', # 15.0 to 19.9 percent GROSS RENT as % hh income
	'rb_24.9' = 'B25070_005', # 20.0 to 24.9 percent GROSS RENT as % hh income
	'rb_29.9' = 'B25070_006', # 25.0 to 29.9 percent GROSS RENT as % hh income
	'rb_34.9' = 'B25070_007', # 30.0 to 34.9 percent GROSS RENT as % hh income
	'rb_39.9' = 'B25070_008', # 35.0 to 39.9 percent GROSS RENT as % hh income
	'rb_49.9' = 'B25070_009', # 40.0 to 49.9 percent GROSS RENT as % hh income
	'rb_55' = 'B25070_010', # 50.0 percent or more GROSS RENT as % hh income
	'rb_nc' = 'B25070_011' # Not computed GROSS RENT as % hh income
	)

decrb_vars <-
	c(
	'rb_tot' = 'H069001', # GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME
	'rb_10.0' = 'H069002', # Less than 10.0 percent GROSS RENT as % hh income
	'rb_14.9' = 'H069003', # 10.0 to 14.9 percent GROSS RENT as % hh income
	'rb_19.9' = 'H069004', # 15.0 to 19.9 percent GROSS RENT as % hh income
	'rb_24.9' = 'H069005', # 20.0 to 24.9 percent GROSS RENT as % hh income
	'rb_29.9' = 'H069006', # 25.0 to 29.9 percent GROSS RENT as % hh income
	'rb_34.9' = 'H069007', # 30.0 to 34.9 percent GROSS RENT as % hh income
	'rb_39.9' = 'H069008', # 35.0 to 39.9 percent GROSS RENT as % hh income
	'rb_49.9' = 'H069009', # 40.0 to 49.9 percent GROSS RENT as % hh income
	'rb_55' = 'H069010', # 50.0 percent or more GROSS RENT as % hh income
	'rb_nc' = 'H069011' # Not computed GROSS RENT as % hh income
	)

#
# Number of occupied units at gross rent breakdowns
# --------------------------------------------------------------------------
tenure_varsACS <-
c(
'total' = 'B25003_001', # Estimate!!Total TENURE
'own' = 'B25003_002', # Estimate!!Total!!Owner occupied TENURE
'rent' = 'B25003_003') # Estimate!!Total!!Renter occupied TENURE


runits_acs2015_vars<-
c(
'ru_tot' = 'B25063_001',  # :: GROSS RENT
'ru_cash' = 'B25063_002',  # With cash rent :: GROSS RENT
'ru_100' = 'B25063_003',  # With cash rent!!Less than $100 :: GROSS RENT
'ru_149' = 'B25063_004',  # With cash rent!!$100 to $149 :: GROSS RENT
'ru_199' = 'B25063_005',  # With cash rent!!$150 to $199 :: GROSS RENT
'ru_249' = 'B25063_006',  # With cash rent!!$200 to $249 :: GROSS RENT
'ru_299' = 'B25063_007',  # With cash rent!!$250 to $299 :: GROSS RENT
'ru_349' = 'B25063_008',  # With cash rent!!$300 to $349 :: GROSS RENT
'ru_399' = 'B25063_009',  # With cash rent!!$350 to $399 :: GROSS RENT
'ru_449' = 'B25063_010',  # With cash rent!!$400 to $449 :: GROSS RENT
'ru_499' = 'B25063_011',  # With cash rent!!$450 to $499 :: GROSS RENT
'ru_549' = 'B25063_012',  # With cash rent!!$500 to $549 :: GROSS RENT
'ru_599' = 'B25063_013',  # With cash rent!!$550 to $599 :: GROSS RENT
'ru_649' = 'B25063_014',  # With cash rent!!$600 to $649 :: GROSS RENT
'ru_699' = 'B25063_015',  # With cash rent!!$650 to $699 :: GROSS RENT
'ru_749' = 'B25063_016',  # With cash rent!!$700 to $749 :: GROSS RENT
'ru_799' = 'B25063_017',  # With cash rent!!$750 to $799 :: GROSS RENT
'ru_899' = 'B25063_018',  # With cash rent!!$800 to $899 :: GROSS RENT
'ru_999' = 'B25063_019',  # With cash rent!!$900 to $999 :: GROSS RENT
'ru_1249' = 'B25063_020',  # With cash rent!!$1 000 to $1 249 :: GROSS RENT
'ru_1499' = 'B25063_021',  # With cash rent!!$1 250 to $1 499 :: GROSS RENT
'ru_1999' = 'B25063_022',  # With cash rent!!$1 500 to $1 999 :: GROSS RENT
'ru_2499' = 'B25063_023',  # With cash rent!!$2 000 to $2 499 :: GROSS RENT
'ru_2999' = 'B25063_024',  # With cash rent!!$2 500 to $2 999 :: GROSS RENT
'ru_3499' = 'B25063_025',  # With cash rent!!$3 000 to $3 499 :: GROSS RENT
'ru_3500' = 'B25063_026',  # With cash rent!!$3 500 or more :: GROSS RENT
'ru_nocash' = 'B25063_027'  # No cash rent :: GROSS RENT
)
runits_acs2014_vars<-
c(
'ru_tot' = 'B25063_001',  # Estimate!!Total :: GROSS RENT
'ru_cash'= 'B25063_002',  # Estimate!!Total!!With cash rent :: GROSS RENT
'ru_100' = 'B25063_003',  # Estimate!!Total!!With cash rent!!Less than $100 :: GROSS RENT
'ru_149' = 'B25063_004',  # Estimate!!Total!!With cash rent!!$100 to $149 :: GROSS RENT
'ru_199' = 'B25063_005',  # Estimate!!Total!!With cash rent!!$150 to $199 :: GROSS RENT
'ru_249' = 'B25063_006',  # Estimate!!Total!!With cash rent!!$200 to $249 :: GROSS RENT
'ru_299' = 'B25063_007',  # Estimate!!Total!!With cash rent!!$250 to $299 :: GROSS RENT
'ru_349' = 'B25063_008',  # Estimate!!Total!!With cash rent!!$300 to $349 :: GROSS RENT
'ru_399' = 'B25063_009',  # Estimate!!Total!!With cash rent!!$350 to $399 :: GROSS RENT
'ru_449' = 'B25063_010',  # Estimate!!Total!!With cash rent!!$400 to $449 :: GROSS RENT
'ru_499' = 'B25063_011',  # Estimate!!Total!!With cash rent!!$450 to $499 :: GROSS RENT
'ru_549' = 'B25063_012',  # Estimate!!Total!!With cash rent!!$500 to $549 :: GROSS RENT
'ru_599' = 'B25063_013',  # Estimate!!Total!!With cash rent!!$550 to $599 :: GROSS RENT
'ru_649' = 'B25063_014',  # Estimate!!Total!!With cash rent!!$600 to $649 :: GROSS RENT
'ru_699' = 'B25063_015',  # Estimate!!Total!!With cash rent!!$650 to $699 :: GROSS RENT
'ru_749' = 'B25063_016',  # Estimate!!Total!!With cash rent!!$700 to $749 :: GROSS RENT
'ru_799' = 'B25063_017',  # Estimate!!Total!!With cash rent!!$750 to $799 :: GROSS RENT
'ru_899' = 'B25063_018',  # Estimate!!Total!!With cash rent!!$800 to $899 :: GROSS RENT
'ru_999' = 'B25063_019',  # Estimate!!Total!!With cash rent!!$900 to $999 :: GROSS RENT
'ru_1249' = 'B25063_020',  # Estimate!!Total!!With cash rent!!$1 000 to $1 249 :: GROSS RENT
'ru_1499' = 'B25063_021',  # Estimate!!Total!!With cash rent!!$1 250 to $1 499 :: GROSS RENT
'ru_1999' = 'B25063_022',  # Estimate!!Total!!With cash rent!!$1 500 to $1 999 :: GROSS RENT
'ru_2000' = 'B25063_023',  # Estimate!!Total!!With cash rent!!$2 000 to $2 499 :: GROSS RENT
'ru_nocash' = 'B25063_024'  # Estimate!!Total!!With cash rent!!$2 500 to $2 999 :: GROSS RENT
)

runits_dec2000_vars <-
c(
'ru_tot' = 'H062001', # Total: Specified renter-occupied housing units ::H62. Gross Rent [24]
'ru_cash'= 'H062002', # Total: With cash rent: ::H62. Gross Rent [24]
'ru_100' = 'H062003', # Total: With cash rent: Less than $100 ::H62. Gross Rent [24]
'ru_149' = 'H062004', # Total: With cash rent: $100 to $149 ::H62. Gross Rent [24]
'ru_199' = 'H062005', # Total: With cash rent: $150 to $199 ::H62. Gross Rent [24]
'ru_249' = 'H062006', # Total: With cash rent: $200 to $249 ::H62. Gross Rent [24]
'ru_299' = 'H062007', # Total: With cash rent: $250 to $299 ::H62. Gross Rent [24]
'ru_349' = 'H062008', # Total: With cash rent: $300 to $349 ::H62. Gross Rent [24]
'ru_399' = 'H062009', # Total: With cash rent: $350 to $399 ::H62. Gross Rent [24]
'ru_449' = 'H062010', # Total: With cash rent: $400 to $449 ::H62. Gross Rent [24]
'ru_499' = 'H062011', # Total: With cash rent: $450 to $499 ::H62. Gross Rent [24]
'ru_549' = 'H062012', # Total: With cash rent: $500 to $549 ::H62. Gross Rent [24]
'ru_599' = 'H062013', # Total: With cash rent: $550 to $599 ::H62. Gross Rent [24]
'ru_649' = 'H062014', # Total: With cash rent: $600 to $649 ::H62. Gross Rent [24]
'ru_699' = 'H062015', # Total: With cash rent: $650 to $699 ::H62. Gross Rent [24]
'ru_749' = 'H062016', # Total: With cash rent: $700 to $749 ::H62. Gross Rent [24]
'ru_799' = 'H062017', # Total: With cash rent: $750 to $799 ::H62. Gross Rent [24]
'ru_899' = 'H062018', # Total: With cash rent: $800 to $899 ::H62. Gross Rent [24]
'ru_999' = 'H062019', # Total: With cash rent: $900 to $999 ::H62. Gross Rent [24]
'ru_1249' = 'H062020', # Total: With cash rent: $1,000 to $1,249 ::H62. Gross Rent [24]
'ru_1499' = 'H062021', # Total: With cash rent: $1,250 to $1,499 ::H62. Gross Rent [24]
'ru_1999' = 'H062022', # Total: With cash rent: $1,500 to $1,999 ::H62. Gross Rent [24]
'ru_2000' = 'H062023', # Total: With cash rent: $2,000 or more ::H62. Gross Rent [24]
'ru_nocash' = 'H062024' # Total: No cash rent ::H62. Gross Rent [24]
)

#
# HCT11. Tenure by Household Income in 1999
# --------------------------------------------------------------------------

acs_hhincten <-
	c(
	'HHIncTen_Total' = 'B25118_001', # Total
	'HHIncTenOwn' = 'B25118_002', # Owner occupied
	'HHIncTenOwn_5' = 'B25118_003', # Owner occupied!!Less than $5,000
	'HHIncTenOwn_10' = 'B25118_004', # Owner occupied!!$5,000 to $9,999
	'HHIncTenOwn_15' = 'B25118_005', # Owner occupied!!$10,000 to $14,999
	'HHIncTenOwn_20' = 'B25118_006', # Owner occupied!!$15,000 to $19,999
	'HHIncTenOwn_25' = 'B25118_007', # Owner occupied!!$20,000 to $24,999
	'HHIncTenOwn_35' = 'B25118_008', # Owner occupied!!$25,000 to $34,999
	'HHIncTenOwn_50' = 'B25118_009', # Owner occupied!!$35,000 to $49,999
	'HHIncTenOwn_75' = 'B25118_010', # Owner occupied!!$50,000 to $74,999
	'HHIncTenOwn_100' = 'B25118_011', # Owner occupied!!$75,000 to $99,999
	'HHIncTenOwn_150' = 'B25118_012', # Owner occupied!!$100,000 to $149,999
	'HHIncTenOwn_151' = 'B25118_013', # Owner occupied!!$150,000 or more
	'HHIncTenRent' = 'B25118_014', # Renter occupied
	'HHIncTenRent_5' = 'B25118_015', # Renter occupied!!Less than $5,000
	'HHIncTenRent_10' = 'B25118_016', # Renter occupied!!$5,000 to $9,999
	'HHIncTenRent_15' = 'B25118_017', # Renter occupied!!$10,000 to $14,999
	'HHIncTenRent_20' = 'B25118_018', # Renter occupied!!$15,000 to $19,999
	'HHIncTenRent_25' = 'B25118_019', # Renter occupied!!$20,000 to $24,999
	'HHIncTenRent_35' = 'B25118_020', # Renter occupied!!$25,000 to $34,999
	'HHIncTenRent_50' = 'B25118_021', # Renter occupied!!$35,000 to $49,999
	'HHIncTenRent_75' = 'B25118_022', # Renter occupied!!$50,000 to $74,999
	'HHIncTenRent_100' = 'B25118_023', # Renter occupied!!$75,000 to $99,999
	'HHIncTenRent_150' = 'B25118_024', # Renter occupied!!$100,000 to $149,999
	'HHIncTenRent_151' = 'B25118_025' # Renter occupied!!$150,000 or more
	)

dec00_hhincten <-
	c(
	'HHIncTen_Total' = 'HCT011001', # Occupied housing unitsk
	'HHIncTenOwn' = 'HCT011002', # Owner occupied:
	'HHIncTenOwn_5' = 'HCT011003', # Owner occupied: Less than $5,000
	'HHIncTenOwn_10' = 'HCT011004', # Owner occupied: $5,000 to $9,999
	'HHIncTenOwn_15' = 'HCT011005', # Owner occupied: $10,000 to $14,999
	'HHIncTenOwn_20' = 'HCT011006', # Owner occupied: $15,000 to $19,999
	'HHIncTenOwn_25' = 'HCT011007', # Owner occupied: $20,000 to $24,999
	'HHIncTenOwn_35' = 'HCT011008', # Owner occupied: $25,000 to $34,999
	'HHIncTenOwn_50' = 'HCT011009', # Owner occupied: $35,000 to $49,999
	'HHIncTenOwn_75' = 'HCT011010', # Owner occupied: $50,000 to $74,999
	'HHIncTenOwn_100' = 'HCT011011', # Owner occupied: $75,000 to $99,999
	'HHIncTenOwn_150' = 'HCT011012', # Owner occupied: $100,000 to $149,999
	'HHIncTenOwn_151' = 'HCT011013', # Owner occupied: $150,000 or more
	'HHIncTenRent' = 'HCT011014', # Renter occupied:
	'HHIncTenRent_5' = 'HCT011015', # Renter occupied: Less than $5,000
	'HHIncTenRent_10' = 'HCT011016', # Renter occupied: $5,000 to $9,999
	'HHIncTenRent_15' = 'HCT011017', # Renter occupied: $10,000 to $14,999
	'HHIncTenRent_20' = 'HCT011018', # Renter occupied: $15,000 to $19,999
	'HHIncTenRent_25' = 'HCT011019', # Renter occupied: $20,000 to $24,999
	'HHIncTenRent_35' = 'HCT011020', # Renter occupied: $25,000 to $34,999
	'HHIncTenRent_50' = 'HCT011021', # Renter occupied: $35,000 to $49,999
	'HHIncTenRent_75' = 'HCT011022', # Renter occupied: $50,000 to $74,999
	'HHIncTenRent_100' = 'HCT011023', # Renter occupied: $75,000 to $99,999
	'HHIncTenRent_150' = 'HCT011024', # Renter occupied: $100,000 to $149,999
	'HHIncTenRent_151' = 'HCT011025' # Renter occupied: $150,000 or more
	)


# ==========================================================================
# Individuals by age, sex, and race
# ==========================================================================

age <- c('Total' = 'B01001_001E',
'Male' = 'B01001_002E',
'M_18_19' = 'B01001_007E',
'M_20' = 'B01001_008E',
'M_21' = 'B01001_009E',
'M_22_24' = 'B01001_010E',
'M_25_29' = 'B01001_011E',
'M_30_34' = 'B01001_012E',
'M_35_39' = 'B01001_013E',
'M_40_44' = 'B01001_014E',
'M_45_49' = 'B01001_015E',
'M_50_54' = 'B01001_016E',
'M_55_59' = 'B01001_017E',
'M_60_61' = 'B01001_018E',
'M_62_64' = 'B01001_019E',
'M_65_66' = 'B01001_020E',
'M_67_69' = 'B01001_021E',
'M_70_74' = 'B01001_022E',
'M_75_79' = 'B01001_023E',
'M_80_84' = 'B01001_024E',
'M_85_over' = 'B01001_025E',
'Female' = 'B01001_026E',
'F_18_19' = 'B01001_031E',
'F_20' = 'B01001_032E',
'F_21' = 'B01001_033E',
'F_22_24' = 'B01001_034E',
'F_25_29' = 'B01001_035E',
'F_30_34' = 'B01001_036E',
'F_35_39' = 'B01001_037E',
'F_40_44' = 'B01001_038E',
'F_45_49' = 'B01001_039E',
'F_50_54' = 'B01001_040E',
'F_55_59' = 'B01001_041E',
'F_60_61' = 'B01001_042E',
'F_62_64' = 'B01001_043E',
'F_65_66' = 'B01001_044E',
'F_67_69' = 'B01001_045E',
'F_70_74' = 'B01001_046E',
'F_75_79' = 'B01001_047E',
'F_80_84' = 'B01001_048E',
'F_85_over' = 'B01001_049E',
'Wh_Total' = 'B01001A_001E',
'Wh_Male' = 'B01001A_002E',
'Wh_M_18_19' = 'B01001A_007E',
'Wh_M_20_24' = 'B01001A_008E',
'Wh_M_25_29' = 'B01001A_009E',
'Wh_M_30_34' = 'B01001A_010E',
'Wh_M_35_44' = 'B01001A_011E',
'Wh_M_45_54' = 'B01001A_012E',
'Wh_M_55_64' = 'B01001A_013E',
'Wh_M_65_74' = 'B01001A_014E',
'Wh_M_75_84' = 'B01001A_015E',
'Wh_M_85_over' = 'B01001A_016E',
'Wh_Female' = 'B01001A_017E',
'Wh_F_18_19' = 'B01001A_022E',
'Wh_F_20_24' = 'B01001A_023E',
'Wh_F_25_29' = 'B01001A_024E',
'Wh_F_30_34' = 'B01001A_025E',
'Wh_F_35_44' = 'B01001A_026E',
'Wh_F_45_54' = 'B01001A_027E',
'Wh_F_55_64' = 'B01001A_028E',
'Wh_F_65_74' = 'B01001A_029E',
'Wh_F_75_84' = 'B01001A_030E',
'Wh_F_85_over' = 'B01001A_031E',
'Bl_Total' = 'B01001B_001E',
'Bl_Male' = 'B01001B_002E',
'Bl_M_18_19' = 'B01001B_007E',
'Bl_M_20_24' = 'B01001B_008E',
'Bl_M_25_29' = 'B01001B_009E',
'Bl_M_30_34' = 'B01001B_010E',
'Bl_M_35_44' = 'B01001B_011E',
'Bl_M_45_54' = 'B01001B_012E',
'Bl_M_55_64' = 'B01001B_013E',
'Bl_M_65_74' = 'B01001B_014E',
'Bl_M_75_84' = 'B01001B_015E',
'Bl_M_85_over' = 'B01001B_016E',
'Bl_Female' = 'B01001B_017E',
'Bl_F_18_19' = 'B01001B_022E',
'Bl_F_20_24' = 'B01001B_023E',
'Bl_F_25_29' = 'B01001B_024E',
'Bl_F_30_34' = 'B01001B_025E',
'Bl_F_35_44' = 'B01001B_026E',
'Bl_F_45_54' = 'B01001B_027E',
'Bl_F_55_64' = 'B01001B_028E',
'Bl_F_65_74' = 'B01001B_029E',
'Bl_F_75_84' = 'B01001B_030E',
'Bl_F_85_over' = 'B01001B_031E',
'AIAN_Total' = 'B01001C_001E',
'AIAN_Male' = 'B01001C_002E',
'AIAN_M_18_19' = 'B01001C_007E',
'AIAN_M_20_24' = 'B01001C_008E',
'AIAN_M_25_29' = 'B01001C_009E',
'AIAN_M_30_34' = 'B01001C_010E',
'AIAN_M_35_44' = 'B01001C_011E',
'AIAN_M_45_54' = 'B01001C_012E',
'AIAN_M_55_64' = 'B01001C_013E',
'AIAN_M_65_74' = 'B01001C_014E',
'AIAN_M_75_84' = 'B01001C_015E',
'AIAN_M_85_over' = 'B01001C_016E',
'AIAN_Female' = 'B01001C_017E',
'AIAN_F_18_19' = 'B01001C_022E',
'AIAN_F_20_24' = 'B01001C_023E',
'AIAN_F_25_29' = 'B01001C_024E',
'AIAN_F_30_34' = 'B01001C_025E',
'AIAN_F_35_44' = 'B01001C_026E',
'AIAN_F_45_54' = 'B01001C_027E',
'AIAN_F_55_64' = 'B01001C_028E',
'AIAN_F_65_74' = 'B01001C_029E',
'AIAN_F_75_84' = 'B01001C_030E',
'AIAN_F_85_over' = 'B01001C_031E',
'Asi_Total' = 'B01001D_001E',
'Asi_Male' = 'B01001D_002E',
'Asi_M_18_19' = 'B01001D_007E',
'Asi_M_20_24' = 'B01001D_008E',
'Asi_M_25_29' = 'B01001D_009E',
'Asi_M_30_34' = 'B01001D_010E',
'Asi_M_35_44' = 'B01001D_011E',
'Asi_M_45_54' = 'B01001D_012E',
'Asi_M_55_64' = 'B01001D_013E',
'Asi_M_65_74' = 'B01001D_014E',
'Asi_M_75_84' = 'B01001D_015E',
'Asi_M_85_over' = 'B01001D_016E',
'Asi_Female' = 'B01001D_017E',
'Asi_F_18_19' = 'B01001D_022E',
'Asi_F_20_24' = 'B01001D_023E',
'Asi_F_25_29' = 'B01001D_024E',
'Asi_F_30_34' = 'B01001D_025E',
'Asi_F_35_44' = 'B01001D_026E',
'Asi_F_45_54' = 'B01001D_027E',
'Asi_F_55_64' = 'B01001D_028E',
'Asi_F_65_74' = 'B01001D_029E',
'Asi_F_75_84' = 'B01001D_030E',
'Asi_F_85_over' = 'B01001D_031E',
'NHPI_Total' = 'B01001E_001E',
'NHPI_Male' = 'B01001E_002E',
'NHPI_M_18_19' = 'B01001E_007E',
'NHPI_M_20_24' = 'B01001E_008E',
'NHPI_M_25_29' = 'B01001E_009E',
'NHPI_M_30_34' = 'B01001E_010E',
'NHPI_M_35_44' = 'B01001E_011E',
'NHPI_M_45_54' = 'B01001E_012E',
'NHPI_M_55_64' = 'B01001E_013E',
'NHPI_M_65_74' = 'B01001E_014E',
'NHPI_M_75_84' = 'B01001E_015E',
'NHPI_M_85_over' = 'B01001E_016E',
'NHPI_Female' = 'B01001E_017E',
'NHPI_F_18_19' = 'B01001E_022E',
'NHPI_F_20_24' = 'B01001E_023E',
'NHPI_F_25_29' = 'B01001E_024E',
'NHPI_F_30_34' = 'B01001E_025E',
'NHPI_F_35_44' = 'B01001E_026E',
'NHPI_F_45_54' = 'B01001E_027E',
'NHPI_F_55_64' = 'B01001E_028E',
'NHPI_F_65_74' = 'B01001E_029E',
'NHPI_F_75_84' = 'B01001E_030E',
'NHPI_F_85_over' = 'B01001E_031E',
'Oth_Total' = 'B01001F_001E',
'Oth_Male' = 'B01001F_002E',
'Oth_M_18_19' = 'B01001F_007E',
'Oth_M_20_24' = 'B01001F_008E',
'Oth_M_25_29' = 'B01001F_009E',
'Oth_M_30_34' = 'B01001F_010E',
'Oth_M_35_44' = 'B01001F_011E',
'Oth_M_45_54' = 'B01001F_012E',
'Oth_M_55_64' = 'B01001F_013E',
'Oth_M_65_74' = 'B01001F_014E',
'Oth_M_75_84' = 'B01001F_015E',
'Oth_M_85_over' = 'B01001F_016E',
'Oth_Female' = 'B01001F_017E',
'Oth_F_18_19' = 'B01001F_022E',
'Oth_F_20_24' = 'B01001F_023E',
'Oth_F_25_29' = 'B01001F_024E',
'Oth_F_30_34' = 'B01001F_025E',
'Oth_F_35_44' = 'B01001F_026E',
'Oth_F_45_54' = 'B01001F_027E',
'Oth_F_55_64' = 'B01001F_028E',
'Oth_F_65_74' = 'B01001F_029E',
'Oth_F_75_84' = 'B01001F_030E',
'Oth_F_85_over' = 'B01001F_031E',
'Two_Total' = 'B01001G_001E',
'Two_Male' = 'B01001G_002E',
'Two_M_18_19' = 'B01001G_007E',
'Two_M_20_24' = 'B01001G_008E',
'Two_M_25_29' = 'B01001G_009E',
'Two_M_30_34' = 'B01001G_010E',
'Two_M_35_44' = 'B01001G_011E',
'Two_M_45_54' = 'B01001G_012E',
'Two_M_55_64' = 'B01001G_013E',
'Two_M_65_74' = 'B01001G_014E',
'Two_M_75_84' = 'B01001G_015E',
'Two_M_85_over' = 'B01001G_016E',
'Two_Female' = 'B01001G_017E',
'Two_F_18_19' = 'B01001G_022E',
'Two_F_20_24' = 'B01001G_023E',
'Two_F_25_29' = 'B01001G_024E',
'Two_F_30_34' = 'B01001G_025E',
'Two_F_35_44' = 'B01001G_026E',
'Two_F_45_54' = 'B01001G_027E',
'Two_F_55_64' = 'B01001G_028E',
'Two_F_65_74' = 'B01001G_029E',
'Two_F_75_84' = 'B01001G_030E',
'Two_F_85_over' = 'B01001G_031E',
'Wh_Lat_Total' = 'B01001H_001E',
'Wh_Lat_Male' = 'B01001H_002E',
'Wh_Lat_M_18_19' = 'B01001H_007E',
'Wh_Lat_M_20_24' = 'B01001H_008E',
'Wh_Lat_M_25_29' = 'B01001H_009E',
'Wh_Lat_M_30_34' = 'B01001H_010E',
'Wh_Lat_M_35_44' = 'B01001H_011E',
'Wh_Lat_M_45_54' = 'B01001H_012E',
'Wh_Lat_M_55_64' = 'B01001H_013E',
'Wh_Lat_M_65_74' = 'B01001H_014E',
'Wh_Lat_M_75_84' = 'B01001H_015E',
'Wh_Lat_M_85_over' = 'B01001H_016E',
'Wh_Lat_Female' = 'B01001H_017E',
'Wh_Lat_F_18_19' = 'B01001H_022E',
'Wh_Lat_F_20_24' = 'B01001H_023E',
'Wh_Lat_F_25_29' = 'B01001H_024E',
'Wh_Lat_F_30_34' = 'B01001H_025E',
'Wh_Lat_F_35_44' = 'B01001H_026E',
'Wh_Lat_F_45_54' = 'B01001H_027E',
'Wh_Lat_F_55_64' = 'B01001H_028E',
'Wh_Lat_F_65_74' = 'B01001H_029E',
'Wh_Lat_F_75_84' = 'B01001H_030E',
'Wh_Lat_F_85_over' = 'B01001H_031E',
'Lat_Total' = 'B01001I_001E',
'Lat_Male' = 'B01001I_002E',
'Lat_M_18_19' = 'B01001I_007E',
'Lat_M_20_24' = 'B01001I_008E',
'Lat_M_25_29' = 'B01001I_009E',
'Lat_M_30_34' = 'B01001I_010E',
'Lat_M_35_44' = 'B01001I_011E',
'Lat_M_45_54' = 'B01001I_012E',
'Lat_M_55_64' = 'B01001I_013E',
'Lat_M_65_74' = 'B01001I_014E',
'Lat_M_75_84' = 'B01001I_015E',
'Lat_M_85_over' = 'B01001I_016E',
'Lat_Female' = 'B01001I_017E',
'Lat_F_18_19' = 'B01001I_022E',
'Lat_F_20_24' = 'B01001I_023E',
'Lat_F_25_29' = 'B01001I_024E',
'Lat_F_30_34' = 'B01001I_025E',
'Lat_F_35_44' = 'B01001I_026E',
'Lat_F_45_54' = 'B01001I_027E',
'Lat_F_55_64' = 'B01001I_028E',
'Lat_F_65_74' = 'B01001I_029E',
'Lat_F_75_84' = 'B01001I_030E',
'Lat_F_85_over' = 'B01001I_031E')


# ==========================================================================
# Gentrification variables
# ==========================================================================

# ==========================================================================
# 2012 and onward
# ==========================================================================

gen_vars12 <-
	c(
	'totrace' = 'B03002_001',
	'White' = 'B03002_003',
	'Black' = 'B03002_004',
	'Asian' = 'B03002_006',
	'Latinx' = 'B03002_012',
	'totwelf' = 'B19057_001', #
	'welf' = 'B19057_002',
	'totpov' = 'B17017_001', #
	'povfamh' = 'B17017_003',
	'povnonfamh' = 'B17017_020',
	'totunemp' = 'B23025_001',
	'unemp' = 'B23025_005',
	'totfemhh' = 'B09019_001',
	'femfamhh' = 'B09019_006',
	'femnonfamhh' = 'B09019_029',
	'totage' = 'B01001_001',
	'Munder5' = 'B01001_003',
	'Mfiveto9' = 'B01001_004',
	'Mtento14' = 'B01001_005',
	'Mfiftto17' = 'B01001_006',
	'Funder5' = 'B01001_027',
	'Ffiveto9' = 'B01001_028',
	'Ftento14' = 'B01001_029',
	'Ffiftto17' = 'B01001_030',
	'Mel1' = 'B01001_020', # Male!!65 and 66 years
	'Mel2' = 'B01001_021', # Male!!67 to 69 years
	'Mel3' = 'B01001_022', # Male!!70 to 74 years
	'Mel4' = 'B01001_023', # Male!!75 to 79 years
	'Mel5' = 'B01001_024', # Male!!80 to 84 years
	'Mel6' = 'B01001_025', # Male!!85 years and over
	'Fel1' = 'B01001_044', # Female!!65 and 66 years
	'Fel2' = 'B01001_045', # Female!!67 to 69 years
	'Fel3' = 'B01001_046', # Female!!70 to 74 years
	'Fel4' = 'B01001_047', # Female!!75 to 79 years
	'Fel5' = 'B01001_048', # Female!!80 to 84 years
	'Fel6' = 'B01001_049', # Female!!85 years and over
	'toted' = 'B15003_001',
	'bach' = 'B15003_022',
	'mas' = 'B15003_023',
	'pro' = 'B15003_024',
	'doc' = 'B15003_025',
	'medhhinc' = 'B19013_001',
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
	'mgrent' = 'B25064_001',
	'totoccupation' = 'C24010_001',
	'Mmgr' = 'C24010_003',
	'Fmgr' = 'C24010_039',
	'totfhc' = 'B11005_001',
	'femfamheadch' = 'B11005_007',
	'femnonfamheadch' = 'B11005_010',
	'totschool' = 'B14002_001',
	'pschlM1' = 'B14002_006',
	'pschlM2' = 'B14002_009',
	'pschlM3' = 'B14002_012',
	'pschlM4' = 'B14002_015',
	'pschlM5' = 'B14002_018',
	'pschlF1' = 'B14002_030',
	'pschlF2' = 'B14002_033',
	'pschlF3' = 'B14002_036',
	'pschlF4' = 'B14002_039',
	'pschlF5' = 'B14002_042'
	)


# ==========================================================================
# 2011 and less
# ==========================================================================
gen_vars11 <-
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
	'totunemp' = 'B23025_001E', #
	'unemp' = 'B23025_005E',
	'totfemhh' = 'B09016_001E',
	'femfamhh' = 'B09016_006E',
	'femnonfamhh' = 'B09016_024E',
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
	'toted' = 'B15002_001E',
	'bachMal' = 'B15002_015E',
	'masMal' = 'B15002_016E',
	'proMal' = 'B15002_017E',
	'docMal' = 'B15002_018E',
	'bachFem' = 'B15002_032E',
	'masFem' = 'B15002_033E',
	'proFem' = 'B15002_034E',
	'docFem' = 'B15002_035E',
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


#
# Displacement Measure
# --------------------------------------------------------------------------

dis_var <- c(
# income
	'mhhinc' = 'B19013_001',
	'mhhinc_wht' = 'B19013A_001',
	'mhhinc_blk' = 'B19013B_001',
	'mhhinc_aian' = 'B19013C_001',
	'mhhinc_asi' = 'B19013D_001',
	'mhhinc_nhop' = 'B19013E_001',
	'mhhinc_oth' = 'B19013F_001',
	'mhhinc_two' = 'B19013G_001',
	'mhhinc_whtnl' = 'B19013H_001',
	'mhhinc_lat' = 'B19013I_001',
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
	'HHIncTen_Total' = 'B25118_001', # Total
	'HHIncTenOwn' = 'B25118_002', # Owner occupied
	'HHIncTenOwn_5' = 'B25118_003', # Owner occupied!!Less than $5,000
	'HHIncTenOwn_10' = 'B25118_004', # Owner occupied!!$5,000 to $9,999
	'HHIncTenOwn_15' = 'B25118_005', # Owner occupied!!$10,000 to $14,999
	'HHIncTenOwn_20' = 'B25118_006', # Owner occupied!!$15,000 to $19,999
	'HHIncTenOwn_25' = 'B25118_007', # Owner occupied!!$20,000 to $24,999
	'HHIncTenOwn_35' = 'B25118_008', # Owner occupied!!$25,000 to $34,999
	'HHIncTenOwn_50' = 'B25118_009', # Owner occupied!!$35,000 to $49,999
	'HHIncTenOwn_75' = 'B25118_010', # Owner occupied!!$50,000 to $74,999
	'HHIncTenOwn_100' = 'B25118_011', # Owner occupied!!$75,000 to $99,999
	'HHIncTenOwn_150' = 'B25118_012', # Owner occupied!!$100,000 to $149,999
	'HHIncTenOwn_151' = 'B25118_013', # Owner occupied!!$150,000 or more
	'HHIncTenRent' = 'B25118_014', # Renter occupied
	'HHIncTenRent_5' = 'B25118_015', # Renter occupied!!Less than $5,000
	'HHIncTenRent_10' = 'B25118_016', # Renter occupied!!$5,000 to $9,999
	'HHIncTenRent_15' = 'B25118_017', # Renter occupied!!$10,000 to $14,999
	'HHIncTenRent_20' = 'B25118_018', # Renter occupied!!$15,000 to $19,999
	'HHIncTenRent_25' = 'B25118_019', # Renter occupied!!$20,000 to $24,999
	'HHIncTenRent_35' = 'B25118_020', # Renter occupied!!$25,000 to $34,999
	'HHIncTenRent_50' = 'B25118_021', # Renter occupied!!$35,000 to $49,999
	'HHIncTenRent_75' = 'B25118_022', # Renter occupied!!$50,000 to $74,999
	'HHIncTenRent_100' = 'B25118_023', # Renter occupied!!$75,000 to $99,999
	'HHIncTenRent_150' = 'B25118_024', # Renter occupied!!$100,000 to $149,999
	'HHIncTenRent_151' = 'B25118_025', # Renter occupied!!$150,000 or more
	'rb_tot' = 'B25070_001', # GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME
	'rb_10.0' = 'B25070_002', # Less than 10.0 percent GROSS RENT as % hh income
	'rb_14.9' = 'B25070_003', # 10.0 to 14.9 percent GROSS RENT as % hh income
	'rb_19.9' = 'B25070_004', # 15.0 to 19.9 percent GROSS RENT as % hh income
	'rb_24.9' = 'B25070_005', # 20.0 to 24.9 percent GROSS RENT as % hh income
	'rb_29.9' = 'B25070_006', # 25.0 to 29.9 percent GROSS RENT as % hh income
	'rb_34.9' = 'B25070_007', # 30.0 to 34.9 percent GROSS RENT as % hh income
	'rb_39.9' = 'B25070_008', # 35.0 to 39.9 percent GROSS RENT as % hh income
	'rb_49.9' = 'B25070_009', # 40.0 to 49.9 percent GROSS RENT as % hh income
	'rb_55' = 'B25070_010', # 50.0 percent or more GROSS RENT as % hh income
	'rb_nc' = 'B25070_011', # Not computed GROSS RENT as % hh income
# race
	'totrace' = 'B03002_001',
	'White' = 'B03002_003',
	'Black' = 'B03002_004',
	'Asian' = 'B03002_006',
	'Latinx' = 'B03002_012',
# disadvantage
	'totwelf' = 'B19057_001', #
	'welf' = 'B19057_002',
	'totpov' = 'B17017_001', #
	'povfamh' = 'B17017_003',
	'povnonfamh' = 'B17017_020',
	'totunemp' = 'B23025_001',
	'unemp' = 'B23025_005',
	'totfemhh' = 'B09019_001',
	'femfamhh' = 'B09019_006',
	'femnonfamhh' = 'B09019_029',
	'totfhc' = 'B11005_001',
	'femfamheadch' = 'B11005_007',
	'femnonfamheadch' = 'B11005_010',
# age
	'totage' = 'B01001_001',
	'Munder5' = 'B01001_003',
	'Mfiveto9' = 'B01001_004',
	'Mtento14' = 'B01001_005',
	'Mfiftto17' = 'B01001_006',
	'Funder5' = 'B01001_027',
	'Ffiveto9' = 'B01001_028',
	'Ftento14' = 'B01001_029',
	'Ffiftto17' = 'B01001_030',
	'Mel1' = 'B01001_020', # Male!!65 and 66 years
	'Mel2' = 'B01001_021', # Male!!67 to 69 years
	'Mel3' = 'B01001_022', # Male!!70 to 74 years
	'Mel4' = 'B01001_023', # Male!!75 to 79 years
	'Mel5' = 'B01001_024', # Male!!80 to 84 years
	'Mel6' = 'B01001_025', # Male!!85 years and over
	'Fel1' = 'B01001_044', # Female!!65 and 66 years
	'Fel2' = 'B01001_045', # Female!!67 to 69 years
	'Fel3' = 'B01001_046', # Female!!70 to 74 years
	'Fel4' = 'B01001_047', # Female!!75 to 79 years
	'Fel5' = 'B01001_048', # Female!!80 to 84 years
	'Fel6' = 'B01001_049', # Female!!85 years and over
# eeducation
	'toted' = 'B15003_001',
	'bach' = 'B15003_022',
	'mas' = 'B15003_023',
	'pro' = 'B15003_024',
	'doc' = 'B15003_025',
	'totenroll' = 'B14007_001', # Estimate!!Total	SCHOOL ENROLLMENT BY DETAILED  LEVEL OF SCHOOL FOR THE POPULATION 3 YEARS AND OVER
	'colenroll' = 'B14007_017', # Estimate!!Total!!Enrolled in school!!Enrolled in college undergraduate years	SCHOOL ENROLLMENT BY DETAILED  LEVEL OF SCHOOL FOR THE POPULATION 3 YEARS AND OVER
	'proenroll' = 'B14007_018', # Estimate!!Total!!Enrolled in school!!Graduate or professional school	SCHOOL ENROLLMENT BY DETAILED  LEVEL OF SCHOOL FOR THE POPULATION 3 YEARS AND OVER

# rent
	'medrent' = 'B25064_001', #MEDIAN GROSS RENT (DOLLARS)
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
	'totrentLAT' = 'B25003I_003' # Estimate!!Total!!Renter occupied TENURE (HISPANIC OR LATINO HOUSEHOLDER)
	)

# ==========================================================================
# Income by rent burden
# ==========================================================================

ir_vars <- c(
ir_tot = 'B25074_001',# Estimate!!Total	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir_9999 = 'B25074_002', # Estimate!!Total!!Less than $10 000	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.19_9999 = 'B25074_003', # Estimate!!Total!!Less than $10 000!!Less than 20.0 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.249_9999 = 'B25074_004', # Estimate!!Total!!Less than $10 000!!20.0 to 24.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.299_9999 = 'B25074_005', # Estimate!!Total!!Less than $10 000!!25.0 to 29.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.349_9999 = 'B25074_006', # Estimate!!Total!!Less than $10 000!!30.0 to 34.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.399_9999 = 'B25074_007', # Estimate!!Total!!Less than $10 000!!35.0 to 39.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.499_9999 = 'B25074_008', # Estimate!!Total!!Less than $10 000!!40.0 to 49.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.5plus_9999 = 'B25074_009', # Estimate!!Total!!Less than $10 000!!50.0 percent or more	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.x_19999 = 'B25074_010', # Estimate!!Total!!Less than $10 000!!Not computed	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir_19999 = 'B25074_011', # Estimate!!Total!!$10 000 to $19 999	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.19_19999 = 'B25074_012', # Estimate!!Total!!$10 000 to $19 999!!Less than 20.0 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.249_19999 = 'B25074_013', # Estimate!!Total!!$10 000 to $19 999!!20.0 to 24.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.299_19999 = 'B25074_014', # Estimate!!Total!!$10 000 to $19 999!!25.0 to 29.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.349_19999 = 'B25074_015', # Estimate!!Total!!$10 000 to $19 999!!30.0 to 34.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.399_19999 = 'B25074_016', # Estimate!!Total!!$10 000 to $19 999!!35.0 to 39.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.499_19999 = 'B25074_017', # Estimate!!Total!!$10 000 to $19 999!!40.0 to 49.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.5plus_19999 = 'B25074_018', # Estimate!!Total!!$10 000 to $19 999!!50.0 percent or more	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.x_19999 = 'B25074_019', # Estimate!!Total!!$10 000 to $19 999!!Not computed	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir_34999 = 'B25074_020', # Estimate!!Total!!$20 000 to $34 999	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.19_34999 = 'B25074_021', # Estimate!!Total!!$20 000 to $34 999!!Less than 20.0 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.249_34999 = 'B25074_022', # Estimate!!Total!!$20 000 to $34 999!!20.0 to 24.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.299_34999 = 'B25074_023', # Estimate!!Total!!$20 000 to $34 999!!25.0 to 29.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.349_34999 = 'B25074_024', # Estimate!!Total!!$20 000 to $34 999!!30.0 to 34.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.399_34999 = 'B25074_025', # Estimate!!Total!!$20 000 to $34 999!!35.0 to 39.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.499_34999 = 'B25074_026', # Estimate!!Total!!$20 000 to $34 999!!40.0 to 49.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.5plus_34999 = 'B25074_027', # Estimate!!Total!!$20 000 to $34 999!!50.0 percent or more	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.x_34999 = 'B25074_028', # Estimate!!Total!!$20 000 to $34 999!!Not computed	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir_49999 = 'B25074_029', # Estimate!!Total!!$35 000 to $49 999	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.19_49999 = 'B25074_030', # Estimate!!Total!!$35 000 to $49 999!!Less than 20.0 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.249_49999 = 'B25074_031', # Estimate!!Total!!$35 000 to $49 999!!20.0 to 24.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.299_49999 = 'B25074_032', # Estimate!!Total!!$35 000 to $49 999!!25.0 to 29.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.349_49999 = 'B25074_033', # Estimate!!Total!!$35 000 to $49 999!!30.0 to 34.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.399_49999 = 'B25074_034', # Estimate!!Total!!$35 000 to $49 999!!35.0 to 39.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.499_49999 = 'B25074_035', # Estimate!!Total!!$35 000 to $49 999!!40.0 to 49.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.5plus_49999 = 'B25074_036', # Estimate!!Total!!$35 000 to $49 999!!50.0 percent or more	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.x_49999 = 'B25074_037', # Estimate!!Total!!$35 000 to $49 999!!Not computed	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir_74999 = 'B25074_038', # Estimate!!Total!!$50 000 to $74 999	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.19_74999 = 'B25074_039', # Estimate!!Total!!$50 000 to $74 999!!Less than 20.0 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.249_74999 = 'B25074_040', # Estimate!!Total!!$50 000 to $74 999!!20.0 to 24.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.299_74999 = 'B25074_041', # Estimate!!Total!!$50 000 to $74 999!!25.0 to 29.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.349_74999 = 'B25074_042', # Estimate!!Total!!$50 000 to $74 999!!30.0 to 34.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.399_74999 = 'B25074_043', # Estimate!!Total!!$50 000 to $74 999!!35.0 to 39.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.499_74999 = 'B25074_044', # Estimate!!Total!!$50 000 to $74 999!!40.0 to 49.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.5plus_74999 = 'B25074_045', # Estimate!!Total!!$50 000 to $74 999!!50.0 percent or more	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.x_74999 = 'B25074_046', # Estimate!!Total!!$50 000 to $74 999!!Not computed	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir_99999 = 'B25074_047', # Estimate!!Total!!$75 000 to $99 999	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.19_99999 = 'B25074_048', # Estimate!!Total!!$75 000 to $99 999!!Less than 20.0 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.249_99999 = 'B25074_049', # Estimate!!Total!!$75 000 to $99 999!!20.0 to 24.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.299_99999 = 'B25074_050', # Estimate!!Total!!$75 000 to $99 999!!25.0 to 29.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.349_99999 = 'B25074_051', # Estimate!!Total!!$75 000 to $99 999!!30.0 to 34.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.399_99999 = 'B25074_052', # Estimate!!Total!!$75 000 to $99 999!!35.0 to 39.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.499_99999 = 'B25074_053', # Estimate!!Total!!$75 000 to $99 999!!40.0 to 49.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.5plus_99999 = 'B25074_054', # Estimate!!Total!!$75 000 to $99 999!!50.0 percent or more	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir.x_99999 = 'B25074_055', # Estimate!!Total!!$75 000 to $99 999!!Not computed	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir_100000 = 'B25074_056', # Estimate!!Total!!$100 000 or more	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir_p100000.19 = 'B25074_057', # Estimate!!Total!!$100 000 or more!!Less than 20.0 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir_p100000.249 = 'B25074_058', # Estimate!!Total!!$100 000 or more!!20.0 to 24.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir_p100000.299 = 'B25074_059', # Estimate!!Total!!$100 000 or more!!25.0 to 29.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir_p100000.349 = 'B25074_060', # Estimate!!Total!!$100 000 or more!!30.0 to 34.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir_p100000.399 = 'B25074_061', # Estimate!!Total!!$100 000 or more!!35.0 to 39.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir_p100000.499 = 'B25074_062', # Estimate!!Total!!$100 000 or more!!40.0 to 49.9 percent	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir_p100000.5plus = 'B25074_063', # Estimate!!Total!!$100 000 or more!!50.0 percent or more	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
ir_p100000.x = 'B25074_064' # Estimate!!Total!!$100 000 or more!!Not computed	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS)
)