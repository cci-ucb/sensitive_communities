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

cal17 <- 
	get_acs(
		geography = "tract", 
		variables = gen_vars12, 
		state = "CA", 
		county = NULL, 
		geometry = TRUE, 
		cache_table = TRUE, 
		output = "wide")

