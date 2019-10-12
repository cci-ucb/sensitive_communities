# ==========================================================================
# Exploring areas that changed in California
# ==========================================================================

# ==========================================================================
# Libraries
# ==========================================================================

	library(data.table)
	library(tidyverse)
	library(tidycensus)
	library(tigris)
	options(tigris_use_cache = TRUE)
	library(tmap)

# ==========================================================================
# Data
# ==========================================================================

	load("~/data/Zillow/RData/zillow.rdata")

	calrent <-
		z.rent %>%
		filter(grepl("^06.", GEO2010)) %>%
		mutate(year = year(date.rent)) %>%
		group_by(GEOID = GEO2010, year) %>%
		summarise(med_rent = median(ZRI)) %>%
		spread(year, med_rent) %>%
		group_by(GEOID) %>%
		mutate(ch_tr = `2016` - `2010`) %>%
		ungroup()

	glimpse(calrent)

	caltr <-
		tracts("CA", class = "sf") %>%
		left_join(calrent) %>%
		group_by(COUNTYFP) %>%
		mutate_at(vars(`2010`:`2016`), list(ctymed = median), na.rm = TRUE) %>%
		mutate(ch_ct = `2016_ctymed` - `2010_ctymed`,
			   dif_ct_tr = ch_tr - ch_ct) %>%
		mutate(cty_sc_dif = scale(dif_ct_tr)) %>%
		ungroup() #%>%
		# mutate(st_sc_dif = scale(dif_ct_tr))

	glimpse(caltr)

	calpums <-
		pumas("CA", class = "sf")

	source("~/git/Functions/Gentrification_Fun.R")

	ca17 <- gen(geography = "tract",
				variables = gen_vars12,
				county = NULL,
			  	state = "CA")

# ==========================================================================
# ESDA
# ==========================================================================

	tmap_mode("view")
#
	# map_cty_sc <-
		tm_shape(caltr) +
    	tm_polygons("cty_sc_dif",
    				breaks = c(-7, -2, -1, -.5, .5, 1, 2, 23),
    				# style = "jenks",
    				alpha = 0.5,
    				palette = "RdBu") +
    	tm_layout(scale = 0)


    map_st_sc <-
		tm_shape(caltr) +
    	tm_polygons("st_sc_dif")

    tmap_arrange(map_cty_sc, map_st_sc)

