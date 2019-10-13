#############################################################################
# Data Creation of Concentrated Disadvantage Index						 	#
# Author: tim thomas t77@uw.edu											 	#
# Created: 160529															#
# Description: This is code to extract and create a scaled Disadvantage  	#
# index described in:														#
#																			#
# Sampson, Robert J, Stephen W Raudenbush, and Felton Earls. 1997.			#
# “Neighborhoods and violent crime: A multilevel study of 					#
# collective efficacy.” Science 277 (5328): 918–24. 						#
#																			#
# Also in:																	#
#																			#
# Sharkey, Patrick. 2014. “Spatial Segmentation and the Black Middle		#
# Class.” American Journal of Sociology 119 (4): 903–54.					#
#																			#
#############################################################################

# ==========================================================================
# Libraries and Data
# ==========================================================================

	rm(list=ls()) #reset
	options(scipen=10) # avoid scientific notation

	library(tidyverse)
	library(tidycensus)
	library(tigris)
	library(sf)

# ==========================================================================
# Data
# ==========================================================================


###############################
### NCDB Disadvantage Index ###
###############################
	# Drawn from code in: /Users/timothythomas/Google Drive/UW/Research/SeattleCrimeProject/R/Data_160321_NeighCat.r

## load in data
	# NCDB
	load("/Users/timothythomas/Dropbox/UW/Data/NCDB/Raw/NCDB_1970_2010.RData")

	# load spatial data
	# sea.tracts <- readOGR("/Users/timothythomas/Google Drive/UW/Data/USCensus/Shapefiles/2010/Washington/Seattle","SeaTr2010", stringsAsFactors=F)
	sea.tracts <- readOGR("/Users/timothythomas/Google Drive/UW/Research/SCP/Data/Shapefiles/Output","SeaSelect2010Tracts", stringsAsFactors=F)
	wa.tracts <- readOGR("/Users/timothythomas/Google Drive/UW/Data/USCensus/Shapefiles/2010/Washington/Tracts","WA2010Tracts", stringsAsFactors=F)

## Get WA and Seattle GEOID10 id's
	seageo <- sea.tracts$GEOID10
	wageo <- wa.tracts$GEOID10

## Subset NCDB for WA
	ncdb.wa <- as.data.frame(ncdb[ncdb$GEO2010 %in% wageo, ]) # Subset
	# glimpse(ncdb.wa)

	rm(ncdb)
## Create Disadvantage Measure for Seattle
		dis <- ncdb.wa %>%
				group_by(GEO2010) %>%
						# 1980
				select(welf80=WELFARE8, # prop welfare
						pov80=POVRAT8, # prop poverty
						unemp80=UNEMPRT8, # prop unemployed
						femh80=FFH8, # prop female headed w/ children
						child80=CHILD8, # prop under 18
						# 1990
						welf90=WELFARE9,
						pov90=POVRAT9,
						unemp90=UNEMPRT9,
						femh90=FFH9,
						child90=CHILD9,
						# 2000
						welf00=WELFARE0,
						pov00=POVRAT0,
						unemp00=UNEMPRT0,
						femh00=FFH0,
						child00=CHILD0,
						# 2010
						welf10=WELFAR1AR,
						pov10=POVRAT1A,
						unemp10=UNEMPRT1A,
						femh10=FFH1A,
						child10=CHILD1A)

	dis[dis == -999.000] <- NA #make -999 (missing data) NAs
	summary(dis)
## Subset only Seattle Tracts
	subsea <- as.data.frame(dis[dis$GEO2010 %in% seageo, ])
	# plot(sort(subsea$unemp), type="l")

	sea.dis <- data.frame(subsea[1],lapply(subsea[2:21],scale)) %>%
				group_by(GEO2010) %>%
				summarise(dis1980=sum(welf80,pov80,unemp80,femh80,child80, na.rm=T),
						dis1990=sum(welf90,pov90,unemp90,femh90,child90, na.rm=T),
						dis2000=sum(welf00,pov00,unemp00,femh00,child00, na.rm=T),
						dis2010=sum(welf10,pov10,unemp10,femh10,child10, na.rm=T))
	summary(sea.dis)

#################################
### ACS 2005-2009 & 2010-2014 ###
#################################

## Data
	# King County 2014 Shapefile: ACS 2010-2014
		akc14 <- readOGR("/Users/timothythomas/Documents/PHA/Data/Shapefiles","KCTract2014_14WGS84")
	# King County 2000 Shapefile: ACS 2004-2009
		akc00 <- readOGR("/Users/timothythomas/Documents/PHA/Data/Shapefiles","KCTract2000_09WGS84")

	# ACS 2004-2009 & 2010-2014 Tables
		AFW00 <- read.csv("/Users/timothythomas/Google Drive/UW/Data/USCensus/Tables/ACS/2004-2009/nhgis0117_ds195_20095_2009_tract.csv",header=T,strip.white=T,na.strings= c("NA", " ", ""),stringsAsFactors=F)
		PU00 <- read.csv("/Users/timothythomas/Google Drive/UW/Data/USCensus/Tables/ACS/2004-2009/nhgis0117_ds196_20095_2009_tract.csv",header=T,strip.white=T,na.strings= c("NA", " ", ""),stringsAsFactors=F)
		AFWU14 <- read.csv("/Users/timothythomas/Google Drive/UW/Data/USCensus/Tables/ACS/2010-2014/nhgis0117_ds206_20145_2014_tract.csv",header=T,strip.white=T,na.strings= c("NA", " ", ""),stringsAsFactors=F)
		P14 <- read.csv("/Users/timothythomas/Google Drive/UW/Data/USCensus/Tables/ACS/2010-2014/nhgis0117_ds207_20145_2014_tract.csv",header=T,strip.white=T,na.strings= c("NA", " ", ""),stringsAsFactors=F)

## tidy and clean data into 5 disadvantage measures
	# 2004-2009 Disadvantage
		dis00 <- AFW00 %>% select(GISJOIN)
	# Under 18
		kids00 <- AFW00 %>%
					select(GISJOIN,RKYE001,RKYE003:RKYE006,RKYE027:RKYE031) %>%
					replace(is.na(.),0) %>%
					mutate(PChi00=rowSums(.[3:11])/RKYE001) %>%
					select(GISJOIN,PChi00)
		dis00 <- left_join(dis00,kids00)
	# Female headed household
		fem00 <- AFW00 %>%
					select(GISJOIN,RL4E001,RL4E006) %>%
					replace(is.na(.),0) %>%
					mutate(PFem00=RL4E006/RL4E001) %>%
					select(GISJOIN,PFem00)
		dis00 <- left_join(dis00,fem00)
	# Welfare
		wel00 <- AFW00 %>%
					select(GISJOIN, ROAE001,ROAE002) %>%
					replace(is.na(.),0) %>%
					mutate(PWel00=ROAE002/ROAE001) %>%
					select(GISJOIN,PWel00)
		dis00 <- left_join(dis00,wel00)
	# Poverty
		pov00 <- PU00 %>%
					select(GISJOIN,R1HE001,R1HE002) %>%
					replace(is.na(.),0) %>%
					mutate(PPov00=R1HE002/R1HE001) %>%
					select(GISJOIN,PPov00)
		dis00 <- left_join(dis00,pov00)

	# Unemployed
		une00 <- PU00 %>%
					select(GISJOIN,R5CE001,R5CE008,R5CE015,R5CE022,R5CE029,R5CE036,R5CE043,R5CE050,R5CE057,R5CE064,R5CE071,R5CE076,R5CE081,R5CE086,R5CE094,R5CE101,R5CE108,R5CE115,R5CE122,R5CE129,R5CE136,R5CE143,R5CE150,R5CE157,R5CE162,R5CE167,R5CE172) %>%
					replace(is.na(.),0) %>%
					mutate(PUne00=rowSums(.[3:28])/R5CE001) %>%
					select(GISJOIN,PUne00)
		dis00 <- left_join(dis00,une00) %>% select(everything()) %>% mutate(GISJOIN=as.factor(GISJOIN))
		glimpse(dis00)
		gc()

## 2010-2014 Disadvantage
		dis14 <- AFWU14 %>% select(GISJOIN)

	# Under 18
		kids14 <- AFWU14 %>%
					select(GISJOIN,ABAQE001,ABAQE003:ABAQE006,ABAQE027:ABAQE031) %>%
					replace(is.na(.),0) %>%
					mutate(PChi14=rowSums(.[3:11])/ABAQE001) %>%
					select(GISJOIN,PChi14)
		glimpse(kids14)
		dis14 <- left_join(dis14,kids14)

	# Female headed household
		fem14 <- AFWU14 %>%
					select(GISJOIN,ABBYE001,ABBYE006) %>%
					replace(is.na(.),0) %>%
					mutate(PFem14=ABBYE006/ABBYE001) %>%
					select(GISJOIN,PFem14)
		dis14 <- left_join(dis14,fem14)

	# Welfare
		wel14 <- AFWU14 %>%
					select(GISJOIN, ABEIE001,ABEIE002) %>%
					replace(is.na(.),0) %>%
					mutate(PWel14=ABEIE002/ABEIE001) %>%
					select(GISJOIN,PWel14)
		dis14 <- left_join(dis14,wel14)

	# Poverty
		pov14 <- P14 %>%
					select(GISJOIN,ABTBE001,ABTBE002) %>%
					replace(is.na(.),0) %>%
					mutate(PPov14=ABTBE002/ABTBE001) %>%
					select(GISJOIN,PPov14)
		dis14 <- left_join(dis14,pov14)

	# Unemployed
		une14 <- AFWU14 %>%
					select(GISJOIN,ABGFE001,ABGFE005) %>%
					replace(is.na(.),0) %>%
					mutate(PUne14=ABGFE005/ABGFE001) %>%
					select(GISJOIN,PUne14)
		dis14 <- left_join(dis14,une14) %>% select(everything()) %>% mutate(GISJOIN=as.factor(GISJOIN))
		glimpse(dis14)
		gc()
		summary(dis14)


## Subset King County for scaling
	# King County
		diskc00 <- dis00 %>% filter(GISJOIN %in% akc00@data[,"GISJOIN"])
		diskc14 <- dis14 %>% filter(GISJOIN %in% akc14@data[,"GISJOIN"])

	# Scale
		kc.dis00 <- data.frame(diskc00[1],lapply(diskc00[2:6],scale)) %>%
			group_by(GISJOIN) %>%
			summarise(dis09=sum(PChi00,PFem00,PWel00,PPov00,PUne00, na.rm=T))
		kc.dis14 <- data.frame(diskc14[1],lapply(diskc14[2:6],scale)) %>%
			group_by(GISJOIN) %>%
			summarise(dis14=sum(PChi14,PFem14,PWel14,PPov14,PUne14, na.rm=T))

		summary(kc.dis00)
		summary(kc.dis14)

##############
### Export ###
##############

	save(kc.dis14,kc.dis00,file="/Users/timothythomas/Google Drive/UW/Data/USCensus/Compilations/DisadvantageIndex/DT_160529_DisIndexKCACS0914.RData")
	write.csv(kc.dis14,"/Users/timothythomas/Google Drive/UW/Data/USCensus/Compilations/DisadvantageIndex/DT_160529_DisIndexKCACS14.csv")
	write.csv(kc.dis00,"/Users/timothythomas/Google Drive/UW/Data/USCensus/Compilations/DisadvantageIndex/DT_160529_DisIndexKCACS00.csv")




rm(AFW00)
rm(PU00)
