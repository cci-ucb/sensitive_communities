tr_rentprop17 = totrentE.y/tottenE.y,

co_medrentprop17 median(tr_rentprop17, na.rm = TRUE),

rent_prank = rank(tr_rentprop17)/length(tr_rentprop17)

tr_propstudent17 = sum(colenrollE.y, proenrollE.y)/totenrollE.y,

tr_ELI_prop17 = tr_ELI_count17/tr_totalinc17

co_ELI_prop17 = co_ELI_count17/co_totalinc17,

tr_VLI_prop17 = tr_VLI_count17/tr_totalinc17,

co_VLI_propmed17 = median(tr_VLI_prop17, na.rm = TRUE),

tr_rbprop17 = sum(rb_55E.y, na.rm = TRUE)/rb_totE.y,

co_medrbprop17 = median(tr_rbprop17, na.rm = TRUE),

tr_propchrent = (tr_medrent17-tr_medrent12)/tr_medrent12,

co_medpropchrent = median(tr_propchrent)

tr_propchrent.lag = (tr_medrent17.lag - tr_medrent12.lag)/( tr_medrent12.lag),

tr_rentgapprop = (tr_medrent17.lag - tr_medrent17)/((tr_medrent17 +  tr_medrent17.lag)/2)

co_rentgapprop = median(tr_rentgapprop),

co_medrent17 = median(tr_medrent17, na.rm = TRUE),

tr_medrent17 = medrentE.y,
tr_medrent17 = case_when(is.na(tr_medrent17) ~ co_medrent17,
		   						 TRUE ~ tr_medrent17),

ct$tr_medrent17.lag <- lag.listw(lw_dist_idwW,ct$tr_medrent17)

tr_rentgap = tr_medrent17.lag - tr_medrent17,

tr_chrent = tr_medrent17-tr_medrent12,
tr_chrent = case_when(is.na(tr_chrent) ~ co_medchrent,
		   					  TRUE ~ tr_chrent),

ct$tr_chrent.lag <- lag.listw(lw_dist_idwW,ct$tr_chrent)

source("~/git/Functions/NeighType_Fun.R")

cal_nt <-
	ntdf(state = "CA") %>%
	select(GEOID,NeighType) %>%
	mutate(v_poc = case_when(# NeighType == "White-Asian" ~ 0,
							 NeighType == "All White" ~ 0,
							 NeighType == "White-Shared" ~ 0,
							 TRUE ~ 1))

pwhite = WhiteE.y/totraceE.y,

pblack = BlackE.y/totraceE.y,

pasian = AsianE.y/totraceE.y,

platinx = LatinxE.y/totraceE.y,

pother = (totraceE.y - sum(WhiteE.y,BlackE.y,AsianE.y,LatinxE.y, na.rm = TRUE))/totraceE.y,

pwelfare = welfE.y/totwelfE.y,

ppoverty = sum(povfamhE.y, povnonfamhE.y, na.rm = TRUE)/totpovE.y,

unemp = unempE.y/totunempE.y,

pfemhhch = sum(femfamheadchE.y, femnonfamheadchE.y, na.rm = TRUE)/totfhcE.y,

pPOC = (totraceE.y - WhiteE.y)/totraceE.y,pwelfare



v_renters_40p = case_when(tr_rentprop17 >= .4 ~ 1, # v2
								  TRUE ~ 0),

irVLI_50p = irVLI_50rb/tot_ir)

v_rbVLI_50rb = case_when(irVLI_50p > irVLI_50_comed ~ 1)

v_VLI_med = case_when(tr_propstudent17 < .20 & # v2
		   				tr_VLI_prop17 > co_VLI_propmed17 ~ 1, # v2
		   				TRUE ~ 0),

irLI_50p
v_rbLI_50rb
dp_chrent_co
dp_rentgap_co

WhiteE.y
totraceE.y
BlackE.y
AsianE.y
LatinxE.y
welfE.y
totwelfE.y
povfamhE.y
povnonfamhE.y
totpovE.y
unempE.y
totunempE.y
femfamheadchE.y
femnonfamheadchE.y
totfhcE.y


---

totrentE.y
tottenE.y
colenrollE.y
proenrollE.y
totenrollE.y
rb_55E.y
rb_totE.y
medrentE.y
WhiteE.y
totraceE.y
BlackE.y
totraceE.y
AsianE.y
totraceE.y
LatinxE.y
totraceE.y
totraceE.y
WhiteE.y
BlackE.y
AsianE.y
LatinxE.y
totraceE.y
welfE.y
totwelfE.y
povfamhE.y
povnonfamhE.y
totpovE.y
unempE.y
totunempE.y
femfamheadchE.y
femnonfamheadchE.y
totfhcE.y
totraceE.y
WhiteE.y
totraceE.y
WhiteE.y
totraceE.y
BlackE.y
AsianE.y
LatinxE.y
welfE.y
totwelfE.y
povfamhE.y
povnonfamhE.y
totpovE.y
unempE.y
totunempE.y
femfamheadchE.y
femnonfamheadchE.y
totfhcE.y

totrent
totten
colenroll
proenroll
totenroll
rb_55
rb_tot
medrent
White
totrace
Black
totrace
Asian
totrace
Latinx
totrace
totrace
White
Black
Asian
Latinx
totrace
welf
totwelf
povfamh
povnonfamh
totpov
unemp
totunemp
femfamheadch
femnonfamheadch
totfhc
totrace
White
totrace
White
totrace
Black
Asian
Latinx
welf
totwelf
povfamh
povnonfamh
totpov
unemp
totunemp
femfamheadch
femnonfamheadch
totfhc


# ==========================================================================
# 
# ==========================================================================

totrent
'totrent' = 'B25003_003', # Estimate!!Total!!Renter occupied TENURE
totten
'totten' = 'B25003_001',  # Estimate!!Total TENURE
colenroll
'colenroll' = 'B14007_017', # Estimate!!Total!!Enrolled in school!!Enrolled in college undergraduate years	SCHOOL ENROLLMENT BY DETAILED  LEVEL OF SCHOOL FOR THE POPULATION 3 YEARS AND OVER
proenroll
'proenroll' = 'B14007_018', # Estimate!!Total!!Enrolled in school!!Graduate or professional school	SCHOOL ENROLLMENT BY DETAILED  LEVEL OF SCHOOL FOR THE POPULATION 3 YEARS AND OVER
totenroll
'totenroll' = 'B14007_001', # Estimate!!Total	SCHOOL ENROLLMENT BY DETAILED  LEVEL OF SCHOOL FOR THE POPULATION 3 YEARS AND OVER
rb_55
'rb_55' = 'B25070_010', # 50.0 percent or more GROSS RENT as % hh income
rb_tot
'rb_tot' = 'B25070_001', # GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME
medrent
'medrent' = 'B25064_001', #MEDIAN GROSS RENT (DOLLARS)
totrace
'totrace' = 'B03002_001',
White
'White' = 'B03002_003',
Black
'Black' = 'B03002_004',
Asian
'Asian' = 'B03002_006',
Latinx
'Latinx' = 'B03002_012',
totwelf
'totwelf' = 'B19057_001', #
welf
'welf' = 'B19057_002',