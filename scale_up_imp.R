rm(list = ls())

setwd("/Users/eideobra/Dropbox/07_Indonesia")

require(raster)
require(rgdal)
require(tmap)
require(data.table)

# scale up functions
source("11_Code/CE_PAPER/scale_up_functions.R")

# 5k population map
Pop2015 <- raster("02_Mapping/Covariates/Dengue_futures_covs/UNICOVS_standard/2015/Pop2015_standard_avgscenarios.grd")
Indonesia <- raster("02_Mapping/Administrative_units/admin0_5k_INDONESIA.tif")
Popu = mask(crop(Pop2015, Indonesia), Indonesia)

# load in finalmaps and add extra NAs to the population raster 
# (not all covariates completely aligned)
load("02_Mapping/Outputs/Octobermaps/finalmaplist_resub.RData")
fmap <- Popu
values(fmap) = finalmap[, 1]
Popu = Popu * (fmap >=0)
Popu_vec = as.vector(Popu)
rm(Pop2015, fmap, finalmap)

# posterior burden predictions
incidence_distro <- fread("02_Mapping/Outputs/Octobermaps/Incidence_draws_full.csv")
incidence_distro = incidence_distro[, 2:6]

# 100m population surface
worldpop <- raster("07_Data/Population/IDN_ppp_v2b_2015_UNadj.tif")


# processing incidence distro into a list of maps
burdmapList_sm <- list()
burdmapList_am <- list()
burdmapList_h <- list()
burdmapList_f <- list()
burdmapList_symp <- list()
# there are 1,000 realisations, just take 100 for now to speed up
for(i in 1:100){
  exmap_sm <- Popu
  exmap_am <- Popu
  exmap_h <- Popu
  exmap_f <- Popu
  exmap_symp <- Popu
  
  t1 <- as.vector(Popu)
  t2 <- as.vector(Popu)
  t3 <- as.vector(Popu)
  t4 <- as.vector(Popu)
  t5 <- as.vector(Popu)
  
  t1[!is.na(t1)] <- incidence_distro$self_managed[((i - 1) * 88432 + 1):(i * 88432)]
  t2[!is.na(t2)] <- incidence_distro$ambulatory[((i - 1) * 88432 + 1):(i * 88432)]
  t3[!is.na(t3)] <- incidence_distro$hospitalized[((i - 1) * 88432 + 1):(i * 88432)]
  t4[!is.na(t4)] <- incidence_distro$fatal[((i - 1) * 88432 + 1):(i * 88432)]
  t5[!is.na(t5)] <- incidence_distro$total_cases[((i - 1) * 88432 + 1):(i * 88432)]
  
  values(exmap_sm) = t1
  values(exmap_am) = t2
  values(exmap_h) = t3
  values(exmap_f) = t4
  values(exmap_symp) = t5
  
  burdmapList_sm[[i]] <- exmap_sm
  burdmapList_am[[i]] <- exmap_am
  burdmapList_h[[i]] <- exmap_h
  burdmapList_f[[i]] <- exmap_f
  burdmapList_symp[[i]] <- exmap_symp
  
  rm(t1, t2, t3, t4, t5)
}

rm(incidence_distro)

# model effectiveness ensemble predictions
load(file = "06_Effectiveness/Effectiveness_ensemble_cov100_resub2.RData")
load(file = "06_Effectiveness/Effectiveness_ensemble_cov50_resub2.RData")




# age related stuff
Age_exp = read.csv("03_Burden/DALY_R/Age_Exp.csv")
# sex averaged life expectancy
Age_exp$Life_Exp_both = rowSums(Age_exp[, c("Life_Exp_Male", "Life_Exp_Female")]) / 2
# Life expectancies
LI = Age_exp$Life_Exp_both
# age discounting
LI_discount = sapply(LI, function(x) sum(0.97^(1:round(x))))
# age distribution of cases at baseline
Indoage = read.csv("03_Burden/Age_dist_cases.csv")

# administrative units
ad1 <- readOGR("02_Mapping/Administrative_units/Admin1(2011)/admin1.shp", "admin1")
ad1 = ad1[ad1$COUNTRY_ID == "IDN", ]
ad2 <- readOGR("02_Mapping/Administrative_units/Admin2(2011)/admin2.shp", "admin2")
ad2 = ad2[ad2$COUNTRY_ID == "IDN", ]

# load costing models
load("05_Costs/Cost_model.RData")
load("05_Costs/Cost_LT_model.RData")



# define areas of interest

# 01 Yogyakarta city
YOG_CITY = ad2[ad2$GAUL_CODE == 1013505, ]

# 02 Yogyakarta SAR
Bantul= ad2[ad2$GAUL_CODE == 1013361, ]
Gunung = ad2[ad2$GAUL_CODE == 1013418, ]
Kulon = ad2[ad2$GAUL_CODE == 1013511, ]
Sleman = ad2[ad2$GAUL_CODE == 1013618, ]

YOG_SAR = rbind(Bantul, Gunung, Kulon, Sleman)
rm(Bantul, Gunung, Kulon, Sleman)

# 03 big cities

# Jakarta
JAK = ad1[ad1$NAME == "DKI JAKARTA", ]
#STAN = ad2[ad2$GAUL_CODE == 1013637, ]
#TAN = ad2[ad2$GAUL_CODE == 1013499, ]
#BEK = ad2[ad2$GAUL_CODE == 1013463, ]
#DEP = ad2[ad2$GAUL_CODE == 1013320, ]
#BOG = ad2[ad2$GAUL_CODE == 1013468, ]
#JAK_BIG <- aggregate(rbind(JAK, STAN, TAN, BEK, DEP, BOG))

# Surabaya
#SUR = ad2[ad2$GAUL_CODE == 1013339, ]
# Bandung
#BAN = ad2[ad2$GAUL_CODE == 1013460, ]
# Medan
#MED = ad2[ad2$GAUL_CODE == 1013331, ]
# Cirebon
#CIR = ad2[ad2$GAUL_CODE == 1013402, ]
# Semarang
#SEM = ad2[ad2$GAUL_CODE == 1013337, ]


# 04 Bali
BALI = ad1[ad1$NAME == "BALI", ]

# Admin 3 files for Jakarta (and Yogya for testing)
Ad3 <- readOGR("02_Mapping/Administrative_units/Admin3/DesaIndonesia.shp", "DesaIndonesia")
#yogya_city_A3 <- Ad3[Ad3$PROPINSI == "DI. Yogyakarta", ]
#yogya_city_A3 <- yogya_city_A3[yogya_city_A3$KABUPATEN == "Kdy. Yogyakarta", ]

Jakarta_city_A3 <- Ad3[Ad3$PROPINSI == "DKI Jakarta", ]
# exclude thousand islands
Jakarta_city_A3 = Jakarta_city_A3[Jakarta_city_A3$KECAMATAN != "Pulau Seribu", ]
rm(ad1, ad2, Ad3)








#############################################
# implementation stage
#############################################

# Part 1: Core tables

# A) Yogcity - runtime ~ 5 mins
YOG_CITY_result <- wol.scale.up(YOG_CITY, YOG_city = TRUE)
save(YOG_CITY_result, file = "06_Effectiveness/CE_paper_estimates/YOG_CITY.RData");rm(YOG_CITY_result)

#### Disaster scenarios
# A1) Resistance
YOG_CITY_result_A1 <- wol.scale.up(YOG_CITY, Disaster = "Resistance", YOG_city = TRUE)
save(YOG_CITY_result_A1, file = "06_Effectiveness/CE_paper_estimates/YOG_CITY_D_Resistance.RData");rm(YOG_CITY_result_A1)

# A1B) Resistance
YOG_CITY_result_A1B <- wol.scale.up(YOG_CITY, Disaster = "Resistance_fixed", YOG_city = TRUE)
save(YOG_CITY_result_A1B, file = "06_Effectiveness/CE_paper_estimates/YOG_CITY_D_Resistance_fixed.RData");rm(YOG_CITY_result_A1B)

# A2) Initially uncompetitive
YOG_CITY_result_A2 <- wol.scale.up(YOG_CITY, Disaster = "Uncompetitive", YOG_city = TRUE)
save(YOG_CITY_result_A2, file = "06_Effectiveness/CE_paper_estimates/YOG_CITY_D_Uncompetitive.RData");rm(YOG_CITY_result_A2)

# A3) Passive_monitoring
YOG_CITY_result_A3 <- wol.scale.up(YOG_CITY, Disaster = "Passive_monitoring", YOG_city = TRUE)
save(YOG_CITY_result_A3, file = "06_Effectiveness/CE_paper_estimates/YOG_CITY_D_Passive_monitoring.RData");rm(YOG_CITY_result_A3)

# A4) Low_coverage
YOG_CITY_result_A4 <- wol.scale.up(YOG_CITY, Disaster = "Low_coverage", YOG_city = TRUE)
save(YOG_CITY_result_A4, file = "06_Effectiveness/CE_paper_estimates/YOG_CITY_D_Low_coverage.RData");rm(YOG_CITY_result_A4)

# A4B) Low_coverage_fixed
YOG_CITY_result_A4B <- wol.scale.up(YOG_CITY, Disaster = "Low_coverage_fixed", YOG_city = TRUE)
save(YOG_CITY_result_A4B, file = "06_Effectiveness/CE_paper_estimates/YOG_CITY_D_Low_coverage_fixed.RData");rm(YOG_CITY_result_A4B)

# A5) Special - for plots over time
wol.scale.up(YOG_CITY, Disaster = "Special")


#### Tornado plots
YOG_CITY_result_T1A <- wol.scale.up(YOG_CITY, BurdFix = "L", YOG_city = TRUE)
YOG_CITY_result_T1B <- wol.scale.up(YOG_CITY, BurdFix = "H", YOG_city = TRUE)
save(YOG_CITY_result_T1A, file = "06_Effectiveness/CE_paper_estimates/YOG_CITY_T_BurdfixL.RData")
save(YOG_CITY_result_T1B, file = "06_Effectiveness/CE_paper_estimates/YOG_CITY_T_BurdfixH.RData")

YOG_CITY_result_T2A <- wol.scale.up(YOG_CITY, EffFix = "L", YOG_city = TRUE)
YOG_CITY_result_T2B <- wol.scale.up(YOG_CITY, EffFix = "H", YOG_city = TRUE)

save(YOG_CITY_result_T2A, file = "06_Effectiveness/CE_paper_estimates/YOG_CITY_T_EffFixL.RData")
save(YOG_CITY_result_T2B, file = "06_Effectiveness/CE_paper_estimates/YOG_CITY_T_EffFixH.RData")

YOG_CITY_result_T3A <- wol.scale.up(YOG_CITY, ProgCostFix = "L", YOG_city = TRUE)
YOG_CITY_result_T3B <- wol.scale.up(YOG_CITY, ProgCostFix = "H", YOG_city = TRUE)
save(YOG_CITY_result_T3A, file = "06_Effectiveness/CE_paper_estimates/YOG_CITY_T_ProgCostFixL.RData")
save(YOG_CITY_result_T3B, file = "06_Effectiveness/CE_paper_estimates/YOG_CITY_T_ProgCostFixH.RData")

YOG_CITY_result_T4A <- wol.scale.up(YOG_CITY, DisCostFix = "L", YOG_city = TRUE)
YOG_CITY_result_T4B <- wol.scale.up(YOG_CITY, DisCostFix = "H", YOG_city = TRUE)
save(YOG_CITY_result_T4A, file = "06_Effectiveness/CE_paper_estimates/YOG_CITY_T_DisCostFixL.RData")
save(YOG_CITY_result_T4B, file = "06_Effectiveness/CE_paper_estimates/YOG_CITY_T_DisCostFixH.RData")







######################
# other geographies
######################

# B) Yog SAR
# use the fast scenario results from this one
YOG_SAR_result <- wol.scale.up(YOG_SAR, YOG_sar = TRUE)
save(YOG_SAR_result, file = "06_Effectiveness/CE_paper_estimates/YOG_SAR.RData")


# C) Cities
JAK_result <- wol.scale.up(JAK)
save(JAK_result, file = "06_Effectiveness/CE_paper_estimates/JAK.RData")

# Admin constrained areas
JAK_result_admin <- wol.scale.up(Jakarta_city_A3, Admin_constrain = TRUE)
save(JAK_result_admin, file = "06_Effectiveness/CE_paper_estimates/JAK_Admin_constrained.RData")


# D) Bali - limit to 1:60 for memory and spped constraints
burdmapList_sm = burdmapList_sm[1:60]
burdmapList_am = burdmapList_am[1:60]
burdmapList_h = burdmapList_h[1:60]
burdmapList_f = burdmapList_f[1:60]
burdmapList_symp = burdmapList_symp[1:60]

BALI_result <- wol.scale.up(BALI)
save(BALI_result, file = "06_Effectiveness/CE_paper_estimates/BALI.RData")























