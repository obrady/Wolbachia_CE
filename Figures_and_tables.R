# CE figures and tables for paper

rm(list = ls())

setwd("/Users/eideobra/Dropbox/Indonesia")

require(raster)
require(rgdal)
require(tmap)
require(ggplot2)
require(RColorBrewer)
require(ggpubr)
require(grid)
require(gridExtra)



##################################################################
####### figure 1- cost inference- see "Area_cost_inference.R"#####
##################################################################



###################################################
####### table 1- cost of different programmes #####
###################################################

## load in the results from the three scales of programme

# yog city
load("06_Effectiveness/CE_paper_estimates/YOG_CITY.RData")

# yog SAR
load("06_Effectiveness/CE_paper_estimates/YOG_SAR.RData")

# Jakarta
load("06_Effectiveness/CE_paper_estimates/JAK.RData")

# Bali
load("06_Effectiveness/CE_paper_estimates/BALI.RData")

# rounding function
round.func <- function(x, YOGCITY = FALSE){
  x[1] = round(x[1] / 1000000, 2)
  x[2] = round(x[2], 1)
  x[3] = round(x[3] / (100 * 100), 2)
  x[4] = round(x[4], 1)
  x[5:10] = round(x[5:10] / 1000000, 2)
  if(YOGCITY){x[8:10] = NA}
  x[11:16] = round(x[11:16], 2)
  
  return(as.numeric(x))
}

# compile into one table
Table1 <- data.frame(YOG_CITY = round.func(YOG_CITY_result[[1]], YOGCITY = TRUE),
                     YOG_SAR = round.func(YOG_SAR_result[[1]]),
                     JAKARTA = round.func(JAK_result[[1]]),
                     BALI = round.func(BALI_result[[1]]))
rownames(Table1) <- c("People_millions",
                      "Perc_covered_by_Wol",
                      "Km2",
                      "Perc_area_covered_by_Wol",
                      "Cost_millions_F_mid",
                      "Cost_millions_F_low",
                      "Cost_millions_F_high",
                      "Cost_millions_S_mid",
                      "Cost_millions_S_low",
                      "Cost_millions_S_high",
                      "Cost_per_person_F_mid",
                      "Cost_per_person_F_low",
                      "Cost_per_person_F_high",
                      "Cost_per_person_S_mid",
                      "Cost_per_person_S_low",
                      "Cost_per_person_S_high")
write.csv(Table1, file = "13_Writeup/CE_paper/Figures/Table1.csv")







#########################################################################
####### figure 2- Example accumulation of costs and benefits #####
#########################################################################

# programme costs
load("05_Costs/Scale_Up/Cost_bene_cum/T_cost_fast.RData")
load("05_Costs/Scale_Up/Cost_bene_cum/T_cost_seq.RData")
load("05_Costs/Scale_Up/Cost_bene_cum/T_cost_slow_central.RData")
T_cost_slow = h5
T_cost_slow[1:13, 2] = T_cost_slow[1:13, 2] + T_cost_slow_central
rm(h5, T_cost_slow_central)
T_cost_fast = data.frame(year = 1:13, costs = T_cost_fast)

## $ benefits
# fast
load("05_Costs/Scale_Up/Cost_bene_cum/Aver_costs_direct_fast_discounted.RData")
load("05_Costs/Scale_Up/Cost_bene_cum/Aver_costs_INdirect_fast_discounted.RData")
load("05_Costs/Scale_Up/Cost_bene_cum/Aver_costs_INdirect_fatal_fast_discounted.RData")
T_avert_fast = Aver_costs_direct_fast_discounted + Aver_costs_INdirect_fast_discounted + Aver_costs_INdirect_fatal_fast_discounted
T_avert_fast = data.frame(year = 4:13, savings = T_avert_fast)
rm(Aver_costs_direct_fast_discounted, Aver_costs_INdirect_fast_discounted, Aver_costs_INdirect_fatal_fast_discounted)

# slow
load("05_Costs/Scale_Up/Cost_bene_cum/Aver_costs_direct_slow.RData")
load("05_Costs/Scale_Up/Cost_bene_cum/Aver_costs_INdirect_slow.RData")
load("05_Costs/Scale_Up/Cost_bene_cum/Aver_costs_INdirect_fatal_slow.RData")
T_avert_slow = h2 + h3 + h4
T_avert_slow$year = T_avert_slow$year / 3
colnames(T_avert_slow)[2] = "savings"
rm(h2, h3, h4)

# cumulative sum and convert to millions
T_cost_fast$costs = cumsum(T_cost_fast$costs) / 10^6
T_cost_slow$costs = cumsum(T_cost_slow$costs) / 10^6
T_avert_fast$savings = cumsum(T_avert_fast$savings) / 10^6
T_avert_slow$savings = cumsum(T_avert_slow$savings) / 10^6



# add programme phases
#T_cost_fast$Phase = c("Phase 1", "Phase 1", "Phase 2", "Phase 3", "Phase 3", "Phase 3",
#                      "Phase 4", "Phase 4", "Phase 4", "Phase 4", "Phase 4", "Phase 4", "Phase 4")
T_cost_fast$Phase = c("1", "1", "2", "3", "3", "3",
                      "4", "4", "4", "4", "4", "4", "4")

#T_cost_slow$Phase = c("Phase 1", "Phase 1", "Phase 2", "Phase 2", "Phase 2", "Phase 2", "Phase 2",
#                      "Phase 2", "Phase 2", "Phase 2", "Phase 3", "Phase 3", "Phase 3",
#                      "Phase 4", "Phase 4", "Phase 4", "Phase 4", "Phase 4", "Phase 4", "Phase 4")
T_cost_slow$Phase = c("1", "1", "2", "2", "2", "2", "2",
                      "2", "2", "2", "3", "3", "3",
                      "4", "4", "4", "4", "4", "4", "4")

# extend time horizon of fast programme
T_cost_fast = rbind(T_cost_fast, data.frame(year = 14:20,
                                            costs = T_cost_fast$costs[13],
                                            Phase = "Uncertainty"))

# projected max increase in programme costs
f_gains <- T_avert_fast$savings[10] - T_avert_fast$savings[9]
f_gains <- (1:7) * f_gains + T_avert_fast$savings[10]
f_gains_slow <- c(max(diff(T_avert_slow$savings)),
                  which.max(diff(T_avert_slow$savings)))
f_gains_slow2 <- (1:(20 - f_gains_slow[2]) * f_gains_slow[1] + T_avert_slow$savings[f_gains_slow[2]])

T_avert_fast_optimistic = rbind(T_avert_fast, data.frame(year = 14:20,
                                              savings = f_gains))
T_avert_fast = rbind(T_avert_fast, data.frame(year = 14:20,
                                              savings = T_avert_fast$savings[10]))
T_avert_slow_optimistic = T_avert_slow
T_avert_slow_optimistic$savings[(f_gains_slow[2] + 1):20] = f_gains_slow2


# df needs a phase for ggplot to accept it
T_avert_fast$Phase = "1" 
T_avert_fast_optimistic$Phase = "1"
T_avert_slow$Phase = "1"
T_avert_slow_optimistic$Phase = "1"

# add years 1-3 to fast data frames
T_avert_fast = rbind(data.frame(year = 1:3, savings = 0, Phase = "1"), T_avert_fast)
T_avert_fast_optimistic = rbind(data.frame(year = 1:3, savings = 0, Phase = "1"), T_avert_fast_optimistic)

# uncertainty polygons
T_avert_poly = rbind(T_avert_fast_optimistic, T_avert_fast[20:1, ])
T_avert_poly_s = rbind(T_avert_slow_optimistic, T_avert_slow[20:1, ])





# begin plots
cbp1 <- c("#E69F00", "#56B4E9", "#009E73",
          "#D55E00", "#999999")


p1 <- ggplot(T_cost_fast, aes(x = year, y = costs, fill = Phase)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cbp1) +
  scale_y_continuous(limits = c(0, 12)) +
  theme_bw() +
  xlab("Programme year") +
  ylab("Cumulative costs and benefits (millions USD)") + 
  guides(fill=guide_legend(title="Programme phase")) +
  geom_line(data = T_avert_fast, mapping = aes(x = year, y = savings), colour = "black") +
  geom_line(data = T_avert_fast_optimistic, mapping = aes(x = year, y = savings), colour = "black") +
  geom_polygon(data = T_avert_poly, mapping = aes(x = year, y = savings), fill = "light gray", alpha = 0.75)

p1

p2 <- ggplot(T_cost_slow, aes(x = year, y = costs, fill = Phase)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 12)) +
  scale_fill_manual(values = cbp1) +
  theme_bw() +
  xlab("Programme year") +
  ylab("Cumulative costs and benefits (millions USD)") + 
  guides(fill=guide_legend(title="Programme phase")) +
  geom_line(data = T_avert_slow, mapping = aes(x = year, y = savings), colour = "black") +
  geom_line(data = T_avert_slow_optimistic, mapping = aes(x = year, y = savings), colour = "black") +
  geom_polygon(data = T_avert_poly_s, mapping = aes(x = year, y = savings), fill = "gray", alpha = 0.75)

p2

g <- grid.arrange(p1, p2, ncol = 2, nrow = 1)
ggsave(filename = "13_Writeup/CE_paper/Figures/Fast_vs_Slow.pdf", g, width = 10, height = 5)


#################################################################
####### table 2- cost effectiveness of different programmes #####
#################################################################


# rounding function
round.func <- function(x, YOGCITY = FALSE){
  return(as.numeric(round(x, 0)))
}

# compile into one table
Table2 <- data.frame(YOG_CITY = round.func(YOG_CITY_result[[2]], YOGCITY = TRUE),
                     YOG_SAR = round.func(YOG_SAR_result[[2]]),
                     JAKARTA = round.func(JAK_result[[2]]),
                     BALI = round.func(BALI_result[[2]]))

rownames(Table2) <- c("Perc_cases_averted_mid",
                      "Perc_cases_averted_low",
                      "Perc_cases_averted_high",
                      "CE_Gross_F_mid",
                      "CE_Gross_F_low",
                      "CE_Gross_F_high",
                      "CE_NET_F_mid",
                      "CE_NET_F_low",
                      "CE_NET_F_high",
                      "CE_Gross_S_mid",
                      "CE_Gross_S_low",
                      "CE_Gross_S_high",
                      "CE_NET_S_mid",
                      "CE_NET_S_low",
                      "CE_NET_S_high")
write.csv(Table2, file = "13_Writeup/CE_paper/Figures/Table2.csv")




###########################
####### Figure 3- CBR #####
###########################

# to GGplot long format
ggdat <- matrix(c(YOG_CITY_result[[3]],
                  YOG_SAR_result[[3]],
                  JAK_result[[3]],
                  BALI_result[[3]]),
                ncol = 3, byrow = T)


ggdat <- data.frame(CBR_mid = unlist(ggdat[, 1]),
                    CBR_low = unlist(ggdat[, 2]),
                    CBR_high = unlist(ggdat[, 3]),
                    Setting = c(rep("YOG_CITY", 3),
                                rep("YOG_SAR", 3),
                                rep("JAK", 3),
                                rep("BALI", 3)),
                    Type = rep(c("C1_Direct", "C2_Indirect", "C3_Fatal"), 4))

# deduct indirect from indirect + fatal
ggdat$CBR_mid[3] = ggdat$CBR_mid[3] - ggdat$CBR_mid[2]
ggdat$CBR_mid[6] = ggdat$CBR_mid[6] - ggdat$CBR_mid[5]
ggdat$CBR_mid[9] = ggdat$CBR_mid[9] - ggdat$CBR_mid[8]
ggdat$CBR_mid[12] = ggdat$CBR_mid[12] - ggdat$CBR_mid[11]

ggdat$CBR_low[3] = ggdat$CBR_low[3] - ggdat$CBR_low[2]
ggdat$CBR_low[6] = ggdat$CBR_low[6] - ggdat$CBR_low[5]
ggdat$CBR_low[9] = ggdat$CBR_low[9] - ggdat$CBR_low[8]
ggdat$CBR_low[12] = ggdat$CBR_low[12] - ggdat$CBR_low[11]

ggdat$CBR_high[3] = ggdat$CBR_high[3] - ggdat$CBR_high[2]
ggdat$CBR_high[6] = ggdat$CBR_high[6] - ggdat$CBR_high[5]
ggdat$CBR_high[9] = ggdat$CBR_high[9] - ggdat$CBR_high[8]
ggdat$CBR_high[12] = ggdat$CBR_high[12] - ggdat$CBR_high[11]


p <- ggplot(ggdat, aes(x=Setting, y=CBR_mid, fill=Type)) + 
  geom_hline(yintercept = 1) +
  geom_bar(stat="identity",
           position = position_stack(reverse = TRUE)) +
  #geom_errorbar(aes(ymin=CBR_low, ymax=CBR_high), width=.2) +
  scale_x_discrete(limits=c("YOG_CITY", "YOG_SAR", "JAK", "BALI"),
                   labels = c("Yogyakarta city",
                              "Yogyakarta region",
                              "Jakarta",
                              "Bali"),
                   name = "") +
  scale_y_continuous(name = "Benefits to costs ratio") + 
  scale_fill_brewer(palette="Paired",
                    labels = c("Direct", "Indirect (non fatal)", "Indirect (fatal)"),
                    name = "Type of benefit") + 
  theme_minimal()

ggsave(filename = "13_Writeup/CE_paper/Figures/F4_CBR_figure.pdf", p, width = 7, height = 5)






###############################
####### Figure 4- CE maps #####
###############################

# load in some background maps
Pop2015 <- raster("02_Mapping/Covariates/Dengue_futures_covs/UNICOVS_standard/2015/Pop2015_standard_avgscenarios.grd")
Indonesia <- raster("02_Mapping/Administrative_units/admin0_5k_INDONESIA.tif")
Popu = mask(crop(Pop2015, Indonesia), Indonesia)
Popu = Popu < 10

ad0 <- readOGR("02_Mapping/Administrative_units/Admin0(2011)/admin0.shp", "admin0")
ad0 = ad0[ad0$COUNTRY_ID == "IDN", ]
ad1 <- readOGR("02_Mapping/Administrative_units/Admin1(2011)/admin1.shp", "admin1")
ad1 = ad1[ad1$COUNTRY_ID == "IDN", ]
ad2 <- readOGR("02_Mapping/Administrative_units/Admin2(2011)/admin2.shp", "admin2")
ad3 <- readOGR("02_Mapping/Administrative_units/Admin3/DesaIndonesia.shp", "DesaIndonesia")

roads <- readOGR("02_Mapping/Administrative_units/Roads/IDN_roads.shp", "IDN_roads")
rivers <- readOGR("02_Mapping/Administrative_units/Rivers/IDN_water_lines_dcw.shp", "IDN_water_lines_dcw")



# colour palette
#pal = c("#C0C0C0", brewer.pal(9, "YlOrRd"))
pal = c("#C0C0C0", rev(brewer.pal(8, "RdYlBu")))


# categorising and standardising the colour scheme
all_CE_vals <- c(as.vector(YOG_CITY_result[[4]][[1]]),
                 as.vector(YOG_SAR_result[[4]][[1]]),
                 as.vector(JAK_result[[4]][[1]]),
                 as.vector(BALI_result[[4]][[1]]))
catvals <- cut(log(all_CE_vals[(all_CE_vals > 0) & (!is.na(all_CE_vals))]), (9 - 1))
# assign back to vector of maps
newvals = all_CE_vals
# 0 is unqiue code for Wolbachia not deployed
newvals[newvals == 0] = "0"
newvals[(all_CE_vals > 0) & (!is.na(all_CE_vals))] = catvals
newvals = as.numeric(newvals)
# assign back to new rasters
YOG_CITY = YOG_CITY_result[[4]][[1]]
values(YOG_CITY) = newvals[1:length(YOG_CITY_result[[4]][[1]])]

YOG_SAR = YOG_SAR_result[[4]][[1]]
values(YOG_SAR) = newvals[(length(YOG_CITY_result[[4]][[1]]) + 1):(length(YOG_CITY_result[[4]][[1]]) + length(YOG_SAR_result[[4]][[1]]))]

JAK = JAK_result[[4]][[1]]
values(JAK) = newvals[(length(YOG_CITY_result[[4]][[1]]) + length(YOG_SAR_result[[4]][[1]]) + 1):(length(YOG_CITY_result[[4]][[1]]) + length(YOG_SAR_result[[4]][[1]]) + length(JAK_result[[4]][[1]]))]

BALI = BALI_result[[4]][[1]]
values(BALI) = newvals[(length(YOG_CITY_result[[4]][[1]]) + length(YOG_SAR_result[[4]][[1]]) + length(JAK_result[[4]][[1]]) + 1):(length(YOG_CITY_result[[4]][[1]]) + length(YOG_SAR_result[[4]][[1]]) + length(JAK_result[[4]][[1]]) + length(BALI_result[[4]][[1]]))]




# YOG city
# load places of special interest
places <- read.csv("07_Data/Population/Yogya_Sites_of_interest.csv")
places <- st_as_sf(places, coords = c("Long", "Lat"))
places <- places %>% st_set_crs(4326)
fmap <- YOG_CITY
ad1_yogcity = crop(ad1, fmap)
ad2_yogcity = crop(ad2, extent(fmap))
ad3_yogcity = ad3[ad3$PROPINSI == "DI. Yogyakarta",]
#ad3_NOTyogcity = ad3[ad3$PROPINSI != "DI. Yogyakarta",]
rivers_yogcity = crop(rivers, extent(fmap))


p1 <- tm_shape(ad2_yogcity) +
  tm_polygons() +
  tm_shape(fmap) +
  tm_raster(palette = pal[na.omit(sort(unique(as.vector(fmap)))) + 1],
            n = length(pal[na.omit(sort(unique(as.vector(fmap))))])) +
  tm_legend(show=FALSE) +
  #tm_shape(rivers_yogcity) +
  #tm_lines(col = "blue") +
  #tm_shape(ad3_NOTyogcity) +
  #tm_polygons()
  tm_shape(ad2_yogcity) +
  tm_borders() +
  tm_shape(places) +
  tm_dots(size = 0.25, alpha = 0.75) +
  tm_text("Name", size = 0.85, ymod = 1) +
  tm_scale_bar(position = c("left", "bottom"), size = 0.75)
p1



# YOG SAR
fmap <- YOG_SAR
ad1_yogSAR = crop(ad1, fmap)
ad2_yogSAR = ad2[ad2$PARENT_ID == 1013669, ]
ad3_yogSAR = ad3[ad3$PROPINSI == "DI. Yogyakarta",]

p2 <- tm_shape(ad1_yogSAR) +
  tm_polygons() +
  tm_shape(fmap) +
  tm_raster(palette = pal[na.omit(sort(unique(as.vector(fmap)))) + 1],
            n = length(pal[na.omit(sort(unique(as.vector(fmap))))])) +
  tm_legend(show=FALSE) +
  tm_shape(ad2_yogSAR) +
  tm_borders() +
  tm_scale_bar(position = c("left", "bottom"), size = 0.75) +
  tm_layout(bg.color = "light blue")
p2



# Jakarta
fmap <- JAK
# remove paracel islands
fmap = crop(fmap, extent(c(106.638703, 106.9737 ,  -6.376053,  -6.034424)))

ad0_JAK = crop(ad0, fmap)
ad1_JAK = crop(ad1, fmap)
ad2_JAK = ad2[ad2$PARENT_ID == 1013676, ]
ad3_JAK = ad3[ad3$PROPINSI == "DKI Jakarta",]

p3 <- tm_shape(ad0_JAK) +
  tm_polygons() +
  tm_shape(fmap) +
  tm_raster(palette = pal[na.omit(sort(unique(as.vector(fmap)))) + 1],
            n = length(pal[na.omit(sort(unique(as.vector(fmap))))])) +
  tm_legend(show=FALSE) +
  tm_shape(ad2_JAK) +
  tm_borders() +
  tm_scale_bar(position = c("left", "bottom"), size = 0.75) +
  tm_layout(bg.color = "light blue")
p3



# BALI
fmap <- BALI

ad0_BALI = crop(ad0, fmap)
ad1_BALI = crop(ad1, fmap)
ad2_BALI = ad2[ad2$PARENT_ID == 1013678, ]
#ad3_BALI = ad3[ad3$PROPINSI == "DKI Jakarta",]

p4 <- tm_shape(ad0_BALI) +
  tm_polygons() +
  tm_shape(fmap) +
  tm_raster(palette = pal[na.omit(sort(unique(as.vector(fmap)))) + 1],
            n = length(pal[na.omit(sort(unique(as.vector(fmap))))])) +
  tm_legend(show=FALSE) +
  tm_shape(ad2_BALI) +
  tm_borders() +
  tm_scale_bar(position = c("left", "bottom"), size = 0.75) +
  tm_layout(bg.color = "light blue")
p4


# group together
#current.mode <- tmap_mode("plot")
#tmap_save(tmap_arrange(p1, p2, p3, p4), file = "13_Writeup/CE_paper/Figures/CE_Maps.pdf",
#          width = 10, height = 10)
#tmap_mode(current.mode)
tmap_save(p1, file = "13_Writeup/CE_paper/Figures/CE_Maps_A.pdf", width = 5, height = 5)
tmap_save(p2, file = "13_Writeup/CE_paper/Figures/CE_Maps_B.pdf", width = 5, height = 5)
tmap_save(p3, file = "13_Writeup/CE_paper/Figures/CE_Maps_C.pdf", width = 5, height = 5)
tmap_save(p4, file = "13_Writeup/CE_paper/Figures/CE_Maps_D.pdf", width = 5, height = 5)







#############################################
### Jakarta pixel vs admin slow programme ###
#############################################

load("06_Effectiveness/CE_paper_estimates/JAK_Admin_constrained.RData")
JAK_pixel <- crop(JAK_result[[4]][[2]], JAK_result_admin[[4]][[2]])
JAK_admin <- JAK_result_admin[[4]][[2]]


p5 <- tm_shape(ad0_JAK) +
  tm_polygons() +
  tm_shape(JAK_pixel) +
  tm_raster(palette = rev(brewer.pal(8, "BuGn"))) +
  tm_legend(show=FALSE) +
  #tm_shape(ad2_JAK) +
  #tm_borders() +
  tm_scale_bar(position = c("left", "bottom"), size = 0.75) +
  tm_layout(bg.color = "light blue")
p5

p6 <- tm_shape(ad0_JAK) +
  tm_polygons() +
  tm_shape(JAK_admin) +
  tm_raster(palette = rev(brewer.pal(8, "BuGn"))) +
  tm_legend(show=FALSE) +
  #tm_shape(ad2_JAK) +
  #tm_borders() +
  #tm_scale_bar(position = c("left", "bottom"), size = 0.75) +
  tm_layout(bg.color = "light blue")
p6


# group together
current.mode <- tmap_mode("plot")
tmap_save(tmap_arrange(p5, p6), file = "13_Writeup/CE_paper/Figures/Admin_maps.pdf",
          width = 10, height = 4)
tmap_mode(current.mode)

# comparison of CE 
admin_CE = JAK_result_admin[[2]]$dollar_per_DALY_fslow_Net_INdirectFAT_median

pixel_CE = JAK_result[[2]]$dollar_per_DALY_fslow_Net_INdirectFAT_median

admin_CE / pixel_CE # largely comparable







##########################
### Disaster scenarios ###
##########################

load("06_Effectiveness/CE_paper_estimates/YOG_CITY_D_Low_coverage.RData")
load("06_Effectiveness/CE_paper_estimates/YOG_CITY_D_Passive_monitoring.RData")
load("06_Effectiveness/CE_paper_estimates/YOG_CITY_D_Replacement.RData")
load("06_Effectiveness/CE_paper_estimates/YOG_CITY_D_Resistance.RData")

# Benefits / costs
BCR = colSums(matrix(unlist(YOG_CITY_result[[3]]), ncol = 3, byrow = T)) - matrix(unlist(YOG_CITY_result[[3]]), ncol = 3, byrow = T)[2, ]
BCR_A4 = colSums(matrix(unlist(YOG_CITY_result_A4[[3]]), ncol = 3, byrow = T)) - matrix(unlist(YOG_CITY_result_A4[[3]]), ncol = 3, byrow = T)[2, ]
BCR_A3 = colSums(matrix(unlist(YOG_CITY_result_A3[[3]]), ncol = 3, byrow = T)) - matrix(unlist(YOG_CITY_result_A3[[3]]), ncol = 3, byrow = T)[2, ]
BCR_A2 = colSums(matrix(unlist(YOG_CITY_result_A2[[3]]), ncol = 3, byrow = T)) - matrix(unlist(YOG_CITY_result_A2[[3]]), ncol = 3, byrow = T)[2, ]
BCR_A1 = colSums(matrix(unlist(YOG_CITY_result_A1[[3]]), ncol = 3, byrow = T)) - matrix(unlist(YOG_CITY_result_A1[[3]]), ncol = 3, byrow = T)[2, ]

# costs
costs = YOG_CITY_result[[1]][c("TotalCost_Slow_median", "TotalCost_Slow_low", "TotalCost_Slow_high")]
costs_A4 = YOG_CITY_result_A4[[1]][c("TotalCost_Slow_median", "TotalCost_Slow_low", "TotalCost_Slow_high")]
costs_A3 = YOG_CITY_result_A3[[1]][c("TotalCost_Slow_median", "TotalCost_Slow_low", "TotalCost_Slow_high")]
costs_A2 = YOG_CITY_result_A2[[1]][c("TotalCost_Slow_median", "TotalCost_Slow_low", "TotalCost_Slow_high")]
costs_A1 = YOG_CITY_result_A1[[1]][c("TotalCost_Slow_median", "TotalCost_Slow_low", "TotalCost_Slow_high")]

# benefits
benefits = BCR * costs
benefits_A4 = BCR_A4 * costs_A4
benefits_A3 = BCR_A3 * costs_A3
benefits_A2 = BCR_A2* costs_A2
benefits_A1 = BCR_A1 * costs_A1

plotdf <- cbind(rbind(costs, costs_A4, costs_A3, costs_A2, costs_A1),
                rbind(benefits, benefits_A4, benefits_A3, benefits_A2, benefits_A1))
colnames(plotdf) = c("Cost", "Cost_low", "Cost_high", "Bene", "Bene_low", "Bene_high")
plotdf$Name = c("Baseline", "Low coverage", "Passive monitoring", "Replacement", "Resistance")

# convert to millions
plotdf[, 1:6] = plotdf[, 1:6] / 1000000

BCR_labs <- data.frame(Names = c("BCR = 1.0", "BCR = 2.0", "BCR = 3.0"),
                       xpos = c(2.5, 2.25, 2),
                       ypos = c(3.5, 5.5, 7.5),
                       rotation = c(22.5, 42.5, 55),
                       col = c("grey", "grey", "grey"))

failure <- ggplot(plotdf, aes(x = Cost, y = Bene))+
  geom_point(size = 4, colour = c("black", "dark red", "dark green", "dark red", "orange")) +
  #geom_errorbarh(aes(xmin = Cost_low, xmax = Cost_high)) +
  #geom_errorbar(aes(ymin = Bene_low, ymax = Bene_high)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey") +
  geom_abline(slope = 2, intercept = 0, colour = "grey") +
  geom_abline(slope = 3, intercept = 0, colour = "grey") +
  #geom_abline(slope = 0.5, intercept = 0, colour = "grey") +
  geom_text(aes(xpos, ypos, label = Names, angle = rotation), BCR_labs, colour = "grey") +
  scale_x_continuous(limits = c(1, 10)) +
  scale_y_continuous(limits = c(1, 15)) +
  xlab("Cost (millions USD)") +
  ylab("Benefits (millions USD)") +
  geom_text(aes(label=Name),hjust=c(0.5, 0.7, 0.1, 0.75, 0.5), vjust=c(-1, 2, 2, -1, 2)) +
  theme_minimal()
failure

ggsave(filename = "13_Writeup/CE_paper/Figures/Failure_figure.pdf", failure, width = 7, height = 5)



####################
### Tornado plot ###
####################

load("06_Effectiveness/CE_paper_estimates/YOG_CITY_T_BurdfixH.RData")
load("06_Effectiveness/CE_paper_estimates/YOG_CITY_T_BurdfixL.RData")
load("06_Effectiveness/CE_paper_estimates/YOG_CITY_T_DisCostFixH.RData")
load("06_Effectiveness/CE_paper_estimates/YOG_CITY_T_DisCostFixL.RData")
load("06_Effectiveness/CE_paper_estimates/YOG_CITY_T_EffFixH.RData")
load("06_Effectiveness/CE_paper_estimates/YOG_CITY_T_EffFixL.RData")
load("06_Effectiveness/CE_paper_estimates/YOG_CITY_T_ProgCostFixH.RData")
load("06_Effectiveness/CE_paper_estimates/YOG_CITY_T_ProgCostFixL.RData")

# YOG_CITY_result_T1A = burden, 2 = effectiveness, 3 = wolbachia cost, 4 = cost per case
# -> A1 = burden, A2 = cost per case, A3 = wolbachis cost, A4 = wolbachia effectivness

plotdf = data.frame(va1 = c("A1",
                            "A4",
                            "A3",
                            "A2",
                            "A1",
                            "A4",
                            "A3",
                            "A2"),
                    var2 = c("upper",
                             "upper",
                             "upper",
                             "upper",
                             "lower",
                             "lower",
                             "lower",
                             "lower"),
                    Direction = c("low",
                                  "low",
                                  "high",
                                  "low",#
                                  "high",
                                  "high",
                                  "low",
                                  "high"),
                    vals = c(YOG_CITY_result_T1A[[2]]$dollar_per_DALY_fslow_Net_INdirectFAT_median,
                             YOG_CITY_result_T2A[[2]]$dollar_per_DALY_fslow_Net_INdirectFAT_median,
                             YOG_CITY_result_T3B[[2]]$dollar_per_DALY_fslow_Net_INdirectFAT_median,
                             YOG_CITY_result_T4A[[2]]$dollar_per_DALY_fslow_Net_INdirectFAT_median,
                             YOG_CITY_result_T1B[[2]]$dollar_per_DALY_fslow_Net_INdirectFAT_median,
                             YOG_CITY_result_T2B[[2]]$dollar_per_DALY_fslow_Net_INdirectFAT_median,
                             YOG_CITY_result_T3A[[2]]$dollar_per_DALY_fslow_Net_INdirectFAT_median,
                             YOG_CITY_result_T4B[[2]]$dollar_per_DALY_fslow_Net_INdirectFAT_median))

plotdf$Plevels = plotdf$va1[order(plotdf$vals)]

# median val of Yog_city result:
yog_med <- YOG_CITY_result[[2]]$dollar_per_DALY_fslow_Net_INdirectFAT_median
# adjust 0
plotdf$vals = plotdf$vals - yog_med
# add colour vector
plotdf$col = as.numeric(plotdf$var2)[c("black", "blue")]



# order of plots

tornado  <- ggplot(plotdf, aes(va1, vals, fill=Direction)) +
  coord_flip() +
  geom_bar(position="identity", stat="identity") +
  scale_fill_manual(values = c(rgb(145/255,207/255,96/255),
                               rgb(252/255,141/255,89/255))) +
  theme_bw() +
  geom_hline(yintercept = -yog_med) +
  scale_y_continuous(name = "Net cost effectiveness (USD)",
                     breaks = c(-1500, yog_med, -500, 0, 500, 1000, 1500) - yog_med,
                     labels = c(-1500, round(yog_med, 0), -500, 0, 500, 1000, 1500)) +
  scale_x_discrete(name = "",
                   labels = rev(c("Cost per case",
                                  "Wolbachia cost",
                                  "Wolbachia Efficacy",
                                  "Number of cases")))
tornado

ggsave(filename = "13_Writeup/CE_paper/Figures/F6_Tornado_figure.pdf", tornado, width = 7, height = 5)


