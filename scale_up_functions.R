
# functions to be used with Cities_scale_up.R

# nested function for map processing:
# focusses burden maps to the target area
newburd.process <- function(x, FArea, Apop){
  # crop to approximate area
  f_extent <- as.vector(extent(FArea))
  f_extent = c(f_extent[1] * 0.99, f_extent[2] * 1.01, f_extent[3] * 1.01, f_extent[4] * 0.99)
  x <- crop(x, extent(f_extent))
  # resample
  x <- resample(x, Apop, method = "bilinear")
  #x <- resample(x, Apop, method = "ngb")
  # mask
  x <- mask(x, Apop)
  return(x)
}

# looks up effectiveness based on baseline incidence
find.eff <- function(x, effmat, EffFix = "N"){
  # ensemble incidence values (for reference)
  ref_incid = round(seq(0, 100, 0.1), 1)
  # incidence of symptomatic cases (per 1)- need per 1,000 to match with model ensemble values
  i_vals <- as.vector(x) * 1000
  i_vals[i_vals < 0] = 0
  i_vals[i_vals > 100] = 100
  i_vals = round(i_vals, 1)
  i_vals_match = match(i_vals, ref_incid)
  
  # corresponding effectivenesss values
  if(EffFix == "N"){
    colsam <- sample(1:ncol(effmat), 1)
    effvals = effmat[cbind(i_vals_match, colsam)]
  }
  if(EffFix == "L"){
    effmat2 = apply(effmat, 1, quantile, probs = 0.025)
    effvals = effmat2[i_vals_match]
    effvals[effvals < 0] = 0
  }
  if(EffFix == "H"){
    effmat2 = apply(effmat, 1, quantile, probs = 0.975)
    effvals = effmat2[i_vals_match]
  }
  
  # insert into new map
  x2 <- x
  values(x2) = effvals
  return(x2)
}

# long term cost calculator (percentage of Phase 2 costs)
Phase3.cost.estimator <- function(Phase2_costs){
  Phase2_costs * 0.16
}
Phase4.cost.estimator <- function(Phase2_costs){
  Phase2_costs * 0.08
}

# cost discounting (fast programme)
# phase 1 and 2 costs are spread over 3 years and discount based on 3% per annum
# billed at the end of the year
cost.discounter <- function(P1_2, P3, P4, Disaster = "N"){
  
  # reistance emerges and nothing is done
  if(Disaster == "Resistance"){
    P1_2 = (P1_2 / 3) * (0.97^1) + 
      (P1_2 / 3) * (0.97^2) + 
      (P1_2 / 3) * (0.97^3)
    P3 = P3 * (0.97^4) +
      P3 * (0.97^5) +
      P3 * (0.97^6)
    P4 = P4 * (0.97^7) +
      P4 * (0.97^8) +
      P4 * (0.97^9) +
      P4 * (0.97^10) +
      P4 * (0.97^11) +
      P4 * (0.97^12) +
      P4 * (0.97^13)
  }
  
  # reistance emerges and re-releases are required
  if(Disaster == "Resistance_fixed"){
    P1_2 = (P1_2 / 3) * (0.97^1) + 
      (P1_2 / 3) * (0.97^2) + 
      (P1_2 / 3) * (0.97^3) +
      (P1_2 / 3) * (0.97^9) +
      (P1_2 / 3) * (0.97^10)
    P3 = P3 * (0.97^4) +
      P3 * (0.97^5) +
      P3 * (0.97^6) +
      P3 * (0.97^11) +
      P3 * (0.97^12) +
      P3 * (0.97^13)
    P4 = P4 * (0.97^7) +
      P4 * (0.97^8) +
      P4 * (0.97^14) +
      P4 * (0.97^15) +
      P4 * (0.97^16) +
      P4 * (0.97^17) +
      P4 * (0.97^18) +
      P4 * (0.97^19) +
      P4 * (0.97^20)
  }
  
  # need to replace Wolbachia strain in year 9
  if(Disaster == "Uncompetitive"){
    P1_2 = (P1_2 / 3) * (0.97^1) + 
      (P1_2 / 3) * (0.97^2) + 
      (P1_2 / 3) * (0.97^3) +
      (P1_2 / 3) * (0.97^4) + 
      (P1_2 / 3) * (0.97^5)
    P3 = P3 * (0.97^6) +
      P3 * (0.97^7) +
      P3 * (0.97^8)
    P4 = P4 * (0.97^9) +
      P4 * (0.97^10) +
      P4 * (0.97^11) +
      P4 * (0.97^12) +
      P4 * (0.97^13) +
      P4 * (0.97^14) +
      P4 * (0.97^15)
  }
  
  # Low coverage
  if(Disaster == "Low_coverage"){
    P1_2 = (P1_2 / 3) * (0.97^1) + 
      (P1_2 / 3) * (0.97^2) + 
      (P1_2 / 3) * (0.97^3)
    P3 = P3 * (0.97^4) +
      P3 * (0.97^5) +
      P3 * (0.97^6)
    P4 = P4 * (0.97^7) +
      P4 * (0.97^8) +
      P4 * (0.97^9) +
      P4 * (0.97^10) +
      P4 * (0.97^11) +
      P4 * (0.97^12) +
      P4 * (0.97^13)
  }
  
  # Low coverage that is then fixed
  if(Disaster == "Low_coverage_fixed"){
    P1_2 = (P1_2 / 3) * (0.97^1) + 
      (P1_2 / 3) * (0.97^2) + 
      (P1_2 / 3) * (0.97^3) +
      (P1_2 / 3) * (0.97^5)
    P3 = P3 * (0.97^4) +
      P3 * (0.97^6) +
      P3 * (0.97^7) +
      P3 * (0.97^8)
    P4 = P4 * (0.97^9) +
      P4 * (0.97^10) +
      P4 * (0.97^11) +
      P4 * (0.97^12) +
      P4 * (0.97^13) +
      P4 * (0.97^14) +
      P4 * (0.97^15)
  }
    
    # transition to cheap monitoring (passive disease surveillance) after Phase 3
  if(Disaster == "Passive_monitoring"){
    P1_2 = (P1_2 / 3) * (0.97^1) + 
      (P1_2 / 3) * (0.97^2) + 
      (P1_2 / 3) * (0.97^3)
    P3 = P3 * (0.97^4) +
      P3 * (0.97^5) +
      P3 * (0.97^6)
    P4 = 0
  }
    
    # innovation efficiencies and economies of scale
  if(Disaster == "Innovation"){
    P1_2 = (P1_2 / 3) * (0.97^1) + 
      (P1_2 / 3) * (0.97^2) + 
      (P1_2 / 3) * (0.97^3)
    P3 = P3 * (0.97^4) +
      P3 * (0.97^5) +
      P3 * (0.97^6)
    P4 = P4 * (0.97^7) +
      P4 * (0.97^8) +
      P4 * (0.97^9) +
      P4 * (0.97^10) +
      P4 * (0.97^11) +
      P4 * (0.97^12) +
      P4 * (0.97^13)
    
    P1_2 = 0.5 * P1_2
    P3 = 0.5 * P3
    P4 = 0.5 * P4
  }
  
  
  if(Disaster == "Special"){
    # just extract median prediction then aggregate across the site
    P1_2 = sum(apply(P1_2, 1, median, na.rm = T), na.rm = T)
    P3 = sum(apply(P3, 1, median, na.rm = T), na.rm = T)
    P4 = sum(apply(P4, 1, median, na.rm = T), na.rm = T)
    # now time components
    P1_2 = c((P1_2 / 3) * (0.97^1),
             (P1_2 / 3) * (0.97^2),
             (P1_2 / 3) * (0.97^3),
             P3 * (0.97^4),
             P3 * (0.97^5),
             P3 * (0.97^6),
             P4 * (0.97^7),
             P4 * (0.97^8),
             P4 * (0.97^9),
             P4 * (0.97^10),
             P4 * (0.97^11),
             P4 * (0.97^12),
             P4 * (0.97^13))
    P3 = rep(0, length(P1_2))
    P4 = rep(0, length(P1_2))
  }
  
  if(Disaster == "N"){
    P1_2 = (P1_2 / 3) * (0.97^1) + 
      (P1_2 / 3) * (0.97^2) + 
      (P1_2 / 3) * (0.97^3)
    P3 = P3 * (0.97^4) +
      P3 * (0.97^5) +
      P3 * (0.97^6)
    P4 = P4 * (0.97^7) +
      P4 * (0.97^8) +
      P4 * (0.97^9) +
      P4 * (0.97^10) +
      P4 * (0.97^11) +
      P4 * (0.97^12) +
      P4 * (0.97^13)
  }
  
  
  # total cost for fast programme
  T_cost <- P1_2 + P3 + P4
  return(T_cost)
}

cost.discounter.sequential <- function(P2, P3, P4, startyear, Disaster = "N"){
  if(Disaster == "Special"){
    # median and sum over area
    P2 = sum(apply(P2, 1, median, na.rm = T), na.rm = T)
    P3 = sum(apply(P3, 1, median, na.rm = T), na.rm = T)
    P4 = sum(apply(P4, 1, median, na.rm = T), na.rm = T)
    
    P2 = c(P2 * (0.97^(1 + startyear)),
           P3 * (0.97^(2 + startyear)),
           P3 * (0.97^(3 + startyear)),
           P3 * (0.97^(4 + startyear)),
           P4 * (0.97^(5 + startyear)),
           P4 * (0.97^(6 + startyear)),
           P4 * (0.97^(7 + startyear)),
           P4 * (0.97^(8 + startyear)),
           P4 * (0.97^(9 + startyear)),
           P4 * (0.97^(10 + startyear)),
           P4 * (0.97^(11 + startyear)))
    P3 = rep(0, length(P2))
    P4 = rep(0, length(P2))
  }else{
    P2 = P2 * (0.97^(1 + startyear))
    P3 = P3 * (0.97^(2 + startyear)) +
      P3 * (0.97^(3 + startyear)) +
      P3 * (0.97^(4 + startyear))
    P4 = P4 * (0.97^(5 + startyear)) +
      P4 * (0.97^(6 + startyear)) +
      P4 * (0.97^(7 + startyear)) +
      P4 * (0.97^(8 + startyear)) +
      P4 * (0.97^(9 + startyear)) +
      P4 * (0.97^(10 + startyear)) +
      P4 * (0.97^(11 + startyear))
  }
  
    
    # total cost for fast programme
    T_cost <- P2 + P3 + P4
    return(T_cost)
}

benefit.discounter <- function(yearVal, startyear, Disaster = "N", yearVal_LC = NULL){
  
  if(Disaster == "Low_coverage"){
    yearVal_tot = yearVal * (0.97^(1 + startyear)) +
      yearVal * (0.97^(2 + startyear)) +
      yearVal * (0.97^(3 + startyear)) +
      yearVal * (0.97^(4 + startyear)) +
      yearVal * (0.97^(5 + startyear)) +
      yearVal * (0.97^(6 + startyear)) +
      yearVal * (0.97^(7 + startyear)) +
      yearVal * (0.97^(8 + startyear)) +
      yearVal * (0.97^(9 + startyear)) +
      yearVal * (0.97^(10 + startyear))
  }
  
  if(Disaster == "Low_coverage_fixed"){
    yearVal_tot = yearVal_LC * (0.97^(1 + startyear)) +
      yearVal_LC * (0.97^(2 + startyear)) +
      yearVal * (0.97^(3 + startyear)) +
      yearVal * (0.97^(4 + startyear)) +
      yearVal * (0.97^(5 + startyear)) +
      yearVal * (0.97^(6 + startyear)) +
      yearVal * (0.97^(7 + startyear)) +
      yearVal * (0.97^(8 + startyear)) +
      yearVal * (0.97^(9 + startyear)) +
      yearVal * (0.97^(10 + startyear)) +
      yearVal * (0.97^(11 + startyear)) +
      yearVal * (0.97^(12 + startyear))
  }
  
  
  if(Disaster == "Resistance"){
    yearVal_tot = yearVal * (0.97^(1 + startyear)) +
      yearVal * (0.97^(2 + startyear)) +
      yearVal * (0.97^(3 + startyear)) +
      yearVal * (0.97^(4 + startyear))
  }
  
  if(Disaster == "Resistance_fixed"){
    yearVal_tot = yearVal * (0.97^(1 + startyear)) +
      yearVal * (0.97^(2 + startyear)) +
      yearVal * (0.97^(3 + startyear)) +
      yearVal * (0.97^(4 + startyear)) +
      yearVal * (0.97^(8 + startyear)) +
      yearVal * (0.97^(9 + startyear)) +
      yearVal * (0.97^(10 + startyear)) +
      yearVal * (0.97^(11 + startyear)) +
      yearVal * (0.97^(12 + startyear)) +
      yearVal * (0.97^(13 + startyear)) +
      yearVal * (0.97^(14 + startyear)) +
      yearVal * (0.97^(15 + startyear)) +
      yearVal * (0.97^(16 + startyear)) +
      yearVal * (0.97^(17 + startyear))
  }
  
  if(Disaster == "Uncompetitive"){
    yearVal_tot = yearVal * (0.97^(3 + startyear)) +
      yearVal * (0.97^(4 + startyear)) +
      yearVal * (0.97^(5 + startyear)) +
      yearVal * (0.97^(6 + startyear)) +
      yearVal * (0.97^(7 + startyear)) +
      yearVal * (0.97^(8 + startyear)) +
      yearVal * (0.97^(9 + startyear)) +
      yearVal * (0.97^(10 + startyear)) +
      yearVal * (0.97^(11 + startyear)) +
      yearVal * (0.97^(12 + startyear))
  }
  
  if(Disaster == "Passive_monitoring"){
    yearVal_tot = yearVal * (0.97^(1 + startyear)) +
      yearVal * (0.97^(2 + startyear)) +
      yearVal * (0.97^(3 + startyear)) +
      yearVal * (0.97^(4 + startyear)) +
      yearVal * (0.97^(5 + startyear)) +
      yearVal * (0.97^(6 + startyear)) +
      yearVal * (0.97^(7 + startyear)) +
      yearVal * (0.97^(8 + startyear)) +
      yearVal * (0.97^(9 + startyear)) +
      yearVal * (0.97^(10 + startyear))
  }
  
  if(Disaster == "Innovation"){
    yearVal_tot = yearVal * (0.97^(1 + startyear)) +
      yearVal * (0.97^(2 + startyear)) +
      yearVal * (0.97^(3 + startyear)) +
      yearVal * (0.97^(4 + startyear)) +
      yearVal * (0.97^(5 + startyear)) +
      yearVal * (0.97^(6 + startyear)) +
      yearVal * (0.97^(7 + startyear)) +
      yearVal * (0.97^(8 + startyear)) +
      yearVal * (0.97^(9 + startyear)) +
      yearVal * (0.97^(10 + startyear))
  }
  
  if(Disaster == "Special"){
    yearVal = sum(apply(yearVal, 1, median, na.rm = T), na.rm = T)
    # 10 years of benefits
    yearVal_tot = c(yearVal * (0.97^(1 + startyear)),
                    yearVal * (0.97^(2 + startyear)),
                    yearVal * (0.97^(3 + startyear)),
                    yearVal * (0.97^(4 + startyear)),
                    yearVal * (0.97^(5 + startyear)),
                    yearVal * (0.97^(6 + startyear)),
                    yearVal * (0.97^(7 + startyear)),
                    yearVal * (0.97^(8 + startyear)),
                    yearVal * (0.97^(9 + startyear)),
                    yearVal * (0.97^(10 + startyear)))
  }
  
  if(Disaster == "N"){
    # 10 years of benefits
    yearVal_tot = yearVal * (0.97^(1 + startyear)) +
      yearVal * (0.97^(2 + startyear)) +
      yearVal * (0.97^(3 + startyear)) +
      yearVal * (0.97^(4 + startyear)) +
      yearVal * (0.97^(5 + startyear)) +
      yearVal * (0.97^(6 + startyear)) +
      yearVal * (0.97^(7 + startyear)) +
      yearVal * (0.97^(8 + startyear)) +
      yearVal * (0.97^(9 + startyear)) +
      yearVal * (0.97^(10 + startyear))
  }
  
  return(yearVal_tot)
}

# quick quantile raster calculator - requires a list of rasters, returns a list of rasters
# with all the same quantile value
raster.quantile <- function(ras, quantperc){
  holdmat <- matrix(NA, nrow = length(as.vector(ras[[1]])), ncol = length(ras))
  for(r in 1:length(ras)){
    holdmat[, r] = as.vector(ras[[r]])
  }
  # to quantiles
  holdmat = apply(holdmat, 1, quantile, probs = quantperc, na.rm = T)
  # insert back into raster
  tempras <- ras[[1]]
  na_ind <- as.vector(ras[[1]]) / as.vector(ras[[1]])
  values(tempras) = holdmat * na_ind
  tempras2 = list()
  for(r in 1:length(ras)){
    tempras2[[r]] = tempras
  }
  return(tempras2)
}

# turns mid high and low dengue case cost estiamtes into 100 uncertainty samples
dCOI_sample <- function(costs, DisCostFix, NUM){
  costs2 <- apply(costs, 2, function(x) rnorm(n = NUM, 
                                              mean = x[2], 
                                              sd = 0.5 * (0.5 * (x[2] - x[1]) + (0.5 * (x[3] - x[2])))))
  # make sure all positive
  costs2[costs2 < 0] = 0
  
  # if tornado plot
  if(DisCostFix == "H"){
    costs2 = apply(costs2, 2, function(x) rep(quantile(x, probs = 0.975), length(x)))
  }
  if(DisCostFix == "L"){
    costs2 = apply(costs2, 2, function(x) rep(quantile(x, probs = 0.025), length(x)))
  }
  return(data.frame(costs2))
}

# solve neighborhood issues with admin unit FAreas
raster.close.fix <- function(Aras){
  c_Aras <- coordinates(Aras)
  Aras_v <- as.vector(Aras)
  
  # trim to non-NA pix
  c_Aras = c_Aras[!is.na(Aras_v), ]
  Aras_v = Aras_v[!is.na(Aras_v)]
  
  probPix_coords <- c_Aras[Aras_v == 1, ]
  ok_coords <- c_Aras[Aras_v != 1, ]
  ok_vals <- Aras_v[Aras_v != 1]
  
  cor_val <- rep(NA, nrow(probPix_coords))
  for(r in 1:nrow(probPix_coords)){
    # find nearest non NA pix and assign the nearest admin unit boundary
    cor_val[r] = ok_vals[which.min(pointDistance(matrix(probPix_coords[r, ], ncol = 2), ok_coords, lonlat = TRUE))]
  }
  
  # replace raster values
  Aras_vals <- as.vector(Aras)
  Aras_vals[(!is.na(Aras_vals)) &(Aras_vals == 1)] = cor_val
  values(Aras) = Aras_vals
  return(Aras)
}



# Area is a shape file of the area in which wolbachia is to be deployed
# for testing:
# FArea <- rbind(STAN, TAN, BEK, DEP, BOG)
# EP1 <- mask(crop(worldpop, JAK), JAK)
# Disaster options:
# Disaster = Resistance
# Disaster = Resistance_fixed
# Disaster = Uncompetitive
# Disaster = Passive_monitoring
# Disaster = Low_coverage
# Disaster = Low_coverage_fixed
# Disaster = Special
# YOG_city = FALSE ; YOG_sar = FALSE ; BurdFix = "N" ; EffFix = "N" ; ProgCostFix = "N" ; DisCostFix = "N" ; Disaster = "N" ; Admin_constrain = FALSE
wol.scale.up <- function(FArea, YOG_city = FALSE, YOG_sar = FALSE,
                         BurdFix = "N", EffFix = "N", ProgCostFix = "N", DisCostFix = "N", 
                         Disaster = "N", Admin_constrain = FALSE){
  ###########################################################
  ### Part 1- predicting cases, DALYs and costs saved by Wolbachia
  ###########################################################
  
  ## A) raster pre-processing to focus the area
  # population
  Apop = mask(crop(worldpop, FArea), FArea)
  Apopv = as.vector(Apop)
  # create an index of pixels in which wolbachia will actually be deployed
  pixIND <- (!is.na(as.vector(Apop))) & (Apopv > 10)
  # burden
  burdmapList_sm_f <- lapply(burdmapList_sm, newburd.process, FArea, Apop)
  burdmapList_am_f <- lapply(burdmapList_am, newburd.process, FArea, Apop)
  burdmapList_h_f <- lapply(burdmapList_h, newburd.process, FArea, Apop)
  burdmapList_f_f <- lapply(burdmapList_f, newburd.process, FArea, Apop)
  burdmapList_symp_f <- lapply(burdmapList_symp, newburd.process, FArea, Apop)
  
  # and adjust if doing tornado plots
  if(BurdFix != "N"){
    if(BurdFix == "H"){qperc = 0.975}else{qperc = 0.025}
    burdmapList_sm_f = raster.quantile(burdmapList_sm_f, quantperc = qperc)
    burdmapList_am_f = raster.quantile(burdmapList_am_f, quantperc = qperc)
    burdmapList_h_f = raster.quantile(burdmapList_h_f, quantperc = qperc)
    burdmapList_f_f = raster.quantile(burdmapList_f_f, quantperc = qperc)
    burdmapList_symp_f = raster.quantile(burdmapList_symp_f, quantperc = qperc)
  }
  
  # effectiveness maps
  if(Disaster == "Low_coverage"){
    eff_maps <- lapply(burdmapList_symp_f, find.eff, effmat = ensem_pred_50, EffFix = EffFix)
  }else{
    if(Disaster == "Low_coverage_fixed"){
      eff_maps_LC <- lapply(burdmapList_symp_f, find.eff, effmat = ensem_pred_50, EffFix = EffFix)
      eff_maps <- lapply(burdmapList_symp_f, find.eff, effmat = ensem_pred_100, EffFix = EffFix)
    }else{
      eff_maps <- lapply(burdmapList_symp_f, find.eff, effmat = ensem_pred_100, EffFix = EffFix)
    }
  }
  
  
  
  
  ## B) calculate baseline burden and cases averted summaries
  # each row is a pixel in the map, each column is a realisation
  baseline_case_mat <- matrix(NA, nrow = length(burdmapList_sm_f), ncol = 4)
  avert_case_mat <- matrix(NA, nrow = length(burdmapList_sm_f), ncol = 4)
  
  pixros <- length(as.vector(burdmapList_sm_f[[1]]))
  colnums <- length(burdmapList_sm_f)
  baseline_case_sm <- baseline_case_am <- baseline_case_h <- baseline_case_f <- matrix(NA, nrow = pixros, ncol = colnums)
  avert_case_sm <- avert_case_am <- avert_case_h <- avert_case_f <- matrix(NA, nrow = pixros, ncol = colnums)
  if(Disaster == "Low_coverage_fixed"){
    avert_case_sm_LC <- avert_case_am_LC <- avert_case_h_LC <- avert_case_f_LC <- matrix(NA, nrow = pixros, ncol = colnums)
  }
  # now go through filling out
  for(i in 1:length(burdmapList_sm_f)){
    # Self managed
    baseline_case_sm[, i] = as.vector(burdmapList_sm_f[[i]] * Apop)
    avert_case_sm[, i] = as.vector(burdmapList_sm_f[[i]] * (eff_maps[[i]]) * Apop) * pixIND
    if(Disaster == "Low_coverage_fixed"){avert_case_sm_LC[, i] = as.vector(burdmapList_sm_f[[i]] * (eff_maps_LC[[i]]) * Apop) * pixIND}
    
    # Ambulatory
    baseline_case_am[, i] = as.vector(burdmapList_am_f[[i]] * Apop)
    avert_case_am[, i] = as.vector(burdmapList_am_f[[i]] * (eff_maps[[i]]) * Apop) * pixIND
    if(Disaster == "Low_coverage_fixed"){avert_case_am_LC[, i] = as.vector(burdmapList_am_f[[i]] * (eff_maps_LC[[i]]) * Apop) * pixIND}
    
    # Hospitalised
    baseline_case_h[, i] = as.vector(burdmapList_h_f[[i]] * Apop)
    avert_case_h[, i] = as.vector(burdmapList_h_f[[i]] * (eff_maps[[i]]) * Apop) * pixIND
    if(Disaster == "Low_coverage_fixed"){avert_case_h_LC[, i] = as.vector(burdmapList_h_f[[i]] * (eff_maps_LC[[i]]) * Apop) * pixIND}
    
    # Fatal
    baseline_case_f[, i] = as.vector(burdmapList_f_f[[i]] * Apop)
    avert_case_f[, i] = as.vector(burdmapList_f_f[[i]] * (eff_maps[[i]]) * Apop) * pixIND
    if(Disaster == "Low_coverage_fixed"){avert_case_f_LC[, i] = as.vector(burdmapList_f_f[[i]] * (eff_maps_LC[[i]]) * Apop) * pixIND}
  }
  
  # clear up
  rm(burdmapList_sm_f, burdmapList_am_f, burdmapList_h_f, burdmapList_f_f)
  
  t_burd <- baseline_case_sm + baseline_case_am + baseline_case_h + baseline_case_f
  t_burd_avert <- avert_case_sm + avert_case_am + avert_case_h + avert_case_f
  if(Disaster == "Low_coverage_fixed"){t_burd_avert_LC <- avert_case_sm_LC + avert_case_am_LC + avert_case_h_LC + avert_case_f_LC}
  
  
  ## C) Treatment costs averted
  # treatment costs averted - will be updated
  Dengue_costs <- data.frame(Dir_Hos = c(220.34, 296.54, 372.74),
                             Dir_Amb = c(11.31, 20.78, 30.38),
                             Dir_NM = c(2.25, 4.31, 6.37),
                             Indir_Hos = c(8.87, 21.82, 34.77),
                             Indir_Amb = c(0, 2.26, 4.57),
                             Indir_NM = c(0, 3.21, 7.09),
                             Fatal_Child = c(98374, 106247, 125622),
                             Fatal_Adult = c(63943, 69061, 81654))
  row.names(Dengue_costs) = c("low", "mid", "high")
  
  Dengue_costs <- dCOI_sample(Dengue_costs, DisCostFix, NUM = ncol(t_burd))
  
  
  # Direct medical costs
  Aver_costs_direct = ((avert_case_h + avert_case_f) * Dengue_costs$Dir_Hos) +
                      (avert_case_am * Dengue_costs$Dir_Amb) +
                      (avert_case_sm * Dengue_costs$Dir_NM)
  
  # Indirect costs
  Aver_costs_INdirect = ((avert_case_h + avert_case_f) * Dengue_costs$Indir_Hos) +
                        (avert_case_am * Dengue_costs$Indir_Amb) +
                        (avert_case_sm * Dengue_costs$Indir_NM)
  # Fatal indirect costs - assume fatalities are 50% adults, 50% children
  Aver_costs_INdirect_fatal <- 0.5 * avert_case_f * Dengue_costs$Fatal_Child + 0.5 * avert_case_f * Dengue_costs$Fatal_Adult
  
  if(Disaster == "Low_coverage_fixed"){
    Aver_costs_direct_LC = ((avert_case_h_LC + avert_case_f_LC) * Dengue_costs$Dir_Hos) +
      (avert_case_am_LC * Dengue_costs$Dir_Amb) +
      (avert_case_sm_LC * Dengue_costs$Dir_NM)
    Aver_costs_INdirect_LC = ((avert_case_h_LC + avert_case_f_LC) * Dengue_costs$Indir_Hos) +
      (avert_case_am_LC * Dengue_costs$Indir_Amb) +
      (avert_case_sm_LC * Dengue_costs$Indir_NM)
    Aver_costs_INdirect_fatal_LC <- 0.5 * avert_case_f_LC * Dengue_costs$Fatal_Child + 0.5 * avert_case_f_LC * Dengue_costs$Fatal_Adult
  }
  
  ## D) DALYs averted
  
  # DALY parameters for non severe and severe (Zheng et al.)
  DALY_NS = 0.031
  DALY_S = 0.032
  # YLDs before
  YLDs_before = (baseline_case_sm + baseline_case_am) * DALY_NS +
                (baseline_case_h + baseline_case_f) * DALY_S
  # YLLs before
  YLLs_before = apply(baseline_case_f, 2, function(x) x * sum(Indoage$Prop_dengue * LI_discount))
  
  rm(baseline_case_sm, baseline_case_am, baseline_case_h, baseline_case_f)
  
  # YLDs averted
  YLDs_averted = (avert_case_sm + avert_case_am) * DALY_NS +
                 (avert_case_h + avert_case_f) * DALY_S
  # YLLs averted
  YLLs_averted = apply(avert_case_f, 2, function(x) x * sum(Indoage$Prop_dengue * LI_discount))
  rm(avert_case_sm, avert_case_am, avert_case_h, avert_case_f)
  
  if(Disaster == "Low_coverage_fixed"){
    YLDs_averted_LC = (avert_case_sm_LC + avert_case_am_LC) * DALY_NS + (avert_case_h_LC + avert_case_f_LC) * DALY_S
    YLLs_averted_LC = apply(avert_case_f_LC, 2, function(x) x * sum(Indoage$Prop_dengue * LI_discount))
    rm(avert_case_sm_LC, avert_case_am_LC, avert_case_h_LC, avert_case_f_LC)
  }
  
  
  
  # final DALY calc
  DALY_before = YLDs_before + YLLs_before
  DALY_averted = YLDs_averted + YLLs_averted
  if(Disaster == "Low_coverage_fixed"){DALY_averted_LC = YLDs_averted_LC + YLLs_averted_LC}
  rm(YLDs_before, YLLs_before, YLDs_averted, YLLs_averted)
  
  
  
  
  ###########################################################
  ### Part 2- predicting Wolbachia programme costs
  ###########################################################
  
  ## A) calculate total km2 to do and how much needs a Phase 1 (for sequential programmes)
  Atodo <- as.vector(Apop)[pixIND]
  Atodo_km <- length(Atodo) / 100
  
  ## B) fast programme cost calculation Phases 1-4
  # in YOG SAR scenario there is already enoguh capacity to strat at phase 2
  if(YOG_sar){
    Phase1_2_spend_fast <- costmod.predict.wrapper(Atodo, Pphase = 2, ProgCostFix, NUM = ncol(t_burd)) 
  }else{
    Phase1_2_spend_fast <- costmod.predict.wrapper(Atodo, Pphase = 1, ProgCostFix, NUM = ncol(t_burd)) 
  }
  # note that Pphase = 1 actually gives a cost of a Phase 1 + Phase 2 programme 
  # under the new defintion
  Phase2_spend_fast <- costmod.predict.wrapper(Atodo, Pphase = 2, ProgCostFix, NUM = ncol(t_burd)) # just dummy variable 
  Phase3_spend_fast <- Phase3.cost.estimator(Phase2_spend_fast) # gives spend per year in Phase 3
  Phase4_spend_fast <- Phase4.cost.estimator(Phase2_spend_fast) # gives spend per year in Phase 4
  
  # total fast programme costs
  T_cost_fast <- cost.discounter(Phase1_2_spend_fast, Phase3_spend_fast, Phase4_spend_fast, Disaster)
  if(Disaster == "Special"){
    save(T_cost_fast, file = "05_Costs/Scale_Up/Cost_bene_cum/T_cost_fast.RData")
    T_cost_fast <- cost.discounter(Phase1_2_spend_fast, Phase3_spend_fast, Phase4_spend_fast, Disaster = "N")
    }
  
  # discounting benefits for fast programme
  if(Disaster == "Special"){
    DALY_averted_fast_discounted <- benefit.discounter(DALY_averted, startyear = 3, Disaster)
    Aver_costs_direct_fast_discounted <- benefit.discounter(Aver_costs_direct, startyear = 3, Disaster)
    Aver_costs_INdirect_fast_discounted <- benefit.discounter(Aver_costs_INdirect, startyear = 3, Disaster)
    Aver_costs_INdirect_fatal_fast_discounted <- benefit.discounter(Aver_costs_INdirect_fatal, startyear = 3, Disaster)
    save(DALY_averted_fast_discounted, file = "05_Costs/Scale_Up/Cost_bene_cum/DALY_averted_fast_discounted.RData")
    save(Aver_costs_direct_fast_discounted, file = "05_Costs/Scale_Up/Cost_bene_cum/Aver_costs_direct_fast_discounted.RData")
    save(Aver_costs_INdirect_fast_discounted, file = "05_Costs/Scale_Up/Cost_bene_cum/Aver_costs_INdirect_fast_discounted.RData")
    save(Aver_costs_INdirect_fatal_fast_discounted, file = "05_Costs/Scale_Up/Cost_bene_cum/Aver_costs_INdirect_fatal_fast_discounted.RData")
    
    DALY_averted_fast_discounted <- benefit.discounter(DALY_averted, startyear = 3, Disaster = "N")[pixIND, ]
    Aver_costs_direct_fast_discounted <- benefit.discounter(Aver_costs_direct, startyear = 3, Disaster = "N")[pixIND, ]
    Aver_costs_INdirect_fast_discounted <- benefit.discounter(Aver_costs_INdirect, startyear = 3, Disaster = "N")[pixIND, ]
    Aver_costs_INdirect_fatal_fast_discounted <- benefit.discounter(Aver_costs_INdirect_fatal, startyear = 3, Disaster = "N")[pixIND, ]
  }else{
    if(Disaster == "Low_coverage_fixed"){
      DALY_averted_fast_discounted <- benefit.discounter(DALY_averted, startyear = 3, Disaster, yearVal_LC = DALY_averted_LC)[pixIND, ]
      Aver_costs_direct_fast_discounted <- benefit.discounter(Aver_costs_direct, startyear = 3, Disaster, yearVal_LC = Aver_costs_direct_LC)[pixIND, ]
      Aver_costs_INdirect_fast_discounted <- benefit.discounter(Aver_costs_INdirect, startyear = 3, Disaster, yearVal_LC = Aver_costs_INdirect_LC)[pixIND, ]
      Aver_costs_INdirect_fatal_fast_discounted <- benefit.discounter(Aver_costs_INdirect_fatal, startyear = 3, Disaster, yearVal_LC = Aver_costs_INdirect_fatal_LC)[pixIND, ]
    }else{
      DALY_averted_fast_discounted <- benefit.discounter(DALY_averted, startyear = 3, Disaster)[pixIND, ]
      Aver_costs_direct_fast_discounted <- benefit.discounter(Aver_costs_direct, startyear = 3, Disaster)[pixIND, ]
      Aver_costs_INdirect_fast_discounted <- benefit.discounter(Aver_costs_INdirect, startyear = 3, Disaster)[pixIND, ]
      Aver_costs_INdirect_fatal_fast_discounted <- benefit.discounter(Aver_costs_INdirect_fatal, startyear = 3, Disaster)[pixIND, ]
      }
    }
  
  # NET costs (fast programme)
  T_cost_fast_NET_direct <- T_cost_fast - Aver_costs_direct_fast_discounted
  T_cost_fast_NET_D_IND <- T_cost_fast - Aver_costs_direct_fast_discounted - Aver_costs_INdirect_fast_discounted
  T_cost_fast_NET_D_IND_F <- T_cost_fast - Aver_costs_direct_fast_discounted - Aver_costs_INdirect_fast_discounted - Aver_costs_INdirect_fatal_fast_discounted
  
  
  
  ## C) slow programme
  # which pixels are done sequentially in extra Phase 2s?
  # if needs to be completed in 10 years then 2 years setup and 8 separate Phase 2 rounds
  # with each Phase 2 round needing to cover
  eligiblePix <- data.frame(pixID = 1:length(Apopv), pop = Apopv)
  eligiblePix = eligiblePix[pixIND, ]
  # divide into 8 rounds (years 3-10)
  # unless in Yog city where the slow programme effectively just happens in 2 separate stages
  if(YOG_city){
    eligiblePix$roundnum = ceiling(2 * (1:nrow(eligiblePix)) / nrow(eligiblePix))
  }else{
    # if the area shapefile contains multiple admin units the slow programme
    # will be constrained to 1 stage per admin unit, otherwise can categorise pixels continuously
    if(Admin_constrain){
      # extract admin unit from each pixel
      Aras <- rasterize(FArea, field = "DESA", Apop > 0, update = TRUE)
      # for spatial conformity
      Aras = raster.close.fix(Aras)
      Aras = mask(Aras, Apop)
      # now assign admin units to eligible pix
      eligiblePix$A3 <- as.vector(Aras)[pixIND]
      # now change pop to admin unit pop
      AuPop <- aggregate(eligiblePix$pop, by = list(eligiblePix$A3), FUN = mean)
      eligiblePix$pop = AuPop$x[match(eligiblePix$A3, AuPop$Group.1)]
      
      # sort and assign staging
      eligiblePix = eligiblePix[order(eligiblePix$pop, decreasing = TRUE), ]
      atab <- data.frame(area = unique(eligiblePix$A3),
                         stage = ceiling(8 * (1:length(unique(eligiblePix$A3))) / 
                                           length(unique(eligiblePix$A3))))
      eligiblePix$roundnum = atab$stage[match(eligiblePix$A3, atab$area)]
      eligiblePix$roundnum = ceiling(8 * (1:nrow(eligiblePix)) / nrow(eligiblePix))
    }else{
      eligiblePix = eligiblePix[order(eligiblePix$pop, decreasing = TRUE), ]
      eligiblePix$roundnum = ceiling(8 * (1:nrow(eligiblePix)) / nrow(eligiblePix))
    }
  }
  
  
  # work out Phase 1+2 costs for central area (Round 1)
  if(YOG_sar){
    Phase1_2_spend_slow_central <- costmod.predict.wrapper(eligiblePix$pop[eligiblePix$roundnum == 1], 
                                                           Pphase = 2, ProgCostFix, NUM = ncol(t_burd))
  }else{
    Phase1_2_spend_slow_central <- costmod.predict.wrapper(eligiblePix$pop[eligiblePix$roundnum == 1], 
                                                           Pphase = 1, ProgCostFix, NUM = ncol(t_burd))
  }
  
  Phase2_spend_slow_central <- costmod.predict.wrapper(eligiblePix$pop[eligiblePix$roundnum == 1], 
                                                       Pphase = 2, ProgCostFix, NUM = ncol(t_burd))
  # and with long term monitoring?
  Phase3_spend_slow_central <- Phase3.cost.estimator(Phase2_spend_slow_central)
  Phase4_spend_slow_central <- Phase4.cost.estimator(Phase2_spend_slow_central)
  # now discount and sum cost over central area
  T_cost_slow_central <- cost.discounter(Phase1_2_spend_slow_central,
                                         Phase3_spend_slow_central,
                                         Phase4_spend_slow_central,
                                         Disaster)
  if(Disaster == "Special"){
    save(T_cost_slow_central, file = "05_Costs/Scale_Up/Cost_bene_cum/T_cost_slow_central.RData")
    T_cost_slow_central <- cost.discounter(Phase1_2_spend_slow_central,
                                           Phase3_spend_slow_central,
                                           Phase4_spend_slow_central,
                                           Disaster = "N")
  }
  T_cost_seq = matrix(0, nrow = nrow(eligiblePix), ncol = ncol(T_cost_fast))
  T_cost_seq[eligiblePix$roundnum == 1, ] = T_cost_slow_central
  
  # set up matrices to store discounted benefits over time
  DALY_averted_slow_discounted = matrix(0, nrow = nrow(eligiblePix), ncol = ncol(T_cost_fast))
  Aver_costs_direct_slow_discounted = matrix(0, nrow = nrow(eligiblePix), ncol = ncol(T_cost_fast))
  Aver_costs_INdirect_slow_discounted = matrix(0, nrow = nrow(eligiblePix), ncol = ncol(T_cost_fast))
  Aver_costs_INdirect_fatal_slow_discounted = matrix(0, nrow = nrow(eligiblePix), ncol = ncol(T_cost_fast))
  
  
  # and discount benefits over central area
  if(Disaster == "Special"){
    h1 <- h2 <- h3 <- h4 <- data.frame(year = 1:20,
                                       costs = rep(0, 20))
    h1$costs[4:13] <- benefit.discounter(DALY_averted[pixIND, ][eligiblePix$roundnum == 1, ], startyear = 4, Disaster)
    h2$costs[4:13] <- benefit.discounter(DALY_averted[pixIND, ][eligiblePix$roundnum == 1, ], startyear = 4, Disaster)
    h3$costs[4:13] <- benefit.discounter(DALY_averted[pixIND, ][eligiblePix$roundnum == 1, ], startyear = 4, Disaster)
    h4$costs[4:13] <- benefit.discounter(DALY_averted[pixIND, ][eligiblePix$roundnum == 1, ], startyear = 4, Disaster)
  }
  DALY_averted_slow_discounted[eligiblePix$roundnum == 1, ] <- benefit.discounter(DALY_averted[pixIND, ][eligiblePix$roundnum == 1, ], startyear = 3)
  Aver_costs_direct_slow_discounted[eligiblePix$roundnum == 1, ] <- benefit.discounter(Aver_costs_direct[pixIND, ][eligiblePix$roundnum == 1, ], startyear = 3)
  Aver_costs_INdirect_slow_discounted[eligiblePix$roundnum == 1, ] <- benefit.discounter(Aver_costs_INdirect[pixIND, ][eligiblePix$roundnum == 1, ], startyear = 3)
  Aver_costs_INdirect_fatal_slow_discounted[eligiblePix$roundnum == 1, ] <- benefit.discounter(Aver_costs_INdirect_fatal[pixIND, ][eligiblePix$roundnum == 1, ], startyear = 3)
  
  
  
  # now work out costs and benefits separately for each sequential area
  for(i in 2:max(eligiblePix$roundnum)){
    # work out costs for Phases2-4
    Phase2_spend_slow_seq <- costmod.predict.wrapper(eligiblePix$pop[eligiblePix$roundnum == i], 
                                                         Pphase = 2, ProgCostFix, NUM = ncol(t_burd))
    Phase3_spend_slow_seq = Phase3.cost.estimator(Phase2_spend_slow_seq)
    Phase4_spend_slow_seq = Phase4.cost.estimator(Phase2_spend_slow_seq)
    # now discount with appropriate time horizon
    if(Disaster == "Special"){
      if(i == 2){
        h5 <- data.frame(year = 1:20,
                         costs = rep(0, 20))
        h5$costs[(i + 2):(i + 12)] = cost.discounter.sequential(Phase2_spend_slow_seq,
                                                                Phase3_spend_slow_seq,
                                                                Phase4_spend_slow_seq,
                                                                startyear = i + 2,
                                                                Disaster)
      }else{
        h5$costs[(i + 2):(i + 12)] = h5$costs[(i + 2):(i + 12)] + cost.discounter.sequential(Phase2_spend_slow_seq,
                                                                                            Phase3_spend_slow_seq,
                                                                                            Phase4_spend_slow_seq,
                                                                                            startyear = i + 2,
                                                                                            Disaster)
          
      }
    }else{
      T_cost_seq[eligiblePix$roundnum == i, ] <- cost.discounter.sequential(Phase2_spend_slow_seq,
                                                                            Phase3_spend_slow_seq,
                                                                            Phase4_spend_slow_seq,
                                                                            startyear = i + 2,
                                                                            Disaster)
    }
    
    # now discount benefits over same time horizon
    if(Disaster == "Special"){
      h1$costs[(i + 3):(i + 12)] = h1$costs[(i + 3):(i + 12)] + benefit.discounter(yearVal = DALY_averted[pixIND, ][eligiblePix$roundnum == i, ], startyear = i + 3, Disaster)
      h2$costs[(i + 3):(i + 12)] = h2$costs[(i + 3):(i + 12)] + benefit.discounter(yearVal = Aver_costs_direct[pixIND, ][eligiblePix$roundnum == i, ], startyear = i + 3, Disaster)
      h3$costs[(i + 3):(i + 12)] = h3$costs[(i + 3):(i + 12)] + benefit.discounter(yearVal = Aver_costs_INdirect[pixIND, ][eligiblePix$roundnum == i, ], startyear = i + 3, Disaster)
      h4$costs[(i + 3):(i + 12)] = h4$costs[(i + 3):(i + 12)] + benefit.discounter(yearVal = Aver_costs_INdirect_fatal[pixIND, ][eligiblePix$roundnum == i, ], startyear = i + 3, Disaster)
    }
    DALY_averted_slow_discounted[eligiblePix$roundnum == i, ] = benefit.discounter(yearVal = DALY_averted[pixIND, ][eligiblePix$roundnum == i, ], 
                                                                                   startyear = i + 3)
    Aver_costs_direct_slow_discounted[eligiblePix$roundnum == i, ] = benefit.discounter(yearVal = Aver_costs_direct[pixIND, ][eligiblePix$roundnum == i, ], 
                                                                                        startyear = i + 3)
    Aver_costs_INdirect_slow_discounted[eligiblePix$roundnum == i, ] = benefit.discounter(yearVal = Aver_costs_INdirect[pixIND, ][eligiblePix$roundnum == i, ], 
                                                                                          startyear = i + 3)
    Aver_costs_INdirect_fatal_slow_discounted[eligiblePix$roundnum == i, ] = benefit.discounter(yearVal = Aver_costs_INdirect_fatal[pixIND, ][eligiblePix$roundnum == i, ], 
                                                                                                startyear = i + 3)
  }
  
  if(Disaster == "Special"){
    save(h5, file = "05_Costs/Scale_Up/Cost_bene_cum/T_cost_seq.RData")
    save(h1, file = "05_Costs/Scale_Up/Cost_bene_cum/DALY_averted_slow.RData")
    save(h2, file = "05_Costs/Scale_Up/Cost_bene_cum/Aver_costs_direct_slow.RData")
    save(h3, file = "05_Costs/Scale_Up/Cost_bene_cum/Aver_costs_INdirect_slow.RData")
    save(h4, file = "05_Costs/Scale_Up/Cost_bene_cum/Aver_costs_INdirect_fatal_slow.RData")
  }
  
  # total up slow programme costs
  T_cost_slow = T_cost_seq
  
  # NET costs (slow programme)
  T_cost_slow_NET_direct <- T_cost_slow - Aver_costs_direct_slow_discounted
  T_cost_slow_NET_D_IND <- T_cost_slow - Aver_costs_direct_slow_discounted - Aver_costs_INdirect_slow_discounted
  T_cost_slow_NET_D_IND_F <- T_cost_slow - Aver_costs_direct_slow_discounted - Aver_costs_INdirect_slow_discounted - Aver_costs_INdirect_fatal_slow_discounted
  
  
  
  
  ###########################################################
  ### Part 3- summing up and returning results
  ###########################################################
  
  ## A) Table 1 -site summary metrics and costs
  table1 <- list()
  table1$People = sum(as.vector(Apop), na.rm = T)
  table1$People_perc_covered = 100 * sum(as.vector(Apop)[pixIND], na.rm = T) / sum(as.vector(Apop), na.rm = T)
  
  table1$Area = sum(!is.na(Apopv)) * 100
  table1$Area_perc_covered = 100 * sum(!is.na(Apopv[pixIND])) / sum(!is.na(Apopv))
  
  #n.b. area is in m^2, want pop density per km2
  table1$Pop_dens_cov_area = (table1$People * table1$People_perc_covered / 100) / ((table1$Area / 10000) * table1$Area_perc_covered / 100)
  
  table1$TotalCost_Fast_median = median(colSums(T_cost_fast))
  table1$TotalCost_Fast_low = quantile(colSums(T_cost_fast), prob = 0.025)
  table1$TotalCost_Fast_high = quantile(colSums(T_cost_fast), prob = 0.975)
  table1$TotalCost_Slow_median = median(colSums(T_cost_slow))
  table1$TotalCost_Slow_low = quantile(colSums(T_cost_slow), prob = 0.025)
  table1$TotalCost_Slow_high = quantile(colSums(T_cost_slow), prob = 0.975)
  
  table1$TotalCost_PP_Fast_median = table1$TotalCost_Fast_median / sum(Apopv[pixIND])
  table1$TotalCost_PP_Fast_low = table1$TotalCost_Fast_low / sum(Apopv[pixIND])
  table1$TotalCost_PP_Fast_high = table1$TotalCost_Fast_high / sum(Apopv[pixIND])
  table1$TotalCost_PP_Slow_median = table1$TotalCost_Slow_median / sum(Apopv[pixIND])
  table1$TotalCost_PP_Slow_low = table1$TotalCost_Slow_low / sum(Apopv[pixIND])
  table1$TotalCost_PP_Slow_high = table1$TotalCost_Slow_high / sum(Apopv[pixIND])
  
  table1 = data.frame(table1)
  
  ## B) Table 2 - Benefits
  table2 <- list()
  # cases
  table2$Baseline_cases_median = quantile(colSums(t_burd, na.rm = T), prob = 0.5)
  table2$Baseline_cases_low = quantile(colSums(t_burd, na.rm = T), prob = 0.025)
  table2$Baseline_cases_high = quantile(colSums(t_burd, na.rm = T), prob = 0.975)
  
  table2$New_cases_median = quantile(colSums(t_burd, na.rm = T) - colSums(t_burd_avert, na.rm = T), prob = 0.5)
  table2$New_cases_low = quantile(colSums(t_burd, na.rm = T) - colSums(t_burd_avert, na.rm = T), prob = 0.025)
  table2$New_cases_high = quantile(colSums(t_burd, na.rm = T) - colSums(t_burd_avert, na.rm = T), prob = 0.975)
  
  # percentage reductions
  table2$Cases_averted_perc_median = 100 * quantile(colSums(t_burd_avert, na.rm = T) / colSums(t_burd, na.rm = T), prob = 0.5)
  table2$Cases_averted_perc_low = 100 * quantile(colSums(t_burd_avert, na.rm = T) / colSums(t_burd, na.rm = T), prob = 0.025)
  table2$Cases_averted_perc_high = 100 * quantile(colSums(t_burd_avert, na.rm = T) / colSums(t_burd, na.rm = T), prob = 0.975)
  
  table2$Cases_averted_RelArea_perc_median = 100 * quantile(colSums(t_burd_avert[pixIND, ], na.rm = T) / colSums(t_burd[pixIND, ], na.rm = T), prob = 0.5)
  table2$Cases_averted_RelArea_perc_low = 100 * quantile(colSums(t_burd_avert[pixIND, ], na.rm = T) / colSums(t_burd[pixIND, ], na.rm = T), prob = 0.025)
  table2$Cases_averted_RelArea_perc_high = 100 * quantile(colSums(t_burd_avert[pixIND, ], na.rm = T) / colSums(t_burd[pixIND, ], na.rm = T), prob = 0.975)
  
  # averted costs
  table2$Aver_costs_direct_median = quantile(colSums(Aver_costs_direct, na.rm = T), prob = 0.5)
  table2$Aver_costs_direct_low = quantile(colSums(Aver_costs_direct, na.rm = T), prob = 0.025)
  table2$Aver_costs_direct_high = quantile(colSums(Aver_costs_direct, na.rm = T), prob = 0.975)
  
  table2$Aver_costs_INdirect_median = quantile(colSums(Aver_costs_INdirect, na.rm = T), prob = 0.5)
  table2$Aver_costs_INdirect_low = quantile(colSums(Aver_costs_INdirect, na.rm = T), prob = 0.025)
  table2$Aver_costs_INdirect_high = quantile(colSums(Aver_costs_INdirect, na.rm = T), prob = 0.975)
  
  table2$Aver_costs_INdirect_fatal_median = quantile(colSums(Aver_costs_INdirect_fatal, na.rm = T), prob = 0.5)
  table2$Aver_costs_INdirect_fatal_low = quantile(colSums(Aver_costs_INdirect_fatal, na.rm = T), prob = 0.025)
  table2$Aver_costs_INdirect_fatal_high = quantile(colSums(Aver_costs_INdirect_fatal, na.rm = T), prob = 0.975)
  
  
  ## C) Table 3 - Cost effectiveness
  table3 <- list()
  
  # gross and net $/DALY
  table3$dollar_per_DALY_fast_Gross_median = quantile(colSums(T_cost_fast, na.rm = T) / colSums(DALY_averted_fast_discounted, na.rm = T), prob = c(0.5))
  table3$dollar_per_DALY_fast_Gross_low = quantile(colSums(T_cost_fast, na.rm = T) / colSums(DALY_averted_fast_discounted, na.rm = T), prob = c(0.025))
  table3$dollar_per_DALY_fast_Gross_high = quantile(colSums(T_cost_fast, na.rm = T) / colSums(DALY_averted_fast_discounted, na.rm = T), prob = c(0.975))
  
  table3$dollar_per_DALY_fast_Net_INdirectFAT_median = quantile(colSums(T_cost_fast_NET_D_IND_F, na.rm = T) / colSums(DALY_averted_fast_discounted, na.rm = T), prob = c(0.5))
  table3$dollar_per_DALY_fast_Net_INdirectFAT_low = quantile(colSums(T_cost_fast_NET_D_IND_F, na.rm = T) / colSums(DALY_averted_fast_discounted, na.rm = T), prob = c(0.025))
  table3$dollar_per_DALY_fast_Net_INdirectFAT_high = quantile(colSums(T_cost_fast_NET_D_IND_F, na.rm = T) / colSums(DALY_averted_fast_discounted, na.rm = T), prob = c(0.975))
  
  table3$dollar_per_DALY_slow_Gross_median = quantile(colSums(T_cost_slow, na.rm = T) / colSums(DALY_averted_slow_discounted, na.rm = T), prob = c(0.5))
  table3$dollar_per_DALY_slow_Gross_low = quantile(colSums(T_cost_slow, na.rm = T) / colSums(DALY_averted_slow_discounted, na.rm = T), prob = c(0.025))
  table3$dollar_per_DALY_slow_Gross_high = quantile(colSums(T_cost_slow, na.rm = T) / colSums(DALY_averted_slow_discounted, na.rm = T), prob = c(0.975))
  
  table3$dollar_per_DALY_slow_Net_INdirectFAT_median = quantile(colSums(T_cost_slow_NET_D_IND_F, na.rm = T) / colSums(DALY_averted_slow_discounted, na.rm = T), prob = c(0.5))
  table3$dollar_per_DALY_slow_Net_INdirectFAT_low = quantile(colSums(T_cost_slow_NET_D_IND_F, na.rm = T) / colSums(DALY_averted_slow_discounted, na.rm = T), prob = c(0.025))
  table3$dollar_per_DALY_slow_Net_INdirectFAT_high = quantile(colSums(T_cost_slow_NET_D_IND_F, na.rm = T) / colSums(DALY_averted_slow_discounted, na.rm = T), prob = c(0.975))
  
  table3 = data.frame(table3)
  
  
  ## C) Table 4 - Benefit - Cost ratios
  table4 <- list()
  
  # medical
  table4$Direct_slow_median <- median((Aver_costs_direct_slow_discounted) / T_cost_slow, na.rm = T)
  table4$Direct_f_slow_low <- quantile((Aver_costs_direct_slow_discounted) / T_cost_slow, probs = 0.025, na.rm = T)
  table4$Direct_f_slow_high <- quantile((Aver_costs_direct_slow_discounted) / T_cost_slow, probs = 0.975, na.rm = T)
  
  
  # societal (excluding fatal)
  table4$INdirect_slow_median <- median((Aver_costs_direct_slow_discounted + Aver_costs_INdirect_slow_discounted) / T_cost_slow, na.rm = T)
  table4$INdirect_slow_low <- quantile((Aver_costs_direct_slow_discounted + Aver_costs_INdirect_slow_discounted) / T_cost_slow, probs = 0.025, na.rm = T)
  table4$INdirect_slow_high <- quantile((Aver_costs_direct_slow_discounted + Aver_costs_INdirect_slow_discounted) / T_cost_slow, probs = 0.975, na.rm = T)
  
  # soceital (including fatal)
  table4$INdirect_f_slow_median <- median((Aver_costs_direct_slow_discounted + Aver_costs_INdirect_slow_discounted + Aver_costs_INdirect_fatal_slow_discounted) / T_cost_slow, na.rm = T)
  table4$INdirect_f_slow_low <- quantile((Aver_costs_direct_slow_discounted + Aver_costs_INdirect_slow_discounted + Aver_costs_INdirect_fatal_slow_discounted) / T_cost_slow, probs = 0.025, na.rm = T)
  table4$INdirect_f_slow_high <- quantile((Aver_costs_direct_slow_discounted + Aver_costs_INdirect_slow_discounted + Aver_costs_INdirect_fatal_slow_discounted) / T_cost_slow, probs = 0.975, na.rm = T)
  
  table4 = data.frame(table4)
  
  ## D) Maps - of gross CE and programme phases
  # gross CE
  map_list = list()
  tempvals <- apply(T_cost_fast / DALY_averted_fast_discounted, 1, median)
  tempRasVals <- as.vector(Apop)
  tempRasVals[tempRasVals < 10] = 0
  tempRasVals[pixIND] = tempvals
  
  map_list[[1]] <- Apop
  values(map_list[[1]]) = tempRasVals
  
  # programme stages in slow
  map_list[[2]] <- Apop
  tempvals <- as.vector(Apop)
  tempvals[tempvals < 10] = 0
  tempvals[eligiblePix$pixID] = eligiblePix$roundnum
  values(map_list[[2]]) = tempvals
  
  ## package up objects to return them
  rtnlist <- list()
  
  rtnlist[[1]] = table1
  rtnlist[[2]] = table2
  rtnlist[[3]] = table3
  rtnlist[[4]] = table4
  rtnlist[[5]] = map_list
  
  return(rtnlist)
}







# simple wrapper for cost model to deal with uncertainty and returning on a log 10 scale
costmod.predict.wrapper <- function(Atodo2, Pphase, ProgCostFix, JAVA = FALSE, NUM){
  rawpred <- predict(glmmod,
                     newdata = data.frame(Pop_density =  log(Atodo2 * 100, 10),
                                          Programme.phase = Pphase,
                                          Type_release = "EGGS",
                                          GDP = 12378),
                     se.fit = T)
  
  rawpred = data.frame(mid = rawpred$fit, sd = rawpred$se.fit)
  
  # sample from the distribution of predicted costs (while still on log scale)
  rawpred <- apply(rawpred, 1, function(x) rnorm(NUM, mean = x[1], sd = x[2]))
  
  # back on to response scale
  rawpred = (10^rawpred) / 100
  rawpred = t(rawpred)
  
  # if tornado plot
  if(ProgCostFix != "N"){
    if(ProgCostFix == "H"){
      rawpred[, 1:NUM] = apply(rawpred, 1, quantile, probs = 0.975)
    }else{
      rawpred[, 1:NUM] = apply(rawpred, 1, quantile, probs = 0.025)
    }
  }
  
  return(rawpred)
}


# simple function for replacing NA land values with -1 for map plotting
# highlights ineligible areas
rasmap.process <- function(plotras, maskSHP){
  temp = as.vector(plotras)
  temp[is.na(temp)] = -1
  tempras = plotras
  values(tempras) = temp
  return(mask(crop(tempras, maskSHP), maskSHP))
}
