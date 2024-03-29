# Estimating cost functions of deployment
rm(list = ls())

setwd("/Users/eideobra/Dropbox/07_Indonesia")
# setwd("C:\\Users\\Eideehen\\Dropbox\\Indonesia")
#setwd("~/Dropbox/LSHTM/Indonesia/")

# load site summary data
#cdat <- read.csv("05_Costs/Site_summary_costs.csv")
cdat <- read.csv("05_Costs/Site_summary_costs_INDO_update.csv")
cdat <- cdat[!is.na(cdat$Cost_per_km2), ]
# main effect = Human population density
# response variable = cost per km^2 of wolbachia 
# confounder = phase of programme
# confounder = type of release, egg or adult

# alternative response variable (cost per person)
cdat$people = cdat$Area * cdat$Pop_density
cdat$Cost_per_person = cdat$Cost_USD / cdat$people


plot(cdat$Pop_density, cdat$Cost_per_km2)
plot(log(cdat$Pop_density), log(cdat$Cost_per_km2))
plot(log(cdat$Pop_density), log(cdat$Cost_per_person))
# log both pop density and cost per km2 and cost per person
cdat$Pop_density = log(cdat$Pop_density, 10)
cdat$Cost_per_km2 = log(cdat$Cost_per_km2, 10)
cdat$Cost_per_person = log(cdat$Cost_per_person, 10)



#glmmod <- glm(Cost_per_km2 ~ Pop_density + Area:Programme.phase + Type_release, 
#              data = cdat)
# not sure if there should be an area term in this equation or not?

glmmod <- glm(Cost_per_km2 ~ Pop_density + Programme.phase + Type_release + GDP, 
              data = cdat)

glmmod_alternative <- glm(Cost_per_person ~ Pop_density + Programme.phase + Type_release + GDP, 
                          data = cdat)

#cdat$Cost_per_km2 = log(cdat$Cost_per_km2)
#cdat$Pop_density = log(cdat$Pop_density)

#glmmod2 <- glm(Cost_per_km2 ~ Pop_density + Programme.phase + Type_release + GDP, 
#              data = cdat)

summary(glmmod)
summary(glmmod_alternative)

cor(cdat$Cost_per_km2, predict(glmmod))^2
cor(cdat$Cost_per_person, predict(glmmod_alternative))^2


#plot(cdat$Cost_per_km2, predict(glmmod))
#plot(glmmod)

# retransform Pop density and cost
#cdat$Pop_density = exp(cdat$Pop_density)
#cdat$Cost_per_km2 = exp(cdat$Cost_per_km2)


pdf(file = "13_Writeup/CE_paper/Figures/F1_Cost_model_plot.pdf", height = 5, width = 8)

par( mar = c(5.1, 4.8, 4.1, 2.1))
plot(cdat$Pop_density, cdat$Cost_per_km2,
     col = c(rgb(0,0,0,0.5), rgb(1,0,0,0.5))[cdat$Programme.phase],
     cex = 5 * (cdat$Area / max(cdat$Area)),
     xlab = expression(paste("Human population density in release area (inhabitants per"~km^2,")")),
     ylab = expression(paste(italic("Wolbachia")," release cost per"~km^2,"(USD)")),
     pch = 16,
     xlim = c(3, 4.4),
     xaxt="n", yaxt="n")
#grid()
#points(cdat$Pop_density, cdat$Cost_per_km2, cex = 5 * (cdat$Area / max(cdat$Area)))
axis(1, at=log(c(1000,  2000,  5000,  10000, 15000, 25000), 10), labels=c(1000,  2000,  5000,  10000, 15000, 25000))
axis(2, at=c(log10(20000), log10(50000), log10(100000), log10(150000)), labels=c("$20k", "$50k", "$100k", "$150k"))

# custom grid
abline(h = log10(20000), lty = 3, col = "grey")
abline(h = log10(50000), lty = 3, col = "grey")
abline(h = log10(100000), lty = 3, col = "grey")
abline(h = log10(150000), lty = 3, col = "grey")
abline(v = log(1000, 10), lty = 3, col = "grey")
abline(v = log(2000, 10), lty = 3, col = "grey")
abline(v = log(5000, 10), lty = 3, col = "grey")
abline(v = log(10000, 10), lty = 3, col = "grey")
abline(v = log(15000, 10), lty = 3, col = "grey")
abline(v = log(25000, 10), lty = 3, col = "grey")


text(4.074048, 4.778645, pos=4, label=expression(italic("Indonesia")), col = rgb(1,0,0,0.85))
text(4.001847, 5.288611, pos=2, label=expression(paste("Colombia")^b), col = rgb(0,0,0,0.85))
text(3.918415, 4.621179, pos=1, label=expression(paste(italic("Colombia"))^a), col = rgb(1,0,0,0.85))
text(4.103060, 4.625335, pos=4, label=expression(paste(italic("Colombia"))^c), col = rgb(1,0,0,0.85))

text(4.432571, 5.044466, pos=2, label=expression(paste("Sri Lanka")^a), col = rgb(0,0,0,0.85))
text(3.813114, 4.763716, pos=3, label=expression(paste(italic("Sri Lanka"))^c), col = rgb(1,0,0,0.85))
text(3.914742, 4.767179, pos=1, label=expression(paste(italic("Sri Lanka"))^b), col = rgb(1,0,0,0.85))
text(3.384681, 4.733796, pos=4, label=expression(paste("Australia")^a), col = rgb(0,0,0,0.85))
text(3.363880, 4.433216, pos=1, label=expression(paste(italic("Australia"))^b), col = rgb(1,0,0,0.85))
text(3.120687, 4.243814, pos=4, label=expression(paste(italic("Australia"))^c), col = rgb(1,0,0,0.85))
text(3.541006, 4.446590, pos=3, label=expression(paste(italic("Australia"))^d), col = rgb(1,0,0,0.85))
text(3.124939, 4.274880, pos=3, label="Vanuatu", col = rgb(0,0,0,0.85))

pred1 <- predict(glmmod, newdata = data.frame(Pop_density = log(c(10^3, 10^5), 10),
                                              Programme.phase = c(1, 1),
                                              Type_release = "EGGS",
                                              GDP = 12378),
                 se.fit = T)
lines(log(c(10^3,10^5), 10), pred1$fit, col = rgb(0,0,0,0.5), lwd = 2)
lines(log(c(10^3,10^5), 10), pred1$fit - 1 * pred1$se.fit, col = rgb(0,0,0,0.5), lty= 2)
lines(log(c(10^3,10^5), 10), pred1$fit + 1 * pred1$se.fit, col = rgb(0,0,0,0.5), lty= 2)

pred2 <- predict(glmmod, newdata = data.frame(Pop_density = log(c(10^3, 10^5), 10),
                                              Programme.phase = c(2, 2),
                                              Type_release = "EGGS",
                                              GDP = 12378),
                 se.fit = T)

lines(log(c(10^3,10^5), 10), pred2$fit, col = rgb(1,0,0,0.5), lwd = 2)
lines(log(c(10^3,10^5), 10), pred2$fit - 1 * pred2$se.fit, col = rgb(1,0,0,0.5), lty = 2)
lines(log(c(10^3,10^5), 10), pred2$fit + 1 * pred2$se.fit, col = rgb(1,0,0,0.5), lty = 2)

legend("topleft", c("Phase 1 and 2", expression(italic("Phase 2"))), col = c(rgb(0,0,0,0.7), rgb(1,0,0,0.7)),
       text.col = c(rgb(0,0,0,0.7), rgb(1,0,0,0.7)),
       pch = 16, cex = 1.2)

dev.off()





#########################################
# potential new figure 1B- cost per person
#########################################


# back to normal scale

#cdat$Pop_density = 10^cdat$Pop_density
#cdat$Cost_per_person = 10^cdat$Cost_per_person


pdf(file = "13_Writeup/CE_paper/Figures/F1_Cost_model_plot_CPP.pdf", height = 5, width = 8)

par( mar = c(5.1, 4.8, 4.1, 2.1))
plot(cdat$Pop_density, cdat$Cost_per_person,
     col = c(rgb(0,0,0,0.5), rgb(1,0,0,0.5))[cdat$Programme.phase],
     cex = 5 * (cdat$Area / max(cdat$Area)),
     xlab = expression(paste("Human population density in release area (inhabitants per"~km^2,")")),
     ylab = expression(paste(italic("Wolbachia")," release cost per person (USD)")),
     xlim = c(3, 4.4),
     pch = 16, yaxt="n", xaxt = "n")
axis(1, at=log(c(1000,  2000,  5000,  10000, 15000, 25000), 10), labels=c(1000,  2000,  5000,  10000, 15000, 25000))
axis(2, at=log(c(5, 10, 15, 20), 10), labels=c("$5", "$10", "$15", "$20"))

# custom grid
abline(h = log(5, 10), lty = 3, col = "grey")
abline(h = log(10, 10), lty = 3, col = "grey")
abline(h = log(15, 10), lty = 3, col = "grey")
abline(h = log(20, 10), lty = 3, col = "grey")
abline(v = log(1000, 10), lty = 3, col = "grey")
abline(v = log(2000, 10), lty = 3, col = "grey")
abline(v = log(5000, 10), lty = 3, col = "grey")
abline(v = log(10000, 10), lty = 3, col = "grey")
abline(v = log(15000, 10), lty = 3, col = "grey")
abline(v = log(25000, 10), lty = 3, col = "grey")


text(log10(11859), log10(6), pos=4, label=expression(italic("Indonesia")), col = rgb(1,0,0,0.85))
text(log10(10042.622), log10(21.223399), pos=2, label=expression(paste("Colombia")^b), col = rgb(0,0,0,0.85))
text(log10(8939.709), log10(4.653960), pos=1, label=expression(paste(italic("Colombia"))^a), col = rgb(1,0,0,0.85))
text(log10(12832.063), log10(3.566764), pos=4, label=expression(paste(italic("Colombia"))^c), col = rgb(1,0,0,0.85))

text(log10(25268.000), log10(4.919203), pos=2, label=expression(paste("Sri Lanka")^a), col = rgb(0,0,0,0.85))
text(log10(6503.000), log10(8.924885), pos=3, label=expression(paste(italic("Sri Lanka"))^c), col = rgb(1,0,0,0.85))
text(log10(9435.000), log10(6.200653), pos=2, label=expression(paste(italic("Sri Lanka"))^b), col = rgb(1,0,0,0.85))
text(log10(2424.828), log10(22.341642), pos=1, label=expression(paste("Australia")^a), col = rgb(0,0,0,0.85))
text(log10(2311.429), log10(11.731007), pos=4, label=expression(paste(italic("Australia"))^b), col = rgb(1,0,0,0.85))
text(log10(1532.216) , log10(13.627463), pos=1, label=expression(paste(italic("Australia"))^c), col = rgb(1,0,0,0.85))
text(log10(2760.619), log10(10.129397), pos=1, label=expression(paste(italic("Australia"))^d), col = rgb(1,0,0,0.85))
text(log10(1533.333), log10(15.123455), pos=3, label="Vanuatu", col = rgb(0,0,0,0.85))



#pred1 <- predict(glmmod_alternative, newdata = data.frame(Pop_density = log(seq(1000, 30000, length.out = 100), 10),
#                                              Programme.phase = c(1, 1),
#                                              Type_release = "EGGS",
#                                              GDP = 12378),
#                 se.fit = T)
#lines(seq(1000, 30000, length.out = 100), 10^pred1$fit, col = rgb(0,0,0,0.5), lwd = 2)
#lines(seq(1000, 30000, length.out = 100), 10^(pred1$fit- 1 * pred1$se.fit), col = rgb(0,0,0,0.5), lty= 2)
#lines(seq(1000, 30000, length.out = 100), 10^(pred1$fit+ 1 * pred1$se.fit), col = rgb(0,0,0,0.5), lty= 2)
#
#pred2 <- predict(glmmod_alternative, newdata = data.frame(Pop_density = log(seq(1000, 30000, length.out = 100), 10),
#                                              Programme.phase = c(2, 2),
#                                              Type_release = "EGGS",
#                                              GDP = 12378),
#                 se.fit = T)
#
#lines(seq(1000, 30000, length.out = 100), 10^pred2$fit, col = rgb(1,0,0,0.5), lwd = 2)
#lines(seq(1000, 30000, length.out = 100), 10^(pred2$fit- 1 * pred2$se.fit), col = rgb(1,0,0,0.5), lty = 2)
#lines(seq(1000, 30000, length.out = 100), 10^(pred2$fit+ 1 * pred2$se.fit), col = rgb(1,0,0,0.5), lty = 2)





pred1 <- predict(glmmod, newdata = data.frame(Pop_density = log(seq(1000, 30000, length.out = 100), 10),
                                              Programme.phase = c(1, 1),
                                              Type_release = "EGGS",
                                              GDP = 12378),
                 se.fit = T)
lines(log10(seq(1000, 30000, length.out = 100)), log10(10^pred1$fit / seq(1000, 30000, length.out = 100)), col = rgb(0,0,0,0.5), lwd = 2)
lines(log10(seq(1000, 30000, length.out = 100)), log10(10^(pred1$fit- 1 * pred1$se.fit) / seq(1000, 30000, length.out = 100)), col = rgb(0,0,0,0.5), lty= 2)
lines(log10(seq(1000, 30000, length.out = 100)), log10(10^(pred1$fit+ 1 * pred1$se.fit) / seq(1000, 30000, length.out = 100)), col = rgb(0,0,0,0.5), lty= 2)

pred2 <- predict(glmmod, newdata = data.frame(Pop_density = log(seq(1000, 30000, length.out = 100), 10),
                                              Programme.phase = c(2, 2),
                                              Type_release = "EGGS",
                                              GDP = 12378),
                 se.fit = T)

lines(log10(seq(1000, 30000, length.out = 100)), log10(10^pred2$fit / seq(1000, 30000, length.out = 100)), col = rgb(1,0,0,0.5), lwd = 2)
lines(log10(seq(1000, 30000, length.out = 100)), log10(10^(pred2$fit- 1 * pred2$se.fit) / seq(1000, 30000, length.out = 100)), col = rgb(1,0,0,0.5), lty = 2)
lines(log10(seq(1000, 30000, length.out = 100)), log10(10^(pred2$fit+ 1 * pred2$se.fit) / seq(1000, 30000, length.out = 100)), col = rgb(1,0,0,0.5), lty = 2)

legend("topright", c("Phase 1 and 2", expression(italic("Phase 2"))), col = c(rgb(0,0,0,0.7), rgb(1,0,0,0.7)),
       text.col = c(rgb(0,0,0,0.7), rgb(1,0,0,0.7)),
       pch = 16, cex = 1.2)

dev.off()

















# predicted cost in Indonesia (phase 2 eggs)
tab = data.frame(p = c(1000, 10000, 22400),
            c = 10^(predict(glmmod, newdata = data.frame(Pop_density = log(c(1000, 10000, 22400), 10),
                                                     Programme.phase = 1,
                                                     Type_release = "EGGS",
                                                     GDP = 12378))))
tab$cpp = tab[, 2] / tab[, 1]

# costs 22k in area with 1000 people per km2 and 169k in area with 22400 people per km2
# 22x more people for only 7.68x the cost

##### saving models
save(glmmod, file = "05_Costs/Cost_model.RData")

# add long term costs out to 10 years post release
# annual cost is pegged to a percentage of a phase 2 programme
# assumed that the phase 2 programme is years 1 and 2
# years 3 and 4 then cost 0.5 * 0.165839256 of the phase 2 cost
# years 5 - 10 then cost 0.5 * 0.082919628 of the phase 2 cost
#cost.LT <- function(phase2cost){
#  phase2cost * 0.5 * 0.165839256 * 2 + phase2cost * 0.5 * 0.082919628 * 6
#}
#save(cost.LT, file = "05_Costs/Cost_LT_model.RData")

cost.LT <- function(phase2cost){
  phase2cost * 0.5 * 0.165839256 * 4 + phase2cost * 0.5 * 0.082919628 * 6
}
save(cost.LT, file = "05_Costs/Cost_LT_model.RData")










# messign around with guesstimating


worldpop <- raster("07_Data/Population/IDN_ppp_v2b_2015_UNadj.tif")
worldpop = aggregate(worldpop, 10, fun = sum)

# range of pop densities in Indonesia = 0 per km2 to 22400 per km2
# range of data = 1,232 to 25,270

Pa <- as.vector(worldpop)
# which pixels to process
#ProcessIND = !is.na(Pa) & Pa > 1232
ProcessIND = !is.na(Pa) & Pa > 1000

workset = Pa[ProcessIND]

workres <- predict(glmmod, newdata = data.frame(Pop_density = workset,
                                                Programme.phase = 1,
                                                Type_release = "EGGS",
                                                GDP = 12378))
workres_phase_2 <- predict(glmmod, newdata = data.frame(Pop_density = workset,
                                                        Programme.phase = 2,
                                                        Type_release = "EGGS",
                                                        GDP = 12378))

maintain <- cost.LT(workres_phase_2)

workres = workres + maintain

# put back in a map
Pa[!is.na(Pa)] = NA
Pa[ProcessIND] = workres
costras <- worldpop
values(costras) = Pa

# basemap
bmap <- as.vector(worldpop)
bmap[ProcessIND] = NA
basemap = worldpop
values(basemap) = bmap

ad1 <- readOGR("02_Mapping/Administrative_units/Admin1(2011)/admin1.shp", "admin1")
ad1 = ad1[ad1$COUNTRY_ID == "IDN", ]

pdf("05_Costs/Costmap.pdf", height = 5, width = 10)
plot(costras, col = rev(heat.colors(100)))
plot(basemap >= 0, col = "grey", add = T, legend = FALSE)
plot(ad1, add = T, lwd = 0.5)
dev.off()

# and total cost?
sum(workres)
# 1.6 billion dollars + ongoing monitoring


