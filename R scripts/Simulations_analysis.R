#------------------------------------#
# Simulation analysis - Only 1 digit #
#------------------------------------#

###
# Aim
###

# This script aims to analyze simulations performed and estimate the
# summary statistic (Nc/Nt).


#################################
# Loading data and dependencies #
#################################
library(ggplot2)
library(cowplot)

comp <- matrix(nrow = 1, ncol = 2)
colnames(comp) <- c("TPSR", "SRI")
rownames(comp) <- c("Results")

setwd("../Simulation_Outputs/")
files_TPSR <- list.files(pattern = "TPSR.*digits1.xlsx"); files_TPSR
files_SRI <- list.files(pattern = "SRI.*digits1.xlsx"); files_SRI

mainland <- lapply(files_TPSR, function(i) readxl::read_excel(i))
for(i in 1:length(mainland)){
  mainland[[i]] <- data.frame(mainland[[i]])
  names(mainland)[[i]] <- files_TPSR[i]
  cat(paste(ncol(mainland[[i]]), "\n"))
}

island <- lapply(files_SRI, function(i) readxl::read_excel(i))
for(i in 1:length(island)){
  island[[i]] <- data.frame(island[[i]])
  names(island)[[i]] <- files_SRI[i]
  cat(paste(ncol(island[[i]]), "\n"))
  
}

#-- Calculate average over all iterations for each number of maternal families --#
for(i in 1:length(mainland)){
  temp <- mainland[[i]]
  av <- apply(temp, 2, mean)
  mainland[[i]] <- av
}

for(i in 1:length(island)){
  temp <- island[[i]]
  av <- apply(temp, 2, mean)
  island[[i]] <- av
}

plot(1:80, mainland[[1]], type = "b", pch=20, xlab = "Number of maternal families",
     ylab = "Phenotypic variance captured (Seed morphology)")
lines(1:80, mainland[[1]])
for(i in 2:length(mainland)){
  points(1:80, mainland[[i]], pch=20)
  lines(1:80, mainland[[i]])
}

plot(1:30, island[[1]], type = "b", pch=20, xlab = "Number of maternal families",
     ylab = "Phenotypic variance captured (Seed morphology)")
lines(1:30, island[[1]])
for(i in 2:length(island)){
  points(1:30, island[[i]], pch=20)
  lines(1:30, island[[i]])
}

#-- Average over all 14 traits --#
res_m <- matrix(nrow = 14, ncol=80)
colnames(res_m) <- names(mainland[[1]])
rownames(res_m) <- names(mainland)
for(i in 1:length(mainland)){
  res_m[i,] <- mainland[[i]]
}

out_m <- matrix(ncol = 80, nrow = 2)
colnames(out_m) <- colnames(res_m)
out_m[1,] <- apply(res_m, 2, mean)
out_m[2,] <- apply(res_m, 2, function(i) sd(i)/sqrt(14))

res_i <- matrix(nrow = 14, ncol=30)
colnames(res_i) <- names(island[[1]])
rownames(res_i) <- names(island)
for(i in 1:length(island)){
  res_i[i,] <- island[[i]]
}

out_i <- matrix(ncol = 30, nrow = 2)
colnames(out_i) <- colnames(res_i)
out_i[1,] <- apply(res_i, 2, mean)
out_i[2,] <- apply(res_i, 2, function(i) sd(i)/sqrt(14))


#-- Final figures for both Torrey pine populations --#
out_m <- t(out_m); out_i <- t(out_i)
out_m <- data.frame(out_m); out_i <- data.frame(out_i)
out_m$nbfam <- 1:80; out_i$nbfam <- 1:30
names(out_m) <- c("mean", "se", "nbfam"); str(out_m)
names(out_i) <- c("mean", "se", "nbfam"); str(out_i)

comp[1,1] <- out_m[which(round(out_m$mean,2)>=0.95),][1,3]
comp[1,2] <- out_i[which(round(out_i$mean,2)>=0.95),][1,3]
comp

d1c <- ggplot()+
  geom_point(data=out_m, aes(x = nbfam/80, y= mean), colour="grey65")+
  geom_line(data=out_m, aes(x = nbfam/80, y= mean), colour="grey65")+
  geom_point(data = out_i, aes(x = nbfam/30, y= mean))+
  geom_line(data = out_i, aes(x = nbfam/30, y= mean))+
  #geom_errorbar(data = out_m, aes(x = nbfam/80, ymin = mean-se, ymax = mean+se, width = 0.015), colour="grey65")+
  #geom_errorbar(data = out_i, aes(x = nbfam/30, ymin = mean-se, ymax = mean+se, width = 0.015))+
  theme_classic()+xlab("\nProportion of maternal families sampled")+
  ylab(expression(paste("N"["c"],"/N"["t"])))+
  geom_vline(xintercept = 54/80, linetype=4, colour = "grey65")+
  geom_vline(xintercept = 24/30, linetype=4)+
  geom_hline(yintercept = 0.95, linetype="dashed")+
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=12))
print(d1c)

#######################
# Percentage analysis #
#######################

# Output the number and percentage of maternal families needed to 
# capture 95% of seed morphological variation in island (SRI) and mainland (TPSR)
# seed collections.

comperc <- comp
comperc[,1] <- comperc[,1]/80; comperc[,2] <- comperc[,2]/30
round(comperc,2)
comp
