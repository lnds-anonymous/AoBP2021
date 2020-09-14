#---------------------------------------------------------------------#
# Torrey pine seed morphology: Within-population sampling simulations #
#---------------------------------------------------------------------#

###
# Aim
###

# This script aims to simulate within-population resampling (TPSR, SRI) to provide raw data to
# estimate the number of maternal families needed to capture 95% of seed morphological variation 
# within each seed collection.

#-----------------#
# Simulation code #
#-----------------#
wp.sampling <- function(data.path = NULL, population = c("TPSR","SRI"), 
                        trait = NULL, rep = 500, digits = 3, out.path = NULL){

  #-- Loading data, standardization, select population, and remove missing values --#
  data <- readxl::read_excel(data.path); data <- data.frame(data)
  for(i in 3:ncol(data)){
    data[,i] <- as.numeric(data[,i])
  }
  for(i in 1:2){
    data[,i] <- as.factor(data[,i])
  }
  data[,trait] <- scale(data[,trait], scale = T, center = T)
  data <- data[which(data$Source==population),]
  sel <- which(is.na(data[,trait])==T); if(length(sel)!=0){data <- data[-sel,]}
  
  #-- Create an object with maternal families and double-check total number after NAs removal --#
  fam <- unique(data$Maternal_ID)
  nb.fam <- length(fam)
  
  #-- Printing parameters used in simulations --#
  cat("#-- Simulation parameters --#\n")
  cat(paste("Path to dataset:", data.path, "\n"))
  cat(paste("Population simulated:", population, "\n"))
  cat(paste("Number of iterations:", rep, "\n"))
  cat(paste("Number of digits for statistic:", digits, "\n"))
  
  if(population=="TPSR" & length(fam)!=80){warning(paste("Number of maternal families set to: ", 1, ":", nb.fam, "\n\n", sep = ""))}
  if(population=="TPSR" & length(fam)==80){cat(paste("Range of maternal families tested: ", 1, ":", nb.fam, "\n\n", sep = ""))}
  if(population=="SRI" & length(fam)!=30){warning(paste("Number of maternal families set to: ", 1, ":", nb.fam, "\n\n", sep = ""))}
  if(population=="SRI" & length(fam)==30){cat(paste("Range of maternal families tested: ", 1, ":", nb.fam, "\n\n", sep = ""))}
  
  #-- Create output matrix (results matrix) --#
  out <- matrix(ncol = nb.fam, nrow = rep)
  column = NULL
  for(i in 1:nb.fam){
    column <- append(column,paste("NbFam", i, sep = ""))
  }
  colnames(out) <- column
  
  #-- Start simulation --#
  cat(paste("Processing:", names(data)[trait], "\n"))
  for(f in 1:nb.fam){
    if(f!=nb.fam){cat(paste(f,"...", sep = ""))} else{cat(paste(f,"...\n", sep = ""))}
    for(r in 1:rep){
      fam.sample <- sample(fam, f, replace = F)
      newdata = data.frame()
      for(i in 1:length(fam.sample)){
        temp <- data[which(data$Maternal_ID==fam.sample[i]),]
        newdata <- rbind(newdata, temp)
      }
      if(names(newdata)[trait]!=names(data)[trait]){stop("Column mismatch")}
      var.captured <- round(newdata[,trait], digits = digits)
      var.total <- round(data[,trait], digits = digits)
      out[r,f] <- length(unique(var.captured))/length(unique(var.total))
    }
  }
  cat("\n")
  cat(paste("Results saved to:", out.path))
  cat("\n")
  setwd(out.path)
  openxlsx::write.xlsx(out, file = paste(population,"_", names(data)[trait],"_it",rep, 
                                         "_digits",digits,".xlsx", sep = ""))
  setwd("..")
  return(out)
}

###############
# Simulations #
###############

# To run the code below, place this script in the same directory where the folders
# "Data" and "Simulation_Outputs" are located.

### TPSR ###
TPSR = list()
for(j in 1:1){
  for(l in 3:16){
    wp.sampling(data.path = "./Data/TP_seeds_R_fmt.xlsx", population = "TPSR",
                trait = l, rep = 500, digits = j, out.path = "./Simulation_Outputs/")
    cat("\n\n")
  }
}

### SRI ###
SRI = list()
for(j in 1:1){
  for(l in 3:16){
    wp.sampling(data.path = "./Data/TP_seeds_R_fmt.xlsx", population = "SRI",
                trait = l, rep = 500, digits = j, out.path = "./Simulation_Outputs/")
    cat("\n\n")
  }
}