#-------------------------------------------------#
# Table 1: Torrey pine seed morphology manuscript #
#-------------------------------------------------#

# This script performs all the calculations and statistics associated with
# Table 1 in the manuscript.

################################
# Import data and dependencies #
################################
library("exactRankTests")
setwd("../Data/")
data <- readxl::read_excel("TP_seeds_maternal_fmt.xlsx"); data <- data.frame(data)
for (i in 3:ncol(data)){
  data[,i] <- as.numeric(data[,i])
}
str (data)

##########################
# Calculate Table values #
##########################

# Torrey pine state reserve
df <- subset(data, data$Source=="TPSR")
for(i in 3:ncol(df)){
  mu <- round(mean(df[,i]), 2)
  se <- round(sd(df[,i])/sqrt(nrow(df)), 3)
  cat(paste("#-- Name: ", names(df)[i], ", mean: ", mu, ", SE: ", se, " --#\n", sep = ""))
}

# Santa Rosa Island
df <- subset(data, data$Source=="SRI")
for(i in 3:ncol(df)){
  mu <- round(mean(df[,i]), 2)
  se <- round(sd(df[,i])/sqrt(nrow(df)), 3)
  cat(paste("#-- Name: ", names(df)[i], ", mean: ", mu, ", SE: ", se, " --#\n", sep = ""))
}

#######################################################################
# Identify significant differences in seed traits between populations #
#######################################################################
name <- names(data[,-c(1:2)])
res <- data.frame(matrix(nrow = length(name), ncol = 4)); colnames(res) <- c("Trait", "p.value","Significant?","Test")
for(i in 1:length(name)){
  s1 <- shapiro.test(data[which(data$Source=="TPSR"), name[i]])$p.value
  s2 <- shapiro.test(data[which(data$Source=="SRI"), name[i]])$p.value
  bart <- bartlett.test(list(data[which(data$Source=="TPSR"), name[i]],
                             data[which(data$Source=="SRI"), name[i]]))$p.value
  if(s1>0.05 & s2>0.05){
    res[i,1] <- name[i]
    if(bart>0.05){res[i,2] <- round(t.test(data[which(data$Source=="TPSR"), name[i]], data[which(data$Source=="SRI"), name[i]], alternative = "two.sided", var.equal = T)$p.value, 20); res[i,4] <- "Student"}
    if(bart<0.05){res[i,2] <- round(t.test(data[which(data$Source=="TPSR"), name[i]], data[which(data$Source=="SRI"), name[i]], alternative = "two.sided", var.equal = F)$p.value, 20); res[i,4] <- "Welch"}
    if(res[i,2]<0.05){res[i,3] <- "Yes"} else{res[i,3] <- "No"}
    res[i,4] <- "Student"
  }
  if(s1<0.05 | s2<0.05){
    res[i,1] <- name[i]
    res[i,2] <- round(wilcox.exact(data[which(data$Source=="TPSR"), name[i]], data[which(data$Source=="SRI"), name[i]], alternative = "two.sided")$p.value, 20)
    if(res[i,2]<0.05){res[i,3] <- "Yes"} else{res[i,3] <- "No"}
    res[i,4] <- "Wilcoxon"
  }
}

# The object "res" show the p value, the test performed (Student or Wilcoxon), and
# whether the test is significant (Yes or No) for each trait separately.
res
