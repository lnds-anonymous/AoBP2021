#----------------------------------------#
# Torrey pine seed measurements analysis #
#----------------------------------------#

###
# Aim
###

# Answer the following questions using measurements of Torrey pine seeds:

# 1) Are Torrey pine populations differentiated based on seed traits? (PCA)
# 2) Do differences in seed traits impact emergence? (Friedman and t tests)

#---------------------#
# Associated function #
#---------------------#

# Show PCA loading values in decreasing order
pca.loadings <- function(data,pc){
  pca <- data$rotation
  ord <- order(abs(pca[,pc]), decreasing = T)
  return(pca[ord,pc])
}

#####################################
# Loading packages and dependencies #
#####################################
setwd("../Data/")
library(ggbiplot)
library(openxlsx)
library(cowplot)
library(exactRankTests)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(rstatix)

##############
# Question 1 #
##############
# 1) Are Torrey pine populations differentiated based on seed traits? (PCA)

#-- Perform Principal Component analysis
data <- readxl::read_excel("TP_seeds_maternal_fmt.xlsx"); data <- data.frame(data)
data[,2] <- as.integer(data[,2])
for(i in 3:ncol(data)){
  data[,i] <- as.numeric(data[,i])
}
pca <- data[,-c(1:2)]
str(pca)
pca <- scale(pca, scale = T, center = T)
dudi <- prcomp(pca, center = F, scale. = F)

ggbiplot(dudi, scale=0, groups = data$Source, ellipse=T, var.axes = F)+
  geom_hline(yintercept = 0, linetype="dashed")+
  geom_vline(xintercept = 0, linetype="dashed")+
  scale_color_manual(values = c("black", "grey65"), labels=c(" Island", " Mainland"))+theme_bw()+
  theme(panel.border = element_rect(color="black", fill = NA),
        panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.title = element_text(size = 12))

pca.loadings(dudi, 1)
pca.loadings(dudi, 2)

# Conclusion: Yes, Torrey pine populations are (highly) differentiated based on seed traits.
# Clear separation of the two populations on PC1.

##############
# Question 2 #
##############
# 2) Do differences in seed traits impact emergence?
df <- readxl::read_excel("TP_emergence_data.xlsx"); df <-data.frame(df)
head(df)
str(df)

ggplot()+geom_boxplot(data=df, aes(x=Date, y=Perc_complete, fill=Source))+#coord_flip()+
  scale_fill_manual(values = c("black", "grey65"), labels=c(" Island", " Mainland"))+
  ylab("Proportion of emerged seeds")+theme_bw()+
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.title = element_text(size=12),
        panel.border = element_rect(colour="black", fill=NA),
        legend.position = c(0.1, 0.9),
        panel.grid = element_blank())

#-- Student t-test to test for differences between population for each timepoint --#
shapiro.test(df$Perc_complete[df$Source=="TPSR" & df$Date=="02.06.18"])
shapiro.test(df$Perc_complete[df$Source=="SRI" & df$Date=="02.06.18"])
wilcox.exact(df$Perc_complete[df$Source=="TPSR" & df$Date=="02.06.18"],
             df$Perc_complete[df$Source=="SRI" & df$Date=="02.06.18"],
             alternative = "two.sided")

shapiro.test(df$Perc_complete[df$Source=="TPSR" & df$Date=="02.16.18"])
shapiro.test(df$Perc_complete[df$Source=="SRI" & df$Date=="02.16.18"])
bartlett.test(list(df$Perc_complete[df$Source=="TPSR" & df$Date=="02.16.18"],
                   df$Perc_complete[df$Source=="SRI" & df$Date=="02.16.18"]))
t.test(df$Perc_complete[df$Source=="TPSR" & df$Date=="02.16.18"],
       df$Perc_complete[df$Source=="SRI" & df$Date=="02.16.18"],
       alternative = "two.sided", var.equal = T)

shapiro.test(df$Perc_complete[df$Source=="TPSR" & df$Date=="03.07.18"])
shapiro.test(df$Perc_complete[df$Source=="SRI" & df$Date=="03.07.18"])
bartlett.test(list(df$Perc_complete[df$Source=="TPSR" & df$Date=="03.07.18"],
                   df$Perc_complete[df$Source=="SRI" & df$Date=="03.07.18"]))
t.test(df$Perc_complete[df$Source=="TPSR" & df$Date=="03.07.18"],
       df$Perc_complete[df$Source=="SRI" & df$Date=="03.07.18"],
       alternative = "two.sided", var.equal = T)

#-- Calculate the proportion of emerged seeds for each timepoint --#
round(mean(df$Perc_complete[df$Source=="TPSR" & df$Date=="03.07.18"]),2)
round(mean(df$Perc_complete[df$Source=="SRI" & df$Date=="03.07.18"]),2)

round(mean(df$Perc_complete[df$Source=="TPSR" & df$Date=="02.16.18"]),2)
round(mean(df$Perc_complete[df$Source=="SRI" & df$Date=="02.16.18"]),2)

round(mean(df$Perc_complete[df$Source=="TPSR" & df$Date=="02.06.18"]),2)
round(mean(df$Perc_complete[df$Source=="SRI" & df$Date=="02.06.18"]),2)

#-- Friedman test to test directionality of change over time --#

# Torrey pine state reserve
df <- readxl::read_excel("TP_emergence_data.xlsx")
df <- df[which(df$Source=="TPSR"),]
df$Date <- as.factor(df$Date)
ggboxplot(df, x = "Date", y = "Perc_complete", add = "jitter")
res.fried <- df %>% friedman_test(Perc_complete ~ Date | Maternal_ID); res.fried
res.effect <- df %>% friedman_effsize(Perc_complete ~ Date | Maternal_ID); res.effect
pwc <- df %>%
  wilcox_test(Perc_complete ~ Date, paired = TRUE, p.adjust.method = "BH")
pwc

# Santa Rosa Island
df <- readxl::read_excel("TP_emergence_data.xlsx")
df <- df[which(df$Source=="SRI"),]
df$Date <- as.factor(df$Date)
ggboxplot(df, x = "Date", y = "Perc_complete", add = "jitter")
res.fried <- df %>% friedman_test(Perc_complete ~ Date | Maternal_ID); res.fried
res.effect <- df %>% friedman_effsize(Perc_complete ~ Date | Maternal_ID); res.effect
pwc <- df %>%
  wilcox_test(Perc_complete ~ Date, paired = TRUE, p.adjust.method = "BH")
pwc

# Conclusion 1 (t-tests): There is no significant differences in emergence between populations
# at each timepoint --> This suggest similar timing and probability of emergence.
# Conclusion 2 (repeated anova): For both populations, emergence increased over time.