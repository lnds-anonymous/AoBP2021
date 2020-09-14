#---------------------------------------------#
# GLMM models for Torrey pine seed morphology #
#---------------------------------------------#

###
# Aim
###

# This script aims to construct a linear mixed model (LMM) using 
# variation in Torrey pine seed morphology and evaluate the contribution of 
# population (TPSR, SRI) and maternal ID (individual trees) to explaining
# this variation.

#################################
# Loading data and dependencies #
#################################
library(lme4)
library(lmerTest)
library(MuMIn)
library(ggplot2)

setwd("../Data")
df <- readxl::read_excel("TP_seeds_R_fmt.xlsx"); df <- data.frame(df)
df[,1] <- as.factor(df[,1]); df[,2] <- as.factor(df[,2])
for(i in 3:ncol(df)){
  df[,i] <- as.numeric(df[,i])
}
str(df)

dfmelt <- reshape2::melt(df)
I <- order(dfmelt$Source)
dfmelt <- dfmelt[I,]
dfmelt$Source <- factor(dfmelt$Source, levels = c("TPSR", "SRI"))

###################################
# Generalized linear mixed models #
###################################

# Note:
# R2m: marginal R squared value associated with fixed effects
# R2c conditional R2 value associated with fixed effects plus the random effects.

res <- matrix(nrow = 14, ncol = 3)
rownames(res) <- colnames(df[3:ncol(df)])
colnames(res) <- c("R2_Pop", "R2_matfam", "R2_tot")

pval <- matrix(nrow = 14, ncol = 2)
rownames(pval) <- colnames(df[3:ncol(df)])
colnames(pval) <- c("Pop", "matfam")

#####
# SL
#####

hist(df$Seed_length_Ave, col="grey")
model <- lmer(Seed_length_Ave~df$Source+(1|Source:Maternal_ID), data = df)
hist(residuals(model), col = "grey")
qqnorm(residuals(model));qqline(residuals(model),col="red")

summary(model)
anova(model)
ranova(model)
var <- r.squaredGLMM(model);var
res[1,1] <- var[,1]; res[1,2] <- var[,2]-var[,1]; res[1,3] <- var[,2]
pval[1,1] <- anova(model)$`Pr(>F)`; pval[1,2] <- ranova(model)$`Pr(>Chisq)`[2]

dfm <- subset(dfmelt, subset = dfmelt$variable=="Seed_length_Ave")
ggplot()+geom_boxplot(data = dfm, aes(x=Maternal_ID, y=value, fill=Source))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="TPSR")], na.rm=T))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="SRI")], na.rm=T), linetype="dashed")
ggplot()+geom_boxplot(data = dfm, aes(x=Source, y=value, fill=Source))

#####
# SW
#####
hist(df$Seed_width_Ave, col="grey")
model <- lmer(Seed_width_Ave~df$Source+(1|Source:Maternal_ID), data = df)
hist(residuals(model), col = "grey")
qqnorm(residuals(model));qqline(residuals(model),col="red")

summary(model)
anova(model)
ranova(model)
var <- r.squaredGLMM(model);var
res[2,1] <- var[,1]; res[2,2] <- var[,2]-var[,1]; res[2,3] <- var[,2]
pval[2,1] <- anova(model)$`Pr(>F)`; pval[2,2] <- ranova(model)$`Pr(>Chisq)`[2]

dfm <- subset(dfmelt, subset = dfmelt$variable=="Seed_width_Ave")
ggplot()+geom_boxplot(data = dfm, aes(x=Maternal_ID, y=value, fill=Source))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="TPSR")], na.rm=T))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="SRI")], na.rm=T), linetype="dashed")
ggplot()+geom_boxplot(data = dfm, aes(x=Source, y=value, fill=Source))

#####
# SCW
#####
hist(df$Seed_coat_Ave_Aves, col="grey")
model <- lmer(Seed_coat_Ave_Aves~df$Source+(1|Source:Maternal_ID), data = df)
hist(residuals(model), col = "grey")
qqnorm(residuals(model));qqline(residuals(model),col="red")

summary(model)
anova(model)
ranova(model)
var <- r.squaredGLMM(model);var
res[3,1] <- var[,1]; res[3,2] <- var[,2]-var[,1]; res[3,3] <- var[,2]
pval[3,1] <- anova(model)$`Pr(>F)`; pval[3,2] <- ranova(model)$`Pr(>Chisq)`[2]

dfm <- subset(dfmelt, subset = dfmelt$variable=="Seed_coat_Ave_Aves")
ggplot()+geom_boxplot(data = dfm, aes(x=Maternal_ID, y=value, fill=Source))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="TPSR")], na.rm=T))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="SRI")], na.rm=T), linetype="dashed")
ggplot()+geom_boxplot(data = dfm, aes(x=Source, y=value, fill=Source))

#####
# EL
#####
hist(df$Embryo_length_Ave, col="grey")
model <- lmer(Embryo_length_Ave~df$Source+(1|Source:Maternal_ID), data = df)
hist(residuals(model), col = "grey")
qqnorm(residuals(model));qqline(residuals(model),col="red")

summary(model)
anova(model)
ranova(model)
var <- r.squaredGLMM(model);var
res[4,1] <- var[,1]; res[4,2] <- var[,2]-var[,1]; res[4,3] <- var[,2]
pval[4,1] <- anova(model)$`Pr(>F)`; pval[4,2] <- ranova(model)$`Pr(>Chisq)`[2]

dfm <- subset(dfmelt, subset = dfmelt$variable=="Embryo_length_Ave")
ggplot()+geom_boxplot(data = dfm, aes(x=Maternal_ID, y=value, fill=Source))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="TPSR")], na.rm=T))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="SRI")], na.rm=T), linetype="dashed")
ggplot()+geom_boxplot(data = dfm, aes(x=Source, y=value, fill=Source))

#####
# EW
#####
hist(df$Embryo_width_Ave, col="grey")
model <- lmer(Embryo_width_Ave~df$Source+(1|Source:Maternal_ID), data = df)
hist(residuals(model), col = "grey")
qqnorm(residuals(model));qqline(residuals(model),col="red")

summary(model)
anova(model)
ranova(model)
var <- r.squaredGLMM(model);var
res[5,1] <- var[,1]; res[5,2] <- var[,2]-var[,1]; res[5,3] <- var[,2]
pval[5,1] <- anova(model)$`Pr(>F)`; pval[5,2] <- ranova(model)$`Pr(>Chisq)`[2]

dfm <- subset(dfmelt, subset = dfmelt$variable=="Embryo_width_Ave")
ggplot()+geom_boxplot(data = dfm, aes(x=Maternal_ID, y=value, fill=Source))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="TPSR")], na.rm=T))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="SRI")], na.rm=T), linetype="dashed")
ggplot()+geom_boxplot(data = dfm, aes(x=Source, y=value, fill=Source))

#####
# SA
#####
hist(df$Seed_area_Ave, col="grey")
model <- lmer(Seed_area_Ave~df$Source+(1|Source:Maternal_ID), data = df)
hist(residuals(model), col = "grey")
qqnorm(residuals(model));qqline(residuals(model),col="red")

summary(model)
anova(model)
ranova(model)
var <- r.squaredGLMM(model);var
res[6,1] <- var[,1]; res[6,2] <- var[,2]-var[,1]; res[6,3] <- var[,2]
pval[6,1] <- anova(model)$`Pr(>F)`; pval[6,2] <- ranova(model)$`Pr(>Chisq)`[2]

dfm <- subset(dfmelt, subset = dfmelt$variable=="Seed_area_Ave")
ggplot()+geom_boxplot(data = dfm, aes(x=Maternal_ID, y=value, fill=Source))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="TPSR")], na.rm=T))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="SRI")], na.rm=T), linetype="dashed")
ggplot()+geom_boxplot(data = dfm, aes(x=Source, y=value, fill=Source))

#####
# ESA
#####
hist(df$Endosperm_area_Ave, col="grey")
model <- lmer(Endosperm_area_Ave~df$Source+(1|Source:Maternal_ID), data = df)
hist(residuals(model), col = "grey")
qqnorm(residuals(model));qqline(residuals(model),col="red")

summary(model)
anova(model)
ranova(model)
var <- r.squaredGLMM(model);var
res[7,1] <- var[,1]; res[7,2] <- var[,2]-var[,1]; res[7,3] <- var[,2]
pval[7,1] <- anova(model)$`Pr(>F)`; pval[7,2] <- ranova(model)$`Pr(>Chisq)`[2]

dfm <- subset(dfmelt, subset = dfmelt$variable=="Endosperm_area_Ave")
ggplot()+geom_boxplot(data = dfm, aes(x=Maternal_ID, y=value, fill=Source))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="TPSR")], na.rm=T))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="SRI")], na.rm=T), linetype="dashed")
ggplot()+geom_boxplot(data = dfm, aes(x=Source, y=value, fill=Source))

#####
# EA
#####
hist(df$Embryo_area_Ave, col="grey")
model <- lmer(Embryo_area_Ave~df$Source+(1|Source:Maternal_ID), data = df)
hist(residuals(model), col = "grey")
qqnorm(residuals(model));qqline(residuals(model),col="red")

summary(model)
anova(model)
ranova(model)
var <- r.squaredGLMM(model);var
res[8,1] <- var[,1]; res[8,2] <- var[,2]-var[,1]; res[8,3] <- var[,2]
pval[8,1] <- anova(model)$`Pr(>F)`; pval[8,2] <- ranova(model)$`Pr(>Chisq)`[2]

dfm <- subset(dfmelt, subset = dfmelt$variable=="Embryo_area_Ave")
ggplot()+geom_boxplot(data = dfm, aes(x=Maternal_ID, y=value, fill=Source))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="TPSR")], na.rm=T))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="SRI")], na.rm=T), linetype="dashed")
ggplot()+geom_boxplot(data = dfm, aes(x=Source, y=value, fill=Source))

#####
# SLW
#####
hist(df$SL_SW_ratio, col="grey")
model <- lmer(SL_SW_ratio~df$Source+(1|Source:Maternal_ID), data = df)
hist(residuals(model), col = "grey")
qqnorm(residuals(model));qqline(residuals(model),col="red")

summary(model)
anova(model)
ranova(model)
var <- r.squaredGLMM(model);var
res[9,1] <- var[,1]; res[9,2] <- var[,2]-var[,1]; res[9,3] <- var[,2]
pval[9,1] <- anova(model)$`Pr(>F)`; pval[9,2] <- ranova(model)$`Pr(>Chisq)`[2]

dfm <- subset(dfmelt, subset = dfmelt$variable=="SL_SW_ratio")
ggplot()+geom_boxplot(data = dfm, aes(x=Maternal_ID, y=value, fill=Source))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="TPSR")], na.rm=T))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="SRI")], na.rm=T), linetype="dashed")
ggplot()+geom_boxplot(data = dfm, aes(x=Source, y=value, fill=Source))

#####
# ELW
#####
hist(df$EL_EW_ratio, col="grey")
model <- lmer(EL_EW_ratio~df$Source+(1|Source:Maternal_ID), data = df)
hist(residuals(model), col = "grey")
qqnorm(residuals(model));qqline(residuals(model),col="red")

summary(model)
anova(model)
ranova(model)
var <- r.squaredGLMM(model);var
res[10,1] <- var[,1]; res[10,2] <- var[,2]-var[,1]; res[10,3] <- var[,2]
pval[10,1] <- anova(model)$`Pr(>F)`; pval[10,2] <- ranova(model)$`Pr(>Chisq)`[2]

dfm <- subset(dfmelt, subset = dfmelt$variable=="EL_EW_ratio")
ggplot()+geom_boxplot(data = dfm, aes(x=Maternal_ID, y=value, fill=Source))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="TPSR")], na.rm=T))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="SRI")], na.rm=T), linetype="dashed")
ggplot()+geom_boxplot(data = dfm, aes(x=Source, y=value, fill=Source))

#####
# SCA
#####
hist(df$Seed_coat_area, col="grey")
model <- lmer(Seed_coat_area~df$Source+(1|Source:Maternal_ID), data = df)
hist(residuals(model), col = "grey")
qqnorm(residuals(model));qqline(residuals(model),col="red")

summary(model)
anova(model)
ranova(model)
var <- r.squaredGLMM(model);var
res[11,1] <- var[,1]; res[11,2] <- var[,2]-var[,1]; res[11,3] <- var[,2]
pval[11,1] <- anova(model)$`Pr(>F)`; pval[11,2] <- ranova(model)$`Pr(>Chisq)`[2]

dfm <- subset(dfmelt, subset = dfmelt$variable=="Seed_coat_area")
ggplot()+geom_boxplot(data = dfm, aes(x=Maternal_ID, y=value, fill=Source))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="TPSR")], na.rm=T))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="SRI")], na.rm=T), linetype="dashed")
ggplot()+geom_boxplot(data = dfm, aes(x=Source, y=value, fill=Source))

#####
# RES
#####
hist(df$RES, col="grey")
model <- lmer(RES~df$Source+(1|Source:Maternal_ID), data = df)
hist(residuals(model), col = "grey")
qqnorm(residuals(model));qqline(residuals(model),col="red")

summary(model)
anova(model)
ranova(model)
var <- r.squaredGLMM(model);var
res[12,1] <- var[,1]; res[12,2] <- var[,2]-var[,1]; res[12,3] <- var[,2]
pval[12,1] <- anova(model)$`Pr(>F)`; pval[12,2] <- ranova(model)$`Pr(>Chisq)`[2]

dfm <- subset(dfmelt, subset = dfmelt$variable=="RES")
ggplot()+geom_boxplot(data = dfm, aes(x=Maternal_ID, y=value, fill=Source))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="TPSR")], na.rm=T))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="SRI")], na.rm=T), linetype="dashed")
ggplot()+geom_boxplot(data = dfm, aes(x=Source, y=value, fill=Source))

#####
# REndS
#####
hist(df$REndS, col="grey")
model <- lmer(REndS~df$Source+(1|Source:Maternal_ID), data = df)
hist(residuals(model), col = "grey")
qqnorm(residuals(model));qqline(residuals(model),col="red")

summary(model)
anova(model)
ranova(model)
var <- r.squaredGLMM(model);var
res[13,1] <- var[,1]; res[13,2] <- var[,2]-var[,1]; res[13,3] <- var[,2]
pval[13,1] <- anova(model)$`Pr(>F)`; pval[13,2] <- ranova(model)$`Pr(>Chisq)`[2]

dfm <- subset(dfmelt, subset = dfmelt$variable=="REndS")
ggplot()+geom_boxplot(data = dfm, aes(x=Maternal_ID, y=value, fill=Source))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="TPSR")], na.rm=T))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="SRI")], na.rm=T), linetype="dashed")
ggplot()+geom_boxplot(data = dfm, aes(x=Source, y=value, fill=Source))

#####
# RSCS
#####
hist(df$RSCS, col="grey")
model <- lmer(RSCS~df$Source+(1|Source:Maternal_ID), data = df)
hist(residuals(model), col = "grey")
qqnorm(residuals(model));qqline(residuals(model),col="red")

summary(model)
anova(model)
ranova(model)
var <- r.squaredGLMM(model);var
res[14,1] <- var[,1]; res[14,2] <- var[,2]-var[,1]; res[14,3] <- var[,2]
pval[14,1] <- anova(model)$`Pr(>F)`; pval[14,2] <- ranova(model)$`Pr(>Chisq)`[2]

dfm <- subset(dfmelt, subset = dfmelt$variable=="RSCS")
ggplot()+geom_boxplot(data = dfm, aes(x=Maternal_ID, y=value, fill=Source))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="TPSR")], na.rm=T))+
  geom_hline(yintercept = mean(dfm$value[which(dfm$Source=="SRI")], na.rm=T), linetype="dashed")
ggplot()+geom_boxplot(data = dfm, aes(x=Source, y=value, fill=Source))

# Final notes:

# the matrix "res" contains the variance explained by population origin, 
# maternal family within populations, and the total amount of variance explained.

# the matrix pval contains all the p values testing the significance of the
# fixed effect (population origin) using anova() as well as all the p values
# testing the significance of the random effect (maternal family within)
# populations using ranova().

######################################################
# Boxplot of variance explained (R2m, R2c) per trait #
######################################################
resplot <- res[,-3]; resplot
resplot <- data.frame(resplot)
resplot$R2_err <- 1-res[,3]
str(resplot)
resplot <- as.matrix(resplot)
resmelt <- reshape2::melt(resplot)
resmelt$Var1 <- rep(c("SL", "SW", "SCW", "EL", "EW", "SA", "ESA", "EA", "SLW", "ELW",
                      "SCA", "RES", "REndS", "RSCS"), times=3)
resmelt$Var2 <- factor(resmelt$Var2, levels = c("R2_Pop", "R2_matfam", "R2_err"))

ggplot(data=resmelt, aes(x=Var1, y=value, fill=Var2))+geom_bar(stat = "identity", position = position_stack(reverse = T))+
  theme_classic()+coord_flip()+theme(panel.grid = element_blank())+
  scale_fill_manual(labels = c("Populations", "Maternal families", "Residuals"),
                    values = c("grey75", "grey55", "grey35"),
                    name = element_blank())+
  ylab("\nProportion of variance explained")+xlab("Seed trait\n")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.position = "top")
