#attach(F2_opt_SF)

install.packages("phonR")
install.packages("dplyr")
install.packages("base")
install.packages("emuR")
install.packages("xlsx")
library(lattice)
library(plyr)
library(dplyr)
library(phonR)
library(lme4)
library(emuR)
library(ggplot2)



#Lobanov normalizing by speaker
#normF1_5 <- with(F2Values_V, normLobanov(F1_five), group=Speaker)
#normF2_5 <- with(F2_opt_SF, normLobanov(F2_5), group=Speaker)
#normF1_50 <- with(F2Values_V, normLobanov(F1_mid), group=Speaker)
#normF2_50 <- with(F2_opt_SF, normLobanov(F2_50), group=Speaker)


#newlobanov <- data.frame(FileName, Speaker, Sex, Vowel, Consonant, POA, normF2_5, normF2_50)

#View(newlobanov)



#Subsetting the data by Speaker and POA then by vowel front_backness=
#SpeakerSF1 to SF5 (female speakers)
SF1data <- subset(ceilings, Speaker=="SF1")
SF2data <- subset(ceilings, Speaker=="SF2")
SF3data <- subset(ceilings, Speaker=="SF3")
SF4data <- subset(ceilings, Speaker=="SF4")
SF5data <- subset(ceilings, Speaker=="SF5")



#SpeakerS1 to S5 (male speakers)
S1data <- subset(ceilings, Speaker=="SM1")
S2data <- subset(ceilings, Speaker=="SM2")
S3data <- subset(ceilings, Speaker=="SM3")
S4data <- subset(ceilings, Speaker=="SM4")
S5data <- subset(ceilings, Speaker=="SM5")

###############################################################################
#POA Subsetting data (SF1)
labdata_SF1 <- subset(SF1data, Consonant =='b' | Consonant=='p')
alvdata_SF1 <- subset(SF1data, Consonant =='d' | Consonant=='t')
veldata_SF1 <- subset(SF1data, Consonant =='g' | Consonant=='k')

#POA Subsetting data (SF2)
labdata_SF2 <- subset(SF2data, Consonant =='b' | Consonant=='p')
alvdata_SF2 <- subset(SF2data, Consonant =='d' | Consonant=='t')
veldata_SF2 <- subset(SF2data, Consonant =='g' | Consonant=='k')

#POA Subsetting data (SF3)
labdata_SF3 <- subset(SF3data, Consonant =='b' | Consonant=='p')
alvdata_SF3 <- subset(SF3data, Consonant =='d' | Consonant=='t')
veldata_SF3 <- subset(SF3data, Consonant =='g' | Consonant=='k')

#POA Subsetting data (SF4)
labdata_SF4 <- subset(SF4data, Consonant =='b' | Consonant=='p')
alvdata_SF4 <- subset(SF4data, Consonant =='d' | Consonant=='t')
veldata_SF4 <- subset(SF4data, Consonant =='g' | Consonant=='k')

#POA Subsetting data (SF5)
labdata_SF5 <- subset(SF5data, Consonant =='b' | Consonant=='p')
alvdata_SF5 <- subset(SF5data, Consonant =='d' | Consonant=='t')
veldata_SF5 <- subset(SF5data, Consonant =='g' | Consonant=='k')


###############################################################################
#POA Subsetting data (S1)
labdata_SM1 <- subset(S1data, Consonant =='b' | Consonant=='p')
alvdata_SM1 <- subset(S1data, Consonant =='d' | Consonant=='t')
veldata_SM1 <- subset(S1data, Consonant =='g' | Consonant=='k')

#POA Subsetting data (S2)
labdata_SM2 <- subset(S2data, Consonant =='b' | Consonant=='p')
alvdata_SM2 <- subset(S2data, Consonant =='d' | Consonant=='t')
veldata_SM2 <- subset(S2data, Consonant =='g' | Consonant=='k')

#POA Subsetting data (S3)
labdata_SM3 <- subset(S3data, Consonant =='b' | Consonant=='p')
alvdata_SM3 <- subset(S3data, Consonant =='d' | Consonant=='t')
veldata_SM3 <- subset(S3data, Consonant =='g' | Consonant=='k')

#POA Subsetting data (S4)
labdata_SM4 <- subset(S4data, Consonant =='b' | Consonant=='p')
alvdata_SM4 <- subset(S4data, Consonant =='d' | Consonant=='t')
veldata_SM4 <- subset(S4data, Consonant =='g' | Consonant=='k')

#POA Subsetting data (S5)
labdata_SM5 <- subset(S5data, Consonant =='b' | Consonant=='p')
alvdata_SM5 <- subset(S5data, Consonant =='d' | Consonant=='t')
veldata_SM5 <- subset(S5data, Consonant =='g' | Consonant=='k')


###########


#LEs for labials_SF1
resultlabial_SF1<- locus(labdata_SF1$F2_50, labdata_SF1$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultlabial_SF1)
resultlabial_SF1$coeff 
resultlabial_SF1$locus

resultlabial_SF2<- locus(labdata_SF2$F2_50, labdata_SF2$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultlabial_SF2)
resultlabial_SF2$coeff
resultlabial_SF2$locus

resultlabial_SF3 <- locus(labdata_SF3$F2_50, labdata_SF3$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultlabial_SF3)
resultlabial_SF3$coeff
resultlabial_SF3$locus

resultlabial_SF4 <- locus(labdata_SF4$F2_50, labdata_SF4$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultlabial_SF4)
resultlabial_SF4$coeff
resultlabial_SF4$locus

resultlabial_SF5<- locus(labdata_SF5$F2_50, labdata_SF5$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultlabial_SF5)
resultlabial_SF5$coeff
resultlabial_SF5$locus
########################################
##################

resultlabial_SM1<- locus(labdata_SM1$F2_50, labdata_SM1$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultlabial_SM1)
resultlabial_SM1$coeff
resultlabial_SM1$locus

resultlabial_SM2 <- locus(labdata_SM2$F2_50, labdata_SM2$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultlabial_SM2)
resultlabial_SM2$coeff
resultlabial_SM2$locus

resultlabial_SM3 <- locus(labdata_SM3$F2_50, labdata_SM3$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultlabial_SM3)
resultlabial_SM3$coeff
resultlabial_SM3$locus

resultlabial_SM4 <- locus(labdata_SM4$F2_50, labdata_SM4$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultlabial_SM4)
resultlabial_SM4$coeff
resultlabial_SM4$locus

resultlabial_SM5<- locus(labdata_SM5$F2_50, labdata_SM5$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultlabial_SM5)
resultlabial_SM5$coeff
resultlabial_SM5$locus


#LEs for alveolars_SF
resultalveolar_SF1 <- locus(alvdata_SF1$F2_50, alvdata_SF1$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultalveolar_SF1)
resultalveolar_SF1$coeff
resultalveolar_SF1$locus


resultalveolar_SF2<- locus(alvdata_SF2$F2_50, alvdata_SF2$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultalveolar_SF2)
resultalveolar_SF2$coeff
resultalveolar_SF2$locus


resultalveolar_SF3<- locus(alvdata_SF3_a$F2_50, alvdata_SF3_a$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultalveolar_SF3)
resultalveolar_SF3$coeff
resultalveolar_SF3$locus


resultalveolar_SF4 <- locus(alvdata_SF4$F2_50, alvdata_SF4$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultalveolar_SF4)
resultalveolar_SF4$coeff
resultalveolar_SF4$locus



resultalveolar_SF5 <- locus(alvdata_SF5$F2_50, alvdata_SF5$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultalveolar_SF5)
resultalveolar_SF5$coeff
resultalveolar_SF5$locus

#####
resultalveolar_SM1 <- locus(alvdata_SM1$F2_50, alvdata_SM1$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultalveolar_SM1)
resultalveolar_SM1$coeff
resultalveolar_SM1$locus

resultalveolar_SM2 <- locus(alvdata_SM2$F2_50, alvdata_SM2$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultalveolar_SM2)
resultalveolar_SM2$coeff
resultalveolar_SM2$locus

resultalveolar_SM3 <- locus(alvdata_SM3$F2_50, alvdata_SM3$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultalveolar_SM3)
resultalveolar_SM3$coeff
resultalveolar_SM3$locus

resultalveolar_SM4 <- locus(alvdata_SM4$F2_50, alvdata_SM4$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultalveolar_SM4)
resultalveolar_SM4$coeff
resultalveolar_SM4$locus

resultalveolar_SM5 <- locus(alvdata_SM5$F2_50, alvdata_SM5$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultalveolar_SM5)
resultalveolar_SM5$coeff
resultalveolar_SM5$locus

######################velar########

resultvelar_SF1 <- locus(veldata_SF1$F2_50, veldata_SF1$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultvelar_SF1)
resultvelar_SF1$coeff
resultvelar_SF1$locus


resultvelar_SF2 <- locus(veldata_SF2$F2_50, veldata_SF2$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultvelar_SF2)
resultvelar_SF2$coeff
resultvelar_SF2$locus

resultvelar_SF3 <- locus(veldata_SF3$F2_50, veldata_SF3$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultvelar_SF3)
resultvelar_SF3$coeff
resultvelar_SF3$locus

resultvelar_SF4<- locus(veldata_SF4$F2_50, veldata_SF4$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultvelar_SF4)
resultvelar_SF4$coeff
resultvelar_SF4$locus

resultvelar_SF5 <- locus(veldata_SF5$F2_50, veldata_SF5$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultvelar_SF5)
resultvelar_SF5$coeff
resultvelar_SF5$locus
##################


resultvelar_SM1 <- locus(veldata_SM1$F2_50, veldata_SM1$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultvelar_SM1)
resultvelar_SM1$coeff
resultvelar_SM1$locus


resultvelar_SM2 <- locus(veldata_SM2$F2_50, veldata_SM2$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultvelar_SM2)
resultvelar_SM2$coeff
resultvelar_SM2$locus

resultvelar_SM3 <- locus(veldata_SM3$F2_50, veldata_SM3$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultvelar_SM3)
resultvelar_SM3$coeff
resultvelar_SM3$locus

resultvelar_SM4<- locus(veldata_SM4$F2_50, veldata_SM4$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultvelar_SM4)
resultvelar_SM4$coeff
resultvelar_SM4$locus

resultvelar_SM5 <- locus(veldata_SM5$F2_50, veldata_SM5$F2_5, labels.vow = TRUE, yxline = TRUE, plotgraph = TRUE, axes = TRUE) 
summary(resultvelar_SM5)
resultvelar_SM5$coeff
resultvelar_SM5$locus

###############################################################

p2 <- ggplot(ceilings, aes(x=Sex, y=F2_5)) + geom_boxplot()
p2 + facet_grid(~ POA)

#######PLOT FOR BACK VOWEL###############


Ceiling <- ggplot(ceilings, aes(F2_50, F2_5, shape=POA, colour=POA, fill=POA)) +
  geom_smooth(method="lm") + geom_point(size=0.5) +
  theme_bw() + xlab("F2V at midpoint of the vowel /u/") +
  ylab("F2C at 5% of the vowel")
Ceiling


#################
###Check the variance of F2C for velars compared to alveolars for [u]?

#install.packages("cowplot")

#library(cowplot)
#plot_grid(FrontV, BackV, labels = "AUTO")

###################
ggplot(ceilings, aes(F2_50, F2_5, shape=POA, colour=POA, fill=POA)) +
  geom_smooth(method="lm") + geom_point(size=0.5) +
  theme_bw() + xlab("F2V at midpoint of the vowel") +
  ylab("F2C at 5% of the vowel")+
  facet_wrap(~Sex)

#############################################
#### ANOVA ##########

attach(slope_intecept_latest_without_neg)
locusmodel <- lmer(Target~POA +Sex + (1|Speaker), data=slope_intecept_latest_without_neg)
locusnull <- lmer(Target~Sex + (1|Speaker), data=slope_intecept_latest_without_neg)
summary(locusmodel)
anova(locusmodel,locusnull)
locusaov<-aov(Target~POA)
TukeyHSD(locusaov)
#newlocus 
boxplot(Target~POA, data = slope_intecept_latest_without_neg, ylab="First order LE Intercepts", xlab="Assamese stop places of articulation")

################
#### Needs to be fixed######

Scatterplot <- qplot(x=, y=F2_5, data=ceilings, geom = "POA") 
scatterplot + geom_abline(aes(intercept=Intercept, slope=Slope,
                              colour=Consonant), data=slope_intecept_latest_without_neg)

################

ggplot(ceilings, aes(F2_50, F2_5, shape=POA, colour=POA, fill=POA)) +
  geom_smooth(method="lm") + geom_point(size=0.5) +
  theme_bw() + xlab("F2V at midpoint of the vowel") +
  ylab("F2C at 5% of the vowel")




