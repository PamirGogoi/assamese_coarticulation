########################################################
#Using output from escuderoOptimization Praat script
#
#Here only one speaker was used (if multiple speakers,
#change .(Vowel, Ceiling) to .(Speaker, Vowel, Ceiling)
#and add Speaker column to output data frame).
#Change line#11 and line#57 for data storage
#change names of columns according to your csv file
########################################################

#load data
#data = read.table("./formantConfig.txt", header=TRUE)
#data = read.table("formantConfig.txt", header=TRUE)
F2_opt_SF$Ceiling = factor(F2_opt_SF$Ceiling) 
#make sure ceiling is categorical
#data$Ceiling <- as.factor(data$Ceiling)


###########change column names here################
str(F2_opt_SF)
#log-transform formant values for mid-vowel formants
F2_opt_SF$LogF2_5 = log(F2_opt_SF$V2_F2_5)
F2_opt_SF$LogF2_50 = log(F2_opt_SF$V2_F2_50)
#get variances
library(plyr)
varData = ddply(F2_opt_SF,
                .(Speaker, Vowel, Ceiling),
                summarise,
                F1var = var(LogF2_5),
                F2var = var(LogF2_50))

varData$F2var5_50 = varData$F1var + varData$F2var
View(F1var)

#set labels: Change labels

varDataA = varData[varData$Vowel=="a", ]
ceilA = as.character(varDataA$Ceiling[which.min(varDataA$F1F2var)])

varDataI = varData[varData$Vowel=="i", ]
ceilI = as.character(varDataI$Ceiling[which.min(varDataI$F1F2var)])

varDataU = varData[varData$Vowel=="u", ]
ceilU = as.character(varDataU$Ceiling[which.min(varDataU$F1F2var)])

#varDataAA = varData[varData$Vowel=="aa", ]
#ceilAA = as.character(varDataAA$Ceiling[which.min(varDataAA$F1F2var)])

#varDataOH = varData[varData$Vowel=="oh", ]
#ceilOH = as.character(varDataOH$Ceiling[which.min(varDataOH$F1F2var)])

#varDataEE = varData[varData$Vowel=="ee", ]
#ceilEE = as.character(varDataEE$Ceiling[which.min(varDataEE$F1F2var)])

ceiling=c(ceilA, ceilI, ceilU)
#output: change to include all labels
ceilings = data.frame(vowel=levels(data$Vowel),
                      ceiling)
write.csv(ceilings, "./ceilings.csv", row.names=FALSE)

######################Create a data frame with all tokens in order#################
#vowel_list = data.frame(vowel = c(data(1:288)), )
#library(sqldf)
#z$color="0" # to avoid conflicts numeric/characters
#z <- sqldf(c("UPDATE z
#             SET color = (SELECT y.color
#             FROM y
#             WHERE z.letter = y.letter
#             )
#             WHERE EXISTS (SELECT 1
#             FROM y
#             WHERE z.letter = y.letter
#             )"
#             , "select * from main.z"
#)
#)
#z