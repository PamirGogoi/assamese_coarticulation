## for vowel plot
install.packages("tidyverse")
install.packages("phonR")
install.packages("phonTools")
install.packages("vowels")
library(tidyverse)
library(phonR)
library(phonTools)
library(vowels)
library(ggplot2)
library(readxl)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

raw <- read_excel("added_data/f1_f2_assamese.xlsx")
f2_only <- read_excel("added_data/f2_assamese.xlsx")
view(assamese)


ggplot(assamese, aes(x = F2_50, y = F1_50, color = POA)) + geom_point(aes(shape = Vowel),alpha = 0.3) + stat_ellipse()+ theme_minimal() +scale_color_manual(values = cbPalette[c(1:9, 1, 2)]) + scale_x_reverse() + scale_y_reverse() + scale_shape_manual(values = 1:10)

ggplot(assamese, aes(x = F2_50, y = F1_50, color = Vowel)) + geom_point(alpha = 0.2) + stat_ellipse()+ theme_minimal() +scale_color_manual(values = cbPalette[c(1:9, 1, 2)]) + scale_x_reverse() + scale_y_reverse() + scale_shape_manual(values = 1:10)
