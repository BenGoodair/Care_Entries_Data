if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools,np,lazyeval, hmisc,janitor,interp, lmtest,gt, modelsummary, dplyr,pdftools, tidyverse,rattle,glmnet,caret, rpart.plot, RcolorBrewer,rpart, tidyr, mice, stringr,randomForest,  curl, plm, readxl, zoo, stringr, patchwork,  sf, clubSandwich, modelsummary, sjPlot)

df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care_Entries_Data/refs/heads/main/Data/Clean/final_data.csv"))

####Harmonising Michelle's graph####


####care entries by geography####

####taken into care deprivation inequality over time ####


####Care entries by age, sex and reason####



