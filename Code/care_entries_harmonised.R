
if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools,np,lazyeval, hmisc,interp, lmtest,gt, modelsummary, dplyr,pdftools, tidyverse,rattle,glmnet,caret, rpart.plot, RcolorBrewer,rpart, tidyr, mice, stringr,randomForest,  curl, plm, readxl, zoo, stringr, patchwork,  sf, clubSandwich, modelsummary, sjPlot)


####Data####
source("https://raw.githubusercontent.com/BenGoodair/Care_Entries_Data/refs/heads/main/Code/full_data_inc_eng_function.R")
characteristics <- create_characteristics_data()

dplyr::mutate(LA_Name = LA_Name %>%
                gsub('&', 'and', .) %>%
                gsub('[[:punct:] ]+', ' ', .) %>%
                gsub('[0-9]', '', .)%>%
                toupper() %>%
                gsub("CITY OF", "",.)%>%
                gsub("UA", "",.)%>%
                gsub("COUNTY OF", "",.)%>%
                gsub("ROYAL BOROUGH OF", "",.)%>%
                gsub("LEICESTER CITY", "LEICESTER",.)%>%
                gsub("UA", "",.)%>%
                gsub("DARWIN", "DARWEN", .)%>%
                gsub("AND DARWEN", "WITH DARWEN", .)%>%
                gsub("NE SOM", "NORTH EAST SOM", .)%>%
                gsub("N E SOM", "NORTH EAST SOM", .)%>%
                str_trim())


df_raw <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/refs/heads/main/Final_Data/outputs/dashboard_data.csv"))

df <- characteristics %>%
  dplyr::filter(category=="started during")







