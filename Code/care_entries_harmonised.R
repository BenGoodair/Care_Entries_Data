
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


df <- characteristics %>%
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
                  str_trim())%>%
  dplyr::filter(category=="started during")%>%
  dplyr::mutate(LA_Name = ifelse(LA_Name=="England", "England_current_year_reporting", LA_Name))




    
    
rural <-     read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/00546ccef58bb7531f75b5b3b378b562af503b4b/Raw_Data/LA_level/Economic_Political_Contextual/17_10_2023_-_Rural-Urban_Classification_2011_lookup_tables_for_higher_level_geographies.csv"), skip=3)%>%
              dplyr::mutate(LA_Name = Upper.Tier.Local.Authority.Area.2021.Name %>%
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
                              gsub("COUNTY DURHAM", "DURHAM", .)%>%
                              str_trim())
  



