if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools,np,lazyeval, hmisc,janitor,interp, lmtest,gt, modelsummary, dplyr,pdftools, tidyverse,rattle,glmnet,caret, rpart.plot, RcolorBrewer,rpart, tidyr, mice, stringr,randomForest,  curl, plm, readxl, zoo, stringr, patchwork,  sf, clubSandwich, modelsummary, sjPlot)

df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care_Entries_Data/refs/heads/main/Data/Clean/final_data.csv"))

####Harmonising Michelle's graph####
michelle <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care_Entries_Data/refs/heads/main/Data/Raw/michelle_data.csv"))%>%
  dplyr::select(newchildren_england, Pop_19under,year)%>%
  dplyr::mutate(michelle_child_per = newchildren_england/(Pop_19under/100000))%>%
  dplyr::rename(michelle_population = Pop_19under,
                michelle_new_children = newchildren_england)

entries <- df %>%
  dplyr::filter(
    (variable=="Total children"& geography_scale=="NATIONAL")|
      (variable=="Total"& subcategory=="Taken into care"& geography_scale=="NATIONAL")|
                 (variable=="Under 18_Total"& geography_scale=="NATIONAL")|
      (category=="Harmonisation data")
                )%>%
  dplyr::mutate(variable = ifelse(variable=="Total", "Total children", variable))%>%
  dplyr::select(year, variable, number)%>%
  tidyr::pivot_wider(id_cols = "year", names_from = "variable", values_from = "number")%>%
  dplyr::rename(updated_new_children = `Children starting to be looked after during the year`,
                concurrent_reported_children = `Total children`,
                ben_population = `Under 18_Total`)%>%
  dplyr::mutate(ben_child_per = as.numeric(updated_new_children)/(as.numeric(ben_population)/100000))
  

plot_df <- full_join(entries, michelle)




####care entries by geography####

####taken into care deprivation inequality over time ####


####Care entries by age, sex and reason####



