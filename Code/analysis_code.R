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
                 (variable=="Under 19_Total"& geography_scale=="NATIONAL")|
      (category=="Harmonisation data")
                )%>%
  dplyr::select(year, variable, number)%>%
  tidyr::pivot_wider(id_cols = "year", names_from = "variable", values_from = "number")%>%
  dplyr::rename(updated_new_children = `Children starting to be looked after during the year`,
                concurrent_reported_children = `Total children`,
                ben_population = `Under 19_Total`)%>%
  dplyr::mutate(ben_child_per = as.numeric(updated_new_children)/(as.numeric(ben_population)/100000))
  

plot_df <- full_join(entries, michelle)

ggplot(plot_df, aes(x=year))+
  geom_line(aes(y=michelle_new_children), colour = "#7F1734")+
  geom_line(aes(y=as.numeric(updated_new_children)), colour = "Navy")+
  theme_bw()+
  labs(title = "Harmonised children starting to be looked after in England",
       subtitle = "Claret = Michelle's data; Navy = updated values with overlap",
       x = "Year",
       y = "Children starting to be looked after in England")+
  coord_cartesian(xlim = c(1970,2024))

ggplot(plot_df%>%dplyr::filter(year>2010), aes(x=year))+
  geom_line(aes(y=ben_child_per), colour = "Navy")+
  theme_bw()+
  labs(title = "Updated children starting to be looked after in England",
       subtitle = "per 100k under 19 population",
       x = "Year",
       y = "Children starting to be looked after in England\n(per 100k under 19 population)")+
  coord_cartesian(xlim = c(2011,2024))+
  scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024))



####care entries by geography####



####taken into care deprivation inequality over time ####


####Care entries by age, sex and reason####



