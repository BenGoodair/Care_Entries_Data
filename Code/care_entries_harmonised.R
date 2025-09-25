
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
  dplyr::mutate(LA_Name = ifelse(LA_Name=="ENGLAND", "England_current_year_reporting", LA_Name))%>%
  dplyr::mutate(LA_Name = ifelse(LA_Name=="DURHAM", "COUNTY DURHAM", LA_Name))



    
    
rural <-read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/00546ccef58bb7531f75b5b3b378b562af503b4b/Raw_Data/LA_level/Economic_Political_Contextual/17_10_2023_-_Rural-Urban_Classification_2011_lookup_tables_for_higher_level_geographies.csv"), skip=3)%>%
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
                              str_trim())%>%
  dplyr::filter(LA_Name!="")%>%
  dplyr::select(-Upper.Tier.Local.Authority.Area.2021.Name)%>%
  dplyr::rename(LA_Code = Upper.Tier.Local.Authority.Area.2021.Code)%>%
  tidyr::pivot_longer(cols = c('Rural.Urban.Classification.2011..6.fold.', 'Rural.Urban.Classification.2011..3.fold.'), names_to = "variable", values_to = "number")%>%
  dplyr::mutate(LA.Number = NA,
                category = "Area variables",
                subcategory = "Rurality",
                percent= NA)%>%
  crossing(year = 2010:2024)

idaci <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care_Entries_Data/refs/heads/main/Data/Raw/idacilarge.csv"))%>%
  dplyr::rename(LA_Code = LAD19CD)%>%
  dplyr::left_join(df %>% dplyr::select(LA_Name, LA_Code)%>% dplyr::distinct(.keep_all = T)%>%dplyr::filter(!is.na(LA_Code)))%>%
  tidyr::pivot_longer(cols = c('IDACI...Average.rank', 'IDACI...Average.score', 'IDACI...Proportion..of.LSOAs.in.most.deprived.10..nationally'), names_to = "variable", values_to = "number")%>%
  dplyr::mutate(LA.Number = NA,
                category = "Area variables",
                subcategory = "Deprivation affecting children",
                percent= NA)%>%
  crossing(year = 2010:2024)


pop1 <- read_csv(
  curl::curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/myebtablesewsn20012011.csv"),
  skip = 1,        # drop the first metadata row
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  rename(
    lad_code  = ladcode18,
    geography = laname18
  ) %>%
  # pick off only age & the "population_YYYY" cols
  pivot_longer(
    cols = starts_with("population_"),
    names_to  = "year",
    names_prefix = "population_",
    values_to = "pop"
  ) %>%
  mutate(
    year = as.integer(year),
    age  = as.integer(age),
    pop = as.integer(gsub(",", "", pop))
    )%>%
  dplyr::select(year,age,lad_code,geography,pop)%>%
  dplyr::filter(year==2010)

pop1 <- pop1 %>%
  mutate(age_group = case_when(
    age < 5 ~ "Under 5",
    age >= 5 & age <= 15 ~ "5–15",
    age >= 16 & age <= 18 ~ "16–18"
  )) %>%
  group_by(year, lad_code, geography, age_group) %>%
  summarise(pop_sum = sum(pop), .groups = "drop") %>%
  bind_rows(
    pop1 %>%
      filter(age < 18) %>%
      group_by(year, lad_code, geography) %>%
      summarise(pop_sum = sum(pop), .groups = "drop") %>%
      mutate(age_group = "Under 18")
  ) %>%
  arrange(year, lad_code, age_group)

pop2 <- read_csv(
  curl::curl("https://raw.githubusercontent.com/BenGoodair/Care_Entries_Data/refs/heads/main/Data/Raw/myebtablesenglandwales20112024.csv"),
  skip = 1,
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  rename(
    lad_code  = ladcode23,
    geography = laname23
  ) %>%
  pivot_longer(
    cols = starts_with("population_"),
    names_to  = "year",
    names_prefix = "population_",
    values_to = "pop"
  ) %>%
  mutate(
    year = as.integer(year),
    age  = as.integer(age),
    pop = as.integer(gsub(",", "", pop))
    )%>%
  dplyr::select(year,age,lad_code,geography,pop)


pop2 <- pop2 %>%
  mutate(age_group = case_when(
    age < 5 ~ "Under 5",
    age >= 5 & age <= 15 ~ "5–15",
    age >= 16 & age <= 18 ~ "16–18"
  )) %>%
  group_by(year, lad_code, geography, age_group) %>%
  summarise(pop_sum = sum(pop), .groups = "drop") %>%
  bind_rows(
    pop2 %>%
      filter(age < 18) %>%
      group_by(year, lad_code, geography) %>%
      summarise(pop_sum = sum(pop), .groups = "drop") %>%
      mutate(age_group = "Under 18")
  ) %>%
  arrange(year, lad_code, age_group)

pop <- rbind(pop1, pop2)%>%
  dplyr::filter(!is.na(age_group))%>%
  dplyr::mutate(ltla_Name = geography %>%
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

utlalookup <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care_Entries_Data/refs/heads/main/Data/Raw/Local_Authority_District_to_County_and_Unitary_Authority_(April_2023)_Lookup_in_EW.csv"))%>%
  dplyr::mutate(ltla_Name = LTLA23NM %>%
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
  


pop_utla <- full_join(pop, utlalookup)%>%
  dplyr::select(UTLA23NM,UTLA23CD, age_group, pop_sum, year)%>%
  dplyr::group_by(age_group, year, UTLA23NM,UTLA23CD )%>%
  dplyr::summarise(pop_sum = sum(pop_sum))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(LA_Name = UTLA23NM %>%
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




popcheck <- full_join(pop_utla,
            df %>%dplyr::select(LA_Name)%>%
              dplyr::distinct(.keep_all = T)%>%
              dplyr::mutate(keep="keep"))%>%
  dplyr::filter(keep=="keep")

regionlookup <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care_Entries_Data/refs/heads/main/Data/Raw/Local_Authority_District_(December_2018)_to_NUTS3_to_NUTS2_to_NUTS1_(January_2018)_Lookup_in_United_Kingdom.csv"))%>%
  dplyr::mutate(ltla_Name = LAD18NM %>%
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


pop_reg <- full_join(pop, regionlookup)%>%
  dplyr::select(NUTS118NM, age_group, pop_sum, year)%>%
  dplyr::group_by(age_group, year, NUTS118NM )%>%
  dplyr::summarise(pop_sum = sum(pop_sum))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(LA_Name = NUTS118NM %>%
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





pop <- 
  
  
  
  
  full_join(pop,
                 df %>%dplyr::select(LA_Name)%>%
                   dplyr::distinct(.keep_all = T)%>%
                   dplyr::mutate(keep="keep"))%>%
  dplyr::filter(keep=="keep")






