
if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools,np,lazyeval, hmisc,janitor,interp, lmtest,gt, modelsummary, dplyr,pdftools, tidyverse,rattle,glmnet,caret, rpart.plot, RcolorBrewer,rpart, tidyr, mice, stringr,randomForest,  curl, plm, readxl, zoo, stringr, patchwork,  sf, clubSandwich, modelsummary, sjPlot)


####Data####
source("https://raw.githubusercontent.com/BenGoodair/Care_Entries_Data/refs/heads/main/Code/full_data_inc_eng_function.R")
characteristics <- create_characteristics_data()




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
  dplyr::mutate(LA_Name = ifelse(LA_Name=="DURHAM", "COUNTY DURHAM", LA_Name),
                LA_Name = ifelse(LA.Number=="201", "CITY OF LONDON", LA_Name))



    
    
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
  crossing(year = 2010:2024)%>%
  dplyr::mutate(LA_Name = ifelse(LA_Name=="LONDON", "CITY OF LONDON", LA_Name))


idaci <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care_Entries_Data/refs/heads/main/Data/Raw/idacilarge.csv"))%>%
  dplyr::rename(LA_Code = LAD19CD)%>%
  dplyr::left_join(df %>% dplyr::select(LA_Name, LA_Code)%>% dplyr::distinct(.keep_all = T)%>%dplyr::filter(!is.na(LA_Code)))%>%
  tidyr::pivot_longer(cols = c('IDACI...Average.rank', 'IDACI...Average.score', 'IDACI...Proportion..of.LSOAs.in.most.deprived.10..nationally'), names_to = "variable", values_to = "number")%>%
  dplyr::mutate(LA.Number = NA,
                category = "Area variables",
                subcategory = "Deprivation affecting children",
                percent= NA)%>%
  crossing(year = 2010:2024)%>%
  dplyr::mutate(LA_Name = ifelse(LA_Name=="LONDON", "CITY OF LONDON", LA_Name))




pop_1 <- read_csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/myebtablesewsn20012011.csv"),
                      skip = 1, col_types = cols(.default = "c")) %>%
  clean_names() %>%
  rename(lad_code = ladcode18, geography = laname18) %>%
  pivot_longer(starts_with("population_"), names_prefix="population_", names_to="year", values_to="pop") %>%
  mutate(year = as.integer(year), age = as.integer(age), pop = as.integer(gsub(",", "", pop)),
         sex = case_when(str_to_upper(str_trim(sex)) %in% c("F","FEMALE") ~ "F",
                         str_to_upper(sex) %in% c("M","MALE") ~ "M",
                         str_to_upper(sex) %in% c("ALL","TOTAL") ~ "All",
                         TRUE ~ NA_character_)) %>%
  filter(year == 2010) -> pop

# age groups and main summaries
by_age <- pop %>%
  mutate(age_group = case_when(age < 5 ~ "Under 5", age <= 15 ~ "5–15", age <= 18 ~ "16–18", TRUE ~ NA_character_)) %>%
  filter(!is.na(age_group)) %>%
  group_by(year, sex, lad_code, geography, age_group) %>%
  summarise(pop_sum = sum(pop, na.rm=TRUE), .groups="drop")

under18      <- pop %>% filter(age < 19) %>% group_by(year, sex, lad_code, geography) %>% summarise(pop_sum=sum(pop), .groups="drop") %>% mutate(age_group="Under 19")
all_by_sex   <- pop %>% group_by(year, sex, lad_code, geography) %>% summarise(pop_sum=sum(pop), .groups="drop") %>% mutate(age_group="All ages")
all_combined <- pop %>% group_by(year, lad_code, geography) %>% summarise(pop_sum=sum(pop), .groups="drop") %>% mutate(age_group="All ages", sex="All")
agegrp_tot   <- by_age %>% group_by(year, lad_code, geography, age_group) %>% summarise(pop_sum=sum(pop_sum), .groups="drop") %>% mutate(sex="All")
under18_all      <- under18 %>% dplyr::select(-sex) %>% group_by(age_group, year, lad_code, geography) %>% summarise(pop_sum=sum(pop_sum), .groups="drop") %>% mutate(sex="All")

pop1 <- bind_rows(by_age, under18, all_by_sex, all_combined, agegrp_tot, under18_all) %>%
  arrange(year, lad_code, geography, age_group, sex) %>%
  mutate(sex = replace_na(sex, "Unknown"), ageandsex = paste0(age_group, "_", sex))

library(tidyverse); library(janitor); library(curl)

pop2 <- read_csv(curl("https://raw.githubusercontent.com/BenGoodair/Care_Entries_Data/refs/heads/main/Data/Raw/myebtablesenglandwales20112024.csv"),
                 skip = 1, col_types = cols(.default = "c")) %>%
  clean_names() %>%
  rename(lad_code = ladcode23, geography = laname23) %>%
  pivot_longer(starts_with("population_"), names_prefix="population_", names_to="year", values_to="pop") %>%
  mutate(year = as.integer(year), age = as.integer(age), pop = as.integer(gsub(",", "", pop)),
         sex = case_when(str_to_upper(str_trim(sex)) %in% c("F","FEMALE") ~ "F",
                         str_to_upper(sex) %in% c("M","MALE") ~ "M",
                         str_to_upper(sex) %in% c("ALL","TOTAL") ~ "All",
                         TRUE ~ NA_character_))

# main age groups
by_age <- pop2 %>%
  mutate(age_group = case_when(age < 5 ~ "Under 5", age <= 15 ~ "5–15", age <= 18 ~ "16–18", TRUE ~ NA)) %>%
  filter(!is.na(age_group)) %>%
  group_by(year, lad_code, geography, age_group, sex) %>%
  summarise(pop_sum = sum(pop, na.rm=TRUE), .groups="drop")

under18      <- pop2 %>% filter(age < 19 ) %>% group_by(year, lad_code, geography, sex) %>% summarise(pop_sum=sum(pop), .groups="drop") %>% mutate(age_group="Under 19")
all_by_sex   <- pop2 %>% group_by(year, lad_code, geography, sex) %>% summarise(pop_sum=sum(pop), .groups="drop") %>% mutate(age_group="All ages")
all_combined <- pop2 %>% group_by(year, lad_code, geography) %>% summarise(pop_sum=sum(pop), .groups="drop") %>% mutate(age_group="All ages", sex="All")
agegrp_tot   <- by_age %>% group_by(year, lad_code, geography, age_group) %>% summarise(pop_sum=sum(pop_sum), .groups="drop") %>% mutate(sex="All")
under18_all      <- under18 %>% dplyr::select(-sex) %>% group_by(age_group, year, lad_code, geography) %>% summarise(pop_sum=sum(pop_sum), .groups="drop") %>% mutate(sex="All")

pop2 <- bind_rows(by_age, under18, all_by_sex, all_combined, agegrp_tot, under18_all) %>%
  arrange(year, lad_code, geography, age_group, sex) %>%
  mutate(sex = replace_na(sex, "Unknown"), ageandsex = paste0(age_group, "_", sex))



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
                  str_trim())%>%
  dplyr::filter(grepl("^E", lad_code))

england_rows <- pop %>%
  dplyr::group_by(year, age_group, sex) %>%
  dplyr::summarise(pop_sum = sum(pop_sum, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    sex = toupper(sex),
    sex = dplyr::case_when(
      sex == "M"   ~ "Male",
      sex == "F"   ~ "Female",
      sex == "ALL" ~ "Total",
      TRUE         ~ NA_character_
    ),
    LA_Name = "ENGLAND"
  ) %>%
  dplyr::select(LA_Name, age_group, pop_sum, year, sex)

  

utlalookup <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care_Entries_Data/refs/heads/main/Data/Raw/Local_Authority_District_to_County_and_Unitary_Authority_(April_2023)_Lookup_in_EW.csv"))%>%
  dplyr::filter(grepl("^E", LTLA23CD))%>%
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
  dplyr::select(UTLA23NM,UTLA23CD, age_group, pop_sum, year, sex)%>%
  dplyr::group_by(age_group, year,sex, UTLA23NM,UTLA23CD )%>%
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
                  str_trim())%>%
  dplyr::mutate(LA_Name = ifelse(LA_Name=="LONDON", "CITY OF LONDON", LA_Name))%>%
  dplyr::select(-UTLA23NM, -UTLA23CD)%>%
  dplyr::filter(!is.na(LA_Name))






regionlookup <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care_Entries_Data/refs/heads/main/Data/Raw/Local_Authority_District_(April_2023)_to_LAU1_to_ITL3_to_ITL2_to_ITL1_(January_2021)Lookup.csv"))%>%
  dplyr::mutate(ltla_Name = LAD23NM %>%
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
  dplyr::select(ITL121NM, age_group, pop_sum, year, sex)%>%
  dplyr::group_by(age_group, year, ITL121NM, sex )%>%
  dplyr::summarise(pop_sum = sum(pop_sum))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(LA_Name = ITL121NM %>%
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
  dplyr::select(-ITL121NM)%>%
  dplyr::filter(LA_Name!="SCOTLAND",
                LA_Name!="WALES")%>%
  dplyr::mutate(LA_Name = ifelse(LA_Name=="EAST", "EAST OF ENGLAND",
                                 ifelse(LA_Name=="WEST MIDLANDS ENGLAND", "WEST MIDLANDS",
                                        ifelse(LA_Name=="EAST MIDLANDS ENGLAND", "EAST MIDLANDS",
                                               ifelse(LA_Name=="SOUTH WEST ENGLAND", "SOUTH WEST",
                                                      ifelse(LA_Name=="NORTH WEST ENGLAND", "NORTH WEST",
                                                             ifelse(LA_Name=="NORTH EAST ENGLAND", "NORTH EAST",
                                                                    ifelse(LA_Name=="SOUTH EAST ENGLAND", "SOUTH EAST",LA_Name
                                                                    ))))))))



pop <- rbind(pop_utla, pop_reg)%>%
  dplyr::mutate(sex = toupper(sex),
                sex = ifelse(sex=="M", "Male", 
                             ifelse(sex=="F", "Female", 
                                    ifelse(sex=="ALL", "Total", NA))))



pop <- rbind(pop, england_rows)


pop <- pop %>%
  arrange(LA_Name, year, age_group, sex) %>%
  unite("ageandsex", age_group, sex, sep = "_", remove = FALSE, na.rm = TRUE)%>%
  dplyr::filter(!is.na(sex))%>%
  dplyr::rename(variable = ageandsex,
                number = pop_sum)%>%
  dplyr::mutate(
                category = "Area variables",
                subcategory = "Population",
                percent = NA,
                LA_Code = NA,
                LA.Number=NA)%>%
  dplyr::select(-sex, -age_group)



area <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care_Entries_Data/refs/heads/main/Data/Raw/SAM_LAD_DEC_2023_UK.csv"))%>%
  dplyr::filter(grepl("^E", LAD23CD))%>%
  dplyr::mutate(ltla_Name = LAD23NM %>%
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
  dplyr::full_join(.,utlalookup)%>%
  dplyr::select(UTLA23NM, AREAEHECT)%>%
  dplyr::group_by(UTLA23NM)%>%
  dplyr::summarise(AREAEHECT = sum(AREAEHECT))%>%
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(UTLA23NM))%>%
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
                  str_trim())%>%
  dplyr::select(-UTLA23NM)%>%
  dplyr::mutate(LA.Number = NA,
                LA_Code = NA,
                percent = NA,
                subcategory = "Density",
                variable = "Area (Hectares)",
                category = "Area variables")%>%
  dplyr::rename(number = AREAEHECT)%>%
  crossing(year = 2010:2024)%>%
  dplyr::mutate(LA_Name = ifelse(LA_Name=="LONDON", "CITY OF LONDON", LA_Name))


  
  
  final_df <- rbind(
    df, pop, area, idaci, rural
  )%>%
    dplyr::select(-LA.Number, -LA_Code)

  
  library(dplyr)
  library(stringr)
  
  final_df2 <- final_df %>%
    mutate(num = suppressWarnings(as.numeric(str_replace_all(as.character(number), ",", "")))) 
  
  areasss <- final_df2 %>% filter(str_detect(variable, "Area")) %>%
    select(LA_Name, year, area_ha = num)
  
  popsss <- final_df2 %>% filter(variable == "All ages_Total") %>%
    select(LA_Name, year, population = num)
  
  dens <- popsss %>%
    left_join(areasss, by = c("LA_Name","year")) %>%
    mutate(number = as.character(round(population/area_ha,2)),
           subcategory = "Density",
           variable = "Population density (people per hectare)",
           percent = NA,
           category = "Area variables") %>%
    dplyr::select(-area_ha, -population)
  
  final_df <- bind_rows(final_df, dens)%>%
    dplyr::select(LA_Name, year, category, subcategory, variable, number, percent)
  
  
  
eng_harm <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care_Entries_Data/refs/heads/main/Data/Raw/historic_eng.csv"))%>%
  dplyr::rename(year = time_period,
                LA_Name = country_name,
                variable = population_count)%>%
  dplyr::mutate(percent = NA,
                category = "Harmonisation data",
                subcategory = "new_children_england",
                year = as.character(year),
                number = as.character(number),
                LA_Name = toupper(LA_Name))%>%
  dplyr::select(LA_Name, year, category, subcategory, variable, number, percent)

final_df <- bind_rows(final_df, eng_harm)%>%
  dplyr::select(LA_Name, year, category, subcategory, variable, number, percent)



regions <- c(
  "NORTH EAST",
  "NORTH WEST",
  "YORKSHIRE AND THE HUMBER",
  "EAST MIDLANDS",
  "WEST MIDLANDS",
  "EAST OF ENGLAND",
  "LONDON",
  "SOUTH EAST",
  "SOUTH WEST",
  "INNER LONDON",
  "OUTER LONDON"
)

final_df <- final_df %>%
  dplyr::rename(geography_name = LA_Name) %>%
  dplyr::mutate(
    geography_scale = dplyr::case_when(
      geography_name == "ENGLAND" ~ "NATIONAL",
      geography_name %in% regions ~ "REGIONAL",
      TRUE ~ "LOCAL AUTHORITY"
    )
  ) 


final_df <- final_df %>%
dplyr::select(geography_name, year, geography_scale, category, subcategory, variable, number, percent)

rejig <- final_df%>%
  filter(subcategory == "Age group") %>%
  mutate(variable = case_when(
    grepl("Under 1|1 to 4", variable) ~ "Under 5",
    grepl("5 to 9|10 to 15", variable) ~ "5–15",
    grepl("16 years", variable) ~ "16–18",
    variable == "Total" ~ "Total",
    TRUE ~ NA_character_
  )) %>%
  # Sum numbers and percents by the new categories
  group_by(geography_name, year, geography_scale, category,subcategory, variable) %>%
  summarise(
    number = sum(as.numeric(number), na.rm = TRUE),
    percent = sum(as.numeric(percent), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(variable = variable)

final_df <- rbind(final_df, rejig)

final_df <- final_df %>%
  dplyr::mutate(variable = ifelse(variable=="Total" & subcategory=="Taken into care", "Total children", variable))


write.csv(final_df, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/Github/Github_new/Care_Entries_Data/Data/clean/final_data.csv")


