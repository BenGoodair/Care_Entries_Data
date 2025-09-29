if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools,np,patchwork,lazyeval,scales,rlang, hmisc,ggrepel,RColorBrewer,janitor,interp, lmtest,gt, modelsummary, dplyr,pdftools, tidyverse,rattle,glmnet,caret, rpart.plot, RcolorBrewer,rpart, tidyr, mice, stringr,randomForest,  curl, plm, readxl, zoo, stringr, patchwork,  sf, clubSandwich, modelsummary, sjPlot)

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
plot_df <- df %>%
  dplyr::filter(variable=="All children taken into care"| variable=="Under 19_Total",
                geography_scale=="LOCAL AUTHORITY")%>%
  dplyr::select(year, variable, number, geography_name)%>%
  tidyr::pivot_wider(id_cols = c("year", "geography_name"), names_from = "variable", values_from = "number")%>%
  dplyr::mutate(child_per = as.numeric(`Total children`)/(as.numeric(`Under 19_Total`)/100000))

plot_df <- plot_df %>% filter(!is.na(year), !is.na(geography_name), !is.na(child_per))%>%dplyr::filter(geography_name!="CITY OF LONDON")

top_n <- 5   # adjust: how many regions to call out
top_regions <- plot_df %>%
  group_by(geography_name) %>%
  summarize(mean_child_per = mean(child_per, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_child_per)) %>%
  slice_head(n = top_n) %>%
  pull(geography_name)

# Colour palette for highlights
pal <- brewer.pal(min(9, top_n), "Set1")    # if top_n > 9 consider using colorRampPalette()

p <- ggplot(plot_df, aes(x = year, group = geography_name)) +
  # background lines for all regions
  geom_line(aes(y = child_per), colour = "grey80", size = 0.5, alpha = 0.7) +
  
  # highlighted lines for top regions
  geom_line(
    data = filter(plot_df, geography_name %in% top_regions),
    aes(y = child_per, colour = geography_name),
    size = 1.15
  ) +
  
  # thin points on highlighted lines (optional)
  geom_point(
    data = filter(plot_df, geography_name %in% top_regions),
    aes(y = child_per, colour = geography_name),
    size = 1.6
  ) +
  
  # labels at the most recent year for highlighted regions
  geom_text_repel(
    data = plot_df %>% filter(geography_name %in% top_regions) %>%
      group_by(geography_name) %>%
      filter(year == max(year)),
    aes(x = year + 0.15, y = child_per, label = geography_name, colour = geography_name),
    size = 3.5, 
    show.legend = FALSE,
    nudge_x = 0.3,
    direction = "y",
    segment.size = 0.3,
    box.padding = 0.2
  ) +
  
  # scales, theme and labels
  scale_colour_manual(values = setNames(rep(pal, length.out = length(top_regions)), top_regions)) +
  scale_x_continuous(breaks = pretty(plot_df$year, n = 8)) +
  scale_y_continuous(labels = label_number(accuracy = 1), expand = expansion(mult = c(0.02, 0.12))) +
  
  labs(
    title = "Variation in children starting to be looked after",
    subtitle = paste0("All LAs shown (grey); top ", top_n, " LAs by mean highlighted"),
    x = "Year",
    y = "Children starting to be looked after\n(per 100,000k under 19)",
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none")            # legend hidden because labels are on pl  )

# If you prefer a legend and no direct labels, set legend.position = "right" and remove geom_text_repel
print(p)


  


####taken into care deprivation inequality over time ####

plot_df <- df %>%
  dplyr::filter(variable=="All children taken into care"| variable=="Under 19_Total"|variable=="IDACI...Average.score",
                geography_scale=="LOCAL AUTHORITY")%>%
  dplyr::select(year, variable, number, geography_name, percent)%>%
  tidyr::pivot_wider(id_cols = c("year", "geography_name"), names_from = "variable", values_from = c("number", "percent"))%>%
  dplyr::filter(geography_name!="CITY OF LONDON",
                year>2010)




# rename for convenience
plot_df <- plot_df %>%
  rename(
    pop = `number_Under 19_Total`,
    idaci = `number_IDACI...Average.score`
  ) %>%
  mutate(
    year = as.integer(year),
    pop = as.numeric(pop),
    idaci = as.numeric(idaci),
    child_per = as.numeric(`percent_All children taken into care`)
  )

df2024 <- plot_df %>% filter(year == 2024)

m2024 <- lm(child_per ~ idaci, data = df2024, weights = pop)
sii2024 <- tidy(m2024) %>% filter(term == "idaci")
ci2024 <- confint(m2024, "idaci", level = 0.95)

p1 <- ggplot(df2024, aes(x = idaci, y = child_per)) +
  geom_point(aes(size = pop), alpha = 0.7) +
  geom_smooth(method = "lm", aes(weight = pop), se = FALSE, colour = "navy", size = 1.2) +
  scale_size_continuous(name = "Under-19 pop") +
  labs(
    title = "Percent of looked after children taken into care (2024)",
    subtitle = "Does deprivation matter?",
    x = "IDACI average score (deprivation)",
    y = "Children taken into care\n(% of those starting to be looked after)",
    caption = sprintf("SII = %.1f (95%% CI %.1f to %.1f)", 
                      sii2024$estimate, ci2024[1], ci2024[2])
  ) +
  theme_bw(base_size = 13)

sii_by_year <- plot_df %>%
  group_by(year) %>%
  group_modify(~{
    m <- lm(child_per ~ idaci, data = .x, weights = pop)
    ci <- confint(m, "idaci")
    tibble(
      sii = coef(m)["idaci"],
      sii_l = ci[1],
      sii_u = ci[2]
    )
  }) %>%
  ungroup()

p2 <- ggplot(sii_by_year, aes(x = year, y = sii)) +
  geom_line(size = 1) +
  geom_point(size = 2.2) +
  geom_errorbar(aes(ymin = sii_l, ymax = sii_u), width = 0.2) +
  labs(
    title = "Slope Index of Inequality (SII) over time",
    subtitle = "Has inequality between areas increased?",
    x = "Year",
    y = "SII (slope per unit IDACI)"
  ) +
  theme_bw(base_size = 13)+
  scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024))


combined <- p1 / p2 + plot_layout(heights = c(1, 0.9))
print(combined)




####Care entries by age, sex and reason####

plot_df <- df %>%
  dplyr::filter(variable=="Female"| variable=="16 years and over"|variable=="N1. Abuse or neglect"|
                  variable=="Voluntary agreement under S20"|variable=="Unaccompanied asylum-seeking children"|
                  variable=="Police protection in LA accommodation"| variable=="Male"|
                  variable=="Interim care order"| variable=="Not taken into care"|
                  variable=="N5. Family dysfunction"| variable == "N8. Absent parenting"|
                  variable=="1 to 4 years"| variable=="5 to 9 years"| variable=="10 to 15 years"|
                  variable=="N6. Socially unacceptable behaviour"|variable=="N3. Parental illness or disability"|
                  variable=="All children taken into care"|variable=="Under 1 year",
                geography_scale=="LOCAL AUTHORITY")%>%
  dplyr::select(year, variable, geography_name, percent)%>%
  tidyr::pivot_wider(id_cols = c("year", "geography_name"), names_from = "variable", values_from = c("percent"))%>%
  dplyr::filter(geography_name!="CITY OF LONDON",
                year>2010)




# --- tidy (rename multiword cols if needed) ---
# Example renaming step — adapt/remove if your column names already match
plot_df <- plot_df %>%
  rename(
    age_over_16 = `16 years and over`,
    age_under_1 = `Under 1 year`,
    abuse_or_neglect = `N1. Abuse or neglect`,
    female = `Female`,
    s20_vol = `Voluntary agreement under S20`,
    taken_into_care = `All children taken into care`,
    uasc = `Unaccompanied asylum-seeking children`
  )%>%
  dplyr::mutate(over16 = as.numeric(over16),
                under1 = as.numeric(under1),
                abuse_or_neglect = as.numeric(abuse_or_neglect),
                female = as.numeric(female),
                s20_vol = as.numeric(s20_vol),
                taken_into_care = as.numeric(taken_into_care),
                uasc = as.numeric(uasc))

# --- convert to long format for plotting ---
vars <- c("over16","under1","abuse_or_neglect","female","s20_vol","taken_into_care","uasc")

plot_df_long <- plot_df %>%
  mutate(year = as.integer(year)) %>%
  select(year, geography_name, all_of(vars)) %>%
  pivot_longer(cols = all_of(vars), names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, levels = vars))

# --- summary by year & metric (mean + IQR + mean +/- sd) ---
summary_df <- plot_df_long %>%
  group_by(metric, year) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE),
    minv = min(value, na.rm = TRUE),
    maxv = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    lower_iqr = q25,
    upper_iqr = q75,
    lower_sd = mean - sd,
    upper_sd = mean + sd
  )

# --- Plot ---
p <- ggplot() +
  # grey thin lines for each LA to show trajectories (context)
  geom_line(
    data = plot_df_long,
    aes(x = year, y = value, group = geography_name),
    colour = "grey80", size = 0.4, alpha = 0.6
  ) +
  # ribbon: IQR (25th to 75th)
  geom_ribbon(
    data = summary_df,
    aes(x = year, ymin = lower_iqr, ymax = upper_iqr, group = metric),
    fill = "steelblue", alpha = 0.18
  ) +
  # optionally an outer lighter ribbon for mean +/- sd
  geom_ribbon(
    data = summary_df,
    aes(x = year, ymin = lower_sd, ymax = upper_sd, group = metric),
    fill = "steelblue", alpha = 0.08
  ) +
  # mean line
  geom_line(
    data = summary_df,
    aes(x = year, y = mean, group = metric),
    colour = "steelblue", size = 1.1
  ) +
  # mean points
  geom_point(
    data = summary_df,
    aes(x = year, y = mean),
    colour = "steelblue", size = 1.8
  ) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2, labeller = labeller(metric = function(x) {
    # prettier labels: convert underscores to spaces, capitalise
    sapply(x, function(z) tools::toTitleCase(gsub("_", " ", z)))
  })) +
  scale_x_continuous(breaks = sort(unique(plot_df_long$year))) +
  labs(
    x = "Year",
    y = NULL,
    title = "Trends in selected child-related metrics over time",
    subtitle = "Grey lines = each local authority; blue line = mean; ribbon = IQR (darker) and ±1 SD (lighter)",
    caption = "Data: your dataframe | Plot: ggplot2 + theme_bw()"
  ) +
  theme_bw(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "grey98"),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.35),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    legend.position = "none"
  )

# print and save
print(p)

