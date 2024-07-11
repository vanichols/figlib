#--make figure of yield issues in perennial things


library(tidyverse)
library(readxl)
library(scales)
library(patchwork)

theme_set(theme_bw())

source("code/00_viz-settings.R")

# canada data --------------------------------------------------------------------

# Canada sites are 4deg C average, and 450 mm rain
dp <- 
  read_excel("data/pgrain_literature-summary.xlsx", sheet = "Daly_ylds") %>% 
  janitor::clean_names() %>% 
  fill(site, rye_type) %>% 
  filter(grepl("perennial cereal rye", rye_type)) %>% 
  mutate(citation = "Daly et al. 2021",
         crop = "Canadian\nperennnial cereal rye**", 
         group1 = paste(site, nfert_kgha),
         standage_yrs = year) %>% 
  filter(year == 1) %>% 
  group_by(crop) %>% 
  summarise(grain_kgha = mean(grain_kgha)/1000)

#--denmark
d <- 
  read_excel("data/dkstats-small-grain-yields-over-time.xlsx", skip = 2) %>% 
  select(-(1:5)) %>% 
  pivot_longer(2:ncol(.)) %>%
  mutate(year = as.numeric(name),
         crop = ifelse(crop == "Rye", "Winter rye", crop),
         yield_Mgha = value /10) %>% 
  filter(crop == "Winter rye", 
         year > 2024-11) %>% 
  group_by(crop) %>% 
  summarise(mean_rye = mean(yield_Mgha))
  
#--iwg for comparison
dk <- 
  read_excel("data/pgrain_literature-summary.xlsx", sheet = "iwg_yields") %>% 
  janitor::clean_names() %>% 
  select(citation, standage_yrs:biomass_kgha) %>% 
  fill(citation) %>% 
  mutate(crop = "Kernza wheatgrass",
         group1 = paste(citation),
         grain_kgha = grainyld_kgha) %>% 
  filter(standage_yrs == 1) %>% 
  group_by(crop) %>% 
  summarise(mean_kernza = mean(grain_kgha))


dp %>% 
  add_row(crop = "German\nperennial cereal rye",
          grain_kgha = 5) %>% 
  add_row(crop = "Danish\nAnnual rye*",
          grain_kgha = d %>% pull(mean_rye)) %>% 
  add_row(crop = "Intermediate wheatgrass grain Kernza",
          grain_kgha = "XX")
  ggplot(aes(reorder(crop, -grain_kgha), grain_kgha)) + 
  geom_col(aes(fill = crop), color = "black", show.legend = F, width = 0.5) +
  scale_fill_manual(values = c(p2_ylw, p2_blu, p2_red))+
  labs(caption = "*10-year average yield in Denmark\n**Data from Daly et al. 2021",
       x = NULL,
       y = "Grain yield (Mg ha-1)",
       title = "Novel German perennial cereal rye variety has high small-plot yields") + 
  th1_gbasic

ggsave("figsR/fig_rye-yield-comparisons.png", width = 10, height = 6)

  

