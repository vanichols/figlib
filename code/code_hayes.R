#--look at hayes data
#--its not that useful, I think it is on a row basis and the hybrid was only grown in canada

library(tidyverse)
library(readxl)
library(scales)
library(patchwork)

theme_set(theme_bw())

source("code/00_viz-settings.R")

draw <- 
  read_excel("data/Hayes2018-data.xlsx") %>% 
  janitor::clean_names() 

draw %>% 
  select(site) %>% 
  distinct()

d <- 
  draw %>% 
  filter(grepl("Secale", species_name)) %>% 
  mutate(rye = ifelse(grepl("Secale", species_name), 1, 0)) %>% 
  group_by(site) %>% 
  mutate(ryeI = sum(rye, na.rm = T))


d %>% 
  filter(ryeI >1 ) %>% 
  ggplot(aes(measurement_year, r_grain)) + 
  geom_jitter(aes(color = species_name), size = 3, width = 0.1) + 
  facet_wrap(.~site)
