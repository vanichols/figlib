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
         crop = "Canadian perennnial cereal rye", 
         group1 = paste(site, nfert_kgha),
         standage_yrs = year)

dk <- 
  read_excel("data/pgrain_literature-summary.xlsx", sheet = "iwg_yields") %>% 
  janitor::clean_names() %>% 
  select(citation, standage_yrs:biomass_kgha) %>% 
  fill(citation) %>% 
  mutate(crop = "Kernza wheatgrass",
         group1 = paste(citation),
         grain_kgha = grainyld_kgha)
  

dk %>% 
  bind_rows(dp) %>% 
  filter(standage_yrs < 3) %>% 
  select(citation, crop, group1, standage_yrs, grain_kgha) %>% 
  ggplot(aes(standage_yrs, grain_kgha/1000, group = group1)) + 
  geom_line(aes(color = crop), linetype = "dashed") +
  geom_point(aes(fill = crop, shape = crop), size = 5, color = "black") +
  geom_hline(yintercept = 5, linetype = "dashed", size = 1, color = p2_red) +
  geom_text(x = 1.1, y = 5.3, label = "German perennial cereal rye", color = p2_red, check_overlap = T, fontface = "italic", hjust = 0) +
  geom_hline(yintercept = 5.7, linetype = "dashed", size = 1, color = "gray70") +
  geom_text(x = 1.1, y = 6, label = "10-year average Denmark annual rye yields", color = "gray70", check_overlap = T, fontface = "italic", hjust = 0) +
  # geom_hline(yintercept = 7.4, linetype = "dashed", size = 2, color = "gray70") +
  # geom_text(x = 1.3, y = 7.7, label = "10-year average Denmark annual wheat yields", color = "gray70", check_overlap = T, fontface = "italic") +
  scale_fill_manual(values = c(p2_red, p2_blu)) +
  scale_color_manual(values = c(p2_red, p2_blu)) +
  scale_shape_manual(values = c(21, 24)) + 
  scale_x_continuous(breaks = c(1, 2)) +
  scale_y_continuous(labels = label_comma(), limits = c(0, 6.5)) +
  labs(x = "Stand Age", 
       y = "Grain yield (Mg ha-1)")


