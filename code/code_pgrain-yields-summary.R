#--make figure of yield issues in perennial things


library(tidyverse)
library(readxl)
library(scales)
library(patchwork)

rm(list = ls())

theme_set(theme_bw())
source("code/00_viz-settings.R")


# data --------------------------------------------------------------------

#--keep only trts that receive some N
d_daly <- 
  read_excel("data/pgrain_literature-summary.xlsx", sheet = "Daly2021", skip = 5) %>% 
  janitor::clean_names() %>% 
  fill(citation, site, crop) %>% 
  filter(nfert_kgha > 0) %>% 
  filter(crop %in% c("perennial cereal rye", "fall cereal rye")) %>% 
  select(citation, site, crop, year, grain_kgha)
  
#--he reported harvest index, I'm not sure how much I trust these numbers, is it total biomass? Or biomass besides grain? I'm assuming total biomass 
d_culm <- 
    read_excel("data/pgrain_literature-summary.xlsx", sheet = "Culman2023", skip = 5) %>% 
  janitor::clean_names() %>% 
  fill(citation, crop) %>% 
  mutate(hi_pct = as.numeric(hi_pct)) %>% 
  mutate(biomass_kgha = (grain_kgha * (1 - hi_pct/100))/(hi_pct/100)) %>% 
  select(citation, site, crop, year, grain_kgha)
  
#--don't trust biomass or hi
d_jaik <- 
  read_excel("data/pgrain_literature-summary.xlsx", sheet = "Jaikumar2012", skip = 5) %>% 
  janitor::clean_names() %>% 
  #--just checking, they don't always match...could be my fault in copy-paste
  mutate(hi_pct2 = grain_kgha / biomass_kgha) %>% 
  fill(citation, site) %>% 
  select(citation, site, crop, year, grain_kgha)

d_hunt <- 
  read_excel("data/pgrain_literature-summary.xlsx", sheet = "Hunter2022", skip = 5) %>% 
  janitor::clean_names() %>% 
  fill(citation) %>% 
  select(citation, site, crop, year, grain_kgha)

d_zimb <- 
  read_excel("data/pgrain_literature-summary.xlsx", sheet = "Zimbric2020", skip = 5) %>% 
  janitor::clean_names() %>% 
  mutate(biomass_kgha = (grain_kgha * (1 - hi_pct/100))/(hi_pct/100)) %>% 
  fill(citation) %>% 
  select(citation, site, crop, year, grain_kgha)

d_fern <- 
  read_excel("data/pgrain_literature-summary.xlsx", sheet = "Fernandez2020", skip = 5) %>% 
  janitor::clean_names() %>% 
  fill(citation, site, crop) %>% 
  select(citation, site, crop, year, grain_kgha)

d_jung <- 
  read_excel("data/pgrain_literature-summary.xlsx", sheet = "Jungers2017", skip = 5) %>% 
  janitor::clean_names() %>% 
  fill(citation, site, crop) %>% 
  select(citation, site, crop, year, grain_kgha)

#--note, these are very small plots, I'm not sure the yields are reliable
d_clar <- 
  read_excel("data/pgrain_literature-summary.xlsx", sheet = "Clark2019", skip = 5) %>% 
  janitor::clean_names() %>% 
  fill(citation, site) %>% 
  select(citation, site, crop, year, grain_kgha) %>% 
  filter(grepl("Higbee", site))

d <- 
  d_daly %>% 
  bind_rows(d_culm) %>% 
  bind_rows(d_jaik) %>% 
  bind_rows(d_hunt) %>% 
  bind_rows(d_zimb) %>% 
  bind_rows(d_fern) %>% 
  bind_rows(d_jung) %>% 
  bind_rows(d_clar)


# figs --------------------------------------------------------------------


d %>% 
  filter(year == 1,
         crop %in% c("IWG", "perennial cereal rye", "perennial wheat")) %>% 
  mutate(crop = str_to_title(crop),
         crop = ifelse(crop == "Iwg", "IWG*", crop)) %>% 
  arrange(-grain_kgha) %>% 
  mutate(
    cropF = fct_inorder(crop)) %>% 
  ggplot(aes(cropF, grain_kgha)) + 
  geom_jitter(aes(fill = citation, shape = citation), width = 0.1, size = 5) + 
  scale_fill_manual(values = c(p7_ora, p7_pur, p7_grn, p7_ltb, p7_blu, p7_ylw, p7_dko, p2_red)) +
  scale_shape_manual(values = c(21, 22, 18, 23, 24, 25, 20, 8)) + 
  scale_y_continuous(labels = label_comma(), limits = c(0, 3000)) +
  th1_gbasic + 
  labs(x = NULL, 
       y = "First year\ngrain yield\n(kg ha-1)", 
       caption = "*Intermediate Wheatgrass, commercially sold grain known as Kernza",
       fill = NULL,
       shape = NULL) + 
  theme(legend.position = c(0.8, 0.8), 
        axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = rel(1.3)), 
        legend.background = element_rect(fill =  "transparent"),
        legend.text = element_text(size = rel(0.9)),
        plot.caption = element_text(face = "italic"))



# all years ---------------------------------------------------------------



d %>% 
#  filter(citation != "Clark et al. 2019") %>% 
  filter(crop %in% c("IWG", "perennial cereal rye", "perennial wheat")) %>% 
  mutate(crop = str_to_title(crop),
         crop = ifelse(crop == "Iwg", "IWG*", ifelse(crop == "Perennial Wheat", "Perennial\nWheat", "Perennial\nCereal Rye")),
         year = ifelse(year == 1, "First year", ifelse(year == 2, "Second Year", "Third Year"))) %>% 
  arrange(-grain_kgha) %>% 
  mutate(
    cropF = fct_inorder(crop)) %>% 
  ggplot(aes(cropF, grain_kgha)) + 
  geom_jitter(aes(fill = citation, shape = citation), width = 0.1, size = 5) + 
  scale_fill_manual(values = c(p7_ora, p7_pur, p7_grn, p7_ltb, p7_blu, p7_ylw, p7_dko, p2_red)) +
  scale_shape_manual(values = c(21, 22, 18, 23, 24, 25, 20, 8)) + 
  scale_y_continuous(labels = label_comma(), limits = c(0, 3000)) +
  th1_gbasic + 
  labs(x = NULL, 
       y = "Grain yield\n(kg ha-1)", 
       caption = "*Intermediate Wheatgrass, commercially sold grain known as Kernza",
       fill = NULL,
       shape = NULL) + 
  theme(legend.position = c(0.9, 0.83), 
        #legend.justification = c(1, 1),
        axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = rel(1.3)), 
        legend.background = element_rect(fill =  "transparent"),
        legend.text = element_text(size = rel(0.9)),
        plot.caption = element_text(face = "italic"),
        axis.text.x = element_text(vjust = 0.5)) + 
  facet_grid(.~year)


ggsave("figsR/fig_perennial-grain-lit-summary.png", width =12, height = 7)
