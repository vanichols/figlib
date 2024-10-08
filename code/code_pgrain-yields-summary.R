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

d_bajg <- 
  read_excel("data/pgrain_literature-summary.xlsx", sheet = "Bajgain2020", skip = 5) %>% 
  janitor::clean_names() %>% 
  fill(citation, site) %>% 
  select(citation, site, crop, year, grain_kgha) 

d_fagn <- 
  read_excel("data/pgrain_literature-summary.xlsx", sheet = "Fagnant2024", skip = 5) %>% 
  janitor::clean_names() %>% 
  fill(citation, site, nfert_kgha, crop) %>% 
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
  bind_rows(d_bajg) %>% 
  bind_rows(d_fagn) %>% 
  bind_rows(d_clar)


# figs --------------------------------------------------------------------




d %>% 
#  filter(citation != "Clark et al. 2019") %>% 
  filter(crop %in% c("IWG", "perennial cereal rye", "perennial wheat")) %>% 
  mutate(crop = str_to_title(crop),
         crop = ifelse(crop == "Iwg", "IWG*", ifelse(crop == "Perennial Wheat", "Perennial\nWheat", "Perennial\nCereal Rye")),
         year = ifelse(year == 1, "Year 1", ifelse(year == 2, "Year 2", "Year 3")),
         yearF = fct_inorder(year)) %>%
  group_by(crop) %>% 
  mutate(n = n()) %>% 
  arrange(-n) %>% 
  mutate(
    cropF = factor(crop, levels = c("IWG*", "Perennial\nWheat", "Perennial\nCereal Rye")),
    cropF2 = fct_inorder(cropF),
    cropF3 = fct_rev(cropF2)) %>% 
  ggplot(aes(yearF, grain_kgha)) + 
  geom_jitter(aes(fill = citation, shape = citation), width = 0.1, size = 5) + 
  scale_fill_manual(values = c(p7_dko, p7_pur, p7_grn, p7_ltb, p7_blu, p7_ylw, p7_ora, p2_red, p4_pnk, p4_grn)) +
  scale_shape_manual(values = c(25, 22, 18, 23, 24, 21, 17, 23, 20, 25)) + 

  scale_y_continuous(labels = label_comma(), limits = c(0, 3000)) +
  th1_gbasic + 
  labs(x = NULL, 
       y = "Grain yield\n(kg ha-1)", 
       caption = "*Intermediate Wheatgrass, commercially sold grain known as Kernza",
       fill = NULL,
       shape = NULL) + 
  theme(legend.position = "right", 
        #legend.justification = c(1, 1),
        axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = rel(1.3)), 
        legend.background = element_rect(fill =  "transparent"),
        legend.text = element_text(size = rel(0.9)),
        plot.caption = element_text(face = "italic"),
        axis.text.x = element_text(vjust = 0.5)) + 
  facet_grid(.~cropF)


ggsave("figsR/fig_perennial-grain-lit-sum-by-crop.png", width =14, height = 7)
ggsave("figsR/fig_perennial-grain-lit-sum-by-crop-skinny.png", width =12, height = 7)



# no citation -------------------------------------------------------------


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
  geom_jitter(aes(fill = crop, shape = crop), width = 0.1, show.legend = F, size = 5) + 
  scale_fill_manual(values = c(p2_blu, p2_red, p2_ylw)) +
  scale_shape_manual(values = c(21, 22, 18)) + 
  scale_y_continuous(labels = label_comma(), limits = c(0, 3000)) +
  th1_gbasic + 
  labs(x = NULL, 
       y = "Grain yield\n(kg ha-1)", 
       caption = "*Intermediate Wheatgrass, commercially sold grain known as Kernza",
       fill = NULL,
       shape = NULL,
       title = "Perennial cereal rye is a promising, under-studied perennial grain",
       subtitle = "Yields from 10 published studies") + 
  theme(legend.position = c(0.9, 0.83), 
        #legend.justification = c(1, 1),
        axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = rel(1.3)), 
        legend.background = element_rect(fill =  "transparent"),
        legend.text = element_text(size = rel(0.9)),
        plot.caption = element_text(face = "italic"),
        plot.title.position = "plot",
        axis.text.x = element_text(vjust = 0.5)) + 
  facet_grid(.~year)


ggsave("figsR/fig_perennial-grain-lit-summary-3colors.png", width =12, height = 5)

# crop as facet -------------------------------------------------------------


d %>% 
  #  filter(citation != "Clark et al. 2019") %>% 
  filter(crop %in% c("IWG", "perennial cereal rye", "perennial wheat")) %>% 
  mutate(crop = str_to_title(crop),
         crop = ifelse(crop == "Iwg", "IWG*", ifelse(crop == "Perennial Wheat", "Perennial\nWheat", "Perennial\nCereal Rye")),
         year = ifelse(year == 1, "First\nyear", ifelse(year == 2, "Second\nyear", "Third\nyear"))) %>% 
  arrange(-grain_kgha) %>% 
  mutate(
    cropF = fct_inorder(crop)) %>% 
  ggplot(aes(year, grain_kgha)) + 
  geom_jitter(aes(fill = crop, shape = crop), width = 0.1, show.legend = F, size = 5) + 
  scale_fill_manual(values = c(p2_blu, p2_red, p2_ylw)) +
  scale_shape_manual(values = c(21, 22, 18)) + 
  scale_y_continuous(labels = label_comma(), limits = c(0, 3000)) +
  th1_gbasic + 
  labs(x = NULL, 
       y = "Grain yield\n(kg ha-1)", 
       caption = "*Intermediate Wheatgrass, commercially sold grain known as Kernza",
       fill = NULL,
       shape = NULL,
       title = "Perennial cereal rye is a promising, under-studied perennial grain",
       subtitle = "Yields from 10 published studies") + 
  theme(legend.position = c(0.9, 0.83), 
        #legend.justification = c(1, 1),
        axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = rel(1.3)), 
        legend.background = element_rect(fill =  "transparent"),
        legend.text = element_text(size = rel(0.9)),
        plot.caption = element_text(face = "italic"),
        plot.title.position = "plot",
        axis.text.x = element_text(vjust = 0.5)) + 
  facet_grid(.~cropF)


ggsave("figsR/fig_perennial-grain-lit-summary-3colors-bycrop.png", width =8, height = 6)
