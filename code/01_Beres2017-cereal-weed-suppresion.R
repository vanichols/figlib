# created 5 june 2024
# purpose: make figure demonstrating rye's weed suppressive ability
# notes: based on Beres et al. 2017 paper
# https://www.cambridge.org/core/journals/weed-technology/article/weedcompetitive-ability-of-spring-and-winter-cereals-in-the-northern-great-plains/A1B33A6C04D1E627369E7D4AE457839D


library(tidyverse)
library(readxl)
rm(list = ls())


source("code/00_viz-settings.R")

# data --------------------------------------------------------------------

#--i used plot digitizer https://plotdigitizer.com/app

d <- read_excel("data/beres2017-weed-suppression-cereals.xlsx", skip = 5)

d2 <- 
  d %>% 
  mutate(tot = dicot + monocot,
         maxtot = max(tot),
         broadleaf = dicot/maxtot,
         grass = monocot/maxtot)


# fig ---------------------------------------------------------------------

#--normal

d2 %>% 
  filter(!grepl("trit", crop)) %>% 
  select(type, crop, broadleaf, grass, tot) %>% 
  pivot_longer(broadleaf:grass) %>% 
  unite(type, crop, col = "crop", sep = " ") %>%
  mutate_if(is.character, str_to_title) %>% 
  ggplot(aes(reorder(crop, tot), value)) + 
  geom_col(aes(fill = name)) + 
  labs(x = NULL,
       y = "Relative\nweed\nbiomass",
       caption = "Based on data from Canada, Beres et al. 2017",
       fill = NULL,
       title = "Rye is valued as a weed-suppressive crop") + 
  th1_gbasic +
  theme(axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        legend.position = c(0.2, 0.8),
        legend.background = element_rect(fill =  "transparent"),
        axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = rel(0.9), 
                                   color = c("red", "black", "red", "black", "black", "black"))) +
  scale_fill_manual(values = c(p2_ylw, p2_blu))


#--flipped
d2 %>% 
  filter(!grepl("trit", crop)) %>% 
  select(type, crop, broadleaf, grass, tot) %>% 
  pivot_longer(broadleaf:grass) %>% 
  unite(type, crop, col = "crop", sep = " ") %>%
  mutate_if(is.character, str_to_sentence) %>% 
  ggplot(aes(reorder(crop, tot), value)) + 
  geom_col(aes(fill = name), color = "black") + 
  labs(x = NULL,
       y = "Relative weed biomass",
       caption = "Based on data from Canada, Beres et al. 2017",
       fill = NULL,
       title = "Rye is valued as a weed-suppressive crop") + 
  th1_gbasic + 
  theme(plot.title.position = "plot",
        plot.title = element_text(size = rel(3)),
        legend.position = c(0.8, 0.2),
        legend.background = element_rect(fill =  "transparent"),
        axis.text.y = element_text(#color = c(p2_red, "black", p2_red, "black", "black", "black"),
                                   face = c("bold", "plain", "bold", "plain", "plain", "plain"))) +
  scale_fill_manual(values = c(p2_ylw, p2_blu)) + 
  coord_flip()

ggsave("figsR/suppressive-value-rye.png", width = 8, height = 6)
