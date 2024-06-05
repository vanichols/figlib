# created 5 june 2024
# purpose: make figure looking at small grain yields in DK
# notes: based on stats from DKstats
# 

library(tidyverse)
library(readxl)
rm(list = ls())


source("code/00_viz-settings.R")

# data --------------------------------------------------------------------

d <- 
  read_excel("data/dkstats-small-grain-yields-over-time.xlsx", skip = 2) %>% 
  select(-(1:5)) %>% 
  pivot_longer(2:ncol(.)) %>%
  mutate(year = as.numeric(name),
         crop = ifelse(crop == "Rye", "Winter rye", crop),
         yield_Mgha = value /10)
  

# fig ---------------------------------------------------------------------

summary(d)

d_ayld <- 
  d %>% 
  group_by(crop) %>% 
  summarise(yld_mn = round(mean(yield_Mgha), 1))

d %>% 
  filter(!grepl("Oats", crop)) %>% 
  mutate(label = ifelse(year == max(year), crop, NA)) %>% 
  ggplot(aes(year, yield_Mgha)) + 
  geom_line(aes(color = crop), size = 2, show.legend = F) + 
  geom_text(aes(x = year + 0.5, label = label, color = crop),
            size = 15/.pt,
            hjust = 0, show.legend = F) + 
  th1_gbasic +
  theme(legend.position = c(0.4, 0.8),
        plot.title.position = "plot",
        axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.9)),
        legend.background = element_rect(fill =  "transparent")) + 
  scale_x_continuous(breaks = c(2006, 2009, 2012, 2015, 2018, 2021, 2024)) +
  expand_limits(x = 2030) +
  labs(x = NULL,
       y = "Grain yield\n(Mg/ha)",
       color = NULL,
       title = paste("Rye yields average", 
                     d_ayld %>% filter(crop == "Winter rye") %>% pull(yld_mn),
                     "Mg/ha in Denmark"),
       subtitle = paste("Winter wheat yields 1.3x higher"),
       caption = "Data from Danmarks Statistik (dst.dk)"
       ) + 
  scale_color_manual(values = c(p4_ylw, p4_blu, p4_grn, p4_pnk))

ggsave("figsR/DK-cereal-yields.png", width = 8, height = 6)
