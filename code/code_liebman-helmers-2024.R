library(tidyverse)
library(readxl)

source("code/00_viz-settings.R")

#--fake data for now, emailed liebman
#--clean it up with his real data

draw <- 
  read_excel("data/COBS water data copy.xlsx") 

d <- 
  draw %>% 
  mutate(crop = case_when(
    Treatment == "CC" ~ "Annual crop",
    Treatment == "CCW" ~ "Annual crop+CC",
    Treatment == "P" ~ "Perennial crop",
    Treatment == "PF" ~ "Perennial crop",
  )) %>% 
  group_by(Year, crop) %>% 
  summarise(no3n_kgha = mean(Nloss, na.rm = T)) %>% 
  ungroup() %>% 
  add_row(crop = "Self-CC annual crop", no3n_kgha = NA, Year = 2010)

graybox_min <- 
  d %>% 
  filter(crop == "Perennial crop") %>% 
  summarise(me = median(no3n_kgha, na.rm = T)) %>% 
  pull(me)

graybox_max <- 
  d %>% 
  filter(crop == "Annual crop+CC") %>% 
  summarise(me = median(no3n_kgha, na.rm = T)) %>% 
  pull(me)

d %>% 
  mutate(cropF = factor(crop, levels = c("Annual crop", 
                                         "Annual crop+CC",
                                         "Self-CC annual crop",
                                         "Perennial crop"))) %>% 
  ggplot(aes(cropF, no3n_kgha)) +
  geom_boxplot(aes(fill = crop), show.legend = F, width = 0.5) +
  annotate("rect", xmin = 2.75, xmax = 3.25, ymin = graybox_min, ymax = graybox_max, x = 3, 
           color = "black",
            fill = "gray80",
           alpha = 0.5,
            linetype = "dashed") +
  scale_fill_manual(values = c(p4_blu, p4_ylw, p4_grn, p4_pnk)) + 
  labs(x = NULL,
       #y = "Nitrate-N (kg ha-1 year)",
       y = myno3lab2)


ggsave("figsR/fig_liebman2024.png", 
       width = 5.75, height = 3.41)
  
  

# simplified, pres --------------------------------------------------------



d %>% 
  mutate(cropF = factor(crop, levels = c("Annual crop", 
                                         "Annual crop+CC",
                                         "Self-CC annual crop",
                                         "Perennial crop"))) %>% 
  ggplot(aes(cropF, no3n_kgha)) +
  geom_boxplot(aes(fill = crop), show.legend = F, width = 0.5) +
  annotate("rect", xmin = 2.75, xmax = 3.25, ymin = graybox_min, ymax = graybox_max, x = 3, 
           color = "black",
           fill = "gray80",
           alpha = 0.5,
           linetype = "dashed") +
  #geom_jitter(, width = 0.1) +
  scale_fill_manual(values = c(p4_blu, p4_ylw, p4_grn, p4_pnk)) + 
  labs(x = NULL,
       #y = "Nitrate-N (kg ha-1 year)",
       y = myno3lab2, 
       caption = "Data from 12 years as reported in Liebman et al. 2024")

ggsave("figsR/fig_liebman2024-pres.png", 
       width = 5.75, height = 3.41)


