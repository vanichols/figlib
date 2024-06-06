#--make figure of yield issues in perennial things


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(readxl)
library(scales)
library(patchwork)

theme_set(theme_bw())
source("00_viz-settings.R")

# canada data --------------------------------------------------------------------

# Canada sites are 4deg C average, and 450 mm rain

d <- 
  read_excel("../data/pgrain_literature-summary.xlsx", sheet = "Daly_ylds") %>% 
  janitor::clean_names() %>% 
  fill(site, rye_type)



# look at at --------------------------------------------------------------

#--It says the biomass is without the grain, so it is not the total aboveground biomass

d %>% 
  filter(nfert_kgha > 0) %>% 
  #mutate(totbio_kgha = grain_kgha + biomass_kgha) %>% 
  pivot_longer(grain_kgha:biomass_kgha) %>% 
  ggplot(aes(rye_type, value, group = rye_type)) +
  geom_col(aes(fill = name)) +
  facet_grid(site ~ year)

# fig ---------------------------------------------------------------------

d_fig <- 
  d %>% 
  filter(nfert_kgha > 0) %>% 
  group_by(rye_type, year) %>% 
  summarise(grain_kgha = mean(grain_kgha),
            biomass_kgha = mean(biomass_kgha)) %>% 
  pivot_longer(grain_kgha:biomass_kgha) %>% 
  mutate(year2 = ifelse(year == 1, "2018", "2019"),
         year2 = as.numeric(year2)) %>% 
  mutate(name2 = ifelse(name == "grain_kgha", "Grain", "Biomass"),
         name2 = factor(name2, levels = c("Grain", "Biomass")))
  
  
  
d_labs <- 
  d_fig %>% 
  group_by(name2, rye_type) %>% 
  summarise(value = mean(value))

#--biomass and grain
f1 <- 
  d_fig %>% 
  ggplot(aes(year2, value)) + 
  geom_line(aes(color = rye_type), size = 2, show.legend = F) + 
  geom_label(data = d_labs, aes(x = 2018.5, y = value, label = rye_type, fill = rye_type), show.legend = F, size = 6, color = "white") +
  facet_grid(.~name2) + 
  scale_y_continuous(labels = label_comma()) +
  scale_x_continuous(limits = c(2017.9, 2019.1), breaks = c(2018, 2019)) +
  scale_color_manual(values = c(p4_blu, p4_grn, p4_pnk, p4_ylw)) +
  scale_fill_manual(values = c(p4_blu, p4_grn, p4_pnk, p4_ylw)) +
  labs(x = NULL,
       y= "kg ha-1",
       title = "Canada trials, two years of data",
       caption = "Data from Daly et al. 2022, averaged over two sites") + 
  th1_gbasic + 
  theme(axis.text.x = element_text(angle = 0, size = rel(1.4), hjust = 0.5))


#--just grain
f1gr <- 
  d_fig %>% 
  filter(name == "grain_kgha") %>% 
  ggplot(aes(year2, value)) + 
  geom_line(aes(color = rye_type), size = 2, show.legend = F) + 
  geom_label(data = d_labs %>% 
               filter(name2 == "Grain"), aes(x = 2018.5, y = value, label = rye_type, fill = rye_type), show.legend = F, size = 6, color = "white") +
  facet_grid(.~name2) + 
  th1_xvert + 
  scale_y_continuous(labels = label_comma()) +
  scale_x_continuous(limits = c(2017.9, 2019.1), breaks = c(2018, 2019)) +
  scale_color_manual(values = c(p4_blu, p4_grn, p4_pnk, p4_ylw)) +
  scale_fill_manual(values = c(p4_blu, p4_grn, p4_pnk, p4_ylw)) +
  labs(x = NULL,
       y= "kg ha-1",
       title = "Canada trials, two years of data") + 
  theme(axis.text.x = element_text(angle = 0, size = rel(1.4), hjust = 0.5))

f1gr

# iwg data ----------------------------------------------------------------


d2 <- 
  read_excel("../data/literature-summary.xlsx", sheet = "iwg_yields") %>% 
  janitor::clean_names() %>% 
  select(citation, standage_yrs:biomass_kgha) %>% 
  fill(citation)

f2 <- 
  d2 %>% 
  pivot_longer(grainyld_kgha:biomass_kgha) %>% 
  mutate(name2 = ifelse(name == "grainyld_kgha", "Grain", "Biomass"),
         name2 = factor(name2, levels = c("Grain", "Biomass"))) %>% 
  ggplot(aes(standage_yrs, value)) + 
  geom_line(aes(color = citation), size = 2) + 
  facet_wrap(~name2, scales = "free") +
  th1_xvert + 
  scale_y_continuous(labels = label_comma()) +
  scale_color_manual(values = c(p4_blu, p4_ylw, p4_pnk)) +
  labs(x = NULL,
       y= "kg ha-1",
       title = "Summary of Kernza trials") + 
  theme(axis.text.x = element_text(angle = 0, size = rel(1.4), hjust = 0.5))

f2gr <- 
  d2 %>% 
  pivot_longer(grainyld_kgha:biomass_kgha) %>% 
  mutate(name2 = ifelse(name == "grainyld_kgha", "Grain", "Biomass"),
         name2 = factor(name2, levels = c("Grain", "Biomass"))) %>% 
  filter(name2 == "Grain") %>% 
  ggplot(aes(standage_yrs, value)) + 
  geom_line(aes(color = citation), size = 2) + 
  facet_wrap(~name2, scales = "free") +
  th1_xvert + 
  scale_y_continuous(labels = label_comma(), limits = c(0, 5500)) +
  scale_color_manual(values = c(p4_blu, p4_ylw, p4_pnk)) +
  labs(x = NULL,
       y= "kg ha-1",
       title = "Summary of Kernza trials") + 
  theme(axis.text.x = element_text(angle = 0, size = rel(1.4), hjust = 0.5))

f2gr

# together ----------------------------------------------------------------

f1 / f2

ggsave("../figs/fig_literature.png", width = 10, height = 10)

f1gr

ggsave("../figs/fig_canada-grain.png", width = 6, height = 6)

(f1gr +
    scale_y_continuous(limits = c(0, 5500))) + f2gr

ggsave("../figs/fig_canada-vs-kernza-grain.png", width = 10, height = 6)
