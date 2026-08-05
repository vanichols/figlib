library(tidyverse)
library(ggrepel)
library(patchwork)

source("code/00_viz-settings.R")


d <- 
  tibble(crop = c("Intermediate Wheatgrass", "Kernza", "Perennial\nwheat", "Perennial\nrye"),
       citations = c(1074, 364, 92, 53),
       grain_yield = c(0.2, 1, 2, 4)) |> 
  mutate(crop = fct_inorder(crop))


d |> 
  filter(crop != "Intermediate Wheatgrass") |> 
  ggplot(aes(crop, citations)) +
  geom_col(aes(fill = crop), show.legend = F, color = "black", width = 0.5) +
  geom_text(aes(x = crop, y = citations + 25, label = citations),
            fontface = "italic", size = 12) +
  labs(y = "Web of Science\ncitations*",
       x = NULL,
       caption = "*Data collected August 2026") +
  scale_y_continuous(limits = c(0, 400)) +
  scale_fill_manual(values = c(p_red4, p_yel1, p_pur5)) +
  th1_pres +
  theme(plot.caption = element_text(face = "italic", color = "gray40"))

ggsave("figsR/fig_pgrain-citations-for-OCH.png",
       width = 8.2, height = 4.9)




# grain yields ------------------------------------------------------------

fig_grain <- 
  d |> 
  filter(crop != "Intermediate Wheatgrass") |> 
  ggplot(aes(crop, grain_yield)) +
  geom_col(aes(fill = crop), show.legend = F, color = "black", width = 0.5) +
  geom_text(aes(x = crop, y = grain_yield + 0.5, label = grain_yield),
            fontface = "italic", size = 8) +
  labs(y = "First year\ngrain yield\nMg ha-1**",
       x = NULL,
       caption = "*Based on generous literature estimates") +
  scale_y_continuous(limits = c(0, 4.5)) +
  scale_fill_manual(values = c(p_red4, p_yel1, p_pur5)) +
  th1_pres +
  theme(plot.caption = element_text(face = "italic", color = "gray40"))

fig_grain 

ggsave("figsR/fig_pgrain-yields-for-OCH.png",
       width = 6, height = 4.9)


# other versions ----------------------------------------------------------

p1 <- 
  d |> 
  filter(crop != "Intermediate Wheatgrass") |> 
  ggplot(aes(crop, citations)) +
  geom_col(aes(fill = crop), show.legend = F, color = "black", width = 0.5) +
  geom_text(aes(x = crop, y = citations + 25, label = citations),
            fontface = "italic", size = 8) +
  labs(y = "Web of Science\ncitations*",
       x = NULL,
       caption = "*Data collected August 2026") +
  scale_y_continuous(limits = c(0, 400)) +
  scale_fill_manual(values = c(p_red4, p_yel1, p_pur5)) +
  th1_pres +
  theme(plot.caption = element_text(face = "italic", color = "gray40"))

p2 <- 
  d |> 
  filter(crop != "Intermediate Wheatgrass") |> 
  ggplot(aes(grain_yield, citations)) +
  geom_smooth(method = "lm", se = F) +
  geom_point(aes(fill = crop), pch = 21, show.legend = F, size = 4) +
  geom_text_repel(aes(y = citations, x = grain_yield, label = crop),
            fontface = "italic", size = 8,
            nudge_y = 10,
            nudge_x = -1) +
  labs(x = "Grain yield\n(Mg ha-1)**",
       y = "Web of Science\ncitations*") +
  scale_x_continuous(limits = c(0, 4.5)) +
  scale_y_continuous(limits = c(0, 400)) +
  scale_fill_manual(values = c(p_red4, p_yel1, p_pur5)) +
  th1_pres +
  theme(plot.caption = element_text(face = "italic", color = "gray40"))


fig_grain + p1 + p2

ggsave("figsR/fig_pgrain-citations-for-OCH2.png",
       width = 15, height = 4.9)

