library(tidyverse)
library(scales)


source("code/00_viz-settings.R")

d <- tibble(x1 = c("Entries tested", 
                   "Exhibited post-harvest re-growth", 
                   "Produced grain in 2nd growing season",
                   "Produced grain in 3rd growing season"),
            x1alt = c("Entries tested", 
                      "Exhibited post-\nharvest re-growth", 
                      "Produced grain in\n2nd growing season",
                   "Produced grain in\n3rd growing season"),
            x2 = c(" ", 
                   "Candidates for self-cover cropping cereal", 
                   "Candidates for bi-annual cereal", 
                   "Candidates for perennial cereal"),
            x2alt = c(" ", 
                   "Candidates for self-cover cropping cereal", 
                   " ", 
                   " "),
            x2alt2 = c("176 entries tested", 
                   "107 candidates for self-cover cropping grain crop", 
                   "43 candidates for bi-annual grain crop", 
                   "3 candidates for perennial grain crop"),
       y1 = c(176, 107, 43, 3),
       y2 = c(171, 102, 38, 7),
       y3 = c(176, 107, 43, 3))

d2 <- 
  d %>% 
  select(x1, x1alt, x2, y1, y2, y3) %>% 
  mutate(y1_frac = y1/176,
         y2_frac = y2/176,
         y1_lab = paste0(round(y1_frac *100, 0), "%")
  )


# light blue, pub ---------------------------------------------------


d2 %>% 
  filter(x1 != "Entries tested") %>% 
  ggplot(aes(reorder(x1alt, y1, min), y1_frac)) +
  geom_col(aes(fill = x1), show.legend = F, color = "black") +
  #--pct labels
  geom_text(aes(x = x1alt, y = y2_frac, label = y1_lab),
            hjust = 1, size = 8) +
  #--text
  geom_text(aes(x = x1alt, y =  y1_frac + .03, label = x2),
            hjust = 0, size = 5, fontface = "italic") +
  coord_flip(clip = "off") +
  scale_y_continuous(limits = c(0, 1),
                     labels = label_percent()) +
  scale_fill_manual(values = c(p4_blu, p7_grn, p4_ylw)) +
  th1_gbasic +
  theme(axis.text = element_text(color = "gray50"),
        plot.margin = margin(0, 2, 0, 0, "cm") ,
        panel.border = element_rect(color = "gray50")) +
  labs(
    x = NULL,
    y = NULL)


#--raw numbers labled
d %>% 
  ggplot(aes(reorder(x1, y1, min), y1)) +
  geom_col(aes(fill = x1), show.legend = F) +
  geom_text(aes(x = x1, y = y2, label = y1),
            hjust = 1, size = 8) +
  geom_text(aes(x = x1, y = y1 + 10, label = x1),
            hjust = 0, size = 5, fontface = "italic") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 275)) +
  scale_fill_manual(values = c(p2_blu, p7_grn, p4_ylw, p2_red)) +
  theme(axis.text.y = element_blank()) +
  th1_gbasic +
  labs(
    x = NULL,
    y = "Number of entries",
    caption = "Data from Hayes et al. 2012")

#--labeled with percent
d2 %>% 
  mutate(y1_lab = ifelse(x1 == "Entries tested", " ", y1_lab)) %>% 
  ggplot(aes(reorder(x1, y1, min), y1)) +
  geom_col(aes(fill = x1), show.legend = F) +
  geom_text(aes(x = x1, y = y1, label = y1_lab),
            hjust = 1, size = 8) +
  geom_text(aes(x = x1, y = y1 + 10, label = x1),
            hjust = 0, size = 5, fontface = "italic") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 275)) +
  scale_fill_manual(values = c(p2_blu, p7_grn, p4_ylw, p2_red)) +
  theme(axis.text.y = element_blank()) +
  th1_gbasic +
  labs(
    x = NULL,
    y = "Number of entries",
    caption = "Data from Hayes et al. 2012")


ggsave("figsR/fig_Hayes2012-pres-for-ryest.png",
       width = 11,
       height = 4)


# simplified, pres --------------------------------------------------------



d %>%
  filter(!grepl("2nd", x1)) %>% 
  ggplot(aes(reorder(x1alt, y1, min), y1)) +
  geom_col(aes(fill = x1), show.legend = F) +
  geom_text(aes(x = x1alt, y = y2alt, label = y1),
            hjust = 1, size = 8) +
  #geom_text(aes(x = x1alt, y = y1 + 3, label = x2),
  #          hjust = 0, size = 5, fontface = "italic") +
  coord_flip(clip = "off") +
  #scale_y_continuous(limits = c(0, 450)) +
  scale_fill_manual(values = c(p4_blu, p7_grn, p4_ylw)) +
  th1_gbasic +
  theme(axis.text = element_text(color = "gray50"),
        plot.margin = margin(0, 2, 0, 0, "cm") ) +
  labs(
    x = NULL,
    y = NULL,
    caption = "Data from Hayes et al. 2012")

ggsave("figsR/fig_Hayes2012-pres.png",
       width = 11,
       height = 4)
