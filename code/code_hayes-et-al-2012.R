library(tidyverse)

source("code/00_viz-settings.R")

d <- tibble(x1 = c("Entries tested", 
                   "Exhibited post-harvest re-growth", 
                   "Produced grain in 2nd growing season",
                   "Produced grain in 3rd growing season"),
            x1alt = c("Entries tested", 
                      "Exhibited post-\nharvest re-growth", 
                      "Produced grain in\n2nd growing season",
                   "Produced grain in\n3rd growing season"),
            x2 = c("", "Candidates for self-cover cropping grain crop", 
                   "Candidates for bi-annual grain crop", 
                   "Candidates for perennial grain crop"),
       y1 = c(176, 107, 43, 3),
       y2 = c(171, 102, 38, 5),
       y2alt = c(171, 102, 38, 4))

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
    y = "Number of entries")


# dark blue, subcaption ---------------------------------------------------


d %>% 
  ggplot(aes(reorder(x1alt, y1, min), y1)) +
  geom_col(aes(fill = x1), show.legend = F) +
  geom_text(aes(x = x1alt, y = y2alt, label = y1),
            hjust = 1, size = 8) +
  geom_text(aes(x = x1alt, y = y1 + 5, label = x2),
            hjust = 0, size = 4, fontface = "italic") +
  coord_flip(clip = "off") +
  #scale_y_continuous(limits = c(0, 450)) +
  scale_fill_manual(values = c(p2_blu, p7_grn, p4_ylw, p2_red)) +
  th1_gbasic +
  theme(axis.text = element_text(color = "gray50")) +
  labs(
    x = NULL,
    y = NULL,
    caption = "Data from Hayes et al. 2012")

ggsave("figsR/fig_Hayes2012.png",
       width = 11,
       height = 4)

# light blue, pub ---------------------------------------------------


d %>% 
  ggplot(aes(reorder(x1alt, y1, min), y1)) +
  geom_col(aes(fill = x1), show.legend = F) +
  geom_text(aes(x = x1alt, y = y2alt, label = y1),
            hjust = 1, size = 8) +
  geom_text(aes(x = x1alt, y = y1 + 3, label = x2),
            hjust = 0, size = 5, fontface = "italic") +
  coord_flip(clip = "off") +
  #scale_y_continuous(limits = c(0, 450)) +
  scale_fill_manual(values = c(p4_blu, p7_grn, p4_ylw, p5_ora)) +
  th1_gbasic +
  theme(axis.text = element_text(color = "gray50"),
        plot.margin = margin(0, 2, 0, 0, "cm") ) +
  labs(
    x = NULL,
    y = NULL)

ggsave("figsR/fig_Hayes2012-pub.png",
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
