library(tidyverse)
library(scales)
library(writexl)

source("code/00_viz-settings.R")

d <- tibble(x1 = c("Instances tested", 
                   "Exhibited post-harvest re-growth", 
                   "Produced grain in 2nd growing season",
                   "Produced grain in 3rd growing season"),
            x1alt = c("Instances tested", 
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


# 2. light blue, pub ---------------------------------------------------


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
    caption = "Data from Hayes et al. 2018")

ggsave("figsR/fig_Hayes2018-pres-for-sweden.png",
       width = 6,
       height = 4)


d %>%
  mutate(x1F = as.factor(x1)) |> 
  ggplot(aes(x1F, y1)) +
  geom_col(aes(fill = x1), show.legend = F) +
  geom_text(aes(x = x1, y = y1/2, label = y1),
            hjust = 0.5, size = 8) +
  geom_text(aes(x = x1, y = y1 + 20, label = x1alt),
            hjust = 0.5, size = 5, fontface = "italic") +
  scale_y_continuous(limits = c(0, 275)) +
  scale_fill_manual(values = c(p2_blu, p7_grn, p4_ylw, p2_red)) +
  theme(axis.text = element_blank()) +
  th1_gbasic +
  labs(
    x = NULL,
    y = "Number of evaluations",
    caption = "Data from Hayes et al. 2018")

ggsave("figsR/fig_Hayes2018-pres-for-sweden.png",
       width = 11,
       height = 4)

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


ggsave("figsR/fig_Hayes2018-pres-for-ryest.png",
       width = 11,
       height = 4)


# 3. simplified, pres --------------------------------------------------------



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


# 4. new stacked bar ---------------------------------------------------------

#--raw numbers labled
d4 <- 
  d |> 
  filter(x1 != "Entries tested") |> 
  select(lab = x1, nu_entries = y1) 

d4 |> 
  write_xlsx("data/tidy_hayes.xlsx")
  
d4 |> 
  mutate(order = c(1, 2, 3)) |>
  arrange(order) |> 
  mutate(lab = as.factor(lab),
         citation = "Hayes et al. 2012") |> 
  ggplot(aes(citation, nu_entries)) +
  geom_col(aes(fill = lab))

df <- 
  d4 |>
  mutate(order = c(1, 2, 3)) |>
  arrange(order) |>
  mutate(lab = factor(lab, levels = unique(lab)),
         lab = fct_rev(lab),                          # reverses the stacking order
         lab = fct_relabel(lab, \(x) str_wrap(x, 22)),
         citation = "Hayes et al. 2018")

total <- sum(df$nu_entries)
top   <- levels(df$lab)[1]                            # first level = top of the stack

ggplot(df, aes(citation, nu_entries, fill = lab)) +
  geom_col(width = 0.6, color = "black") +
  # centered labels for all segments except the top one
  geom_text(data = \(d) filter(d, lab != top),
            aes(label = nu_entries),
            position = position_stack(vjust = 0.5),
            size = 10, fontface = "bold", color = "white") +
  # label for the top (smallest) segment, placed above the bar
  geom_text(data = \(d) filter(d, lab == top),
            aes(y = total, label = nu_entries),
            vjust = -0.4, size = 10, fontface = "bold") +
  scale_fill_manual(values = c("#e76f51", "#2a9d8f", "#264653")) +  # colors follow the reversed levels
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +        # headroom for the label
  labs(x = NULL, y = "Number of entries", fill = NULL) +
  theme_minimal(base_size = 28) +
  theme(
    legend.text        = element_text(size = 24, lineheight = 0.9),
    legend.key.size    = unit(1.2, "cm"),
    legend.key.spacing.y = unit(0.8, "cm"),    # space between legend entries
    axis.text.x        = element_text(size = 26, face = "bold"),
    axis.title.y       = element_text(size = 26),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave("figsR/hayes_stacked.png", width = 8, height = 7, dpi = 300, bg = "white")

