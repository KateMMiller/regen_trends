#-----------------------------------
# Code for Figure 8- comparison of GETT results with ash as canopy vs. subcanopy species
#-----------------------------------

library(tidyverse)

reg_stat <- read.csv("./results/Figure_2_as_spreadsheet.csv") %>% filter(park == "GETT") %>% 
  mutate(type = "Prob. Failure \n Ash as Subcan.") %>% select(-park_dm, -park_ord)


reg_stat_frax <- read.csv("./results/ash_as_canopy/Figure_8_as_spreadsheet_ash_as_canopy.csv") %>% 
  mutate(type = "Secure \n Ash as Canopy")

names(reg_stat)
names(reg_stat_frax)

reg_comb <- rbind(reg_stat, reg_stat_frax) %>% arrange(desc(type), order)

reg_comb$park_reggrp[reg_comb$park_reggrp %in% "Sec."] <- "Secure"
reg_comb$labels[reg_comb$labels == "Seedling Composition"] <- "Seedling Comp."
reg_comb$labels[reg_comb$labels == "Sapling Composition"] <- "Sapling Comp."

reg_comb$label_order <- factor(reg_comb$labels, 
                               levels = c("Tree BA", "Tree Density", "Sapling BA", "Sapling Density", "Seedling Density", 
                                          "% Stocked Plots", "Stocking Index","Deer Browse Impacts", "Flat Tree Diam. Dist.",
                                          "Sapling Comp.", "Seedling Comp.", 
                                          "Sorensen Sapling", "Sorensen Seedling"))

reg_comb2 <- reg_comb %>% filter(metgrp != "Total") %>% 
             mutate(metgrp = case_when(metgrp == 'Status' ~ "Regen. Status",
                                       metgrp == 'Native Canopy' ~ 'Native Canopy \n Trends',
                                       metgrp == 'Native Subcan.' ~ 'Native Subcan. \n Trends',
                                       metgrp == 'Exotic' ~ 'Exotic \n Trends'
             ))

reg_comb2$metgrp <- factor(reg_comb2$metgrp, levels = c("Regen. Status",  "Native Canopy \n Trends", "Native Subcan. \n Trends",
                                                        "Exotic \n Trends"))
results_plot <- 
  ggplot(reg_comb2, aes(x = type, y = label_order))+
  geom_tile(aes(fill = sign), color = 'grey')+
  geom_text(aes(label = case_when(sign == "critical" ~ "●",
                                  sign == "caution" ~ "○",
                                  sign == "signdec_bad" ~ "-",
                                  sign == "signinc_bad" ~ "+", 
                                  sign == "signdec_good" ~ "-",
                                  sign == "signinc_good" ~ "+", 
                                  TRUE ~ "")))+
  facet_grid(metgrp ~ type, scales = 'free', space = 'free', switch = "y")+
  scale_fill_manual(values = c('critical' = "#FF5B5B", 
                               'caution' = "#FFFF79",
                               'acceptable' = '#BDEBA7',
                               "signdec_bad" = "#CD5C5C", 
                               "signinc_good" = "#7DD66D", 
                               "signdec_good" = "#6DA6D6", 
                               "signinc_bad" = "#FF8C00",
                               "nonsign" = "#E3E3E3", "notmod" = "white"),
                    labels = c("●          Status: Critical",
                               "○          Status: Caution",
                               "Status: Acceptable",
                               " -         Sign. Decline (Native)",
                               "+          Sign. Increase (Native)",
                               " -         Sign. Decline (Exotic)",
                               "+          Sign. Increase (Exotic)",
                               "Not Significant",
                               "Not Modeled"),
                    name = NULL)+
  scale_x_discrete(position = 'top')+
  scale_y_discrete(limits = rev)+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 9.5),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 9.5),
        text = element_text(size = 9),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.position = 'right', legend.justification = 'bottom',
        legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9, margin = margin(l = -0.9, unit = 'cm')),
        legend.background = element_rect(color = "#B9B9B9", size = 0.25))+
  labs(x = NULL, y = NULL)+ 
  guides(fill = guide_legend(ncol = 1, 
                             hjust = 1))

write.csv(reg_comb2, "./results/ash_as_canopy/Figure_8_as_spreadsheet.csv", row.names = FALSE)

ggsave('./results/Figures/Fig8_results_grid_GETT.svg', height = 8, width = 6, units = 'in')
