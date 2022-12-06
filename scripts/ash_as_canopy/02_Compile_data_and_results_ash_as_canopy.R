#---------------------------------------------------------------------------------------------------
# Preparing GETT ash as canopy species data for Figure 8
#---------------------------------------------------------------------------------------------------
library(tidyverse)
library(stringr) #for word()

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

# Raw Data for GETT and cycle 3
dens_df1 <- read.csv("./data/ash_as_canopy/regen_data_2008-2019_ash_as_canopy.csv")
dens_df <- dens_df1 %>% select(Plot_Name:Tree_BA_Total, Tree_BA_NatCan, Tree_BA_NatOth, Tree_BA_Exotic, 
                               Tree_Dens_Total, Tree_Dens_NatCan, Tree_Dens_NatOth, Tree_Dens_Exotic, 
                               Sap_Dens_Total, Sap_Dens_NatCan, Sap_Dens_NatOth, Sap_Dens_Exotic, 
                               Sap_BA_Total, Sap_BA_NatCan,  Sap_BA_NatOth, Sap_BA_Exotic,
                               Seed_Dens_Total, Seed_Dens_NatCan, Seed_Dens_NatOth, Seed_Dens_Exotic,
                               stock_final, Sor_sap, Sor_seed
                               ) %>% filter(Unit_Code == "GETT")

dens_df$park_ord <- reorder(dens_df$Unit_Code, desc(dens_df$lat_rank))
names(dens_df)
dens_df <- dens_df[,c(1:16, 40, 17:39)]
dens_c3 <- dens_df %>% filter(cycle == 3) 

# Percent Regen. Composition
comp <- read.csv( "./data/ash_as_canopy/regen_proportion_data_2016-2019_ash_as_canopy.csv") %>% 
  filter(Unit_Code == "GETT")

# Trend analysis results
boot_results <- read.csv("./results/ash_as_canopy/trend_analysis_results_GETT_ash_as_canopy.csv")

#----- Setting up data for results grid -----

# Trend metrics
tile_metrics <- data.frame(resp = c("Tree_BA_Total", "Tree_Dens_Total", "Sap_BA_Total", "Sap_Dens_Total", 
                                    "Seed_Dens_Total",
                                    "Tree_BA_NatCan", "Tree_Dens_NatCan", "Sap_BA_NatCan", "Sap_Dens_NatCan", 
                                    "Seed_Dens_NatCan",
                                    "Tree_BA_NatOth", "Tree_Dens_NatOth", "Sap_BA_NatOth", "Sap_Dens_NatOth", 
                                    "Seed_Dens_NatOth",
                                    "Tree_BA_Exotic", "Tree_Dens_Exotic", "Sap_BA_Exotic", "Sap_Dens_Exotic", 
                                    "Seed_Dens_Exotic",
                                    "stock_final", 
                                    "Sor_sap", "Hor_sap", "Sor_seed", "Hor_seed"),
                           labels = c("Tree BA", "Tree Density", "Sapling BA", 
                                      "Sapling Density", "Seedling Density", 
                                      "Tree BA", "Tree Density", "Sapling BA", 
                                      "Sapling Density", "Seedling Density", 
                                      "Tree BA", "Tree Density", "Sapling BA", 
                                      "Sapling Density", "Seedling Density", 
                                      "Tree BA", "Tree Density", "Sapling BA", 
                                      "Sapling Density", "Seedling Density", 
                                      "Stocking Index", 
                                      "Sorensen Sapling", "Horn Sapling",
                                      "Sorensen Seedling", "Horn Seedling"),
                           order = 10:34) # first 9 are status metrics
#1:15)

result_sum <- boot_results %>% filter(term == "Slope") %>% 
  mutate(sign = case_when(lower95 > 0 & !grepl("Exotic", resp) ~ "signinc_good",
                          upper95 < 0 & !grepl("Exotic", resp) ~ "signdec_bad",
                          lower95 > 0 & grepl("Exotic", resp) ~ "signinc_bad",
                          upper95 < 0 & grepl("Exotic", resp) ~ "signdec_good",
                          is.na(lower95) ~ "notmod",
                          TRUE ~ "nonsign")) %>% 

  left_join(tile_metrics, ., by = "resp") %>% arrange(order)

result_sum2 <- result_sum %>% #group_by(park_ord) %>% 
  mutate(num_sign_bad = sum(sign %in% c("signdec_bad", "signinc_bad"))) %>% ungroup()

result_sum2 <- result_sum2 %>% mutate(metgrp = factor(case_when(grepl("Total", resp) ~ "Total",
                                                                grepl("NatCan|stock", resp) ~ "Native Canopy",
                                                                grepl("NatOth", resp) ~ "Native Subcan.",
                                                                grepl("Exotic", resp) ~ "Exotic",
                                                                grepl("Sor|Hor", resp) ~ "Similarity", 
                                                                TRUE ~ "Unk"),
                                                      levels = c("Total", "Native Canopy", "Native Subcan.",
                                                                 "Exotic", "Similarity")
)) %>% select(park, order, labels, metgrp, sign)

head(result_sum2)
table(result_sum2$metgrp)

# decided to drop similarity trends because more confusing than helpful
result_sum3 <- result_sum2 %>% filter(metgrp != "Similarity") %>% droplevels()
levels(result_sum3$metgrp)
head(dens_df)

status_metrics_3a <- dens_df %>% filter(cycle == 3) %>% 
  group_by(Unit_Code) %>% 
  summarize(avg_sap_dens = mean(Sap_Dens_NatCan, na.rm = T), 
            avg_seed_dens = mean(Seed_Dens_NatCan, na.rm = T),
            avg_stock = mean(stock_final, na.rm = T),
            avg_sor_sap = mean(Sor_sap, na.rm = T),
            avg_sor_seed = mean(Sor_seed, na.rm = T),
            avg_dbi = mean(DBI, na.rm = T)) %>% ungroup() %>% 
  
  mutate(`Sapling Density` = case_when(avg_sap_dens < 0.1 ~ "critical", 
                                       between(avg_sap_dens, 0.1, 0.16) ~ "caution",
                                       avg_sap_dens > 0.16 ~ "acceptable",
                                       TRUE ~ "unknown"),
         `Seedling Density` = case_when(avg_seed_dens < 0.25 ~ "critical", 
                                        between(avg_seed_dens, 0.25, 1.99) ~ "caution",
                                        avg_seed_dens >= 2 ~ "acceptable", 
                                        TRUE ~ 'unknown'),
         `Stocking Index` = case_when(avg_stock < 25 ~ "critical",
                                      between(avg_stock, 25, 100) ~ "caution", 
                                      avg_stock >= 100 ~ "acceptable",
                                      TRUE ~ "unknown"),
         `Sorensen Sapling` = ifelse(avg_sor_sap < 0.2, "critical", "acceptable"),
         `Sorensen Seedling` = ifelse(avg_sor_seed < 0.2, "critical", "acceptable"),
         `Deer Browse Impacts` = case_when(avg_dbi >= 4 ~ 'critical', 
                                           between(avg_dbi, 3.01, 4) ~ 'caution', 
                                           avg_dbi <= 3 ~ 'acceptable',
                                           TRUE ~ "unknown"),
         `Flat Tree Diam. Dist.` = ifelse(Unit_Code %in% c('SAHI', 'MORR'), "critical", "acceptable"))


status_metrics_3b <- left_join(status_metrics_3a, 
                               comp %>% select(Network, Unit_Code, sap_dens_pct_NatCan, seed_dens_pct_NatCan), 
                               by = "Unit_Code") 

status_metrics_3 <- status_metrics_3b %>% mutate(
  `Sapling Composition` = case_when(sap_dens_pct_NatCan < 0.5 ~ "critical",
                                    between(sap_dens_pct_NatCan, 0.5, 0.7) ~ "caution",
                                    sap_dens_pct_NatCan > 0.7 ~ "acceptable"),
  `Seedling Composition` = case_when(seed_dens_pct_NatCan < 0.5 ~ "critical",
                                     between(seed_dens_pct_NatCan, 0.5, 0.7) ~ "caution",
                                     seed_dens_pct_NatCan > 0.7 ~ "acceptable")) %>% 
  select(-starts_with("pctNatCan"))

names(status_metrics_3)
# Reorder columns
status_metrics_3 <- status_metrics_3 %>% select(Network, Unit_Code, avg_sap_dens:`Stocking Index`, 
                                                `Sapling Composition`, `Seedling Composition`,
                                                everything())

# Flat Tree Diam. Dist. was determined by modelling linear and exponential fit to diameter distribution.
# If linear had the lowest AIC, then the distribution has fewer small trees than expected.
names(status_metrics_3)

status_met_long <- status_metrics_3 %>% select(Unit_Code, `Sapling Density`:`Flat Tree Diam. Dist.`) %>% 
  pivot_longer(-Unit_Code, names_to = "labels", values_to = "sign") %>% 
  mutate(metgrp = "Status", order = c(1:2,4:10)) %>% rename(park = Unit_Code) %>% 
  select(park, order, labels, metgrp, sign)


# Add proportion of stocked plots based on park average DBI
dens_c3 <- dens_df %>% filter(cycle == 3) 

dbi_stock_c3 <- dens_c3 %>% group_by(Unit_Code) %>% 
  mutate(avg_DBI = mean(DBI, na.rm = T)) %>% ungroup() %>% 
  mutate(stocked = case_when(avg_DBI <= 3 & stock_final >= 50 ~ 1,
                             avg_DBI > 3 & stock_final >= 100 ~ 1,
                             TRUE ~ 0))

dbi_stock_sum <- dbi_stock_c3 %>% group_by(Unit_Code) %>% 
  summarize(avg_stock = mean(stock_final, na.rm = TRUE),
            avg_DBI = first(avg_DBI),
            num_stocked = sum(stocked, na.rm = TRUE),
            num_plots = sum(!is.na(stocked)),
            prop_stocked = num_stocked/num_plots) %>% 
  rename(park = Unit_Code) %>% mutate(order = 3)

prop_stock <- dbi_stock_sum %>% mutate(labels = "% Stocked Plots",
                                       metgrp = "Status", 
                                       sign = case_when(prop_stocked < 0.33 ~ "critical", 
                                                        between(prop_stocked, 0.33, 0.669) ~ 'caution', 
                                                        prop_stocked > 0.67 ~ "acceptable",
                                                        TRUE ~ 'unknown')) %>% 
  select(park, order, labels, metgrp, sign)

# Determing groupings based on # critical status metrics
status_met_long_final <- rbind(status_met_long, prop_stock) %>% arrange(park, order)


regstat_group <- status_met_long_final %>% filter(metgrp == "Status") %>% 
  group_by(park) %>% summarize(num_crit = sum(sign == "critical", na.rm = T),
                               park_reggrp = case_when(num_crit > 5 ~ "Imminent Failure",
                                                       between(num_crit, 4, 5) ~ "Probable Failure",
                                                       between(num_crit, 2, 3) ~ "Insecure",
                                                       num_crit < 2 ~ "Sec.",
                                                       TRUE ~ 'undefined'))

results_comb <- rbind(status_met_long_final, result_sum3) %>% arrange(park, order) 


results_comb2 <- left_join(results_comb, regstat_group, by = 'park') %>% arrange(park, order)

results_comb2$label_order <- factor(results_comb2$labels, 
                                    levels = c("Tree BA", "Tree Density", "Sapling BA", "Sapling Density", "Seedling Density", 
                                               "% Stocked Plots", "Stocking Index","Deer Browse Impacts", "Flat Tree Diam. Dist.",
                                               "Sapling Composition", "Seedling Composition", 
                                               "Sorensen Sapling", "Sorensen Seedling"))
results_comb2$metgrp <- factor(results_comb2$metgrp, levels = c("Status", "Total", "Native Canopy", "Native Subcan.",
                                                                "Exotic"))

results_comb2$park_reggrp <- factor(results_comb2$park_reggrp, levels = c("Imminent Failure", "Probable Failure", "Insecure", "Sec."))

results_final <- results_comb2 %>% arrange(park, metgrp, label_order)


write.csv(results_final, "./results/ash_as_canopy/Figure_8_as_spreadsheet_ash_as_canopy.csv", row.names = F)

