#---------------------------------------------------------------------------------------------------
# Plotting trends results
#---------------------------------------------------------------------------------------------------
#library(forestTrends)
library(tidyverse)
library(grid) # for digging into str of ggplot grobs
library(gridExtra) # for ggarrange
library(gtable)
library(stringr) #for word()

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

datapath <- "./data/"

# Raw data all years
dens_df <- read.csv(paste0(datapath, "regen_data_2008-2019.csv"))
# #spp_df <- read.csv(paste0(datapath, "EFWG_species_dataset_20220325.csv"))
# dens_df <- left_join(dens_df1, spp_df, by =  c("Plot_Name", "Unit_Code", "Year", "Network", 
#                                                "cycle", "lat_rank", "excludeEvent"))
dens_df$park_ord <- reorder(dens_df$Unit_Code, desc(dens_df$lat_rank))
names(dens_df)
dens_df <- dens_df[,c(1:16, 87, 17:86)]

# Raw Data cycle 3 only
dens_c3 <- dens_df %>% filter(cycle == 3) 

# Percent Regen. Composition
comp <- read.csv("./data/regen_proportion_data_2016-2019.csv")

# Trend Analysis Results
boot_results <- read.csv("./results/trend_analysis_results.csv")

#---- Set up lists to iterate over ----
# column names for response
names(dens_df)
head(dens_df)
#metrics <- c(names(dens_df[, c(18:ncol(dens_df))]))

metrics <- c(names(dens_df[, c(18:87)]))

# response titles for plotting
metric_names <- c(rep("Tree BA (sq.m/ha)", 4), 
                  rep("Tree Density (stems/ha)", 4),
                  rep("Tree BA 10 cm (sq.m/ha)", 4),  
                  rep("Tree BA 20 cm (sq.m/ha)", 4), 
                  rep("Tree BA 30 cm (sq.m/ha)", 4),
                  rep("Tree BA 40 cm+ (sq.m/ha)", 4),
                  rep("Tree Dens. 10 cm (stems/ha)", 4),  
                  rep("Tree Dens. 20 cm (stems/ha)", 4), 
                  rep("Tree Dens. 30 cm (stems/ha)", 4),
                  rep("Tree Dens. 40 cm+ (stems/ha)", 4),
                  rep("Sapling Density (stems/sq.m)", 4),
                  rep("Sapling BA (sq.m/ha)", 4),
                  rep("Seedling Density (stems/sq.m)", 4),
                  "Tree BA (sq.m/ha)", "Tree Density (stems/ha)", 
                  "Tree BA 10 cm (sq.m/ha)", "Tree Dens. 10 cm (stems/ha)", 
                  "Tree BA 20 cm (sq.m/ha)", "Tree Dens. 20 cm (stems/ha)",
                  "Tree BA 30 cm (sq.m/ha)", "Tree Dens. 30 cm (stems/ha)",
                  "Tree BA 40 cm (sq.m/ha)", "Tree Dens. 40 cm (stems/ha)",
                  "Sapling BA (sq.m/ha)", "Sapling Density (stems/sq.m)", 
                  "Seedling Density (stems/sq.m)",    
                  "Stocking Index", "Sorensen Sapling", 
                  "Horn Sapling", "Sorensen Seedling", 
                  "Horn Seedling")

# match metric columns and titles
met_df <- data.frame(metrics, metric_names)
met_df

park_met_list <- data.frame(expand.grid(unique(dens_df$Unit_Code), metrics)) %>% 
  set_names("Unit_Code", "metrics") %>% 
  arrange(Unit_Code, metrics) %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  left_join(., met_df, by = "metrics")

park_list <- c(park_met_list[,1])
met_list <- c(park_met_list[,2])

boot_results$strp_col <- case_when(boot_results$Network == "ERMN" ~ "#A5BDCD",
                                   boot_results$Network == "MIDN" ~ "#E7CDA4",
                                   boot_results$Network == "NCBN" ~ "#CFB9D9",
                                   boot_results$Network == "NCRN" ~ "#E1E59B",
                                   boot_results$Network == "NETN" ~ "#AACCA7") 


# Plot function for park-level plot for tree metrics by spp type
plot_trends_by_grp <- function(df, xlab, ylab, group, var, facet_scales = 'fixed'){ 
  
  if(!is.na(group)){group_sym <- sym(group)}
  var_sym <- sym(var)
  
  df$time <- as.numeric(gsub("\\D", "", df$term))
  
  df1 <- df %>% filter(metric == var) 
  
  df_sign <- df1 %>% filter(term == "Slope") %>% 
    mutate(sign = case_when(lower95 > 0 | upper95 < 0 ~ "sign",
                            is.na(lower95) ~ "notmod",
                            TRUE ~ "nonsign")) %>% select(park, !!group_sym, metric, sign)
  
  df2 <- 
    left_join(df1 %>% filter(!term %in% c("Slope", "Intercept")), 
              df_sign, 
              by = c("park", group, "metric")) %>%
    filter(!term %in% c("Intercept", "Slope"))
  
  # hacky way to plot groups that didn't get modeled and so don't have errorbars or ribbons
  df2$upper95 <- ifelse(is.na(df2$upper95), df2$estimate, df2$upper95)
  df2$lower95 <- ifelse(is.na(df2$lower95), df2$estimate, df2$lower95)
  
  p <- 
    ggplot(df2, aes(x = time, y = estimate))+ 
    facet_wrap(~park_ord, scales = facet_scales, ncol = 8)+
    geom_point(aes(color = factor(!!group_sym), shape = !!group_sym, fill = !!group_sym), 
               size = 2, na.rm = TRUE)+
    geom_errorbar(aes(ymin = lower95, ymax = upper95, x = time,
                      colour = !!group_sym), width = 0.1, size = 0.5, na.rm = TRUE)+
    geom_line(aes(y = estimate, x = time, colour = !!group_sym, linetype = sign), na.rm = TRUE)+
    scale_linetype_manual(values = c('notmod' = 'dotted', 'nonsign' = 'dashed', 
                                     'sign' = 'solid'))+
    scale_color_manual(values = c("NatCan" = "#2BA12E", "NatOth" = "#B9B9B9", "Exotic" = "#CD5C5C"))+
    scale_fill_manual(values = c("NatCan" = "#2BA12E", "NatOth" = "#B9B9B9", "Exotic" = "#CD5C5C"))+
    scale_shape_manual(values = c("NatCan" = 24, "NatOth" = 21, "Exotic" = 25))+
    theme_bw()+
    theme(axis.text = element_text(size = 11), 
          axis.title = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 12, margin = margin(t = 10, b = -15)),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line = element_blank(), 
          legend.position = 'none') + 
    labs(x = xlab, y = ylab)#+
  return(p)
} 

#---- Combine most important species groups into 1 plot per park
# Set up lists to iterate over (exclude metrics that don't involve species groups) 
sort(unique(boot_results$resp))
boot_sppgrp <- boot_results %>% filter(!resp %in% c("Hor_sap", "Hor_seed", "Sor_sap", "Sor_seed", "stock_final"))

# Set up groups
boot_sppgrp$sppgrp <- gsub("^.*_", "", boot_sppgrp$resp)
sppgrp_list <- c(paste0("_", unique(boot_sppgrp$sppgrp), collapse = "|"))
boot_sppgrp$metric <- gsub(sppgrp_list, "", boot_sppgrp$resp)

boot_sppgrp_comb <- boot_sppgrp %>% filter(sppgrp %in% c("NatCan", "NatOth", "Exotic")) %>% arrange(park, resp, term)

boot_sppgrp_comb$park_ord <- reorder(boot_sppgrp_comb$park, desc(boot_sppgrp_comb$lat_rank))
boot_sppgrp_comb$estimate[boot_sppgrp_comb$estimate == 0] <- NA
boot_sppgrp_comb$lower95[is.na(boot_sppgrp_comb$estimate)] <- NA
boot_sppgrp_comb$upper95[is.na(boot_sppgrp_comb$estimate)] <- NA

metrics <- c(unique(boot_sppgrp_comb$metric))

# response titles for plotting
metric_names <- c("Sapling BA (sq.m/ha)",
                  "Sapling Density (stems/sq.m)",
                  "Seedling Density (stems/sq.m)",
                  "Tree BA 10 cm (sq.m/ha)",  
                  "Tree Dens. 10 cm (stems/ha)",  
                  "Tree BA 20 cm (sq.m/ha)", 
                  "Tree Dens. 20 cm (stems/ha)", 
                  "Tree BA 30 cm (sq.m/ha)",
                  "Tree Dens. 30 cm (stems/ha)",
                  "Tree BA 40 cm+ (sq.m/ha)",
                  "Tree Dens. 40 cm+ (stems/ha)",
                  "Tree BA (sq.m/ha)", 
                  "Tree Density (stems/ha)")

# Matching networks to colors for facet strips
park_cols <- rbind(data.frame(park_ord = "BLNK", strp_col = "white"),
                   unique(data.frame(park_ord = boot_sppgrp_comb$park_ord,
                                     strp_col = boot_sppgrp_comb$strp_col)) %>% arrange(desc(park_ord))
)

park_cols <- park_cols %>% mutate(facet_ord = c(rev(202:209), rev(210:217), rev(218:225), 
                                                rev(226:233), rev(234:241))) %>% 
  arrange(facet_ord)

fills <- c(park_cols$strp_col) 

# Fake plot for network legend
leg_net <- ggplot(data = data.frame(network = c("ERMN", "MIDN", "NCBN", "NCRN", "NETN"),
                                    x = c(1, 2, 3, 4, 5)),
                  aes(x = x, fill = network))+
  geom_histogram()+
  scale_fill_manual(values = c("#A5BDCD", "#E7CDA4", "#CFB9D9", "#E1E59B", "#AACCA7"), 
                    name = "Network:")+ theme_bw()+
  theme(legend.position = 'bottom', 
        legend.title = element_text(size = 7), 
        legend.text=element_text(size = 7),
        #legend.background = element_blank()
        legend.background = element_rect(color = "#B9B9B9", size = 0.25)
  )

# Fake plot for trend legend
leg_line2 <- ggplot(data = data.frame(sign = c("notmod", "nonsign", "sign"),
                                      x = c(1, 2, 3)), 
                    aes(x = x, y = x, color = sign, fill = sign, linetype = sign))+
  theme_bw()+ geom_line(size = 0.5)+ # geom_point(size = 1.5, shape = 21, alpha = 0.8)+
  scale_fill_manual(values = c("notmod" = "#686868", "nonsign" =  "#686868", "sign" = "#686868"), 
                    labels = c("Not Modeled", "Not Sign.", "Sign."),
                    name = "Trends:",
                    drop = FALSE)+
  scale_color_manual(values = c("notmod" = "#686868", "nonsign" =  "#686868", "sign" = "#686868"), 
                     labels = c("Not Modeled", "Not Sign.", "Sign."),
                     name = "Trends:",
                     drop = FALSE)+
  scale_linetype_manual(values = c("notmod" = 'dotted', "nonsign" = 'dashed', "sign" = 'solid'), 
                        labels = c("Not Modeled", "Not Sign.", "Sign."),
                        name = "Trends:",
                        drop = FALSE)+
  theme(legend.position = 'bottom', legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7), #legend.background = element_blank()
        legend.background = element_rect(colour = "#B9B9B9", size = 0.25)
  )

# Fake plot for species group legend
leg_linesp <- ggplot(data = data.frame(spgrp = c("Exotic", "NatCan", "NatOth"),
                                       x = c(1, 2, 3)), 
                     aes(x = x, y = x, color = spgrp, fill = spgrp, shape = spgrp))+
  theme_bw()+ geom_line(size = 0.5)+  
  geom_point(size = 1.5)+
  scale_fill_manual(values = c("NatCan" = "#2BA12E", "NatOth" = "#B9B9B9", "Exotic" = "#CD5C5C"), 
                    labels = c("Native Canopy", "Native Subcanopy", "Exotic"),
                    name = "Groups:",
                    drop = FALSE)+
  scale_color_manual(values = c("NatCan" = "#2BA12E", "NatOth" = "#B9B9B9", "Exotic" = "#CD5C5C"), 
                     labels = c("Native Canopy", "Native Subcanopy", "Exotic"),
                     name = "Groups:",
                     drop = FALSE)+
  scale_shape_manual(values = c("NatCan" = 24, "NatOth" = 21, "Exotic" = 25),
                     labels = c("Native Canopy", "Native Subcanopy", "Exotic"),
                     name = "Groups:",
                     drop = FALSE)+
  theme(legend.position = 'bottom', legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7), #legend.background = element_blank()
        legend.background = element_rect(colour = "#B9B9B9", size = 0.25)
  )

leg_gnet <- gtable_filter(ggplot_gtable(ggplot_build(leg_net)), "guide-box")
leg_gline2 <- gtable_filter(ggplot_gtable(ggplot_build(leg_line2)), "guide-box")
leg_glinesp <- gtable_filter(ggplot_gtable(ggplot_build(leg_linesp)), "guide-box")

# match metric columns and titles
met_df <- data.frame(metrics = metrics, metric_names)

walk2(metrics[2:3], c("Fig4", "Fig5"), function(metric, fig){
  figure = paste0(fig, "_")
  title = as.character(met_df$metric_names[met_df$metrics == metric])
  var = as.character(met_df$metrics[met_df$metrics == metric])
  sppgrp = as.character(unique(boot_sppgrp_comb$sppgrp[boot_sppgrp_comb$metric == metric]))
  
  p <- plot_trends_by_grp(boot_sppgrp_comb, 
                          xlab = "Cycle", ylab = title,
                          group = "sppgrp", var = var, 
                          facet_scales = 'fixed')+
    scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))
  
  g <- ggplot_gtable(ggplot_build(p))
  strp_col <- which(grepl('strip-t', g$layout$name))
  
  k <- 1
  for (i in strp_col) {
    g$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- fills[k]
    k <- k+1
  }
  
  svg(paste0("./results/Figures/", figure, metric, ".svg"), 
      height = 8, width = 11) 
  
  grid.arrange(grobs = list(g, leg_glinesp, leg_gline2, leg_gnet),
               heights = c(6.5, 0.5),
               widths = c(0.55, 2.75, 3.3, 3.0, 0.25),
               layout_matrix = rbind(c(1, 1, 1, 1, 1),
                                     c(NA, 2, 3, 4, NA)))
  dev.off()
  cat(metric, "\n")  
})


