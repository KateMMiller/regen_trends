#---------------------------------------------------------------------------------------------------
# Plotting status results
#---------------------------------------------------------------------------------------------------
library(tidyverse)
library(boot)
library(grid) # for digging into str of ggplot grobs
library(gridExtra) # for ggarrange
library(gtable)

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

# Raw data all years
dens_df <- read.csv("./data/regen_data_2008-2019.csv")

# Cycle 3 status stats
c3_stats <- read.csv("./results/status_metric_stats.csv")

# Plotting function that sorts from high to low and color codes by network
plot_c3_status <- function(df, met, xlabel, ylabel){
  df2 <- df %>% filter(metric == met)
  df2$park_ord <- reorder(df2$park, desc(df2$mean))
  
  p <- ggplot(df2, 
              aes(x = park_ord, y = mean, fill = as.factor(Network)))+
    geom_bar(stat = 'identity', aes(fill = as.factor(Network)), col = "#868686")+ 
    geom_errorbar(aes(ymin = lower95, ymax = upper95), color = "#868686", 
                  width = 0.15, size = 0.4, na.rm = TRUE)+
    scale_fill_manual(values = c("ERMN" = "#A5BDCD", "MIDN" = "#E7CDA4", "NCBN" = "#CFB9D9", 
                                 "NCRN" = "#E1E59B", "NETN" = "#AACCA7"), 
                      name = "Network:")+
    theme_bw()+
    labs(x = xlabel, y = ylabel)+
    theme(axis.text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(hjust = 0.5, size = 14, 
                                    margin = margin(t = 10, b = -15)),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line = element_blank(), 
          legend.position = 'bottom')
  
  return(p)
  
}

plot_c3_status(c3_stats, "stock_final", xlabel = NULL, ylabel = "Mean Stocking Index")+
  geom_hline(yintercept = 25, linetype = 2, lwd = 0.75, color = "DimGrey")+ #'#CD5C5C')+
  geom_text(x = 39, y = 22.5, label = "Critical", hjust = 1, size = 4.5, check_overlap = TRUE, color = "#696969")+
  geom_text(x = 39, y = 64.5, label = "Caution", hjust = 1, size = 4.5, check_overlap = TRUE, color = "#696969")+
  geom_hline(yintercept = 100, linetype = 2, lwd = 0.75, color = "DimGrey")+ #"#228B22")+
  geom_text(x = 39, y = 104, hjust = 1, label = "Acceptable", size = 4.5, check_overlap = TRUE, color = "#696969")


ggsave("./results/Figures/Fig3B_mean_stocking_index.svg", width = 11, height = 8, units = 'in')

#---- Summarize proportion of stocked plots based on deer browse index
dens_c3 <- dens_df %>% filter(cycle == 3) 

dbi_stock_c3 <- dens_c3 %>% group_by(Network, Unit_Code, lat_rank) %>% 
  mutate(avg_DBI = mean(DBI, na.rm = T)) %>% ungroup() %>% 
  mutate(stocked = case_when(avg_DBI <= 3 & stock_final >= 50 ~ 1,
                             avg_DBI > 3 & stock_final >= 100 ~ 1,
                             TRUE ~ 0))

dbi_stock_sum <- dbi_stock_c3 %>% group_by(Network, Unit_Code, lat_rank) %>% 
                   summarize(avg_stock = mean(stock_final, na.rm = TRUE),
                             avg_DBI = first(avg_DBI),
                             num_stocked = sum(stocked, na.rm = TRUE),
                             num_plots = sum(!is.na(stocked)),
                             prop_stocked = num_stocked/num_plots) %>% 
                   rename(park = Unit_Code)

dbi_stock_sum$park_ord <- reorder(dbi_stock_sum$park, desc(dbi_stock_sum$prop_stocked))

p <- 
  ggplot(dbi_stock_sum,
            aes(x = park_ord, y = prop_stocked, fill = as.factor(Network)))+
  geom_bar(stat = 'identity', aes(fill = as.factor(Network)), color = "#868686")+ 
  scale_fill_manual(values = c("ERMN" = "#A5BDCD", "MIDN" = "#E7CDA4", "NCBN" = "#CFB9D9", 
                               "NCRN" = "#E1E59B", "NETN" = "#AACCA7"), 
                    name = "Network:")+
  scale_y_continuous(limits = c(0, 0.7), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7), 
                     labels = c("0", "10", "20", "30", "40", "50", "60", "70"))+  
  theme_bw()+
  labs(x = NULL, y = "% Stocked Plots")+
  theme(axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 14, 
                                  margin = margin(t = 10, b = -15)),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_blank(), 
        legend.position = 'bottom')+
  geom_hline(yintercept = 0.32, lty = 2, lwd = 0.75, color = "#696969")+
  geom_text(x = 39, y = 0.30, label = "Critical", hjust = 1, size = 4.5, check_overlap = TRUE, color = "#696969")+
  geom_hline(yintercept = 0.66, lty = 2, lwd = 0.75, color = "#696969")+
  geom_text(x = 39, y = 0.495, label = "Caution", hjust = 1, size = 4.5, check_overlap = TRUE, color = "#696969")+
  geom_text(x = 39, y = 0.68, hjust = 1, label = "Acceptable", size = 4.5, check_overlap = TRUE, color = "#696969")
  
p
ggsave("./results/Figures/Fig3A_pct_stocked.svg", width = 11, height = 8, units = 'in')
