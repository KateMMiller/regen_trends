#---------------------------------------------------------------------------------------------------
# Compiling cycle 3 (2016:2019) data for comparisons of tree and regeneration status across parks
#---------------------------------------------------------------------------------------------------
library(tidyverse)
library(boot)
library(grid) # for digging into str of ggplot grobs
library(gridExtra) # for ggarrange
library(gtable)

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

datapath <- "./data"

# Raw data from all years
dens_df <- read.csv("./data/regen_data_2008-2019.csv")

# Raw data from cycle 3
dens_c3 <- dens_df %>% filter(cycle == 3) 
park_list <- unique(dens_df$Unit_Code)
status_metrics <- c("stock_final", 
                    "Sap_Dens_Total", "Seed_Dens_Total",
                    "Sap_Dens_NatCan", "Seed_Dens_NatCan",
                    "Sor_sap", "Hor_sap", "Sor_seed", "Hor_seed")

# Set up park lists
park_metrics <- expand.grid(park_list, status_metrics)
parks <- park_metrics[,1]
metrics <- park_metrics[,2]

# Create mean function used in bootstrap
mean_fun <- function(data, i){
  d <- data[i, ]
  return(mean(d, na.rm = TRUE))
}

# Iterate over parks and metrics to calc. mean and 95% CI of mean 
c3_stats <- map2_dfr(parks, metrics, function(park, metric){
  df <- dens_c3 %>% filter(Unit_Code == park) 
  results_df <- boot(df[, paste0(metric), drop = FALSE], mean_fun, R = 1000)
  df2 <- data.frame(park = park, 
                    Network = unique(df$Network),
                    metric = metric, 
                    mean = results_df$t0,
                    lower95 = quantile(results_df$t, probs = 0.025, na.rm = T),
                    upper95 = quantile(results_df$t, probs = 0.975, na.rm = T),
                    row.names = NULL)
  return(df2)
})

write.csv(c3_stats, "./results/status_metric_stats.csv", row.names = FALSE)
