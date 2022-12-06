#----------------------------------
# GETT trend analysis with ash included as canopy species
#----------------------------------

#devtools::install_github("KateMMiller/forestTrends") #uncomment to install
library(forestTrends)
library(tidyverse)
# library(furrr) # for parallel processing with map
# library(future)

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

dens_df <- read.csv("./data/ash_as_canopy/regen_data_2008-2019_ash_as_canopy.csv")
dens_df2 <- dens_df %>% select(-contains("Native")) # Only interested in NatCan and NatOth

stay <- names(dens_df[,1:16])
reord <- sort(names(dens_df2[,17:ncol(dens_df2)]))

dens_df2 <- dens_df2[, c(stay, reord)]
names(dens_df2)

#---- Set up lists to iterate over ----
# column names for response
metrics <- c(names(dens_df2[, c(19:33, 66:ncol(dens_df2))]))
metrics

# response titles for plotting
metric_names <- c(rep("Sapling BA (sq.m/ha)", 4),
                  rep("Sapling Density (stems/sq.m)", 4),
                  rep("Seedling Density (stems/sq.m)", 4), 
                  "Sorensen Sapling", "Sorensen Seedling",
                  "Stocking Index",
                  rep("Tree BA (sq.m/ha)", 4), 
                  rep("Tree Density (stems/ha)", 4)
)

# match metric columns and titles
met_df <- data.frame(metrics, metric_names)
met_df

park_met_list <- data.frame(expand.grid("GETT", metrics)) %>% 
  set_names("Unit_Code", "metrics") %>% 
  arrange(Unit_Code, metrics) %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  left_join(., met_df, by = "metrics")

park_list <- c(park_met_list[,1])
met_list <- c(park_met_list[,2])

# # Set up parallel processing
# availableCores() #12
# future::plan(multicore, workers = 11)
gett <- dens_df2 %>% filter(Unit_Code == "GETT")

# Run the random intercept model for each combination of park and metric.
boot_results <- map_dfr(met_list,
                  function(y){case_boot_lmer(df = gett,
                                             x = "cycle", y = y, ID = "Plot_Name",
                                             random_type = 'intercept',
                                             num_reps = 1000, chatty = TRUE) %>%
                  mutate(park = "GETT", resp = paste(y))})

write.csv(boot_results, "./results/ash_as_canopy/trend_analysis_results_GETT_ash_as_canopy.csv", row.names = F)

