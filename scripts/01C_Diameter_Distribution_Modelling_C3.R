library(tidyverse)
tree_dbh_dist <- read.csv("./data/tree_diameter_dist_2008-2019.csv")

head(tree_dbh_dist)

park_list <- unique(tree_dbh_dist$Unit_Code)

AIC_test <- map_dfr(park_list, function(park){
  df <- tree_dbh_dist %>% filter(Unit_Code == park & cycle == 3)
  lin_mod <- lm(density ~ class, data = df)
  exp_mod <- lm(log(density + 1) ~ class, data = df)
  aic_check <- data.frame(Unit_Code = park, 
                          linear = AIC(lin_mod),
                          exp = AIC(exp_mod) + sum(2*log(df$density + 1)))
})

AIC_test$best_mod <- ifelse(AIC_test$linear < AIC_test$exp, "linear", "log-linear")
AIC_test

write.csv(AIC_test, "./results/tree_diameter_dist_results.csv", row.names = FALSE)
