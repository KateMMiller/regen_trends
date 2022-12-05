#----------------------------------------
# Plotting regression results
#----------------------------------------

# Run code chunks from RMD to add results dataframes to working environment
source(knitr::purl("./scripts/01D_Regression_Analysis.Rmd"))

library(tidyverse)
library(grid)
library(gridExtra)

# Calculate percet of plots >80% canopy cover 
comb_df <- comb_df %>% mutate(over80 = ifelse(cancov >= 80, 1, 0))
sum(comb_df$over80)/nrow(comb_df) * 100

# Set up data for plotting
grp <- "DBI2"
xvar = "avg.cover"
yvar = "pred_stock_tr"
xlab = "Average % Invasive Cover"

reg_new$grp_fac <- as.factor(reg_new[,grp])
dbi_cols <- c("#15C400", "#FFA600", "#E51E02")
phy_labs <- c("Dry", "Dry-Mesic", "Mesic", "Hydric")
names(phy_labs) <- c("dry", "dry-mesic", 'mesic', 'hydric')  
names(reg_new)

# For MS dbi vs inv. cov
# seedlings
seeds <-
  ggplot(reg_new, aes_string(x = xvar, y = "pred_seed_tr", group = "grp_fac", fill = "grp_fac",
                        color = "grp_fac", shape = "grp_fac", size = "grp_fac")) +
  geom_line(size = 0.5) + 
  geom_point(size = 2) + 
  theme_bw() + 
  facet_wrap(~physio, ncol = 4, labeller = as_labeller(phy_labs)) +
  scale_color_manual(values = dbi_cols, 
                     labels = c("Low/Medium", "High", "Very High"), 
                     name = "Deer Browse Impact") +
  scale_fill_manual(values = dbi_cols,
                    labels = c("Low/Medium", "High", "Very High"),
                    name = "Deer Browse Impact") +
  scale_shape_manual(values = c(25, 21, 24), labels = c("Low/Medium", "High", "Very High"),
                     name = "Deer Browse Impact") +
  labs(x = NULL, y = "Seedling Density") +
  theme(panel.grid = element_blank(), 
        legend.position = 'none',
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 11),
        axis.text = element_text(size = 11),
        axis.title.y = element_text(size = 12, margin = margin(b = 0, t = 0, l = 3.9, r = 4.9)))

# saplings
saps <-
  ggplot(reg_new, aes_string(x = xvar, y = "pred_sap_tr", group = "grp_fac", fill = "grp_fac",
                        color = "grp_fac", shape = "grp_fac", size = "grp_fac")) +
  geom_line(size = 0.5) + 
  geom_point(size = 2) + 
  theme_bw() + 
  facet_wrap(~physio, ncol = 4, labeller = as_labeller(phy_labs)) +
  scale_color_manual(values = dbi_cols, 
                     labels = c("Low/Medium", "High", "Very High"), 
                     name = "Deer Browse Impact") +
  scale_fill_manual(values = dbi_cols,
                    labels = c("Low/Medium", "High", "Very High"),
                    name = "Deer Browse Impact") +
  scale_shape_manual(values = c(25, 21, 24), labels = c("Low/Medium", "High", "Very High"),
                     name = "Deer Browse Impact") +
  labs(x = NULL, y = "Sapling Density") +
  theme(panel.grid = element_blank(), 
        legend.position = 'none',
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 11),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

# stocking
stock <-
  ggplot(reg_new, aes_string(x = xvar, y = "pred_stock_tr", group = "grp_fac", fill = "grp_fac",
                        color = "grp_fac", shape = "grp_fac", size = "grp_fac")) +
  geom_line(size = 0.5) + 
  geom_point(size = 2) + 
  theme_bw() + 
  facet_wrap(~physio, ncol = 4, labeller = as_labeller(phy_labs)) +
  scale_color_manual(values = dbi_cols, 
                     labels = c("Low/Medium", "High", "Very High"), 
                     name = "Deer Browse Impact") +
  scale_fill_manual(values = dbi_cols,
                    labels = c("Low/Medium", "High", "Very High"),
                    name = "Deer Browse Impact") +
  scale_shape_manual(values = c(25, 21, 24), labels = c("Low/Medium", "High", "Very High"),
                     name = "Deer Browse Impact") +
  labs(x = "% Cover of Invasive Plants", y = "Stocking Index") +
  theme(panel.grid = element_blank(), 
        legend.position = 'bottom',
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 11),
        axis.text = element_text(size = 11),
        axis.title.x = element_text(size = 12, margin = margin(t = 10, b = 0)),
        axis.title.y = element_text(size = 12, margin = margin(b = 0, t = 0, l = 6.1, r = 5.4)))

# histogram of x axis
inv_dist <- 
  ggplot(comb_df, aes(x = avg.cover, group = "physio")) + 
  geom_density(color = 'black', fill = '#CACACA') + 
  facet_wrap(~physio, ncol = 4, labeller = as_labeller(phy_labs)) + 
  labs(x = NULL, y = "Density")+
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(0, 100)) +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3), limits = c(0, 0.25)) +
  theme_bw()+
  theme(panel.grid = element_blank(), 
        legend.position = 'none',
        strip.text = element_text(size = 11),
        axis.text = element_text(size = 11),
        axis.title.x = element_text(size = 12, margin = margin(t = 6, b = 0)),
        axis.title.y = element_text(size = 12, margin = margin(b = 0, t = 0, l = 6, r = 3)))

plots <- grid.arrange(inv_dist, seeds, saps, stock, nrow = 4, heights = c(0.6, 1, 1, 1.28))

ggsave("./results/Figures/Fig6_DBI_invcov.png", plots, dpi = 300, height = 8.5, width = 7)

#-------------------------
# For MS dbi vs tree
grp <- "DBI2"

xvar = "Tree_Dens_Total"
xlab = "Live Tree Density (stems/ha)"

reg_new2$grp_fac <- as.factor(reg_new2[,grp])
dbi_cols <- c("#15C400", "#FFA600", "#E51E02")
phy_labs <- c("Dry", "Dry-Mesic", "Mesic", "Hydric")
names(phy_labs) <- c("dry", "dry-mesic", 'mesic', 'hydric')  
names(reg_new2)

# seedlings
seeds <-
  ggplot(reg_new2, aes_string(x = xvar, y = "pred_seed_tr", group = "grp_fac", fill = "grp_fac",
                        color = "grp_fac", shape = "grp_fac", size = "grp_fac")) +
  geom_line(size = 0.5) + 
  geom_point(size = 2) + 
  theme_bw() + 
  facet_wrap(~physio, ncol = 4, labeller = as_labeller(phy_labs)) +
  scale_color_manual(values = dbi_cols, 
                     labels = c("Low/Medium", "High", "Very High"), 
                     name = "Deer Browse Impact") +
  scale_fill_manual(values = dbi_cols,
                    labels = c("Low/Medium", "High", "Very High"),
                    name = "Deer Browse Impact") +
  scale_shape_manual(values = c(25, 21, 24), labels = c("Low/Medium", "High", "Very High"),
                     name = "Deer Browse Impact") +
  labs(x = NULL, y = "Seedling Density") +
  theme(panel.grid = element_blank(), 
        legend.position = 'none',
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 11),
        axis.text = element_text(size = 10.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        #panel.spacing = unit(1.5, 'lines'),
        axis.title.y = element_text(size = 12, 
                                    margin = margin(b = 0, t = 0, l = 12, r = 4.9)))
# saplings
saps <-
  ggplot(reg_new2, aes_string(x = xvar, y = "pred_sap_tr", group = "grp_fac", fill = "grp_fac",
                        color = "grp_fac", shape = "grp_fac", size = "grp_fac")) +
  geom_line(size = 0.5) + 
  geom_point(size = 2) + 
  theme_bw() + 
  facet_wrap(~physio, ncol = 4, labeller = as_labeller(phy_labs)) +
  scale_color_manual(values = dbi_cols, 
                     labels = c("Low/Medium", "High", "Very High"), 
                     name = "Deer Browse Impact") +
  scale_fill_manual(values = dbi_cols,
                    labels = c("Low/Medium", "High", "Very High"),
                    name = "Deer Browse Impact") +
  scale_shape_manual(values = c(25, 21, 24), labels = c("Low/Medium", "High", "Very High"),
                     name = "Deer Browse Impact") +
  labs(x = NULL, y = "Sapling Density") +
  theme(panel.grid = element_blank(), 
        legend.position = 'none',
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 11),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10.5),
        #panel.spacing = unit(1.5, 'lines'),
        #        axis.title = element_text(size = 12),
        axis.title.y = element_text(size = 12, 
                                    margin = margin(b = 0, t = 0, l = 0.5, r = 2)))

# stocking
stock <-
  ggplot(reg_new2, aes_string(x = xvar, y = "pred_stock_tr", group = "grp_fac", fill = "grp_fac",
                        color = "grp_fac", shape = "grp_fac", size = "grp_fac")) +
  geom_line(size = 0.5) + 
  geom_point(size = 2) + 
  theme_bw() + 
  facet_wrap(~physio, ncol = 4, labeller = as_labeller(phy_labs)) +
  scale_color_manual(values = dbi_cols, 
                     labels = c("Low/Medium", "High", "Very High"), 
                     name = "Deer Browse Impact") +
  scale_fill_manual(values = dbi_cols,
                    labels = c("Low/Medium", "High", "Very High"),
                    name = "Deer Browse Impact") +
  scale_shape_manual(values = c(25, 21, 24), labels = c("Low/Medium", "High", "Very High"),
                     name = "Deer Browse Impact") +
  labs(x = "Live Tree Density (stems/ha)", y = paste0("Stocking Index")) +
  theme(panel.grid = element_blank(), 
        legend.position = 'bottom',
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 11),
        axis.text = element_text(size = 10.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        #panel.spacing = unit(1.5, 'lines'),
        axis.title.x = element_text(size = 12, 
                                    margin = margin(t = 6, b = 0)),
        axis.title.y = element_text(size = 12, 
                                    margin = margin(b = 0, t = 0, l = 13.5, r = 5.4)))

# histogram of x axis
tree_dist <- 
  ggplot(comb_df, aes(x = Tree_Dens_Total, group = "physio")) + 
  geom_density(color = 'black', fill = '#CACACA') + 
  facet_wrap(~physio, ncol = 4, labeller = as_labeller(phy_labs)) + 
  labs(x = NULL, y = "Density")+
  #scale_x_continuous(breaks = c(0, 1000, 2000, 2500), limits = c(0, 2500)) +
  scale_y_continuous(breaks = c(0, 0.001, 0.002),
                     labels = function(x) format(x, scientific = T)) +
  theme_bw()+
  theme(panel.grid = element_blank(), 
        legend.position = 'none',
        strip.text = element_text(size = 11),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
        axis.title.x = element_text(size = 12, margin = margin(t = 6, b = 0)),
        axis.title.y = element_text(size = 12, 
                                    margin = margin(b = 0, t = 0, l = 0, r = 0)))

plots <- grid.arrange(tree_dist, seeds, saps, stock, nrow = 4, heights = c(0.6, 1, 1, 1.28))

ggsave("./results/Figures/Fig7_DBI_treedens.png", plots, dpi = 300, height = 8.5, width = 7)

