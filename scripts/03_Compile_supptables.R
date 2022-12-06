#--------------------------------------------
# Compiling supplemental tables
#--------------------------------------------

# Appendix S1 Table 2:
#---- Dependencies and import data using NPSForVeg ----
#devtools::install_github("NCRN/NPSForVeg")
library(NPSForVeg)
library(tidyverse)
#library(rlang)
#library(vegan)
library(boot)

options(scipen = 100)

datapath <- "./data/raw_data/" 

#---- Compile Number of species per strata and group -----
ncrn <- importNCRN(paste0(datapath,"NCRN_data"))
ermn <- importERMN(paste0(datapath, "ERMN_data"))                   
midn <- importMIDN(paste0(datapath, "MIDN_data"))
netn <- importNETN(paste0(datapath, "NETN_data"))

spp_list <- read.csv(paste0(datapath, "tree_species_groups.csv")) 
# Demote Fraxinus to not canopy tree
spp_list$Canopy_Tree[grepl("Fraxinus", spp_list$Species)] <- 0


#arglist <- list(years = 2008:2019, status = 'alive')
yrs_all <- 2008:2019
c1 <- 2008:2011; c2 <- c1+4; c3 <- c2+4
cycles <- list(c1, c2, c3)
cycle_names <- c("C1", "C2", "C3")

network_codes <- c("ERMN", "MIDN", "NCRN", "NETN")

sum_by_cycle <- function(network, network_name, group, year_span, value, units, value_name, cycle_name){
  df <- SiteXSpec(network, group = group, years = year_span, values = value, plot.type = 'active',
                  area = units, status = 'alive') %>% 
        pivot_longer(-Plot_Name, names_to = "Species", values_to = value_name) %>%
        mutate(cycle = cycle_name, Network = network_name)
return(data.frame(df))
}

sum_by_cycle_dbh <- function(network, network_name, group, year_span, value, units, 
                             value_name, cycle_name, size_min, size_max){
  df <- SiteXSpec(network, group = group, years = year_span, values = value, plot.type = 'active',
                  area = units, status = 'alive', size.min = size_min, size.max = size_max) %>% 
    pivot_longer(-Plot_Name, names_to = "Species", values_to = value_name) %>%
    mutate(cycle = cycle_name, Network = network_name)
  return(data.frame(df))
}
  
#---- Set up plot x visit df for left join ----
plot_visit_df1 <- rbind(getEvents(ermn, years = yrs_all, plot.type = 'active', output = 'dataframe') %>% 
                          select(Plot_Name, Unit_Code, Event_Year) %>% mutate(Network = "ERMN"),
                        getEvents(midn, years = yrs_all, plot.type = 'active', output = 'dataframe') %>% 
                          select(Plot_Name, Unit_Code, Event_Year) %>% mutate(Network = "MIDN"),
                        getEvents(ncrn, years = yrs_all, plot.type = 'active', output = 'dataframe') %>% 
                          select(Plot_Name, Unit_Code, Event_Year) %>% mutate(Network = "NCRN"),
                        getEvents(netn, years = yrs_all, plot.type = 'active', output = 'dataframe') %>% 
                          select(Plot_Name, Unit_Code, Event_Year) %>% mutate(Network = "NETN")) %>% 
#                  filter(!Plot_Name %in% plot_exclude) %>% 
                  rename(Year = Event_Year) %>% 
                  mutate(cycle = case_when(Year %in% 2008:2011 ~ 1,
                                           Year %in% 2012:2015 ~ 2,
                                           Year %in% 2016:2019 ~ 3,
                                           TRUE ~ NA_real_))

plot_sizes <- rbind(read.csv(paste0(datapath, "ERMN_data/MetaData.csv")) %>% 
                      select(Network, ParkCode, SapPlotNum, SapPlotSize, SeedPlotNum, SeedPlotSize,
                             ShrubPlotNum, ShrubPlotSize, HPlotNum, HPlotSize),
                    read.csv(paste0(datapath, "MIDN_data/MetaData.csv")) %>% 
                      select(Network, ParkCode, SapPlotNum, SapPlotSize, SeedPlotNum, SeedPlotSize,
                             ShrubPlotNum, ShrubPlotSize, HPlotNum, HPlotSize),
                    read.csv(paste0(datapath, "NCRN_data/MetaData.csv")) %>% 
                      select(Network, ParkCode, SapPlotNum, SapPlotSize, SeedPlotNum, SeedPlotSize,
                             ShrubPlotNum, ShrubPlotSize, HPlotNum, HPlotSize),
                    read.csv(paste0(datapath, "NETN_data/MetaData.csv")) %>% 
                      select(Network, ParkCode, SapPlotNum, SapPlotSize, SeedPlotNum, SeedPlotSize,
                             ShrubPlotNum, ShrubPlotSize, HPlotNum, HPlotSize)
                    )

plot_sizes$Network[plot_sizes$Network == "NCBN"] <- "MIDN"

# Relate subplot numbers and areas to each visit
  # Set up NCRN Events so same columns as rest of networks to ensure plots missing seeds/saps are included
  # and plots missing a subplot are also specified
ncrn_events <- left_join(read.csv(paste0(datapath, "NCRN_data/Events.csv")) %>% 
                 mutate(numHerbPlots = numSeedPlots), 
               read.csv(paste0(datapath, "NCRN_data/MetaData.csv")) %>% 
                 select(Network, ParkCode, SapPlotNum, SapPlotSize, SeedPlotNum, SeedPlotSize,
                        ShrubPlotNum, ShrubPlotSize, HPlotNum, HPlotSize), 
               by = c("Unit_Code" = "ParkCode")) %>% 
               mutate(numSapPlots = SapPlotNum, excludeEvent = 0) %>% 
               select(Plot_Name, Event_Year, numSapPlots, numSeedPlots, numHerbPlots, excludeEvent)


event_info <- rbind(read.csv(paste0(datapath, "ERMN_data/Events.csv")) %>% 
                      select(Plot_Name, Event_Year, numSapPlots, numSeedPlots, numHerbPlots, 
                             excludeEvent),
                    read.csv(paste0(datapath, "MIDN_data/Events.csv")) %>% 
                      select(Plot_Name, Event_Year, numSapPlots, numSeedPlots, numHerbPlots, 
                             excludeEvent),
                    ncrn_events,
                    read.csv(paste0(datapath, "NETN_data/Events.csv")) %>% 
                      select(Plot_Name, Event_Year, numSapPlots, numSeedPlots, numHerbPlots,
                             excludeEvent)
                    )

plot_visit_df2 <- left_join(plot_visit_df1, plot_sizes, by = c("Unit_Code" = "ParkCode", "Network"))
head(plot_visit_df2)
head(event_info)
# mutates take event-level number of subplots if not NA. Otherwise, take the park-level number of subplots
# from the metadata file. Use excludeEvent from Events.csv to drop ACAD-029-2010
plot_visit_df <- left_join(plot_visit_df2, event_info, by = c("Plot_Name", "Year" = "Event_Year")) %>% 
                 mutate(SapPlotNum = ifelse(!is.na(numSapPlots), numSapPlots, SapPlotNum),
                        SeedPlotNum = ifelse(!is.na(numSeedPlots), numSeedPlots, SeedPlotNum),
                        HPlotNum = ifelse(!is.na(numHerbPlots), numHerbPlots, HPlotNum)) %>% 
                 select(-starts_with("num")) #%>% 
                 #filter(excludeEvent != 1) #excludes ACAD-029-2010 and COLO-380-2018

## write.csv(plot_visit_df, "EFWG_plot_visit_left_join.csv", row.names = F)

head(plot_visit_df)

# Check for duplicates
plot_visit_df %>% 
  group_by(Plot_Name, Network, Unit_Code, Year) %>% 
  summarize(num_events = n(), .groups = 'drop')%>% 
  filter(num_events > 1) #0- no duplicate events

table(plot_visit_df$Unit_Code, plot_visit_df$Year)
table(plot_visit_df$Unit_Code, plot_visit_df$cycle)
length(unique(plot_visit_df$Plot_Name)) #1515 unique plots
nrow(plot_visit_df) # 4464 plot x cycle rows
table(complete.cases(plot_visit_df)) # all 4462 T
head(plot_visit_df)

# Find max DBH of trees (partial check on ERMN's 999999 and for dbh dist.)
t(rbind(lapply(seq_along((ermn)), function(x){
  max(ermn[[x]]@Trees$Equiv_Live_DBH_cm, na.rm = T)})) %>% data.frame())  #137.7

t(rbind(lapply(seq_along((midn)), function(x){
  max(midn[[x]]@Trees$Equiv_Live_DBH_cm, na.rm = T)})) %>% data.frame())  #147.8

t(rbind(lapply(seq_along((ncrn)), function(x){
  max(ncrn[[x]]@Trees$Equiv_Live_DBH_cm, na.rm = T)})) %>% data.frame())  #172.5

t(rbind(lapply(seq_along((netn)), function(x){
  max(netn[[x]]@Trees$Equiv_Live_DBH_cm, na.rm = T)})) %>% data.frame())  #132.4

# Bring in deer browse index, so can order facets by Deer Browse Impacts in most recent cycle
dbi <- rbind(read.csv(paste0(datapath, "/ERMN_data/Events.csv")) %>% 
               select(Plot_Name, Event_Year, Deer_Browse_Line_ID) %>% 
               rename(DBI = Deer_Browse_Line_ID), 
             read.csv(paste0(datapath, "/MIDN_data/Events.csv")) %>% 
               select(Plot_Name, Event_Year, Deer_Browse_Line_ID) %>% 
               rename(DBI = Deer_Browse_Line_ID), 
             read.csv(paste0(datapath, "/NCRN_data/Plot_Visit_Data.csv")) %>% 
               select(Plot_Name, Sample_Year, Deer_Impact) %>% 
               rename(Event_Year = Sample_Year, DBI = Deer_Impact),
             read.csv(paste0(datapath, "/NETN_data/Events.csv")) %>% 
               select(Plot_Name, Event_Year, Deer_Browse_Line_ID) %>% 
               rename(DBI = Deer_Browse_Line_ID)) 

names(plot_visit_df)
pv_dbi <- left_join(plot_visit_df, dbi %>% select(Plot_Name, Event_Year, DBI), 
                    by = c("Plot_Name", "Year" = "Event_Year")) 

plots <- c(unique(pv_dbi$Plot_Name))
missing_plots <- plot_visit_df %>% filter(!Plot_Name %in% plots) %>% select(Plot_Name, Unit_Code)
missing_plots 

table(pv_dbi$Unit_Code, pv_dbi$Year)
table(pv_dbi$DBI, useNA = 'always') # There shouldn't be any 1s in the dataset (I checked), so converting to 2.
pv_dbi$DBI[pv_dbi$DBI == 1] <- 2
table(pv_dbi$excludeEvent)
length(unique(pv_dbi$Plot_Name)) #1515
pv_dbi$DBI[pv_dbi$DBI >= 9] <- NA
pv_dbi$DBI[pv_dbi$excludeEvent == 1] <- NA


#---- Live tree density by species group in stems/ha ----
live_tree_dens_ermn <- rbind(sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(ermn, network_name = "ERMN",  
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 3))

live_tree_dens_midn <- rbind(sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 3))

live_tree_dens_ncrn <- rbind(sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 3))

live_tree_dens_netn <- rbind(sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'count', units = 'ha', 
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'count', units = 'ha', 
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'count', units = 'ha', 
                                          value_name = "Dens", cycle_name = 3))

live_tree_dens <- rbind(live_tree_dens_ermn, live_tree_dens_midn, live_tree_dens_ncrn, live_tree_dens_netn)
live_tree_dens_spp1 <- right_join(spp_list, live_tree_dens, by = "Species") %>% 
                       mutate(Group = ifelse(Species == "Total", "Total", Group))

live_tree_dens_spp <- live_tree_dens_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
                      left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
                                by = c("Plot_Name", "cycle"))
head(live_tree_dens_spp)


spp_list2 <- unique(data.frame(live_tree_dens_spp$Species, live_tree_dens_spp$Group))

live_tree_dens_total <- live_tree_dens_spp %>% 
                          filter(Group == "Total") %>% 
                          select(Network, Plot_Name, cycle, Dens) %>% 
                          rename(Tree_Dens_Total = Dens)

live_tree_dens_native <- live_tree_dens_spp %>% 
                           filter(Group != "Total") %>% 
                           filter(Native == 1) %>% 
                           group_by(Network, Plot_Name, cycle) %>% 
                           summarize(Tree_Dens_Native = sum(Dens, na.rm = T), 
                                     .groups = 'drop')

live_tree_dens_natcan <- live_tree_dens_spp %>% 
                           filter(Group != "Total") %>% 
                           filter(Native == 1 & Canopy_Tree == 1) %>% 
                           group_by(Network, Plot_Name, cycle) %>% 
                           summarize(Tree_Dens_NatCan = sum(Dens, na.rm = T), 
                                     .groups = 'drop')

live_tree_dens_exotic <- live_tree_dens_spp %>% 
                           filter(Group != "Total") %>% 
                           filter(Native == 0) %>% 
                           group_by(Network, Plot_Name, cycle) %>% 
                           summarize(Tree_Dens_Exotic = sum(Dens, na.rm = T), 
                                    .groups = 'drop')

live_tree_dens_final <- list(plot_visit_df, live_tree_dens_total, live_tree_dens_native, 
                             live_tree_dens_natcan, live_tree_dens_exotic) %>% 
                        reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

head(live_tree_dens_final)


# Checking work
length(unique(plot_visit_df$Plot_Name))  
length(unique(live_tree_dens_final$Plot_Name))
nrow(plot_visit_df) #4464
nrow(live_tree_dens_final)#4464
table(complete.cases(live_tree_dens_final))

#---- Live tree BA by species group in m2/ha ----
live_tree_ba_ermn <- rbind(sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 1),
                             sum_by_cycle(ermn, network_name = "ERMN",  
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 2),
                             sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 3))

live_tree_ba_midn <- rbind(sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 1),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 2),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 3))

live_tree_ba_ncrn <- rbind(sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 1),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 2),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 3))

live_tree_ba_netn <- rbind(sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'size', units = 'ha', 
                                          value_name = "BA", cycle_name = 1),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'size', units = 'ha', 
                                          value_name = "BA", cycle_name = 2),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'size', units = 'ha', 
                                          value_name = "BA", cycle_name = 3))

live_tree_ba <- rbind(live_tree_ba_ermn, live_tree_ba_midn, live_tree_ba_ncrn, live_tree_ba_netn)
live_tree_ba_spp1 <- right_join(spp_list, live_tree_ba, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

live_tree_ba_spp <- live_tree_ba_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
  left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
            by = c("Plot_Name", "cycle"))

spp_list2 <- unique(data.frame(live_tree_ba_spp$Species, live_tree_ba_spp$Group))

live_tree_ba_total <- live_tree_ba_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, BA) %>% 
  rename(Tree_BA_Total = BA)

live_tree_ba_native <- live_tree_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Tree_BA_Native = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_ba_natcan <- live_tree_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Tree_BA_NatCan = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_ba_exotic <- live_tree_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Tree_BA_Exotic = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_ba_final <- list(plot_visit_df, live_tree_ba_total, live_tree_ba_native, 
                             live_tree_ba_natcan, live_tree_ba_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_ba_final$Plot_Name)) #1515
nrow(plot_visit_df)#4464
nrow(live_tree_ba_final)#4464
table(complete.cases(live_tree_ba_final)) #4464 T
head(live_tree_ba_final)

tree_dens_stats <- live_tree_dens_spp %>% filter(!Species %in% "Total") %>% select(-cycle) %>% 
  mutate(park = substr(Plot_Name, 1, 4)) %>% 
  group_by(park, Canopy_Tree, Native, Species) %>% 
  summarize(dens = sum(Dens, na.rm = T), 
            pres = ifelse(sum(dens > 0), 1, 0)) %>% filter(pres > 0) %>% 
  group_by(park, Canopy_Tree, Native) %>% summarize(numspp_tree = sum(pres > 0)) %>% 
  mutate(species_type = case_when(Canopy_Tree == 1 & Native == 1 ~ "NatCan",
                                  Canopy_Tree == 0 & Native == 1 ~ "NatSub",
                                  Canopy_Tree == 1 & Native == 0 ~ "Exotic", 
                                  Canopy_Tree == 0 & Native == 0 ~ "Exotic", 
                                  TRUE ~ "Unk")) %>% 
  group_by(park, species_type) %>% summarize(numspp_tree= sum(numspp_tree))

#---- Live sapling density by species group in stems/m2 ----
live_sap_dens_ermn <- rbind(sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'saplings', year_span = 2008:2011,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(ermn, network_name = "ERMN",  
                                          group = 'saplings', year_span = 2012:2015,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'saplings', year_span = 2016:2019,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 3))

live_sap_dens_midn <- rbind(sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'saplings', year_span = 2008:2011,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'saplings', year_span = 2012:2015,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'saplings', year_span = 2016:2019,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 3))

live_sap_dens_ncrn <- rbind(sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'saplings', year_span = 2008:2011,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'saplings', year_span = 2012:2015,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'saplings', year_span = 2016:2019,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 3))

live_sap_dens_netn <- rbind(sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'saplings', year_span = 2008:2011,
                                          value = 'count', units = 'plot', 
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'saplings', year_span = 2012:2015,
                                          value = 'count', units = 'plot', 
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'saplings', year_span = 2016:2019,
                                          value = 'count', units = 'plot', 
                                          value_name = "Dens", cycle_name = 3))

live_sap_dens <- rbind(live_sap_dens_ermn, live_sap_dens_midn, live_sap_dens_ncrn, live_sap_dens_netn)

live_sap_dens_spp1 <- right_join(spp_list, live_sap_dens, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group)) 

live_sap_dens_spp <- live_sap_dens_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
  left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
            by = c("Plot_Name", "cycle"))

spp_list2 <- unique(data.frame(live_sap_dens_spp$Species, live_sap_dens_spp$Group))

sort(unique(live_sap_dens_spp$Group))

live_sap_dens_total <- live_sap_dens_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, Dens) %>% 
  rename(Sap_Dens_Total = Dens)

live_sap_dens_native <- live_sap_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_Dens_Native = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_sap_dens_natcan <- live_sap_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_Dens_NatCan = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_sap_dens_exotic <- live_sap_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_Dens_Exotic = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_sap_dens_comb <- list(plot_visit_df, live_sap_dens_total, live_sap_dens_native, 
                             live_sap_dens_natcan, live_sap_dens_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

live_sap_dens_final <- live_sap_dens_comb %>% mutate(Sap_Dens_Total = Sap_Dens_Total/(SapPlotNum * SapPlotSize),
                                                     Sap_Dens_Native = Sap_Dens_Native/(SapPlotNum * SapPlotSize),
                                                     Sap_Dens_NatCan = Sap_Dens_NatCan/(SapPlotNum * SapPlotSize),
                                                     Sap_Dens_Exotic = Sap_Dens_Exotic/(SapPlotNum * SapPlotSize)) %>% 
                                              select(Plot_Name:cycle, Sap_Dens_Total, Sap_Dens_Native, 
                                                     Sap_Dens_NatCan, Sap_Dens_Exotic)


# Checking work
length(unique(live_sap_dens_final$Plot_Name)) #1515
nrow(live_sap_dens_final) #4464
table(complete.cases(live_sap_dens_final)) # 6F: b/c plots are missing regen data, which is correct.

#---- Live sapling BA by species group in m2/ha----
live_sap_ba_ermn <- rbind(sum_by_cycle(ermn, network_name = "ERMN", 
                                        group = 'saplings', year_span = 2008:2011,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 1),
                           sum_by_cycle(ermn, network_name = "ERMN",  
                                        group = 'saplings', year_span = 2012:2015,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 2),
                           sum_by_cycle(ermn, network_name = "ERMN", 
                                        group = 'saplings', year_span = 2016:2019,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 3))

live_sap_ba_midn <- rbind(sum_by_cycle(midn, network_name = "MIDN", 
                                        group = 'saplings', year_span = 2008:2011,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 1),
                           sum_by_cycle(midn, network_name = "MIDN", 
                                        group = 'saplings', year_span = 2012:2015,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 2),
                           sum_by_cycle(midn, network_name = "MIDN", 
                                        group = 'saplings', year_span = 2016:2019,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 3))

live_sap_ba_ncrn <- rbind(sum_by_cycle(ncrn, network_name = "NCRN", 
                                        group = 'saplings', year_span = 2008:2011,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 1),
                           sum_by_cycle(ncrn, network_name = "NCRN", 
                                        group = 'saplings', year_span = 2012:2015,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 2),
                           sum_by_cycle(ncrn, network_name = "NCRN", 
                                        group = 'saplings', year_span = 2016:2019,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 3))

live_sap_ba_netn <- rbind(sum_by_cycle(netn, network_name = "NETN", 
                                        group = 'saplings', year_span = 2008:2011,
                                        value = 'size', units = 'plot', 
                                        value_name = "BA", cycle_name = 1),
                           sum_by_cycle(netn, network_name = "NETN", 
                                        group = 'saplings', year_span = 2012:2015,
                                        value = 'size', units = 'plot', 
                                        value_name = "BA", cycle_name = 2),
                           sum_by_cycle(netn, network_name = "NETN", 
                                        group = 'saplings', year_span = 2016:2019,
                                        value = 'size', units = 'plot', 
                                        value_name = "BA", cycle_name = 3))

live_sap_ba <- rbind(live_sap_ba_ermn, live_sap_ba_midn, live_sap_ba_ncrn, live_sap_ba_netn)
live_sap_ba_spp1 <- right_join(spp_list, live_sap_ba, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

live_sap_ba_spp <- live_sap_ba_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
  left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
            by = c("Plot_Name", "cycle"))

spp_list2 <- unique(data.frame(live_sap_ba_spp$Species, live_sap_ba_spp$Group))

live_sap_ba_total <- live_sap_ba_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, BA) %>% 
  rename(Sap_BA_Total = BA)

live_sap_ba_native <- live_sap_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_BA_Native = sum(BA, na.rm = T), 
            .groups = 'drop')

live_sap_ba_natcan <- live_sap_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_BA_NatCan = sum(BA, na.rm = T), 
            .groups = 'drop')

live_sap_ba_exotic <- live_sap_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_BA_Exotic = sum(BA, na.rm = T), 
            .groups = 'drop')

head(live_sap_ba_natcan)

live_sap_ba_comb <- list(plot_visit_df, live_sap_ba_total, live_sap_ba_native, 
                         live_sap_ba_natcan, live_sap_ba_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

live_sap_ba_final <- live_sap_ba_comb %>% mutate(Sap_BA_Total = (10000*Sap_BA_Total)/(SapPlotNum * SapPlotSize),
                                                 Sap_BA_Native = (10000*Sap_BA_Native)/(SapPlotNum * SapPlotSize),
                                                 Sap_BA_NatCan = (10000*Sap_BA_NatCan)/(SapPlotNum * SapPlotSize),
                                                 Sap_BA_Exotic = (10000*Sap_BA_Exotic)/(SapPlotNum * SapPlotSize)) %>% 
  select(Plot_Name:cycle, Sap_BA_Total, Sap_BA_Native, 
         Sap_BA_NatCan, Sap_BA_Exotic)

# Checking work
length(unique(plot_visit_df$Plot_Name)) #1515 
length(unique(live_sap_ba_final$Plot_Name)) #1515
nrow(plot_visit_df) #4464
nrow(live_sap_ba_final)#4464
table(complete.cases(live_sap_ba_final)) # 6F: b/c plots are missing regen data, which is correct.
head(live_sap_dens_spp)

sap_dens_stats <- live_sap_dens_spp %>% filter(!Species %in% "Total") %>% select(-cycle) %>% 
  mutate(park = substr(Plot_Name, 1, 4)) %>% 
  group_by(park, Canopy_Tree, Native, Species) %>% 
  summarize(dens = sum(Dens, na.rm = T),
            pres = ifelse(sum(dens > 0), 1, 0)) %>% filter(pres > 0) %>% 
  group_by(park, Canopy_Tree, Native) %>% summarize(numspp_sap = sum(pres > 0)) %>% 
  mutate(species_type = case_when(Canopy_Tree == 1 & Native == 1 ~ "NatCan",
                                  Canopy_Tree == 0 & Native == 1 ~ "NatSub",
                                  Canopy_Tree == 1 & Native == 0 ~ "Exotic", 
                                  Canopy_Tree == 0 & Native == 0 ~ "Exotic", 
                                  TRUE ~ "Unk")) %>% 
  group_by(park, species_type) %>% summarize(numspp_sap = sum(numspp_sap))

#---- Live seedling density by species group in stems/m2 ----
live_seed_dens_ermn <- rbind(sum_by_cycle(ermn, network_name = "ERMN", 
                                         group = 'seedlings', year_span = 2008:2011,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 1),
                            sum_by_cycle(ermn, network_name = "ERMN",  
                                         group = 'seedlings', year_span = 2012:2015,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 2),
                            sum_by_cycle(ermn, network_name = "ERMN", 
                                         group = 'seedlings', year_span = 2016:2019,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 3))

live_seed_dens_midn <- rbind(sum_by_cycle(midn, network_name = "MIDN", 
                                         group = 'seedlings', year_span = 2008:2011,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 1),
                            sum_by_cycle(midn, network_name = "MIDN", 
                                         group = 'seedlings', year_span = 2012:2015,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 2),
                            sum_by_cycle(midn, network_name = "MIDN", 
                                         group = 'seedlings', year_span = 2016:2019,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 3))

live_seed_dens_ncrn <- rbind(sum_by_cycle(ncrn, network_name = "NCRN", 
                                         group = 'seedlings', year_span = 2008:2011,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 1),
                            sum_by_cycle(ncrn, network_name = "NCRN", 
                                         group = 'seedlings', year_span = 2012:2015,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 2),
                            sum_by_cycle(ncrn, network_name = "NCRN", 
                                         group = 'seedlings', year_span = 2016:2019,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 3))

live_seed_dens_netn <- rbind(sum_by_cycle(netn, network_name = "NETN", 
                                         group = 'seedlings', year_span = 2008:2011,
                                         value = 'count', units = 'plot', 
                                         value_name = "Dens", cycle_name = 1),
                            sum_by_cycle(netn, network_name = "NETN", 
                                         group = 'seedlings', year_span = 2012:2015,
                                         value = 'count', units = 'plot', 
                                         value_name = "Dens", cycle_name = 2),
                            sum_by_cycle(netn, network_name = "NETN", 
                                         group = 'seedlings', year_span = 2016:2019,
                                         value = 'count', units = 'plot', 
                                         value_name = "Dens", cycle_name = 3))

live_seed_dens <- rbind(live_seed_dens_ermn, live_seed_dens_midn, live_seed_dens_ncrn, live_seed_dens_netn)

live_seed_dens_spp1 <- right_join(spp_list, live_seed_dens, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

live_seed_dens_spp <- live_seed_dens_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
  left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
            by = c("Plot_Name", "cycle"))

spp_list2 <- unique(data.frame(live_seed_dens_spp$Species, live_seed_dens_spp$Group))

sort(unique(live_seed_dens_spp$Group))

live_seed_dens_total <- live_seed_dens_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, Dens) %>% 
  rename(Seed_Dens_Total = Dens)

live_seed_dens_native <- live_seed_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Seed_Dens_Native = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_seed_dens_natcan <- live_seed_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Seed_Dens_NatCan = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_seed_dens_exotic <- live_seed_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Seed_Dens_Exotic = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_seed_dens_comb <- list(plot_visit_df, live_seed_dens_total, live_seed_dens_native, 
                           live_seed_dens_natcan, live_seed_dens_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

live_seed_dens_final <- live_seed_dens_comb %>% mutate(Seed_Dens_Total = Seed_Dens_Total/(SeedPlotNum * SeedPlotSize),
                                                       Seed_Dens_Native = Seed_Dens_Native/(SeedPlotNum * SeedPlotSize),
                                                       Seed_Dens_NatCan = Seed_Dens_NatCan/(SeedPlotNum * SeedPlotSize),
                                                       Seed_Dens_Exotic = Seed_Dens_Exotic/(SeedPlotNum * SeedPlotSize)) %>% 
  select(Plot_Name:cycle, Seed_Dens_Total, Seed_Dens_Native, 
         Seed_Dens_NatCan, Seed_Dens_Exotic)


# Checking work
length(unique(live_seed_dens_final$Plot_Name)) #1515
nrow(live_seed_dens_final) #4464
table(complete.cases(live_seed_dens_final)) #10 FALSE; correct
live_seed_dens_final[which(!complete.cases(live_seed_dens_final)), c("Plot_Name", "Year", "Seed_Dens_Total")]

names(plot_visit_df)


seed_dens_stats <- live_seed_dens_spp %>% filter(!Species %in% "Total") %>% select(-cycle) %>% 
  mutate(park = substr(Plot_Name, 1, 4)) %>% 
  group_by(park, Canopy_Tree, Native, Species) %>% 
  summarize(dens = sum(Dens, na.rm = T),
            pres = ifelse(sum(dens > 0), 1, 0)) %>% filter(pres > 0) %>% 
  group_by(park, Canopy_Tree, Native) %>% summarize(numspp_seed = sum(pres > 0)) %>% 
  mutate(species_type = case_when(Canopy_Tree == 1 & Native == 1 ~ "NatCan",
                                  Canopy_Tree == 0 & Native == 1 ~ "NatSub",
                                  Canopy_Tree == 1 & Native == 0 ~ "Exotic", 
                                  Canopy_Tree == 0 & Native == 0 ~ "Exotic", 
                                  TRUE ~ "Unk")) %>% 
  group_by(park, species_type) %>% summarize(numspp_seed = sum(numspp_seed))

#----- Combine spp stats tables -----
numspp_table <- reduce(list(tree_dens_stats, sap_dens_stats, seed_dens_stats), left_join, by = c("park", "species_type")) %>% 
  mutate(Species_Group = case_when(species_type == "NatCan" ~ "Native Canopy",
                                   species_type == "NatSub" ~ "Subcanopy",
                                   species_type == "Exotic" ~ "Exotic"),
         sort_order = case_when(species_type == "NatCan" ~ 1,
                                species_type == "NatSub" ~ 2,
                                TRUE ~ 3)) %>% 
  arrange(park, sort_order) %>% 
  select(park, Species_Group, numspp_tree, numspp_sap, numspp_seed) 

write.csv(numspp_table, "./results/supptables/Number_species_per_strata_group.csv", row.names = F)


#----- Categorical Predictors in regression analysis -----
reg_df <- read.csv("./data/regression_data_2016-2019.csv") %>% 
  select(Plot_Name, Park, hummod300m, DBI, Province = PROVINCE, Physiographic_Class, 
         cancov, tmax, precip, stage_fac, QMD, mDBH, Tree_Dens_Total,
         avg.cover)

dbi <- reg_df %>% group_by(Park, DBI) %>% summarize(num_plots = n()) %>% 
  pivot_wider(names_from = DBI, values_from = num_plots, names_prefix = "DBI_", values_fill = 0)

write.csv(dbi, "./results/supptables/Regress_DBI.csv", row.names = F)

strstg <- reg_df %>% group_by(Park, stage_fac) %>% summarize(num_plots = n()) %>% 
  pivot_wider(names_from = stage_fac, values_from = num_plots, values_fill = 0)

write.csv(strstg, "./results/supptables/Regress_Structural_Stage.csv", row.names = F)

physio <- reg_df %>% group_by(Park, Physiographic_Class) %>% summarize(num_plots = n()) %>% 
  pivot_wider(names_from = Physiographic_Class, values_from = num_plots, values_fill = 0)

write.csv(physio, "./results/supptables/Regress_Physiographic_Class.csv", row.names = F)

prov <- reg_df %>% group_by(Park, Province) %>% summarize(num_plots = n()) %>% 
  pivot_wider(names_from = Province, values_from = num_plots, values_fill = 0)

write.csv(prov, "./results/supptables/Regress_Ecological_Provinces_by_park.csv", row.names = F)

