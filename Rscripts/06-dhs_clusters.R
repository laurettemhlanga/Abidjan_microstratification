rm(list=ls())

source("load_path.R", echo=FALSE) 

#read files function 
read.files <- function(path, general_pattern, specific_pattern, fun) {
  files <- list.files(path = path , pattern = general_pattern, full.names = TRUE, recursive = TRUE)
  files<- files[(grep(specific_pattern, files))]
  sapply(files, fun, simplify = F)
}


options(survey.lonely.psu="adjust") 

# Read in PR and KR data files and rename state variable
pr_files <- read.files(DHS_data, "*CIPR.*\\.DTA", 'CIPR62FL|CIPR81FL', read_dta)
hr_files <- read.files(DHS_data, "*CIHR.*\\.DTA", 'CIHR62FL|CIHR81FL', read_dta)


pr_files[[1]]$region <- as_label(pr_files[[1]]$hv024)
pr_files[[2]]$region <- as_label(pr_files[[2]]$hv024)
hr_files[[1]]$region <- as_label(hr_files[[1]]$hv024)
hr_files[[2]]$region <- as_label(hr_files[[2]]$hv024)

pr_data <- bind_rows(pr_files)
hr_data <- bind_rows(hr_files)


lapply(pr_files, function(x) table(x$hml32))
lapply(pr_files, function(x) table(x$hv006))


# kr_files[[5]]$state <- as_label(kr_files[[5]]$sstate)
# kr_files[[6]]$state <- as_label(kr_files[[6]]$sstate)
# kr_files[[7]]$state <- as_label(kr_files[[7]]$v024)
# kr_data <- bind_rows(kr_files)


#load spatial points
sf12 = sf::st_read(file.path(DHS_data, "2012_CI", "CIGE61FL", "CIGE61FL.shp"),)

sf21 = sf::st_read(file.path(DHS_data, "2021_CI", "CIGE81FL", "CIGE81FL.shp"),) 


sf_all = rbind(sf12, sf21) %>%  
  rename(cluster = DHSCLUST)

##############################################################################################################################################################
# compute malaria prevalence in Cote d'Ivoire  at regional level 
###############################################################################################################################################################
malaria_prev <- pr_data %>%
  filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) ) %>%
  filter(hml32 <= 1) %>% 
  mutate(malaria = ifelse(hml32 == 1, 1, 0), wt = hv005/1000000, id  = hv021, strat=hv022) %>% 
  srvyr:: as_survey_design(., ids= id,strata=strat,nest=T,weights= wt) %>%
  group_by(region, year = hv007) %>% 
  summarize( prev =round(survey_mean(malaria),2) * 100,
             total_malaria = survey_total(), first_month_survey = as.character(first(hv006))) %>%
  mutate(class= cut(prev,  c(seq(0, 20, 5),30,50, 100), include.lowest = T)) 

##############################################################################################################################################################
# compute malaria prevalence in COte d'Ivoire  at cluster level 
###############################################################################################################################################################

malaria_prev <- pr_data %>%
  filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) ) %>%
  filter(hml32 <= 1) %>% 
  mutate(malaria = ifelse(hml32 == 1, 1, 0), wt = hv005/1000000, id  = hv021, strat=hv022) %>% 
  srvyr:: as_survey_design(., ids= id,strata=strat,nest=T,weights= wt) %>%
  group_by(cluster = hv001, year = hv007) %>% 
  summarize( prev =round(survey_mean(malaria),2) * 100,
             total_malaria = survey_total(), first_month_survey = as.character(first(hv006))) %>%
  mutate(class= cut(prev,  c(seq(0, 20, 5),30,50, 100), include.lowest = T)) %>% 
  inner_join(sf_all, by = c("year" = "DHSYEAR","cluster" = "cluster" )) %>%
  drop_na(prev)


##############################################################################################################################################################
# obtain health districts shapefiles for Abidjan 
###############################################################################################################################################################
Abidjan = Abi_shapefile[[3]] %>% filter(NAME_1 == "Abidjan")
df_abidjan1 = st_intersection(Abi_shapefile[[7]], Abidjan)
ggplot(data = Abidjan)+
  geom_sf(data = df_abidjan1, color = "black", fill = 	"#ece9f7")+
  geom_text_repel(
    data = df_abidjan1,
    aes(label = str_to_sentence(NOM), geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs(title="All 15 health districts of Abidjan", 
       fill = "", x = NULL, y = NULL)+
  map_theme()


#######################################################################################
#plot prevalence data on Abidjan file 
##########################################################################################

new_plottingdata <- sf::st_as_sf(malaria_prev, coords = c("LONGNUM", "LATNUM"))

sf::st_crs(new_plottingdata) <- 4326
sf::st_crs(df_abidjan1) <- 4326

sf::st_crs(new_plottingdata) <-  sf::st_crs(df_abidjan1)

new_plotd <- st_intersection(df_abidjan1, new_plottingdata)  



discrete_palettes <- list(rev(RColorBrewer::brewer.pal(7, "RdYlBu")))

ggplot(data = df_abidjan1) +
  geom_sf(fill = "white", color = "black") +
  geom_sf(data = new_plotd, aes(color = class, geometry =geometry, shape=first_month_survey), size = 3)+
  geom_text_repel(
    data = df_abidjan1,
    aes(label = str_to_sentence(NOM), geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs(subtitle = '', fill = "TPR ", x = "", y = "", color = "") +
  facet_grid(col = vars(year))+
  scale_fill_discrete(drop=FALSE, name="TPR", type = discrete_palettes)+
  scale_color_discrete(drop=FALSE, name="TPR", type = discrete_palettes)+
  map_theme()


ggsave(file.path(plots,"DHS", "Abidjan_map_points_shape_month_survey.pdf"),
       width = 12, height = 9, dpi = 300)


ggplot(data = df_abidjan1) +
  geom_sf(fill = "white", color = "black") +
  geom_sf(data = new_plotd, aes(fill = class, geometry =geometry, size=total_malaria),  shape=21)+
  geom_text_repel(
    data = df_abidjan1,
    aes(label = str_to_sentence(NOM), geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs(subtitle = '', fill = "TPR ", x = "", y = "", color = "") +
  facet_grid(col = vars(year))+
  scale_fill_discrete(drop=FALSE, name="TPR", type = discrete_palettes)+
  scale_color_discrete(drop=FALSE, name="TPR", type = discrete_palettes)+
  map_theme()


ggsave(file.path(plots,"DHS", "Abidjan_map_points_size_total_sampled.pdf"),
       width = 12, height = 9, dpi = 300)


ggplot(data = df_abidjan1) +
  geom_sf(fill = "white", color = "black") +
  geom_sf(data = new_plotd, aes(fill = class, geometry =geometry), size=3, shape=21)+
  geom_text_repel(
    data = df_abidjan1,
    aes(label = str_to_sentence(NOM), geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs(subtitle = '', fill = "TPR ", x = "", y = "", color = "") +
  facet_grid(col = vars(year))+
  scale_fill_discrete(drop=FALSE, name="TPR", type = discrete_palettes)+
  scale_color_discrete(drop=FALSE, name="TPR", type = discrete_palettes)+
  map_theme()

ggsave(file.path(plots,"DHS", "Abidjan_dhs_clusters_regular.pdf"),
       width = 12, height = 9, dpi = 300)


#######################################################################################
#estimate net use and plot 
##########################################################################################

malaria_itn <- pr_data %>%
  filter(hv103 == 1) %>% 
  mutate(net_use = ifelse(hml12 %in% c(1) ,1, 0), wt = hv005/1000000, id  = hv021, strat=hv022) %>% 
  srvyr:: as_survey_design(., ids= id,strata=strat,nest=T,weights= wt) %>%
  group_by(cluster = hv001, year = hv007) %>% 
  summarize(net_use_new =round(survey_mean(net_use),2) * 100,
             total_pop_net_use = survey_total(), first_month_survey = as.character(first(hv006))) %>%
  mutate(class= cut(net_use_new,   c(seq(0, 20, 5),30,50, 60, 70, 80, 90, 100), include.lowest = T)) %>% 
  inner_join(sf_all, by = c("year" = "DHSYEAR","cluster" = "cluster" )) %>%
  drop_na(net_use_new)


new_plottingdata <- sf::st_as_sf(malaria_itn, coords = c("LONGNUM", "LATNUM"))

sf::st_crs(new_plottingdata) <- 4326
sf::st_crs(df_abidjan1) <- 4326

sf::st_crs(new_plottingdata) <-  sf::st_crs(df_abidjan1)

new_plot_use <- st_intersection(df_abidjan1, new_plottingdata)  



discrete_palettes <- list(rev(RColorBrewer::brewer.pal(11, "RdYlBu")))

ggplot(data = df_abidjan1) +
  geom_sf(fill = "white", color = "black") +
  geom_sf(data = new_plot_use, aes(fill = class, geometry =geometry), size = 3, shape=21)+
  geom_text_repel(
    data = df_abidjan1,
    aes(label = str_to_sentence(NOM), geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  facet_grid(col = vars(year))+
  scale_fill_discrete(drop=FALSE, name="Net use", type = discrete_palettes)+
  scale_color_discrete(drop=FALSE, name="Net use", type = discrete_palettes)+
  map_theme()

ggsave(file.path(plots,"DHS", "Abidjan_dhs_clusters_net_use.pdf"),
       width = 12, height = 9, dpi = 300)

#######################################################################################
#estimate net access and plot 
##########################################################################################

malaria_itn_access <- hr_data %>%
  mutate(potuse = 2 * rowSums(dplyr::select(., contains('hml10')),na.rm=T),
         slept_night = rowSums(dplyr::select(., contains('hv103')), na.rm=T),
         potuse2 = ifelse(potuse/slept_night > 1, slept_night, potuse),
         access = potuse2/slept_night,
    wt = hv005/1000000, id  = hv021, strat=hv022) %>% 
  drop_na(access) %>% 
  srvyr:: as_survey_design(., ids= id,strata=strat,nest=T,weights= wt) %>%
  group_by(cluster = hv001, year = hv007) %>% 
  summarize(net_access =round(survey_mean(access),2) * 100,
            total_pop_net_access = survey_total(), first_month_survey = as.character(first(hv006))) %>%
  mutate(class= cut(net_access,  c(seq(0, 20, 5),30,50, 60, 70, 80, 90, 100), include.lowest = T)) %>% 
  inner_join(sf_all, by = c("year" = "DHSYEAR","cluster" = "cluster" )) %>%
  drop_na(net_access)

new_plottingdata <- sf::st_as_sf(malaria_itn_access, coords = c("LONGNUM", "LATNUM"))

sf::st_crs(new_plottingdata) <- 4326
sf::st_crs(df_abidjan1) <- 4326

sf::st_crs(new_plottingdata) <-  sf::st_crs(df_abidjan1)

new_plot_access <- st_intersection(df_abidjan1, new_plottingdata)  



discrete_palettes <- list(rev(RColorBrewer::brewer.pal(11, "RdYlBu")))

ggplot(data = df_abidjan1) +
  geom_sf(fill = "white", color = "black") +
  geom_sf(data = new_plot_access, aes(fill = class, geometry =geometry), size = 3, shape=21)+
  geom_text_repel(
    data = df_abidjan1,
    aes(label = str_to_sentence(NOM), geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  facet_grid(col = vars(year))+
  scale_fill_discrete(drop=FALSE, name="Net access", type = discrete_palettes)+
  scale_color_discrete(drop=FALSE, name="Net access", type = discrete_palettes)+
  map_theme()

ggsave(file.path(plots,"DHS", "Abidjan_dhs_clusters_net_access.pdf"),
       width = 12, height = 9, dpi = 300)


#######################################################################################
#reason net was not used 
##########################################################################################
kept_points <- st_intersection(sf21, df_abidjan1)


malaria_itn_not_used <- hr_data %>%
  filter(hv007 == 2021) %>% 
  inner_join(kept_points, by = c("hv007" = "DHSYEAR","hv001" = "DHSCLUST")) %>% 
  group_by(hml24_1) %>%  summarise(Net_not_used_reason =n()) %>% ungroup %>% drop_na() %>%  
  mutate(percent = Net_not_used_reason/sum(Net_not_used_reason)* 100) %>% filter(percent > 10)

malaria_itn_not_used$hml24_1 <- as.character(malaria_itn_not_used$hml24_1)

ggplot(malaria_itn_not_used, aes(y=percent, x=hml24_1))+ 
  geom_bar(position = "stack", stat ="identity", width = 0.5, fill = "#33cc99") +
  theme_manuscript()+ 
  scale_x_discrete(labels=c("Too hot", "Don't like smell", "Unable to hang net", "No Mosquitoes/no malaria", "Extra net/saving for later"))+ 
  coord_flip()+ 
  labs(y = "Percent", x = "Reasons for not sleeping under a net")
  
ggsave(file.path(plots,"DHS", "reasons_for_not_using_nets_DHS.pdf"),
       width = 12, height = 9, dpi = 300)

  
#######################################################################################
#net use given access 
#########################################################################################
st_geometry(new_plot_access) <- NULL
new_plot_access2 <- new_plot_access %>%  dplyr::select(year, cluster, net_access)
netU_access <- left_join(new_plot_use, new_plot_access2, by = c("year" = "year","cluster" = "cluster" ))
netU_access$netU_access = netU_access$net_use_new/netU_access$net_access * 100
netU_access$netU_access2 <- ifelse(netU_access$netU_access > 100, 100, netU_access$netU_access) 

netU_access2 <- netU_access%>% 
  mutate(class= cut(netU_access2,  c(seq(0, 20, 5),30,50, 60, 70, 80, 90, 100), include.lowest = T)) 


discrete_palettes <- list(rev(RColorBrewer::brewer.pal(11, "RdYlBu")))
ggplot(data = df_abidjan1) +
  geom_sf(fill = "white", color = "black") +
  geom_sf(data = netU_access2, aes(fill = class, geometry =geometry), size = 3, shape=21)+
  geom_text_repel(
    data = df_abidjan1,
    aes(label = str_to_sentence(NOM), geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  facet_grid(col = vars(year))+
  scale_fill_discrete(drop=FALSE, name="Net use given access", type = discrete_palettes)+
  scale_color_discrete(drop=FALSE, name="Net use given access", type = discrete_palettes)+
  map_theme()

ggsave(file.path(plots,"DHS", "net_use_given_access.pdf"),
       width = 12, height = 9, dpi = 300)
