rm(list=ls())

source("~/Abidjan/load_path.R", echo=FALSE)


##############################################################################################################################################################
# visualize shapefiles  
###############################################################################################################################################################
names(Abi_shapefile)

#note adm0 is map of Cote d'Ivoire

ggplot(data = Abi_shapefile[[2]])+  #Admin 1 - 14 
  geom_sf(color = "black", fill = "#e79a9a")+
  geom_text_repel(
    data = Abi_shapefile[[2]],
    aes(label =  NAME_1, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs(title="All 14 Districts of Cote d'Ivoire, 
       Abidjan and Yamoussoukro are autonomous", 
       fill = "", x = NULL, y = NULL)+
  map_theme()

ggsave(paste0(plots, "/", 'Jan_18_2024', "/", Sys.Date(), 
              '_districts_CoteDIvoire.pdf'), 
       width = 8, height =5)


ggplot(data = Abi_shapefile[[3]])+ #Admin 2 - 33 
  geom_sf(color = "black", fill = "#7edfc4")+
  geom_text_repel(
    data = Abi_shapefile[[3]],
    aes(label =  NAME_2, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs(title="All 33 regions of Cote d' Ivoire , 
       Abidjan and Yamoussoukro are autonomous", 
       fill = "", x = NULL, y = NULL)+
  map_theme()
ggsave(paste0(plots, "/", 'Jan_18_2024', "/", Sys.Date(), 
              '_regions_CoteDIvoire.pdf'),  width = 8, height =5)


ggplot(data = Abi_shapefile[[4]])+ #Admin 3 - 113 
  geom_sf(color = "black", fill = 	"#c08daa")+
  labs(title="All 113 departments of Cote d' Ivoire , 
       Abidjan and Yamoussoukro do not have departments", 
       fill = "", x = NULL, y = NULL)+
  map_theme()
ggsave(paste0(plots, "/", 'Jan_18_2024', "/", Sys.Date(), 
              '_departments_CoteDIvoire.pdf'),  width = 8, height =5)


ggplot(data = Abi_shapefile[[5]])+ #Admin 4 - 191
  geom_sf(color = "black", fill = 	"#b4ebe8")+
  labs(title="All 191 sub-prefectures of Cote d' Ivoire , 
       Abidjan and Yamoussoukro do not have sub-prefectures", 
       fill = "", x = NULL, y = NULL)+
  map_theme()
ggsave(paste0(plots, "/", 'Jan_18_2024', "/", Sys.Date(), 
              '_subprectures_CoteDIvoire.pdf'),  width = 8, height =5)


ggplot(data = Abi_shapefile[[6]])+ #facies - 11
  geom_sf(color = "black", fill = 	"#ece9f7")+
  geom_text_repel(
    data = Abi_shapefile[[6]],
    aes(label =  facies, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs(title="All 11 facies of Cote d' Ivoire", 
       fill = "", x = NULL, y = NULL)+
  map_theme()
ggsave(paste0(plots, "/", 'Jan_18_2024', "/", Sys.Date(), 
              '_facies_CoteDIvoire.pdf'),  width = 8, height =5)



ggplot(data = Abi_shapefile[[7]])+ #health_district - 113
  geom_sf(color = "black", fill = 	"#ece9f7")+
  # geom_text_repel(
  #   data = Abi_shapefile[[7]],
  #   aes(label =  NOM, geometry = geometry),color ='black',
  #   stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs(title="All 113 health districts of Cote d' Ivoire", 
       fill = "", x = NULL, y = NULL)+
  map_theme()
ggsave(paste0(plots, "/", 'Jan_18_2024', "/", Sys.Date(), 
              '_health_district113_CoteDIvoire.pdf'),  width = 8, height =5)

ggplot(data = Abi_shapefile[[8]])+ #health district coarse - 109
  geom_sf(color = "black", fill = 	"#ece9f7")+
  # geom_text_repel(
  #   data = Abi_shapefile[[7]],
  #   aes(label =  NOM, geometry = geometry),color ='black',
  #   stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs(title="All 109 health district coarse of Cote d' Ivoire", 
       fill = "", x = NULL, y = NULL)+
  map_theme()


ggsave(paste0(plots, "/", 'Jan_18_2024', "/", Sys.Date(), 
              '_health_district109_CoteDIvoire.pdf'),  width = 8, height =5)

ggplot(data = Abi_shapefile[[9]])+ #health regions - 33
  geom_sf(color = "black", fill = 	"#ffd6c2")+
  geom_text_repel(
    data = Abi_shapefile[[9]],
    aes(label =   str_to_sentence(health_region), geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs(title="All 33 health regions of Cote d' Ivoire", 
       fill = "", x = NULL, y = NULL)+
  map_theme()


ggsave(paste0(plots, "/", 'Jan_18_2024', "/", Sys.Date(), 
              '_health_regions33_CoteDIvoire.pdf'),  width = 8, height =5)


##############################################################################################################################################################
# obtain health districts shapefiles for Abidjan 
###############################################################################################################################################################

#get Abidjan health district from the first file  
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


ggsave(paste0(plots, "/", 'Jan_18_2024', "/", Sys.Date(), 
              '_health_districts15_Abidjan.pdf'),  width = 8, height =5)

#get Abidjan health district from the second file 
df_abidjan2 = st_intersection(Abi_shapefile[[8]], Abidjan)
ggplot(data = Abidjan)+
  geom_sf(data = df_abidjan2, color = "black", fill = 	"#ece9f7")+
  geom_text_repel(
    data = df_abidjan2,
    aes(label = str_to_sentence(NOM), geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs(title="All 13 health districts of Abidjan", 
       fill = "", x = NULL, y = NULL)+
  map_theme()

ggsave(paste0(plots, "/", 'Jan_18_2024', "/", Sys.Date(),
              '_health_districts13_Abidjan.pdf'),  width = 8, height =5)


##############################################################################################################################################################
# generate map of Abidjan sub-prefecture 
###############################################################################################################################################################

df_abidjan = st_intersection(Abi_shapefile[[5]], Abidjan)

ggplot(data = Abidjan)+
  geom_sf(data = df_abidjan, color = "black", fill = 	"#ece9f7")+
  geom_text_repel(
    data = df_abidjan,
    aes(label = NAME_4, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs(title="All 4 sub-prefectures of Abidjan", 
       fill = "", x = NULL, y = NULL)+
  map_theme()

ggsave(paste0(plots, "/", 'Jan_18_2024', "/", Sys.Date(),
              '_sub_prefectures_Abidjan.pdf'),  width = 6, height =4)


##############################################################################################################################################################
# nets distribution 
####################################################################################################################################################

net_df <- left_join(df_abidjan1, campign_dat, by = c("NOM" = "district_name")) %>%  filter(!is.na(LLINs_distributed)) %>% 
  mutate(name_excess = paste0(str_to_sentence(NOM),", ", excess_LLINs_prop_available ))

p <- ggplot(net_df, aes(x=fct_reorder(str_to_sentence(NOM), LLINs_distributed), y=LLINs_distributed, fill= Type_insecticide)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_manuscript()+
  scale_fill_manual(values= c( "#a17eac", "#ebadb2")) +
  theme(legend.title = element_blank())+
  labs(x ="", y = "Number of bets nets distributed in Abidjan in 2021 campaign (in thousands)")

p1 <- ggplot(net_df, aes(fill= excess_LLINs_prop_available)) +
  geom_sf()+
  scale_fill_gradient(low ="#ffecc6" , high = "#f73b3b") +
  geom_text_repel(
    data = net_df,
    aes(label = name_excess, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs( fill = "", x = NULL, y = "Left over nets after campaign as a percentage of estimated need")+
  map_theme()

p/p1 + plot_annotation(tag_levels = 'A')

ggsave(paste0(plots, "/", 'Jan_18_2024', "/", Sys.Date(), 
              '_nets_distributed_Left_over_2021_Abdijan.pdf'),  width = 8, height =7)

##############################################################################################################################################################
# routine data for Abidjan 
###############################################################################################################################################################
#tpr 
head(routine_dat)
glimpse(routine_dat)
table(routine_dat$health_region)
df <- routine_dat %>%  filter(grepl("ABIDJAN", health_region))

month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


ggplot(df, aes(x=month, y =tpr, color =health_district))+
  geom_point()+
  geom_line()+
  scale_color_brewer(palette = "Paired")+
  facet_wrap(~year)+
  scale_x_continuous(breaks = 1:12, labels = month_labels)+
  theme_manuscript()+
  theme(legend.position = "bottom")
  
ggsave(paste0(plots, "/", 'Jan_18_2024', "/", Sys.Date(), 
              '_TPR_health_districts_Abidjan.pdf'),  width = 11, height =7)


df2 <- df %>%  group_by(month, health_district) %>% 
  summarise(mean(tpr))

ggplot(df2, aes(x=month, y =`mean(tpr)`, color =health_district))+
  geom_point()+
  geom_line()+
  scale_color_brewer(palette = "Paired")+
  scale_x_continuous(breaks = 1:12, labels = month_labels)+
  theme_manuscript()+
  theme(legend.position = "bottom")

ggsave(paste0(plots, "/", 'Jan_18_2024', "/", Sys.Date(), 
              '_TPR_health_districts_Abidjan_all_years.pdf'),  width = 11, height =7)


df3 <- df %>%  
  filter(year == '2022')

ggplot(df3, aes(x=month, y =tpr, color =health_district))+
  geom_point()+
  geom_line()+
  scale_color_brewer(palette = "Paired")+
  theme_manuscript()+
  theme(legend.position = "bottom")
ggsave(paste0(plots, "/", 'Jan_18_2024', "/", Sys.Date(), 
              '_TPR_health_districts_Abidjan_2022.pdf'),  width = 11, height =7)

#####summary tpr

average_tpr <- df %>%
  group_by(health_district) %>%
  summarise(avg_tpr = mean(tpr, na.rm = TRUE))

tpr_regions <- df_abidjan1[1:(nrow(df_abidjan1) - 5), ]
tpr_plottingdata <- inner_join(average_tpr, tpr_regions, by= c("health_district" = "NOM"))

ggplot()+
  geom_sf(data = tpr_plottingdata, aes(geometry =geometry, fill =avg_tpr))+
  geom_text_repel(data =tpr_plottingdata, 
                  aes(label=str_to_sentence(health_district), geometry=geometry), color ='black',
                  stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  scale_fill_continuous(name = "TPR", low = "lightyellow", high = "maroon")+
  labs(title = "Average Test Positivity Rate (2017-2022) ",
       fill = "TPR", x = "", y= "")+
  map_theme()

#Abidjan_var <- read.csv(file.path(AbidjanDir, "Abidjan Data Variables.csv"))
#joined_df <- left_join(Abidjan_var, average_tpr, by = c("HealthDistrict" = "health_district")) #, all.x = TRUE)
#write.csv(joined_df, file.path(AbidjanDir, "Abidjan Data Variables.csv"), row.names = FALSE)
  

#incidence
check <- df %>%  
  dplyr::select(month, incidence_adjusted3, year, health_district) %>% 
  filter(month == 1)

ggplot(check, aes(x=year, y =incidence_adjusted3, color =health_district))+
  geom_point()+
  geom_line()+
  scale_color_brewer(palette = "Paired")+
  theme_manuscript()+
  theme(legend.position = "bottom")

ggsave(paste0(plots, "/", 'Jan_18_2024', "/", Sys.Date(), 
              '_incidence_adjusted_Abidjan_2022.pdf'),  width = 11, height =7)
