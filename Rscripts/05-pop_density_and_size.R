#population size and density maps for Abidjan

source("load_path.R", echo=FALSE)



raster <- raster(file.path(AbidjanDir, "pop_density","gpw_v4_population_density_rev11_2020_30_sec.tif"))

pop_den <- raster::extract(raster, df_abidjan1, buffer = buffer, fun = mean, df =TRUE)

df_popd = cbind(df_abidjan1, pop_den)


p1=ggplot(df_popd) +
  geom_sf(aes(fill = gpw_v4_population_density_rev11_2020_30_sec))+
  scale_fill_gradient(low = "#eed6f1", high = "#ba5dc8", na.value = NA) +
  map_theme() + 
  labs(title= "Health district's population denisty (Abidjan)",fill = "people/km²" )+
  xlab("")+
  ylab("")

ggsave(paste0(plots, "/", Sys.Date(), 
              '_Abidjan_wards_colored_by_density.pdf'),
       p1, width = 10, height =8)



##############################################################################################################################################################
#Population sizes from the microplan
#######################################################################################################

pop_sizes <-read_csv(file.path(AbidjanDir, "program_data", "microplan_abidjan_brief.csv")) %>% 
  dplyr::select(district_name, total_population_enumerated)

#link to ward file 
pop_shp <- left_join(df_abidjan1, pop_sizes, by =c("NOM" = "district_name"))


ggplot(pop_shp) +
  geom_sf(aes(fill = total_population_enumerated))+
  scale_fill_gradient(low = "#ffcde9", high = "#fe0492", na.value = NA) +
  ggrepel::geom_text_repel( data = pop_shp,aes(label =  NOM, geometry = geometry) ,color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme() + 
  labs(title= "Total population enumerated (2023 microplan)", fill = "")+
  xlab("")+
  ylab("")

ggsave(paste0(plots,"/", Sys.Date(), '_Abidjan_ward_pop_sizes_microplan.pdf'), width = 6, height = 4)

