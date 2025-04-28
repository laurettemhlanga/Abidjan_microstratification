abidjan_footprint <- sf::st_read(file.path(AbidjanDir, "/Building Height/abidjan_height.shp"))

abidjan_hieght_ext <- abidjan_footprint %>% 
  dplyr::select(Id, area_mn
                # , 
                # latitud,   longitd
                )

sf::st_write(abidjan_hieght_ext, file.path(AbidjanDir, "/Building Height/abidjan_height_ext.shp"))


