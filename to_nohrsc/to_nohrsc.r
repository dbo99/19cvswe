

{
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("libs.r")


off_fcast_pnt <- read_csv("off_fcast_pnt.csv")
basin_descripts <- read_csv("BasinDescriptions.csv")


## zones ##

zone_min_elevs <- read_csv("zone_mins.csv")
zone_max_elevs <- read_csv("zone_maxes.csv")
zones <- readOGR(".", "zones") %>% st_as_sf() %>% as.data.frame() %>% select(-geometry)
zones <- right_join(zones, zone_min_elevs)
zones <- right_join(zones, zone_max_elevs)

zones <- zones %>% mutate(ztype = str_sub(Name, start= -2))
zones <- zones %>% group_by(Basin) %>% mutate(num_zon = n()) %>% ungroup()
zones <- zones %>% mutate(num_zon = ifelse(ztype == "OF", 0, num_zon))
zoneelev_bounds <- zones %>% transmute(Name, MinElev_m, MaxElev_m, ztype)
zoneelev_bounds_of <- zoneelev_bounds %>% filter(ztype == "OF") %>% 
                      transmute(OF = Name, MinElev_m_of = MinElev_m)
zoneelev_bounds_lf <- zoneelev_bounds %>% filter(ztype == "LF") %>% 
                      transmute(LF = Name, MinElev_m_lf = MinElev_m)
zoneelev_bounds_mf <- zoneelev_bounds %>% filter(ztype == "MF") %>% 
                      transmute(MF = Name, MinElev_m_mf = MinElev_m)
zoneelev_bounds_uf <- zoneelev_bounds %>% filter(ztype == "UF") %>% 
                      transmute(UF = Name, MinElev_m_uf = MinElev_m)


tail(zones)
nozones <- zones %>% group_by(Basin) %>% summarize(num_zon = n()) %>% ungroup()


## basins ##

basin_min_elevs <- read_csv("basin_mins.csv")
basins <- readOGR(".", "basins_wgs84") %>% st_as_sf() #%>% as.data.frame() %>% select(-geometry)

basins <- right_join(basins, nozones)

zones_spread <- zones %>% spread(ztype, Name) %>% transmute(Basin, OF, LF, MF, UF)
head(zones_spread)
zones_spread <- zones_spread %>% 
  group_by(Basin) %>% 
  summarize_all(funs(trimws(paste(., collapse = ''))))

zones_spread$OF <- gsub("NA", "", zones_spread$OF) #total hack - make sure no basins have in them "NA"
zones_spread$LF <- gsub("NA", "", zones_spread$LF)
zones_spread$MF <- gsub("NA", "", zones_spread$MF)
zones_spread$UF <- gsub("NA", "", zones_spread$UF)

#basins$lf <-trimws(basins$lf)
#basins$lf <-trimws(basins$lf)
#basins$lf <-trimws(basins$lf)
#write_csv(zones_spread, "cnrfc_basins&zones.csv")

basins <- left_join(   basins, zones_spread)
basins <- basins %>% mutate(OF = trimws(OF))
basins <- basins %>% mutate(LF = trimws(LF))
basins <- basins %>% mutate(MF = trimws(MF))
basins <- basins %>% mutate(UF = trimws(UF))



head(basins)

basins <- left_join(basins, zoneelev_bounds_of)
basins <- left_join(basins, zoneelev_bounds_lf)
basins <- left_join(basins, zoneelev_bounds_mf)
basins <- left_join(basins, zoneelev_bounds_uf)


basins <- left_join(basins, off_fcast_pnt)
basins <- basins %>% mutate(TYPE = ifelse(TYPE == "fcast", "f", TYPE))
basins <- basins %>% mutate(TYPE = ifelse(is.na(TYPE), "d", TYPE))


basins <- basins %>% mutate(num_zon = ifelse(num_zon == 1, 0, num_zon))


basins <- basins %>% mutate(ZONE_1_MIN_ELEV = MinElev_m_lf,
                            ZONE_2_MIN_ELEV = ifelse(is.na(MinElev_m_mf), 
                                              MinElev_m_uf, MinElev_m_mf))


basins <- basins %>%  mutate(ZONE_3_MIN_ELEV = 
                      ifelse(ZONE_1_MIN_ELEV < 1e10 & 
                             ZONE_2_MIN_ELEV < 1e10  &
                             MinElev_m_mf < 1e10, MinElev_m_uf, NA))  

basins <- left_join(basins, basin_descripts)
  
basins <- basins %>% mutate(ZONE_1_SHEF = ifelse(nchar(LF) == 8, LF, NA))

basins <- basins %>% mutate(ZONE_2_SHEF = ifelse(nchar(ZONE_1_SHEF) == 8 &
                                                nchar(MF) == 8, MF, UF))

basins <- basins %>% mutate(ZONE_3_SHEF = ifelse(nchar(ZONE_1_SHEF) == 8 &
                                                  nchar(ZONE_2_SHEF)== 8 &
                                                   num_zon == 3,
                                                 UF, NA))
                         


basins <- basins %>% transmute(NAME = Basin, RFC, HUC, CWA, FCST_GP, CH5_ID = Basin,
                               TYPE, DESCRIPTION, ATTRIBUTE = NA, NUMBER_ZONES = num_zon,
                               ZONE_1_MIN_ELEV, ZONE_2_MIN_ELEV, ZONE_3_MIN_ELEV,
                               ZONE_1_SHEF , ZONE_2_SHEF, ZONE_3_SHEF#,
                               #OF, LF, MF, UF
                               )


}

st_write(basins, "out4.shp")

basin_summ <- basins %>% as.data.frame() %>% select(-geometry)
write_csv(basin_summ, "basins_to_nohrsc.csv")

zones_summ
#basins_shp_to_join <- readOGR(".", "basins") %>% st_as_sf()



#basins <- basins %>% transmute(NAME = Basin, RFC, HUC, CWA, FCAST_GP, CH5_ID = Basin,
#                               TYPE, 
#                               #DESCRIPTION,
#                               #ATTRIBUTE,
#                               NUMBER_ZONES = num_zon,
#                               ZONE_1_MIN(ELEV) = MinElev_m_)

#basins <- basins %>% mutate(num_zon = ifelse(is.na(OF), num_zon, 0))

#basins <- right_join(basins, basin_min_elevs) 
#head(basins)
#basins <- basins %>%
#           mutate(ZONE_1_MIN = ifelse(nchar(LF) == 8, MinElev_m, NA))
#
#basins$FCST_GP <- trimws(basins$FCST_GP)
#basins <- basins %>% mutate(ncharLF = nchar(LF))
#basins <- basins %>%
#  mutate(ZONE_2_MIN = ifelse(FCST_GP == "EastSierra" && nchar(LF) == 8, 2438, NA))
#
#unique(basins$FCST_GP)

#zones_spdf <- zones
#as(zones_spdf, "Spatial")
#writeOGR(zones, td, "R_int_zones", driver = "ESRI Shapefile") 



#zones <- zones %>% mutate(ztype_gp = ifelse()
#
#tail(zones)
#unique(zones$FCST_GP)
#
##zones <- zones %>% mutate(Zone_1_MIN = ifelse(ztype == "LF" | ztype == "OF", MinElev_m, NA)) 
##zones <- zones %>% mutate(Zone_2_MIN = ifelse(ztype == "LF" | ztype == "OF", 1524, NA)) 
#
#
#tail(zones)
#
#zones <- zones %>% mutate(Zone_2_MIN = ifelse(
#        FCAST_GP == SanJoaquin |  FCAST_GP == Tulare  | FCAST_GP == N_SanJoaquin &&
#          ztype == 
#                
#))
#
#
#
#nwsid <- read_csv("usgs_to_nws_key.csv")
