
rm(list = ls())
{
  
  library(rgdal)
  library(tidyverse)
  library(sp)
  library(mapdata)
  library(cowplot)
  # library(nhdR)
  # library(nhdplusTools)
  library(ggmap)
  library(leaflet)
  library(dataRetrieval)
  library(mapview)
 # library(lwgeom)
  library(sf)
}


cvsnowbasins <- read_csv("CNRFC_melt_basinIDs.csv")
swevol <- read_csv("snowv_test.3.26.19.csv") %>% mutate(swe_kaf = round(swe_vol/1000,0) ) %>%
  mutate(zone = str_sub(basin_8,start = -2)) %>% 
  mutate(zone = ifelse (zone == "TF" , "total", ifelse(zone == "UF", "upper", 
                                                       ifelse(zone == "OF", "single", "lower")))) %>%  mutate(basin_8 = basin_8) %>%
  mutate(basin = ifelse (zone == "total",  str_sub(basin_8, 1,5), basin_8))  %>%
  mutate(Name = basin)
#lmpdswevol <- basins %>% filter(zone == "total") 

df <- inner_join(cvsnowbasins , swevol, by = "basin_8") %>% mutate(Name = basin)


spdf_5 <- readOGR("basins.kml", "cnrfc_09122018_basins_thin")
spdf_8 <- readOGR(".","cnrfc_zones_wgs84aux")

#pnts1 <- readOGR("riverFcast.kml", "![CDATA[River Guidance <br><a href="http://www.cnrfc.noaa.gov/rfc_guidance.php">CNRFC River Forecast Web Page</a>]]")

# Convert spatialpolydf to an sf object
sf_5 <- spdf_5 %>%  st_as_sf() %>% transmute(Name, geometry)  %>%
  # Join the data
  left_join(df, by = c("Name")) %>% 
  # get area 
  mutate(area_m = st_area(geometry), area_a = as.numeric(area_m*0.000247105), area_mi = round(area_a/640, 0),
         swe_ft = round(swe_kaf*1000 / area_a , 2))  %>%
  mutate(swe_ft = ifelse(swe_ft == Inf, 0, swe_ft))
#sf_5nona <- sf_5 #%>% na.omit()

sf_8 <- spdf_8 %>% st_as_sf() %>% transmute(Name, geometry)  %>%
  # Join the data
  left_join(df, by = c("Name")) %>%
  # get area 
  mutate(area_m = st_area(geometry), area_a = as.numeric(area_m*0.000247105), area_mi = round(area_a/640,0),
         swe_ft = round(swe_kaf*1000 / area_a , 2)) %>% 
  mutate(swe_ft = ifelse(swe_ft == Inf, 0, swe_ft))  %>% filter(!zone == "total")




maptypes = c("Stamen.TonerLite", "Stamen.Terrain", "Stamen.TopOSMRelief", "Esri.WorldTopoMap" , "Esri.WorldPhysical",  "OpenTopoMap" ,
              "NASAGIBS.ModisTerraSnowCover", "NASAGIBS.ModisTerraTrueColorCR", "NASAGIBS.ModisTerraBands367CR")

grp <- c(    "usgs hydrography",   "0.5 reflectivity","hrrr p_1hr", "hrrr p_2hr",   "hrrr p_4hr", "hrrr p_6hr",
         "mrms p_1hr", "mrms p_24hr", "mrms p_48hr", "mrms p_72hr") # "Coarse Geo") # 

{
m <- mapview(sf_5["swe_ft"], burst = TRUE, hide = TRUE, col.regions = viridisLite::viridis, alpha.regions = 0.4,  map.types = maptypes,
               popup = popupTable(sf_5, zcol = c("Name", "river", "swe_ft", "swe_kaf", "area_mi")),
               layer.name = "sierra_swe_ft")   +
    
mapview(sf_8["swe_ft"], col.regions = viridisLite::viridis, alpha.regions = 0.4, map.types = maptypes,
            popup = popupTable(sf_8, zcol = c("Name", "river", "swe_ft", "swe_kaf", "zone", "area_mi")),
            layer.name = "sierra_swe_ft_zone",   hide = TRUE)  +
 
mapview(sf_5["swe_kaf"], col.regions = viridisLite::viridis, alpha.regions = 0.4, map.types = maptypes,
          popup = popupTable(sf_5, zcol = c("Name", "river", "swe_ft", "swe_kaf", "area_mi")),
          layer.name = "sierra_swe_kaf",   hide = TRUE) +
  
mapview(sf_8["swe_kaf"], col.regions = viridisLite::viridis, alpha.regions = 0.4,map.types = maptypes,
          popup = popupTable(sf_8, zcol = c("Name", "river", "swe_ft", "swe_kaf",  "zone", "area_mi")),
          layer.name = "sierra_swe_kaf_zone", hide = TRUE) 

  
  m@map = m@map %>% 
    
    addTiles() %>%
    setView(-119.6, 38.05, zoom = 6.4) %>%   
    
    addWMSTiles(group= grp[1], baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer", layers = "0",
                options = WMSTileOptions(format = "image/png", transparent = TRUE), attribution = att) %>%
    
    #addWMSTiles(group= grp[2], baseUrl="https://geoplatform1.epa.gov/arcgis/services/NEF/WetnessIndex/MapServer/WMSServer?", layers = "0",
    #         options = WMSTileOptions(format = "image/png", transparent = TRUE), attribution = "EPA") %>%
    
    addWMSTiles( group = grp[2],baseUrl = 
                   "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi", 
                 #"https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?",
                 layers = "nexrad-n0r-900913",
                 options = WMSTileOptions(format = "image/png", transparent = TRUE),
                 attribution = "Weather data © 2012 IEM Nexrad") %>%
    
    addWMSTiles( group = grp[3],baseUrl = 
                   
                   "https://mesonet.agron.iastate.edu/cgi-bin/wms/hrrr/refd.cgi?",
                 layers = "refd_0060", 
                 options = WMSTileOptions(format = "image/png", transparent = TRUE),
                 attribution = "Iowa State University") %>%
    
    addWMSTiles( group = grp[4],baseUrl = 
                   
                   "https://mesonet.agron.iastate.edu/cgi-bin/wms/hrrr/refd.cgi?",
                 layers = "refd_0120", 
                 options = WMSTileOptions(format = "image/png", transparent = TRUE),
                 attribution = "Iowa State University") %>%
    
    addWMSTiles( group = grp[5],baseUrl = 
                   
                   "https://mesonet.agron.iastate.edu/cgi-bin/wms/hrrr/refd.cgi?",
                 layers = "refd_0240", 
                 options = WMSTileOptions(format = "image/png", transparent = TRUE),
                 attribution = "Iowa State University") %>%
    
    addWMSTiles( group = grp[6],baseUrl = 
                   
                   "https://mesonet.agron.iastate.edu/cgi-bin/wms/hrrr/refd.cgi?",
                 layers = "refd_0360", 
                 options = WMSTileOptions(format = "image/png", transparent = TRUE),
                 attribution = "Iowa State University") %>%
    
    
    addWMSTiles( group = grp[7],baseUrl = 
                   
                   "https://mesonet.agron.iastate.edu/cgi-bin/wms/us/mrms_nn.cgi?",
                 layers = "mrms_p1h", 
                 options = WMSTileOptions(format = "image/png", transparent = TRUE),
                 attribution = "Iowa State University") %>%
    
    addWMSTiles( group = grp[8],baseUrl = 
                   # "http://idpgis.ncep.noaa.gov/arcgis/services/NWS_Forecasts_Guidance_Warnings/rfc_hourly_qpe/MapServer/WmsServer?",
                   "https://mesonet.agron.iastate.edu/cgi-bin/wms/us/mrms_nn.cgi?",
                 layers = "mrms_p24h", 
                 options = WMSTileOptions(format = "image/png", transparent = TRUE),
                 attribution = "Iowa State University") %>%
    
    addWMSTiles( group = grp[9],baseUrl = 
                   # "http://idpgis.ncep.noaa.gov/arcgis/services/NWS_Forecasts_Guidance_Warnings/rfc_hourly_qpe/MapServer/WmsServer?",
                   "https://mesonet.agron.iastate.edu/cgi-bin/wms/us/mrms_nn.cgi?",
                 layers = "mrms_p48h", 
                 options = WMSTileOptions(format = "image/png", transparent = TRUE),
                 attribution = "Iowa State University") %>%
    
    addWMSTiles( group = grp[10],baseUrl = 
                   # "http://idpgis.ncep.noaa.gov/arcgis/services/NWS_Forecasts_Guidance_Warnings/rfc_hourly_qpe/MapServer/WmsServer?",
                  "https://mesonet.agron.iastate.edu/cgi-bin/wms/us/mrms_nn.cgi?",
                # "http://idpgis.ncep.noaa.gov/arcgis/services/NWS_Climate_Outlooks/cpc_cmorph_dly_025deg/MapServer/WmsServer",
               # "http://mrdata.usgs.gov/services/ca?request=getcapabilities&service=WMS&version=1.1.1&",
               layers = "mrms_p72h", 
                #layers = "NWS_Climate_Outlooks_cpc_cmorph_dly_025deg", 
               #layers = 0,
                 options = WMSTileOptions(format = "image/png", transparent = TRUE),
                 attribution = "Iowa State University") %>%
    

  mapview:::mapViewLayersControl(names = grp) %>% hideGroup(grp[1]) %>% hideGroup(grp[2]) %>% hideGroup(grp[3]) %>% hideGroup(grp[4]) %>%
         hideGroup(grp[5]) %>% hideGroup(grp[6]) %>% hideGroup(grp[7]) %>% hideGroup(grp[8]) %>%
       hideGroup(grp[9]) %>% hideGroup(grp[10])

  m
}

