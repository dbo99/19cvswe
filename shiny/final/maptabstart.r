

setwd("~/R/proj/nohrsc/shiny/final/data")


df_current_swe <- df %>% filter(basin_zone == "Entire Basin", date == max(df$date), param == "swe")

ebasin_kml <- readOGR("basins.kml", "cnrfc_09122018_basins_thin")

# Convert spatialpolydf to an sf object
sf_ebasin_kml_swe  <- ebasin_kml  %>%  st_as_sf() %>% transmute(Name, geometry)  %>%
                      left_join(df_current_swe, by = c("Name" = "nwscode"))

#sf_ebasin_kml_se  <- ebasin_kml  %>%  st_as_sf() %>% transmute(Name, geometry)  %>%
#                      left_join(df_current_se, by = c("Name" = "nwscode"))
#


maptypes = c("Stamen.TonerLite", "Stamen.Terrain", "Stamen.TopOSMRelief", "Esri.WorldTopoMap" , "Esri.WorldPhysical",  "OpenTopoMap" ,
             "NASAGIBS.ModisTerraSnowCover", "NASAGIBS.ModisTerraTrueColorCR", "NASAGIBS.ModisTerraBands367CR")

grp <- c(    "usgs hydrography",   "0.5 reflectivity","hrrr p_1hr", "hrrr p_2hr",   "hrrr p_4hr", "hrrr p_6hr",
             "mrms p_1hr", "mrms p_24hr", "mrms p_48hr", "mrms p_72hr") # "Coarse Geo") # 


  m <- mapview(sf_ebasin_kml_swe["numval"], burst = TRUE, hide = TRUE, col.regions = viridisLite::viridis, alpha.regions = 0.4,  map.types = maptypes,
               popup = popupTable(sf_ebasin_kml_swe, zcol = c("nws_basin_code", "date", "numval")), 
               layer.name = "latest_nohrsc_swe_in")   
  
  m@map = m@map %>% 
    
    addTiles() %>%
    setView(-119.6, 38.05, zoom = 6.4) %>%   
    
    addWMSTiles(group= grp[1], baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer", layers = "0",
                options = WMSTileOptions(format = "image/png", transparent = TRUE), attribution = "USGS") %>%

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
    
    
    mapview:::mapViewLayersControl(names = grp) %>% #hideGroup(grp[1]) %>% 
    hideGroup(grp[2]) %>% hideGroup(grp[3]) %>% hideGroup(grp[4]) %>%
    hideGroup(grp[5]) %>% hideGroup(grp[6]) %>% hideGroup(grp[7]) %>% hideGroup(grp[8]) %>%
    hideGroup(grp[9]) %>% hideGroup(grp[10])
  
  m

#mapshot(m, url = paste0(getwd(), "/map.html"),
#        file = paste0(getwd(), "/map.png"), selfcontained = F )

  GetURL <- function(service, host = "basemap.nationalmap.gov") {
    sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)}
GetURL("USGSHydroCached")
