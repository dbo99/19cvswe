spdf_entbasins <- readOGR("basins.kml", "cnrfc_09122018_basins_thin")
#spdf_basinzones <- readOGR(".","cnrfc_zones_wgs84aux")

#pnts1 <- readOGR("riverFcast.kml", "![CDATA[River Guidance <br><a href="http://www.cnrfc.noaa.gov/rfc_guidance.php">CNRFC River Forecast Web Page</a>]]")

# Convert spatialpolydf to an sf object
sf_entbasins <- spdf_entbasins %>%  st_as_sf()  %>% 
  mutate(Description = as.character(Description), 
         rivgroupkml = gsub(".*<tr> <td>Group</td> <td>(.+)</td> </tr> <tr bgcolor=\"#D4E4F3.*", "\\1", Description ),
         desckml = gsub(".*Basin</td> <td>(.+)</td> </tr> </table> </td> </tr> </table> </body> </html>", "\\1", Description ),
         nwscodekml = Name) %>% select(-Description, -Name) #%>%
nwscode <- sf_entbasins$nwscodekml
rivgroup <- sf_entbasins$rivgroupkml
nwscode_to_rivgroup <- data.frame(nwscode, rivgroup)
