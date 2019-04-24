rm(list = ls()) 
#setwd("~/R/proj/nohrsc/shiny/final/firstpublish")
#setwd("~/R/proj/nohrsc/shiny/final")
source("libs.r")
source("fun_defs.r")
#setwd("~/Documents/shiny_nohrsc/final/firstpublish")
#setwd("~/R/proj/nohrsc/shiny/final/data")


#addend <- as.data.frame(fread("4_2019.csv")) %>% mutate(date = as.character(date), date = ymd(date), wy = 
#          water_year(date), yday = yday(date))
#addend <- distinct(addend)
#addend <- addend[,order(colnames(addend))] 
#df <- rbind(addend, df)
#df <- distinct(df)
#write_csv(df, "fullthruapr21.csv")

#df <- unzip("fullthruapr21.zip") 
#df <- as.data.frame(fread("fullthruapr21.csv")) %>% mutate(date = as.character(date), date = ymd(date), wy = 
#                                                       water_year(date), yday = yday(date))
#df <- df %>% filter(wy >2016)
#write_csv(df, "wy2017_on.csv")
#

dowy <- as.data.frame(fread("daily_dowy.csv"))
nwscode_to_rivgroup <- as.data.frame(fread("nwscode_to_rivgroup.csv"))
df_cumdoy <- read_csv("leap_yrs.csv") 
dowy <- read_csv("daily_dowy.csv")

#df <- unzip("fulldb_thru_2019-04-23.zip")

df <- as.data.frame(fread("fulldb_thru_2019-04-23.csv"))


df <- df %>% mutate(date = as.character(date)) %>% mutate(date = ymd(date))
df <- df %>% mutate(year = year(date), yday = yday(date))
#as_tibble(df)


#setwd("~/Documents/shiny_nohrsc/final")
#setwd("~/R/proj/nohrsc/shiny/final")


df$cumdoy <- df_cumdoy$cumdoy[match(df$year,df_cumdoy$year)] 
df$dowy <- dowy$dowy[match(df$date,dowy$date)]   #faster than dplyr _join
as_tibble(df)
as_tibble(tail(df))
param <- c("swe", "sca", "se", "sm", "sb", "sd", "su")
paramnam <- c("water equivalent (swe)", "areal extent of coverage (sca)", "average snowpack temperature (se)",
              "melt (sm)", "blowing snow sublimation (sb)" , "depth (sd)", "surface sublimation (su)" )
p_unit <- c("swe [in]", "sca [%]", "se [F]", "sm [in]", "sb [in]", "sd [in]", "su [in]")
addparamnam <- data.frame(param, paramnam, p_unit)
df$paramnam <- addparamnam$paramnam[match(df$param,addparamnam$param)]  
df$p_unit <- addparamnam$p_unit[match(df$param,addparamnam$param)]
#as_tibble(df)



df <- df %>% mutate(nws_basin_code = as.factor(paste0(basin," (", nwscode, ")")), nwscode = as.factor(nwscode), basin = as.factor(basin),
                    param = as.factor(param), basin_zone = as.factor(basin_zone)) %>% select(-cumdoy, -yday)


                    



#ggplot(df %>% filter(wy == "2010" | wy == "2011" | wy == "2012", param == "swe", nwscode == "FRAC1"), aes(date, numval)) + geom_line()
rm(addparamnam, df_cumdoy, nwscode_to_rivgroup)

#setwd("~/R/proj/nohrsc/shiny/final/data")
#setwd("~/Documents/shiny_nohrsc/final/data")
ebasin_kml <- readOGR("basins.kml", "cnrfc_09122018_basins_thin")









#date <- seq(ymd('2003-01-01'),ymd('2022-09-30'), by = '1 day')
#wy <- water_year(date)
#wyjoin <- data.frame(date, wy)
#df$wy <- wyjoin$wy[match(df$date,wy$date)]  
