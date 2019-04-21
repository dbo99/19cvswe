rm(list = ls()) 
setwd("~/Documents/shiny_nohrsc/final")
#setwd("~/R/proj/nohrsc/shiny/final")
source("libs.r")
source("fun_defs.r")

setwd("~/Documents/shiny_nohrsc/final/data")
#setwd("~/R/proj/nohrsc/shiny/final/data")


dowy <- as.data.frame(fread("daily_dowy.csv"))
nwscode_to_rivgroup <- as.data.frame(fread("nwscode_to_rivgroup.csv"))
df_cumdoy <- read_csv("leap_yrs.csv") 
dowy <- read_csv("daily_dowy.csv")
#df <- as.data.frame(fread("archivewith2011.csv"))
df <- as.data.frame(fread("archivewith2011_wy17on.csv")) #just for dev
as_tibble(tail(df))
tail(df)

df <- df %>% mutate(date = ymd(date))
df <- df %>% mutate(year = year(date))
as_tibble(df)
setwd("~/Documents/shiny_nohrsc/final")
#setwd("~/R/proj/nohrsc/shiny/final")


df$cumdoy <- df_cumdoy$cumdoy[match(df$year,df_cumdoy$year)] 
df$dowy <- dowy$dowy[match(df$date,dowy$date)]   #faster than dplyr _join
as_tibble(df)
as_tibble(tail(df))
param <- c("swe", "sca", "se", "sm", "sb", "sd", "su")
paramnam <- c("water equivalent (swe) [in]", "areal extent of coverage (sca) [%]", "average snowpack temperature (se) [F]",
              "melt (sm) [in]", "blowing snow sublimation (sb) [in]" , "depth (sd) [in]", "surface sublimation (su) [in]" )

addparamnam <- data.frame(param, paramnam)
df$paramnam <- addparamnam$paramnam[match(df$param,addparamnam$param)]  
as_tibble(df)



df <- df %>% mutate(nws_basin_code = as.factor(paste0(basin," (", nwscode, ")")), nwscode = as.factor(nwscode), basin = as.factor(basin),
                    param = as.factor(param), basin_zone = as.factor(basin_zone)) %>% select(-cumdoy, -yday)

as_tibble(df)
                    
#date <- seq(ymd('2003-01-01'),ymd('2022-09-30'), by = '1 day')
#wy <- water_year(date)
#wyjoin <- data.frame(date, wy)
#df$wy <- wyjoin$wy[match(df$date,wy$date)]  


ggplot(df %>% filter(wy == "2010" | wy == "2011" | wy == "2012", param == "swe", nwscode == "FRAC1"), aes(date, numval)) + geom_line()
rm(addparamnam, df_cumdoy, nwscode_to_rivgroup)


