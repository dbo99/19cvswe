#setwd("~/Documents/shiny_nohrsc")
setwd("~/R/proj/nohrsc/shiny")
source("libs.r")
source("fun_defs.r")
#source("nwscode_to_rivgroup.r")
#2018


nwscode_to_rivgroup <- as.data.frame(fread("nwscode_to_rivgroup.csv"))

df <- as.data.frame(fread("archive4.csv")) 
as_tibble(df)
df <- df %>% mutate(date = ymd(date), year = year(date))

df_cumdoy <- read_csv("leap_yrs.csv") 
df$cumdoy <- df_cumdoy$cumdoy[match(df$year,df_cumdoy$year)] 

df <- df %>% mutate(yday = yday(date))
dowy <- read_csv("daily_dowy.csv")
df$dowy <- dowy$dowy[match(df$date,dowy$date)]   #faster than dplyr _join

param <- c("swe", "sca", "se", "sm", "sb", "sd", "su")
paramnam <- c("water equivalent (swe) [in]", "areal extent of coverage (sca) [%]", "average snowpack temperature (se) [F]",
              "melt (sm) [in]", "blowing snow sublimation (sb) [in]" , "depth (sd) [in]", "surface sublimation (su) [in]" )

addparamnam <- data.frame(param, paramnam)


df$paramnam <- addparamnam$paramnam[match(df$param,addparamnam$param)]  
df <- df %>% mutate(nws_basin_code = as.factor(paste0(basin," (", nwscode, ")")), nwscode = as.factor(nwscode), basin = as.factor(basin),
                    param = as.factor(param), basin_zone = as.factor(basin_zone), wy = as.factor(water_year(date))) %>% select(-cumdoy, -yday)

ggplot(df %>% filter(wy == "2011", param == "swe", nwscode == "FRAC1"), aes(date, numval)) + geom_line()
rm(addparamnam, df_cumdoy, dowy, nwscode_to_rivgroup)

