rm(list = ls()) 
setwd("~/Documents/shiny_nohrsc/final")
#setwd("~/R/proj/nohrsc/shiny")
source("libs.r")
source("fun_defs.r")
#source("nwscode_to_rivgroup.r")
#2018

setwd("~/Documents/shiny_nohrsc/final/data")

nwscode_to_rivgroup <- as.data.frame(fread("nwscode_to_rivgroup.csv"))
df_cumdoy <- read_csv("leap_yrs.csv") 
dowy <- read_csv("daily_dowy.csv")
#without2011 <- as.data.frame(fread("nohrsc_thru_04.11.2019_thin.csv")) 
df <- as.data.frame(fread("archive.csv"))
as_tibble(df)
tail(df)


#df[df=='NA'] <- NA
#df_dateisna <- df[is.na(df$date),]


df <- df %>% mutate(date = as.Date(date))
df <- df %>% mutate(year = as.factor(year(date)))
as_tibble(df)
setwd("~/Documents/shiny_nohrsc")


df$cumdoy <- df_cumdoy$cumdoy[match(df$year,df_cumdoy$year)] 

df <- df %>% mutate(yday = yday(date))

df$dowy <- dowy$dowy[match(df$date,dowy$date)]   #faster than dplyr _join

param <- c("swe", "sca", "se", "sm", "sb", "sd", "su")
paramnam <- c("water equivalent (swe) [in]", "areal extent of coverage (sca) [%]", "average snowpack temperature (se) [F]",
              "melt (sm) [in]", "blowing snow sublimation (sb) [in]" , "depth (sd) [in]", "surface sublimation (su) [in]" )

addparamnam <- data.frame(param, paramnam)


df$paramnam <- addparamnam$paramnam[match(df$param,addparamnam$param)]  
as_tibble(df)


#df <- df %>% mutate(date = ymd(date))
df <- df %>% mutate(nws_basin_code = as.factor(paste0(basin," (", nwscode, ")")), nwscode = as.factor(nwscode), basin = as.factor(basin),
                   param = as.factor(param), basin_zone = as.factor(basin_zone)) %>% select(-cumdoy, -yday)
df <- df %>% mutate(wy = water_year(date)
as_tibble(df)

date <- seq(ymd('2003-01-01'),ymd('2022-09-30'), by = '1 day')
wy <- water_year(date)
wyjoin <- data.frame(date, wy)
df$wy <- wyjoin$wy[match(df$date,wy$date)]  


ggplot(df %>% filter(wy == "2010" | wy == "2011" | wy == "2012", param == "swe", nwscode == "FRAC1"), aes(date, numval)) + geom_line()
rm(addparamnam, df_cumdoy, dowy, nwscode_to_rivgroup)

oh3to10 <- df %>% filter(date <= ymd("2010-12-31"))
oh3to10wy <- oh3to10 %>% mutate(wy = water_year(date))

oh3to10 <- df %>% filter(date <= ymd("2010-12-31"))
twelvto19 <- df %>% filter(date >= "2012-01-01") %>% mutate(wy = water_year(date))
as_tibble(twelvto19)
