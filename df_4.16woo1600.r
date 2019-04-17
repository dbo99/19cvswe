setwd("~/Documents/shiny_nohrsc")
#setwd("~/R/proj/nohrsc/shiny")
source("libs.r")
source("fun_defs.r")
#source("nwscode_to_rivgroup.r")
#2018


{
nwscode_to_rivgroup <- as.data.frame(fread("nwscode_to_rivgroup.csv"))

df <- #as.data.frame(fread("nohrsc_thru_04.11.2019_thin.csv"))  %>% 
      as.data.frame(fread("shinysubsettertester.csv"))  %>% 
  
  mutate(date = ymd(date), year = year(date))
#as_tibble(df)
df_cumdoy <- read_csv("leap_yrs.csv") 
#as_tibble(df_cumdoy)

df$cumdoy <- df_cumdoy$cumdoy[match(df$year,df_cumdoy$year)] 
df <- df %>% mutate(yday = yday(date))
#as_tibble(df)

dowy <- read_csv("daily_dowy.csv")
#as_tibble(dowy)

df$dowy <- dowy$dowy[match(df$date,dowy$date)]   #faster than dplyr _join
#as_tibble(df)

#df <- left_join(df, nwscode_to_rivgroup, by = c("nwscode")) %>% mutate(nwscode = as.factor(nwscode))  #if group column needed, abandon slow left_join and match like above 

param <- c("swe", "sca", "se", "sm", "sb", "sd", "su")
paramnam <- c("water equivalent (swe) [in]", "areal extent of coverage (sca) [%]", "average snowpack temperature (se) [F]",
              "melt (sm) [in]", "blowing snow sublimation (sb) [in]" , "depth (sd) [in]", "surface sublimation (su) [in]" )

addparamnam <- data.frame(param, paramnam)
#tibble(addparamnam)

df$paramnam <- addparamnam$paramnam[match(df$param,addparamnam$param)]  

df <- df %>% mutate(nws_basin_code = as.factor(paste0(basin," (", nwscode, ")")), nwscode = as.factor(nwscode), basin = as.factor(basin),
                    param = as.factor(param), basin_zone = as.factor(basin_zone)) %>% select(-cumdoy, -yday)

#as_tibble(df)


#df2 <- df %>% filter(nwscode == "FRAC1" | nwscode == "TRCC1" | nwscode == "PFTC1")
#write_csv(df2, "shinysubsettertester.csv")
as_tibble(df)
}
