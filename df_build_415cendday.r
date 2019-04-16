#setwd("~/Documents/shiny_nohrsc")
setwd("~/R/proj/nohrsc/shiny")
source("libs.r")
source("fun_defs.r")
source("nwscode_to_rivgroup.r")
#2018




#################################################
### (start with saved data.frame) #####
#################################################

df <- as.data.frame(fread("nohrsc_thru_04.11.2019_thin.csv"))  %>% 
  mutate(date = ymd(date), year = year(date))

df_yr <- as.data.frame(fread("leap_yrs.csv"))

df2 <- df

df2$yday <- df2$yday[match(df$year,df2$year)]


d$y2 <- d2$y2[match(d1$x,d2$x)]




df <- df %>% mutate(dowy =   ifelse(cumdoy > 365, 
                                    ifelse(yday>274, yday-274, yday+92), 
                                    ifelse(yday>273, yday-273, yday+92)),
                    wy = water_year(date), wm =water_month(date)) %>% select(-cumdoy)




add_timeclasses <- function(df) {
  df <- df %>% mutate(year = year(date), yday = yday(date))
  df_yr <- df %>% group_by(year) %>%  summarize(cumdoy= max(yday, na.rm=T))
  df <- left_join(df, df_yr, by.x = "year", by.y = "year", all.x=TRUE)
  df <- df %>% mutate(dowy =   ifelse(cumdoy > 365, 
                                      ifelse(yday>274, yday-274, yday+92), 
                                      ifelse(yday>273, yday-273, yday+92)),
                      wy = water_year(date), wm =water_month(date)) %>% select(-cumdoy)
  
}

 
 
  #add_timeclasses() #%>%
  #mutate(nwscode = as.factor(nwscode), unit = as.factor(unit), year = as.integer(year(date)), dowy = as.integer(dowy),
  #       yday = as.integer(yday), wy = as.factor(wy), wm = as.integer(wm), param = as.factor(param),
  #       year = as.factor(year), basin = as.character(basin), basin_zone = location) %>% select(-location) %>% select(-X1) #X1 for big file 





df <- left_join(df, nwscode_to_rivgroup, by = c("nwscode")) %>% mutate(nwscode = as.factor(nwscode))


param <- c("swe", "sca", "se", "sm", "sb", "sd", "su")

paramnam <- c("water equivalent (swe) [in]", "areal extent of coverage (sca) [%]", "average snowpack temperature (se) [F]",
              "melt (sm) [in]", "blowing snow sublimation (sb) [in]" , "depth (sd) [in]", "surface sublimation (su) [in]" )
addparamnam <- data.frame(param, paramnam)
#tibble(df)
df <- inner_join(df, addparamnam, by = "param")

df <- df %>% mutate(nws_basin_code = paste0(basin," (", nwscode, ")"))
