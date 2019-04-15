#setwd("~/Documents/shiny_nohrsc")
setwd("~/R/proj/nohrsc/shiny")
source("libs.r")
source("fun_defs.r")
source("nwscode_to_rivgroup.r")
#2018


### start with saved data.frame

df <- #read_csv("nohrsc_thru_04.11.2019.csv") %>%
  read_csv("3_2019.csv") %>%
  normcolnams() %>% 
  add_timeclasses() %>%
  mutate(nwscode = as.factor(nwscode), unit = as.factor(unit), year = as.integer(year(date)), dowy = as.integer(dowy),
         yday = as.integer(yday), wy = as.factor(wy), wm = as.integer(wm), param = as.factor(param),
         year = as.factor(year), basin = as.character(basin)) #%>% select(-X1)

#df_new <- 


df <- left_join(df, nwscode_to_rivgroup, by = c("nwscode")) %>% mutate(nwscode = as.factor(nwscode))


param <- c("swe", "sca", "se", "sm", "sb", "sd", "su")

paramnam <- c("water equivalent (swe) [in]", "areal extent of coverage (sca) [%]", "average snowpack temperature (se) [F]",
              "melt (sm) [in]", "blowing snow sublimation (sb) [in]" , "depth (sd) [in]", "surface sublimation (su) [in]" )
addparamnam <- data.frame(param, paramnam)
#tibble(df)
df <- inner_join(df, addparamnam, by = "param")

df <- df %>% mutate(nws_basin_code = paste0(basin," (", nwscode, ")"))


inputparams <- unique(df$paramnam)
inputparams
