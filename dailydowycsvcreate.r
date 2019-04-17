nwscode_to_rivgroup <- as.data.frame(fread("nwscode_to_rivgroup.csv"))
df <- as.data.frame(fread("nohrsc_thru_04.11.2019_thin.csv"))  %>% 
  mutate(date = ymd(date), year = year(date))
as_tibble(df)
df_cumdoy <- read_csv("leap_yrs.csv") 
as_tibble(df_cumdoy)

df$cumdoy <- df_cumdoy$cumdoy[match(df$year,df_cumdoy$year)] 
df <- df %>% mutate(yday = yday(date))
as_tibble(df)

## build daily time series csv using a slow way to get dowy

date <- seq(ymd('2003-01-01'),ymd('2022-09-30'), by = '1 day')
year <- year(date_dowy)
dowy <- data.frame(date, year)
as_tibble(dowy)
dowy$cumdoy <- df_cumdoy$cumdoy[match(dowy$year,df_cumdoy$year)]   
as_tibble(dowy)
dowy <- dowy %>% mutate(yday = yday(date), dowy =   ifelse(cumdoy > 365, 
                 ifelse(yday>274, yday-274, yday+92), 
                 ifelse(yday>273, yday-273, yday+92)))
                 
as_tibble(dowy)               
  #               wy = water_year(date), wm =water_month(date)) 

# now use the csv for fast joining later on
write_csv(dowy, "daily_dowy.csv")
as_tibble(df)

df$dowy <- dowy$dowy[match(df$date,dowy$date)]   
as_tibble(df)

          