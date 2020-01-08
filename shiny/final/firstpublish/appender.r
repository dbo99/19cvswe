setwd("~/R/proj/nohrsc/shiny/final/firstpublish")

archive <- as.data.frame(fread("fulldb_thru_2019-08-22.csv")) %>% mutate(date = as.character(date)) %>% 
           mutate(date = ymd(date))

#dft <- archive %>% filter(nwscode == "FRAC1", basin_zone == "Entire Basin", param == "swe")
#ggplot(dft, aes(date, numval)) + geom_line()

#write_csv(dft, "test.csv")
archive <- archive[,order(colnames(archive))] 
as_tibble(archive)
as_tibble(tail(archive))
new <-  as.data.frame(fread("2019-09-30.csv")) %>% mutate(date = as.character(date)) %>% 
        mutate(date = ymd(date)) %>% mutate(wy = water_year(date), yday = yday(date))
new <- new[,order(colnames(new))] 
last_date <- max(new$date)
as_tibble(new)
as_tibble(tail(new))
fulldb <- rbind(new, archive)
fulldb <- distinct(fulldb)
as_tibble(fulldb)
as_tibble(tail(fulldb))
filename <- paste0("fulldb_thru_", last_date, ".csv")
write_csv(fulldb, filename)


## get wy 17on from full db

wy2017on <- fulldb %>% filter(wy > 2016) %>% distinct()
write_csv(wy2017on, "wy2017_on.csv")

#wy2017on_frac1 <- df %>% filter(nws_basin_code == "San Joaquin - Friant Dam (FRAC1)") %>% filter(
 #                               wy > 2016)
#write_csv(wy2017on_frac1, "wy2017_on_frac1.csv")
# move wy2017on to right folder
 #redeploy


