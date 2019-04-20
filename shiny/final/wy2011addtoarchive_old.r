#rm(list = ls()) 
setwd("~/Documents/shiny_nohrsc/final/data")
#setwd("~/R/proj/nohrsc/shiny/wy2011")


{
oct <- read_csv("10_2010.csv") 
nov <- read_csv("11_2010.csv") 
dec <- read_csv("12_2010.csv")
jan <- read_csv("1_2011.csv") 
feb <- read_csv("2_2011.csv") 
mar <- read_csv("3_2011.csv") 
apr <- read_csv("4_2011.csv") 
may <- read_csv("5_2011.csv") 
jun <- read_csv("6_2011.csv") 
jul <- read_csv("7_2011.csv") 
aug <- read_csv("8_2011.csv") 
sep <- read_csv("9_2011.csv") 
}


wy2011 <- rbind(oct, nov, dec, jan, feb, mar, apr, may, jun, jul, aug, sep) 
rm(oct, nov, dec, jan, feb, mar, apr, may, jun, jul, aug, sep) 
wy2011 <- wy2011 %>% mutate(year = as.factor(year(date)), yday = yday(date))
as_tibble(wy2011)
tail(wy2011)
wy2011 <- distinct(wy2011)
wy2011 <- wy2011 [,order(colnames(wy2011 ))]
as_tibble(wy2011)

archive <- as.data.frame(fread("nohrsc_thru_04.11.2019_thinf.csv"))
as_tibble(archive)
tail(archive)
archive <- archive %>% mutate(year= year(date))
archive <- archive %>% mutate(yday = yday(date))
archive <- archive[,order(colnames(archive))] #columns in alph order
archive <- distinct(archive)
as_tibble(archive)


#wy2011 <- wy2011 %>% transmute(nwscode, basin, basin_zone, param, date, numval = as.character(value), numval = as.double(numval)) %>% #somehow 2011 didn't make it in the first time
#             wy2011 %>% transmute(nwscode, basin, basin_zone, param, date, numval)) %>% #somehow 2011 didn't make it in the first time
#                     mutate(year = year(date), yday = yday(date))


#ggplot(wy2011 %>% filter(param == "swe", nwscode == "FRAC1"), aes(date, numval)) + geom_line()
ggplot(archive %>% filter(param == "swe", nwscode == "FRAC1", date >= "2004-03-30", date <= "2018-10-20"), aes(date, numval)) + geom_line()

archive <- rbind(archive, wy2011) #%>% arrange(date, nwscode)  #2011 appended here
archive <- distinct(archive)
as_tibble(archive)

setwd("~/Documents/shiny_nohrsc/final/data")
write_csv(archive, "archive.csv")
