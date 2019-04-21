#rm(list = ls()) 
#setwd("~/Documents/shiny_nohrsc/final/data")
setwd("~/R/proj/nohrsc/shiny/final/data")


{
  oct <- read_csv("10_2010.csv") %>% mutate(wy = water_year(date), yday = yday(date))
  nov <- read_csv("11_2010.csv") %>% mutate(wy = water_year(date), yday = yday(date))
  dec <- read_csv("12_2010.csv") %>% mutate(wy = water_year(date), yday = yday(date))
  jan <- read_csv("1_2011.csv")  %>% mutate(wy = water_year(date), yday = yday(date))
  feb <- read_csv("2_2011.csv")  %>% mutate(wy = water_year(date), yday = yday(date))
  mar <- read_csv("3_2011.csv")  %>% mutate(wy = water_year(date), yday = yday(date))
  apr <- read_csv("4_2011.csv")  %>% mutate(wy = water_year(date), yday = yday(date)) 
  may <- read_csv("5_2011.csv")  %>% mutate(wy = water_year(date), yday = yday(date))
  jun <- read_csv("6_2011.csv")  %>% mutate(wy = water_year(date), yday = yday(date))
  jul <- read_csv("7_2011.csv")  %>% mutate(wy = water_year(date), yday = yday(date))
  aug <- read_csv("8_2011.csv")  %>% mutate(wy = water_year(date), yday = yday(date))
  sep <- read_csv("9_2011.csv")  %>% mutate(wy = water_year(date), yday = yday(date))

}

wy2011 <- rbind(oct, nov, dec, jan, feb, mar, apr, may, jun, jul, aug, sep) 
rm(oct, nov, dec, jan, feb, mar, apr, may, jun, jul, aug, sep) 
#wy2011 <- wy2011 %>% mutate(year = as.factor(year(date)), yday = yday(date))
as_tibble(wy2011)
tail(wy2011)
wy2011 <- distinct(wy2011)
wy2011 <- wy2011 [,order(colnames(wy2011 ))]
as_tibble(wy2011)

#without2011 <- as.data.frame(fread("nohrsc_thru_04.11.2019_thinf.csv"))
#as_tibble(without2011)
#tail(without2011)
#without2011 <- without2011 %>% mutate(date = ymd(date))
#without2011 <- without2011 %>% mutate(wy= water_year(date))
#without2011 <- without2011 %>% mutate(yday = yday(date))
#without2011 <- distinct(without2011)
#without2011 <- without2011[,order(colnames(without2011))] #columns in alph order
#as_tibble(without2011)
#tail(without2011)
#write_csv(without2011, "without2011_8cols.csv")
without2011 <- as.data.frame(fread("without2011_8cols.csv"))
as_tibble(without2011)

#ggplot(without2011 %>% filter(param == "swe", nwscode == "FRAC1", date >= "2004-03-30", date <= "2018-10-20"), aes(date, numval)) + geom_line()

with2011 <- rbind(archive, wy2011)  #2011 appended here
with2011 <- distinct(with2011)
as_tibble(with2011)

setwd("~/R/proj/nohrsc/shiny/final/data")
write_csv(with2011, "archivewith2011.csv")

archivewith2011 <- as.data.frame(fread("archivewith2011.csv"))
as_tibble(archivewith2011)
archivewith2011 <-archivewith2011 %>% mutate(date = ymd(date))
ggplot(archivewith2011 %>% filter(param == "swe", nwscode == "FRAC1", date >= "2004-03-30", date <= "2018-10-20"), 
                       aes(date, numval)) + geom_line()
