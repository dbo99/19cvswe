rm(list = ls()) 
setwd("~/Documents/shiny_nohrsc/final/wy2011")
#setwd("~/R/proj/nohrsc/shiny/wy2011")
{
oct_a <- read_csv("10.01.10_10.14.10.csv") 
as_tibble(oct_a)
oct_b <-  read_csv("10.15.2010_10.25.2010.csv")
as_tibble(oct_b)
oct_c <- read_csv("10.25.2010_11.7.2010.csv")
as_tibble(oct_c)
nov_a <-  read_csv("wy2011_allp_11.5.2010_12.05.2010.csv")
as_tibble(nov_a)
nov_b <- read_csv("wy2011_allp_11.4.2010_11.06.2010.csv")
as_tibble(nov_b)
rest  <- read_csv("wy2011_allp_12.6.2010_09302011.csv")
as_tibble(rest)
}

wy2011 <- rbind(oct_a, oct_b, oct_c, nov_a, nov_b, rest) %>% select(-calc, -unit)
rm(nov_a, nov_b, oct_a, oct_b, oct_c, rest)
wy2011 <- wy2011 %>% mutate(year = year(date), yday = yday(date))
as_tibble(wy2011)


#setwd("~/Documents/shiny_nohrsc/final/data")
#archive <- as.data.frame(fread("nohrsc_thru_04.11.2019_thin.csv"))
#setwd("~/Documents/shiny_nohrsc/final")
#
#
#archive <- archive %>% mutate(year = as.factor(year(date)))  #somehow 2011 didn't make it in the first time
#archive <- archive %>% mutate(yday = yday(date))
#archive <- archive[,order(colnames(archive))]
#archive <- distinct(archive)
#as_tibble(archive)


wy2011 <- wy2011 %>% transmute(nwscode, basin, basin_zone = location, param, date, numval = as.character(value), numval = as.double(numval)) %>% 
                     mutate(year = year(date), yday = yday(date))
wy2011 <- distinct(wy2011)
wy2011 <- wy2011 [,order(colnames(wy2011 ))]
as_tibble(wy2011)

#ggplot(wy2011 %>% filter(param == "swe", nwscode == "FRAC1"), aes(date, numval)) + geom_line()
#ggplot(archive %>% filter(param == "swe", nwscode == "FRAC1", date >= "2010-03-30", date <= "2011-10-20"), aes(date, numval)) + geom_line()

archive <- rbind(archive, wy2011) #%>% arrange(date, nwscode)  #2011 appended here
archive <- distinct(archive)
as_tibble(archive)

setwd("~/Documents/shiny_nohrsc/final/data")
write_csv(archive, "archive.csv")
