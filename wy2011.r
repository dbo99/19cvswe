source("libs.r")
#setwd("~/Documents/shiny_nohrsc/wy2011")
setwd("~/R/proj/nohrsc/shiny/wy2011")

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


wy2011 <- rbind(oct_a, oct_b, oct_c, nov_a, nov_b, rest) 
#setwd("~/Documents/shiny_nohrsc")
setwd("~/R/proj/nohrsc/shiny")

real <- as.data.frame(fread(("nohrsc_thru_04.11.2019_thin.csv")))
as_tibble(real)


wy2011 <- distinct(wy2011)
as_tibble(wy2011)
wy2011 <- wy2011 %>% transmute(nwscode, basin, basin_zone = location, param, date, numval = as.character(value), numval = as.double(numval))
as_tibble(wy2011)
as_tibble(real)
ggplot(check %>% filter(param == "swe", nwscode == "FRAC1"), aes(date, numval)) + geom_line()

nohrsc_thru_04.11.2019_thin_w2011 <- rbind(real, wy2011) #%>% arrange(date, nwscode)
as_tibble(nohrsc_thru_04.11.2019_thin_w2011)
nohrsc_thru_04.11.2019_thin_w2011 <- distinct(nohrsc_thru_04.11.2019_thin_w2011)
as_tibble(nohrsc_thru_04.11.2019_thin_w2011)
t <- nohrsc_thru_04.11.2019_thin_w2011
as_tibble(t)

t <- t %>% mutate(date = ymd(date), year = year(date))


write_csv(nohrsc_thru_04.11.2019_thin_w2011, "archive4.csv")
