setwd("~/Documents/shiny_nohrsc/wy2011")
source("libs.r")
#oct_a <- read_csv("10.01.10_10.14.10.csv") 
#as_tibble(oct_a)
#oct_b <-  read_csv("10.15.2010_10.25.2010.csv")
#as_tibble(oct_b)
#oct_c <- read_csv("10.25.2010_11.7.2010.csv")
#as_tibble(oct_c)
#nov_a <-  read_csv("wy2011_allp_11.5.2010_12.05.2010.csv")
#as_tibble(nov_a)
#nov_b <- read_csv("wy2011_allp_11.4.2010_11.06.2010.csv")
#as_tibble(nov_b)
#rest  <- read_csv("wy2011_allp_11.4.2010_11.06.2010.csv")
#as_tibble(rest)
#setwd("~/Documents/shiny_nohrsc")

real <- read_csv("nohrsc_thru_04.11.2019_thin.csv")
as_tibble(real)

wy2011 <- rbind(oct_a, oct_b, oct_c, nov_a, nov_b, rest) 

wy2011 <- unique(wy2011)

wy2011 <- wy2011 %>% transmute(nwscode, basin, basin_zone = location, param, date, numval = as.double(value))
as_tibble(wy2011)

#write_csv(wy2011, "wy2011_all.csv")

wy2011 <- read_csv("wy2011_all.csv")

nohrsc_thru_04.11.2019_thin_w2011 <- rbind(real, wy2011) %>% unique()
