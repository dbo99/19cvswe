rm(list = ls()) 
library(tidyverse)
library(rvest)
library(lubridate)
library(gganimate)
library(gifski)
library(transformr)
library(tweenr)
library(sf)
library(rgdal)
library(ggrepel)
#library(cowplot)

setwd("~/Documents/Rspat")
source("fun_defs.r")
source("df_init.r")
params <- c ("swe",
             "sca", 
           "se", 
             "sm", 
             "sb", 
             "sd", 
             "su")
paramnams <- c("Snow Water Equivalent [in]",
                "Areal Extent of Basin Snow Cover [%]",
                "Average Snowpack Temperature [F]",
                "Snow Melt [in]", 
                "Blowing Snow Sublimation [in]" , 
                "Snow Depth [in]", 
                "Snow Surface Sublimation [in]" )


start_date <- ymd("2015-10-01")
end_date <- ymd("2016-09-30")

dates <- seq(start_date, end_date, by = "days") %>% rev()



for (i in 1:length(dates)) { 
  
  year <- as.character(year(dates[i]))
  monthnum <- as.character(month(dates[i]))
  dayofmon <- as.character(day(dates[i]))
  
        for (j in 1:length(params)) {

    url1 <- "https://www.nohrsc.noaa.gov/shef_archive/index.html?rfc=cnrfc&product="
    url2 <- params[j]
    url3 <- "&year="
    url4 <- year
    url5 <- "&month="
    url6 <- monthnum
    url7 <- "&day="
    url8 <- dayofmon
    url9 <- "&hour=12"
    
    ####### snow water equivalent (swe) [inches] ##########
    
    #swe = "https://www.nohrsc.noaa.gov/shef_archive/index.html?rfc=cnrfc&product=swe&year=2019&month=3&day=27&hour=6"
    urlfin <- paste0(url1, url2, url3, url4, url5, url6, 
                                  url7, url8, url9)
    # scrape
    #if (url8 == 1 | url8 == 5 | url8 == 10 | url8 == 15 | url8 == 20 | url8 == 25 ) { Sys.sleep(0.5) }
    #if (url8 %% 2) = 0) {Sys.sleep(1) }
    #if (url8 %% 9) = 0) {Sys.sleep(1.5) }
    #if (url8 == 28) { Sys.sleep(0.5) }

    scrapedtext = html_text(html_node(read_html(urlfin),".notes"))
    #scrapedtext
    scrapedtext = gsub("WASH\n11          0.0 : Avg","\nLVCN2       0.0 : Avg", scrapedtext)
    #scrapedtext
    scrapedtext = gsub("CALIENTE\n33          0.0 : Avg","\nMVYN2       0.0 : Avg", scrapedtext)
    #scrapedtext
    scrapedtext = gsub("APLO3I","APLX8", scrapedtext) #11.2.2010 
    scrapedtext = gsub("APRO3X","APRX9", scrapedtext) #11.2.2010 
   # scrapedtext = gsub("OWYO3I","OWYX9", scrapedtext) #11.2.2010 
    scrapedtext = gsub("\\<KRBO3L\\>",":     ", scrapedtext) #10.14.2010

    scrapedtext = gsub("\\<MVYN2L\\>","MVYN9", scrapedtext) #10.14.2010
    scrapedtext = gsub("\\<KRBO3U\\>",":     ", scrapedtext) #10.14.2010
    scrapedtext = gsub("\\<OWYO3IL\\>",":      ", scrapedtext) #10.14.2010
    scrapedtext = gsub("\\<OWYO3IM\\>",":      ", scrapedtext) #10.14.2010
    scrapedtext = gsub("\\<OWYO3IU\\>",":      ", scrapedtext) #10.14.2010
    
    scrapedtext = gsub("\\<MFDO3L\\>",":    ", scrapedtext) #10.14.2010
    scrapedtext = gsub("\\<MFDO3U\\>",":    ", scrapedtext) #10.14.2010
    scrapedtext = gsub("\\<MOAN2H\\>","XXXX1", scrapedtext) #10.14.2010
    
    scrapedtext = gsub("\\<LKSA3L\\>","LKSA3", scrapedtext) #10.14.2010
    scrapedtext = gsub("LAS VEGAS WASH - 3 KIDS ","Las Vegas Wash", scrapedtext)
    scrapedtext = gsub("MEADOW VALLEY WASH NR CALIENTE","Meadow Valley Wash Nr Caliente", scrapedtext)
    scrapedtext = gsub("ILLINOIS R AT KERBY","Illinois River at Kerby", scrapedtext)
    scrapedtext = gsub("APPLEGATE RESERVOIR","Applegate Reservoir", scrapedtext)
    scrapedtext = gsub("APPLEGATE RIV NR APPLEGATE","Applegate Riv nr Applegate", scrapedtext)
    scrapedtext = gsub("BEAR CREEK AT MEDFORD","Bear Creek at Medford", scrapedtext)
    scrapedtext = gsub("LAKE MEAD","Lake Mead", scrapedtext)
    scrapedtext = gsub("OWYHEE DAM","Owyhee Dam", scrapedtext)
    scrapedtext = gsub("MUDDY RIVER NR MOAPA","Muddy River nr Moapa", scrapedtext)
   

    scrapedtext = gsub("Basin name not known","Basin name - Not known", scrapedtext)
    scrapedtext = unlist(str_split(scrapedtext,"\n"))
    block_index = findBlocks(scrapedtext)
    dftemp_i = readAllBlocks(block_index, scrapedtext) %>% mutate( param = params[j], date = dates[i])
    

    df <- rbind(df,dftemp_i) %>% unique()
    message(dates[i], " ", params[j])
 
        }  

}
#
df <- data.frame(df)
write_csv(df, "02.01.16_08.17.16.csv")

