setwd("~/Documents/shiny_nohrsc/final/firstpublish")
source("libs.r")
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


start_date <- ymd("2019-04-22")
end_date <- ymd("2019-04-22")

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
    
    scrapedtext = html_text(html_node(read_html(urlfin),".notes"))
    
    scrapedtext = gsub("WASH\n11          0.0 : Avg","\nLVCN2       0.0 : Avg", scrapedtext)

    scrapedtext = gsub("CALIENTE\n33          0.0 : Avg","\nMVYN2       0.0 : Avg", scrapedtext)

    scrapedtext = gsub("APLO3I","APLX8", scrapedtext) #11.2.2010 dates around here have these bad data
    scrapedtext = gsub("APRO3X","APRX9", scrapedtext) #11.2.2010 
    
   # scrapedtext = gsub("APLO3I","APRX9", scrapedtext) #11.2.2010 
   # scrapedtext = gsub("APRO3X","APRX9", scrapedtext) #11.2.2010 
    
    scrapedtext = gsub("\\OWYO3I\\>","OWYX9", scrapedtext) #11.2.2010 
    scrapedtext = gsub("\\<KRBO3L\\>",":     ", scrapedtext) #10.14.2010
    
    scrapedtext = gsub("\\<MVYN2L\\>","MVYN9", scrapedtext) #10.14.2010 dates around here have these bad data
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
    scrapedtext = gsub("MEADOW VALLEY WASH NR ","Meadow Valley Wash Nr Caliente", scrapedtext)
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
df <- df %>% mutate(numval = as.character(value), numval = as.double(numval)) %>% select(-calc, -unit, -value)
as_tibble(df)
#df <- data.frame(df)
#filename <- paste0(month(start_date), "_", year(start_date), ".csv")
filename <- paste0(Sys.Date(), ".csv")
write_csv(df,filename)
#write_csv(data.frame(scrapedtext),"2009.06.01_debug.csv")

#ggplot(df %>% filter(param == "swe", nwscode == "PFTC1"), aes(date, numval)) + geom_line()