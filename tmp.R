
library(dplyr)
library(ggplot2)
library(lubridate)


dat <- read.csv("stormdata.csv")
ddat <- tbl_df(dat)



#Custom filters

tornadoes <- ddat %>%
    filter( !is.na(F) ) %>%
    group_by(F) %>%
    mutate( EVTYPE=ifelse(!is.na("F"),paste("TORNADO_F",as.character(F),sep=""),"BLAH"))  %>%
    group_by( EVTYPE ) %>% 
    summarise( ave_fatal = mean(FATALITIES,rm.na=TRUE),
               ave_injuries=mean(INJURIES,rm.na=TRUE),
               ave_propdmg = mean(PROPDMG,rm.na=TRUE),
               ave_cropdmg = mean(CROPDMG,rm.na=TRUE)
              )

hurricaines <-
ddat %>%
    filter( grepl(".*hail.*", EVTYPE,perl=T,ignore.case=T) )%>%
    select(EVTYPE)






