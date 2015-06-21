
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

hail <-
ddat %>%
    filter( grepl(".*hail.*", EVTYPE,perl=T,ignore.case=T) )%>%
    filter( !grepl(".*tornado.*",EVTYPE,perl=T,ignore.case=T) )%>%
    mutate( EVTYPE="HAIL")  %>%
    group_by( EVTYPE ) %>% 
    summarise( ave_fatal = mean(FATALITIES,rm.na=TRUE),
               ave_injuries=mean(INJURIES,rm.na=TRUE),
               ave_propdmg = mean(PROPDMG,rm.na=TRUE),
               ave_cropdmg = mean(CROPDMG,rm.na=TRUE)
              )

#
# Hurricain
hurricane <- 
ddat %>% filter( grepl(".*hur[r]?i.*", EVTYPE,perl=T,ignore.case=T) ) %>%
    mutate( EVTYPE="HURRICANE") %>%
    group_by(EVTYPE) %>%
    summarise( ave_fatal = mean(FATALITIES,rm.na=TRUE),
              ave_injuries=mean(INJURIES,rm.na=TRUE),
              ave_propdmg = mean(PROPDMG,rm.na=TRUE),
              ave_cropdmg = mean(CROPDMG,rm.na=TRUE)
              )

hail <- dat %>% filter( grepl(".*hail.*", EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*tornado.*",  EVTYPE,perl=T,ignore.case=T) ) %>%
    mutate( EVTYPE="HAIL") %>%
    group_by(EVTYPE) %>%
    summarise( ave_fatal = mean(FATALITIES,rm.na=TRUE),
              ave_injuries=mean(INJURIES,rm.na=TRUE),
              ave_propdmg = mean(PROPDMG,rm.na=TRUE),
              ave_cropdmg = mean(CROPDMG,rm.na=TRUE)
              )

# thunderstorm and wind
tstorm_wind <-
dat %>% filter( grepl(".*(wind|thunder|tstm).*", EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*tornado.*",  EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*hurricane.*",  EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*(cold|chill).*",  EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*(flood).*",  EVTYPE,perl=T,ignore.case=T) ) %>%
    mutate( EVTYPE="WIND_OR_TSTM") %>%
    group_by(EVTYPE) %>%
    summarise( ave_fatal = mean(FATALITIES,rm.na=TRUE),
              ave_injuries=mean(INJURIES,rm.na=TRUE),
              ave_propdmg = mean(PROPDMG,rm.na=TRUE),
              ave_cropdmg = mean(CROPDMG,rm.na=TRUE)
              )

cold <- 
dat %>% filter( grepl(".*(cold|chill|snow).*", EVTYPE,perl=T,ignore.case=T) ) %>%
    mutate( EVTYPE="COLD") %>%
    group_by(EVTYPE) %>%
    summarise( ave_fatal = mean(FATALITIES,rm.na=TRUE),
              ave_injuries=mean(INJURIES,rm.na=TRUE),
              ave_propdmg = mean(PROPDMG,rm.na=TRUE),
              ave_cropdmg = mean(CROPDMG,rm.na=TRUE)
              )

heat <- 
dat %>% filter( grepl(".*(heat).*", EVTYPE,perl=T,ignore.case=T) ) %>%
    mutate( EVTYPE="HEAT") %>%
    group_by(EVTYPE) %>%
    summarise( ave_fatal = mean(FATALITIES,rm.na=TRUE),
              ave_injuries=mean(INJURIES,rm.na=TRUE),
              ave_propdmg = mean(PROPDMG,rm.na=TRUE),
              ave_cropdmg = mean(CROPDMG,rm.na=TRUE)
              )





## select(EVTYPE) %>% unique()





