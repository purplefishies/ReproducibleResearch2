
library(dplyr)
library(ggplot2)
library(lubridate)


dat <- read.csv("stormdata.csv")
ddat <- tbl_df(dat)


#Custom filters
merge_all <- function(x) {
    tmpmrg <- x[1]
    for( i in 2:length(x) ) {
        tmpmrg <- merge(tmpmrg,x[i],all=T)
    }
    tmpmrg
}

thing_to_return <- list(~mean(FATALITIES,rm.na=TRUE),~mean(PROPDMG,rm.na=TRUE))
othing <- function() {
    setNames(list(~mean(FATALITIES,rm.na=TRUE),
                  ~mean(PROPDMG,rm.na=TRUE),
                  ~mean(PROPDMG,rm.na=TRUE),
                  ~mean(CROPDMG,rm.na=TRUE)
                  ),
             c("ave_fatalities","ave_injuries","ave_propdmg","ave_cropdmg" )
             )
}

tornadoes <- ddat %>%
    filter( !is.na(F) ) %>%
    group_by(F) %>%
    mutate( EVTYPE=ifelse(!is.na(F),paste("TORNADO_F",as.character(F),sep=""),"BLAH"))  %>%
    group_by( EVTYPE ) %>% 
    summarise_( .dots=othing() )

# Hurricane
hurricane <- ddat %>%
    filter( grepl(".*hur[r]?i.*", EVTYPE,perl=T,ignore.case=T) ) %>%
    mutate( EVTYPE="HURRICANE") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=othing() )

    
# Hail
hail <- ddat %>%
    filter( grepl(".*hail.*", EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*tornado.*",  EVTYPE,perl=T,ignore.case=T) ) %>%
    mutate( EVTYPE="HAIL") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=othing() )

# thunderstorm and wind
tstorm_wind <- ddat %>%
    filter( grepl(".*(wind|thunder|tstm).*", EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*tornado.*",  EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*hurricane.*",  EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*(cold|chill).*",  EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*(flood).*",  EVTYPE,perl=T,ignore.case=T) ) %>%
    mutate( EVTYPE="WIND_OR_TSTM") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=othing() )

cold_noice_snow <- ddat %>%
    filter( grepl(".*(ice|cold|chill|snow).*", EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*(flood).*",  EVTYPE,perl=T,ignore.case=T) ) %>%
    mutate( EVTYPE="COLD") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=othing() )
    
heat <- ddat %>%
    filter( grepl(".*(heat).*", EVTYPE,perl=T,ignore.case=T) ) %>%
    mutate( EVTYPE="HEAT") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=othing() )

flood <- ddat %>%
    filter( grepl(".*(flood).*", EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*(thunder|tstm).*",  EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*(wind).*",  EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*(snow).*",  EVTYPE,perl=T,ignore.case=T) ) %>%
    mutate( EVTYPE="FLOOD") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=othing() )

    
cold_ice_snow <- ddat %>%
    filter( grepl(".*(ice|snow).*", EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*(flood).*",  EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*(chill|snow).*",EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*(wind).*",EVTYPE,perl=T,ignore.case=T) ) %>%
    mutate( EVTYPE="ICE_SNOW") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=othing() )

volcano <- ddat %>%
    filter( grepl(".*volc.*", EVTYPE,perl=T,ignore.case=T) ) %>%
    mutate( EVTYPE="VOLCANO") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=othing() )

dust <- ddat %>%
    filter( grepl(".*dust.*", EVTYPE,perl=T,ignore.case=T) ) %>%
    filter(!grepl(".*(wind).*",EVTYPE,perl=T,ignore.case=T) ) %>%
    mutate( EVTYPE="DUST") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=othing() )


all_data <- Reduce( function(x,y){ merge(x,y,all=T) }, list(tornadoes, hurricane, hail, tstorm_wind, cold_noice_snow, heat, flood, cold_ice_snow, volcano, dust ) )

##select(EVTYPE) %>% unique()
##select(EVTYPE) %>% unique()
##select(EVTYPE) %>% unique()





