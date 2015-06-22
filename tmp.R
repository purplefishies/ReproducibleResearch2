
library(dplyr)
library(ggplot2)
library(lubridate)


dat <- read.csv("stormdata.csv")
ddat <- tbl_df(dat)


capture_stats <- function() {
    setNames(list(~mean(FATALITIES,rm.na=TRUE),
                  ~mean(PROPDMG,rm.na=TRUE),
                  ~mean(PROPDMG,rm.na=TRUE),
                  ~mean(CROPDMG,rm.na=TRUE),
                  ~n()
                  ),
             c("ave_fatalities","ave_injuries","ave_propdmg","ave_cropdmg","nevents")
             )
}


## Filters
tornado_filter <- ~!is.na(F)
hurricane_filter <- ~grepl(".*hur[r]?i.*", EVTYPE,perl=T,ignore.case=T )
hail_filter <- ~grepl(".*hail.*", EVTYPE,perl=T,ignore.case=T) & !grepl(".*tornado.*",  EVTYPE,perl=T,ignore.case=T)
wind_tstorm_filter <- ~grepl(".*(wind|thunder|tstm).*", EVTYPE,perl=T,ignore.case=T) &
    !grepl(".*tornado.*",  EVTYPE,perl=T,ignore.case=T) & 
    !grepl(".*hurricane.*",  EVTYPE,perl=T,ignore.case=T) &
    !grepl(".*(cold|chill).*",  EVTYPE,perl=T,ignore.case=T) &
    !grepl(".*(flood).*",  EVTYPE,perl=T,ignore.case=T)

cold_noicesnow_filter <- ~grepl(".*(cold|chill).*", EVTYPE,perl=T,ignore.case=T) &
    !grepl(".*(flood).*",  EVTYPE,perl=T,ignore.case=T) &
    !grepl(".*(snow|ice).*", EVTYPE,perl=T,ignore.case=T)

icesnow_filter <- ~grepl(".*(ice|snow).*", EVTYPE,perl=T,ignore.case=T) & 
	!grepl(".*(flood).*",  EVTYPE,perl=T,ignore.case=T) &
	!grepl(".*(chill|snow).*",EVTYPE,perl=T,ignore.case=T) &
	!grepl(".*(wind).*",EVTYPE,perl=T,ignore.case=T)


head_filter <- ~grepl(".*(heat).*", EVTYPE,perl=T,ignore.case=T)

flood_filter <- ~grepl(".*(flood).*", EVTYPE,perl=T,ignore.case=T) &
    !grepl(".*(thunder|tstm).*",  EVTYPE,perl=T,ignore.case=T) & 
    !grepl(".*(wind).*",  EVTYPE,perl=T,ignore.case=T) & 
    !grepl(".*(snow).*",  EVTYPE,perl=T,ignore.case=T) 

volcano_filter <- ~grepl(".*volc.*", EVTYPE,perl=T,ignore.case=T)
dust_filter <- ~grepl(".*dust.*", EVTYPE,perl=T,ignore.case=T) &
    !grepl(".*(wind).*",EVTYPE,perl=T,ignore.case=T)



tornadoes <- ddat %>%
    filter_( .dots=tornado_filter ) %>%
    group_by(F) %>%
    mutate( EVTYPE=ifelse(!is.na(F),paste("TORNADO_F",as.character(F),sep=""),"BLAH"))  %>%
    group_by( EVTYPE ) %>% 
    summarise_( .dots=capture_stats() )


# Hurricane
hurricane <- ddat %>%
    filter_( .dots=hurricane_filter ) %>%
    mutate( EVTYPE="HURRICANE") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=capture_stats() )

   
# Hail
hail <- ddat %>%
    filter_( .dots=hail_filter ) %>%
    mutate( EVTYPE="HAIL") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=capture_stats() )

# Thunderstorm and wind
wind_tstorm <- ddat %>%
    filter_( .dots=wind_tstorm_filter ) %>%
    mutate( EVTYPE="WIND_OR_TSTM") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=capture_stats() )

cold_noicesnow <- ddat %>%
    filter_( .dots=cold_noicesnow_filter ) %>%
    mutate( EVTYPE="COLD") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=capture_stats() )
    
heat <- ddat %>%
    filter_( .dots=head_filter ) %>%
    mutate( EVTYPE="HEAT") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=capture_stats() )

flood <- ddat %>%
    filter_( .dots=flood_filter ) %>%
    mutate( EVTYPE="FLOOD") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=capture_stats() )
    
icesnow <- ddat %>%
    filter_( .dots=icesnow_filter ) %>%
    mutate( EVTYPE="ICE_SNOW") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=capture_stats() )

volcano <- ddat %>%
    filter_( .dots=volcano_filter ) %>% 
    mutate( EVTYPE="VOLCANO") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=capture_stats() )

dust <- ddat %>%
    filter_( .dots=dust_filter ) %>% 
    mutate( EVTYPE="DUST") %>%
    group_by(EVTYPE) %>%
    summarise_( .dots=capture_stats() )


all_data <- Reduce( function(x,y){ merge(x,y,all=T) }, list(tornadoes, hurricane, hail, wind_tstorm, cold_noicesnow, heat, flood, icesnow, volcano, dust ) )

mindate <- min(mdy_hms(as.character(ddat$BGN_DATE)))
maxdate <- max(mdy_hms(as.character(ddat$BGN_DATE)))
timescale <- as.double(maxdate-mindate,units="days")

people_injury <- all_data %>%
    transmute( EVTYPE=EVTYPE,exp_fatal=nevents*ave_fatalities/timescale,exp_injure=nevents*ave_injuries/timescale)


property_damage <- all_data %>%
    transmute( EVTYPE=EVTYPE,exp_propdmg=nevents*ave_propdmg/timescale,exp_cropdmg=nevents*ave_cropdmg/timescale)




