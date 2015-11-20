# Health and Economic Impacts of U.S. Storms
treepruner  
November 15, 2015  





### Synopsis

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage. The file contains data from 1950 - 2011.

The United States population health is impacted by weather events in terms of both fatalities and injuries.

The most fatalities occur due to tornados. Heat is the second most prevelant cause of fatalities.

More injuries are incurred as a result of tornados more than any other event group.

The greatest economic consequences are more difficult to determine due to the inconsistency of the cost unit coding. If ambiguous units are assumed to be in millions, extremes of water- drought and flooding, with drought being more than twice as expensive as flooding in the crop damage rankings.

For property damages, hurricanes, flooding and coastal flooding are the most expensive event groups.


### Data Processing

####  Load Data 


```r
library(RCurl)
library(lubridate)
library(dplyr)
library(sqldf)
library(xtable)
library(ggplot2)
library(scales)
```




```r
if (!file.exists("./proj2/repdata%2Fdata%2FStormData.csv.bz2")) { 
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
setInternet2(use = TRUE)
download.file(fileURL, "./proj2/repdata%2Fdata%2FStormData.csv.bz2", method = "curl")}

sd <- read.csv("./proj2/repdata%2Fdata%2FStormData.csv.bz2")

# save original
saveRDS(sd, "./proj2/stormData.rds")
```


#### Explore Data


```r
str(sd)
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : Factor w/ 16335 levels "1/1/1966 0:00:00",..: 6523 6523 4242 11116 2224 2224 2260 383 3980 3980 ...
##  $ BGN_TIME  : Factor w/ 3608 levels "00:00:00 AM",..: 272 287 2705 1683 2584 3186 242 1683 3186 3186 ...
##  $ TIME_ZONE : Factor w/ 22 levels "ADT","AKS","AST",..: 7 7 7 7 7 7 7 7 7 7 ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: Factor w/ 29601 levels "","5NM E OF MACKINAC BRIDGE TO PRESQUE ISLE LT MI",..: 13513 1873 4598 10592 4372 10094 1973 23873 24418 4598 ...
##  $ STATE     : Factor w/ 72 levels "AK","AL","AM",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : Factor w/ 35 levels "","  N"," NW",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_LOCATI: Factor w/ 54429 levels "","- 1 N Albion",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_DATE  : Factor w/ 6663 levels "","1/1/1993 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_TIME  : Factor w/ 3647 levels ""," 0900CST",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : Factor w/ 24 levels "","E","ENE","ESE",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_LOCATI: Factor w/ 34506 levels "","- .5 NNW",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ WFO       : Factor w/ 542 levels ""," CI","$AC",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ STATEOFFIC: Factor w/ 250 levels "","ALABAMA, Central",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ ZONENAMES : Factor w/ 25112 levels "","                                                                                                                               "| __truncated__,..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : Factor w/ 436781 levels "","-2 at Deer Park\n",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
```

```r
head(sd, 1)
```

```
##   STATE__          BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
## 1       1 4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
##    EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END
## 1 TORNADO         0                                               0
##   COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES
## 1         NA         0                        14   100 3   0          0
##   INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES
## 1       15      25          K       0                                    
##   LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
## 1     3040      8812       3051       8806              1
```

#### Clean/Preprocess Data

##### <span style="color:blue">Dates </span>

lubridate was used to convert the dates that are factors into date fields so that date parts such as year could be used in the figures.


```r
sd$BGN_DATE <- mdy_hms(sd$BGN_DATE)
sd$END_DATE <- mdy_hms(sd$END_DATE)
```


##### <span style="color:blue"> EVTYPE Code Groupings </span>

There are 985 unique EVTYPE codes, many of which are very similar. Rankings using this EVTYPE as it stands would be too disaggregated to be useful.

The EVTYPE codes were grouped together into 38 groups. This mapping was created by extracting the 985 unique EVTYPE codes and manually assigning them to a group. The storm data file was joined to the mapping file on EVTYPE with sqldf and the new EnvTypeGroup code was added.

To make this reproducible by others, the entire mapping file has been read into this document in the appendix.


```r
envTypeMapping <- read.csv("./proj2/envTypeList.csv",  sep = ",")[, 2:3]

sd <- sqldf( 
        "select s.*, e.envTypeGroup   
         from sd s join envTypeMapping e on s.EVTYPE = e.ENVTYPE")
```

```
## Loading required package: tcltk
```


##### <span style="color:blue"> Duplicates  </span>

This section checks for duplicates and determines if the REFNUM identifies a unique row. Removed records that were exactly the same in all fields so that only 1 record out of the multiples remained in the file. 

Others in the class discovered that there is another pair of records, REFNUM 605943 and 567221, that although they have different REFNUMs, appear to be for the same event. The REMARKS refer to millions in damage, so REFNUM 605943 which referred to billions in PROPDMGEXP, was removed.


```r
dup_refnum <- sqldf( 
        "select s.REFNUM, count(*)   
         from sd s 
         group by s.REFNUM
         having count(*) > 1")

dup_refnum
```

```
##   REFNUM count(*)
## 1 233600        2
## 2 233653        2
```

```r
sd <- sqldf( 
        "select distinct s.*   
         from sd s" 
         )

# this code also works and was actually faster
# sd1 <- sd[!duplicated(sd),] 
```

##### <span style="color:blue"> Economic Damage Fields </span>

The events in this file range from 1950 to 2011, however, the documentation states it was best documented from 1996 forward. In light of this, the analysis is just including the BGN_DATES from 1996 - 2011. I wasn't able to get the date syntax correct with sqldf, so I'm doing that part with dplyr.



```r
sd0 <- sd %>% filter(year(BGN_DATE) >= 1996) 

# save 1996-2011 data  
saveRDS(sd0, "./proj2/sd0.rds") 
```



The PROPDMG field is the numerical component of the property damage and the PROPDMGEXP is the units, however, the units field is dirty. The numerical field was multiplied as follows before dividing by 1,000,000 in order to present the amounts consistently in millions: 

 * Bb = 1,000,000,000
 * Mm = 1,000,000,
 * Kk = 1,000
 * Hh = 100
 * ?  = 0
 * anything else = 1,000,000
 
The CROPDMG field is the numerical component of the crop damage and the CROPDMGEXP is the units, however, the units field is also dirty. The numerical field was multiplied as follows before dividing by 1,000,000 in order to present the amounts consistently in millions: 

 * Bb = 1,000,000,000
 * Mm = 1,000,000
 * Kk = 1,000
 * Hh = 100
 * ?  = 0
 * anything else = 1,000,000

Another field was added to indicate the reliability of the numbers. Records containing  "anything else"  and "?" were coded as low reliability. 

The REMARKS have cost numbers that do not always match the cost fields for that record. A future analysis should compare these fields and either adjust the  reliability and/or update the damage numbers.

To truely be comparable and understandable to the modern audience, the damage dollar amounts should be adjusted using an appropriate price index into current dollars in order to properly rank the financial impact of each event. This was deemed out of scope for our assignment. 


```r
sd1 <- sqldf( 
        "select  REFNUM
        , BGN_DATE
        , END_DATE
        , EVTYPE
        , EnvTypeGroup
        , FATALITIES
        , INJURIES
        , PROPDMG
        , PROPDMGEXP
        , case 
                when upper(PROPDMGEXP) = 'B' then round(PROPDMG * 1000000000,0)/1000000
                when upper(PROPDMGEXP) = 'M' then round(PROPDMG * 1000000,0)/1000000
                when upper(PROPDMGEXP) = 'K' then round(PROPDMG * 1000,0)/1000000
                when upper(PROPDMGEXP) = 'H' then round(PROPDMG * 100,0)/1000000
                when upper(PROPDMGEXP) = '?' then round(PROPDMG * 0,0)/1000000
           else round(PROPDMG * 1000000,0)/1000000
           end as PropertyDamageAmt
        , CROPDMG
        , CROPDMGEXP
        , case 
                when upper(CROPDMGEXP) = 'B' then round(CROPDMG * 1000000000,0)/1000000
                when upper(CROPDMGEXP) = 'M' then round(CROPDMG * 1000000,0)/1000000
                when upper(CROPDMGEXP) = 'K' then round(CROPDMG * 1000,0)/1000000
                when upper(CROPDMGEXP) = 'H' then round(CROPDMG * 100,0)/1000000
                when upper(CROPDMGEXP) = '?' then round(CROPDMG * 0,0)/1000000
           else round(CROPDMG * 1000000,0)/1000000
           end as CropDamageAmt
        , case 
                when upper(PROPDMGEXP) not in ('B', 'M', 'K', 'H') then 'Low'
                when upper(CROPDMGEXP) not in ('B', 'M', 'K', 'H') then 'Low'
          else 'High'
          end as reliability
        , REMARKS   
         from sd0
         where REFNUM <> 605943 "          )

# save cleaned subset
saveRDS(sd1, "./proj2/sd1.rds")
```
  


 
         
### Results by Event Group


#### Population Health Statistics by Event Group



```r
pop_fatalities <- 
        sd1 %>% 
        group_by(EnvTypeGroup) %>% 
        summarise(n = n()
                  , Fatalities.Count = sum(FATALITIES) 
                  , Fatalities.Mean = mean(FATALITIES)
                  , Fatalities.Median = median(FATALITIES)
                  , Fatalities.Min = min(FATALITIES)
                  , Fatalities.Max = max(FATALITIES)        ) %>% 
        arrange(desc(Fatalities.Count))
```


The fatalities from 1996 - 2011 are ranked as follows:

```r
fatalities <- xtable( pop_fatalities[pop_fatalities$Fatalities.Count != 0,], digits =  c(0,0,0,0,0,0,0,0) )
print(fatalities, type="html", include.rownames=FALSE)
```

<!-- html table generated in R 3.1.3 by xtable 1.8-0 package -->
<!-- Thu Nov 19 17:07:49 2015 -->
<table border=1>
<tr> <th> EnvTypeGroup </th> <th> n </th> <th> Fatalities.Count </th> <th> Fatalities.Mean </th> <th> Fatalities.Median </th> <th> Fatalities.Min </th> <th> Fatalities.Max </th>  </tr>
  <tr> <td> Heat </td> <td align="right"> 2760 </td> <td align="right"> 2036 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 99 </td> </tr>
  <tr> <td> Tornado/Waterspout </td> <td align="right"> 32617 </td> <td align="right"> 1513 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 158 </td> </tr>
  <tr> <td> Flooding </td> <td align="right"> 78796 </td> <td align="right"> 1331 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 20 </td> </tr>
  <tr> <td> Wind </td> <td align="right"> 235508 </td> <td align="right"> 763 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 8 </td> </tr>
  <tr> <td> Lightning </td> <td align="right"> 13203 </td> <td align="right"> 650 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 4 </td> </tr>
  <tr> <td> Rip Current </td> <td align="right"> 734 </td> <td align="right"> 542 </td> <td align="right"> 1 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 6 </td> </tr>
  <tr> <td> Wintery Mix </td> <td align="right"> 37349 </td> <td align="right"> 469 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 8 </td> </tr>
  <tr> <td> Cold </td> <td align="right"> 4083 </td> <td align="right"> 398 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 14 </td> </tr>
  <tr> <td> Avalanche </td> <td align="right"> 378 </td> <td align="right"> 223 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 6 </td> </tr>
  <tr> <td> Coastal Flooding </td> <td align="right"> 2351 </td> <td align="right"> 170 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 11 </td> </tr>
  <tr> <td> Hurricane </td> <td align="right"> 273 </td> <td align="right"> 125 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 15 </td> </tr>
  <tr> <td> Rain </td> <td align="right"> 11656 </td> <td align="right"> 94 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 19 </td> </tr>
  <tr> <td> Fire </td> <td align="right"> 4199 </td> <td align="right"> 87 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 14 </td> </tr>
  <tr> <td> Blizzard </td> <td align="right"> 2636 </td> <td align="right"> 70 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 7 </td> </tr>
  <tr> <td> Fog </td> <td align="right"> 1728 </td> <td align="right"> 69 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 11 </td> </tr>
  <tr> <td> Tropical Storm </td> <td align="right"> 682 </td> <td align="right"> 57 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 22 </td> </tr>
  <tr> <td> Landslide </td> <td align="right"> 620 </td> <td align="right"> 43 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 14 </td> </tr>
  <tr> <td> Tsunami </td> <td align="right"> 20 </td> <td align="right"> 33 </td> <td align="right"> 2 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 32 </td> </tr>
  <tr> <td> Thunderstorm </td> <td align="right"> 12183 </td> <td align="right"> 22 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 5 </td> </tr>
  <tr> <td> Heavy Seas </td> <td align="right"> 15 </td> <td align="right"> 14 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 4 </td> </tr>
  <tr> <td> Dust Storm </td> <td align="right"> 557 </td> <td align="right"> 13 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 3 </td> </tr>
  <tr> <td> Hail </td> <td align="right"> 208217 </td> <td align="right"> 7 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1 </td> </tr>
  <tr> <td> Drowning </td> <td align="right"> 1 </td> <td align="right"> 1 </td> <td align="right"> 1 </td> <td align="right"> 1 </td> <td align="right"> 1 </td> <td align="right"> 1 </td> </tr>
  <tr> <td> Marine Mishap </td> <td align="right"> 1 </td> <td align="right"> 1 </td> <td align="right"> 1 </td> <td align="right"> 1 </td> <td align="right"> 1 </td> <td align="right"> 1 </td> </tr>
   </table>




```r
pop_fatalities_yr <- 
        sd1 %>% 
        mutate(Year = year(BGN_DATE)) %>% 
        group_by(Year, EnvTypeGroup) %>% 
        summarise(n = n()
                  , Fatalities.Count = sum(FATALITIES) ) %>%
        arrange(Year) %>%
        filter(EnvTypeGroup %in% c("Tornado/Waterspout", "Heat", "Flooding" ))
```




```r
ggplot( data = pop_fatalities_yr
        , aes(x = Year
        , y = Fatalities.Count , colour = EnvTypeGroup)) +
        geom_point() + 
        scale_x_continuous(breaks=seq(1996, 2011, 1)) +
        facet_grid(EnvTypeGroup ~ .) +
        geom_line()  + 
        theme(legend.position = "bottom", plot.background = element_rect(fill = "grey")) +
        ggtitle("Figure 1: Fatalities by Event Groups Since 1996")
```

![Figure 1: Fatalities](PA2_template_files/figure-html/FatalitiesFigure-1.png) 





```r
pop_injuries <- 
        sd1 %>% 
        group_by(EnvTypeGroup) %>% 
        summarise(n = n()
                  , Injuries.Count = sum(INJURIES) 
                  , Injuries.Mean = mean(INJURIES)
                  , Injuries.Median = median(INJURIES)
                  , Injuries.Min = min(INJURIES)
                  , Injuries.Max = max(INJURIES)    ) %>% 
        arrange(desc(Injuries.Count))
```


The injuries from  1996 - 2011 are ranked as follows:


```r
injuries <- xtable(pop_injuries[pop_injuries$Injuries.Count != 0,], digits = c(0,0,0,0,0,0,0,0) )
print(injuries, type="html", include.rownames=FALSE)
```

<!-- html table generated in R 3.1.3 by xtable 1.8-0 package -->
<!-- Thu Nov 19 17:07:51 2015 -->
<table border=1>
<tr> <th> EnvTypeGroup </th> <th> n </th> <th> Injuries.Count </th> <th> Injuries.Mean </th> <th> Injuries.Median </th> <th> Injuries.Min </th> <th> Injuries.Max </th>  </tr>
  <tr> <td> Tornado/Waterspout </td> <td align="right"> 32617 </td> <td align="right"> 20670 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1150 </td> </tr>
  <tr> <td> Flooding </td> <td align="right"> 78796 </td> <td align="right"> 8512 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 800 </td> </tr>
  <tr> <td> Heat </td> <td align="right"> 2760 </td> <td align="right"> 7702 </td> <td align="right"> 3 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 519 </td> </tr>
  <tr> <td> Wind </td> <td align="right"> 235508 </td> <td align="right"> 6638 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 70 </td> </tr>
  <tr> <td> Lightning </td> <td align="right"> 13203 </td> <td align="right"> 4140 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 51 </td> </tr>
  <tr> <td> Wintery Mix </td> <td align="right"> 37349 </td> <td align="right"> 3223 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 165 </td> </tr>
  <tr> <td> Fire </td> <td align="right"> 4199 </td> <td align="right"> 1458 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 90 </td> </tr>
  <tr> <td> Hurricane </td> <td align="right"> 273 </td> <td align="right"> 1328 </td> <td align="right"> 5 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 780 </td> </tr>
  <tr> <td> Fog </td> <td align="right"> 1728 </td> <td align="right"> 855 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 78 </td> </tr>
  <tr> <td> Hail </td> <td align="right"> 208217 </td> <td align="right"> 723 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 88 </td> </tr>
  <tr> <td> Rip Current </td> <td align="right"> 734 </td> <td align="right"> 503 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 35 </td> </tr>
  <tr> <td> Dust Storm </td> <td align="right"> 557 </td> <td align="right"> 415 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 40 </td> </tr>
  <tr> <td> Blizzard </td> <td align="right"> 2636 </td> <td align="right"> 385 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 150 </td> </tr>
  <tr> <td> Tropical Storm </td> <td align="right"> 682 </td> <td align="right"> 338 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 200 </td> </tr>
  <tr> <td> Coastal Flooding </td> <td align="right"> 2351 </td> <td align="right"> 298 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 55 </td> </tr>
  <tr> <td> Rain </td> <td align="right"> 11656 </td> <td align="right"> 234 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 32 </td> </tr>
  <tr> <td> Avalanche </td> <td align="right"> 378 </td> <td align="right"> 156 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 7 </td> </tr>
  <tr> <td> Cold </td> <td align="right"> 4083 </td> <td align="right"> 135 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 29 </td> </tr>
  <tr> <td> Tsunami </td> <td align="right"> 20 </td> <td align="right"> 129 </td> <td align="right"> 6 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 129 </td> </tr>
  <tr> <td> Thunderstorm </td> <td align="right"> 12183 </td> <td align="right"> 59 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 13 </td> </tr>
  <tr> <td> Landslide </td> <td align="right"> 620 </td> <td align="right"> 55 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 10 </td> </tr>
  <tr> <td> Heavy Seas </td> <td align="right"> 15 </td> <td align="right"> 8 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 5 </td> </tr>
  <tr> <td> Drought </td> <td align="right"> 2514 </td> <td align="right"> 4 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 4 </td> </tr>
  <tr> <td> Unknown </td> <td align="right"> 126 </td> <td align="right"> 4 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 4 </td> </tr>
  <tr> <td> Marine Mishap </td> <td align="right"> 1 </td> <td align="right"> 2 </td> <td align="right"> 2 </td> <td align="right"> 2 </td> <td align="right"> 2 </td> <td align="right"> 2 </td> </tr>
   </table>




```r
pop_injuries_yr <- 
        sd1 %>% 
        mutate(Year = year(BGN_DATE)) %>% 
        group_by(Year, EnvTypeGroup) %>% 
        summarise(n = n()
                  , Injuries.Count = sum(INJURIES) ) %>%
        arrange(Year) %>%
        filter(EnvTypeGroup %in% c("Tornado/Waterspout", "Heat", "Flooding", "Wind", "Wintery Mix", "Lightning", "Lighting" ))
```





```r
ggplot( data = pop_injuries_yr
        , aes(x = Year
        , y = Injuries.Count, colour = EnvTypeGroup)) +
        geom_point() + 
        scale_x_continuous(breaks=seq(1996, 2011, 1)) +
        facet_grid(EnvTypeGroup ~ .) +
        geom_line() + 
        theme(legend.position = "bottom", plot.background = element_rect(fill = "grey")) +
        ggtitle("Figure 2: Injuries by Event Groups Since 1996")
```

![Figure 2: Injuries](PA2_template_files/figure-html/InjuriesFigure-1.png) 




#### Economic Impact Statistics (displayed in millions)


```r
cost_PropertyDamage <- 
        sd1 %>% 
        group_by(EnvTypeGroup) %>% 
        summarise(n = n()
                  , PropertyDamageAmt.Sum = sum(PropertyDamageAmt) 
                  , PropertyDamageAmt.Mean = mean(PropertyDamageAmt)
                  , PropertyDamageAmt.Median = median(PropertyDamageAmt)
                  , PropertyDamageAmt.Min_ = min(PropertyDamageAmt)
                  , PropertyDamageAmt.Max = max(PropertyDamageAmt)   ) %>% 
        arrange(desc(PropertyDamageAmt.Sum))

cost_PropertyDamage_High <- 
        sd1 %>% 
        filter(reliability == "High")  %>% 
        group_by(EnvTypeGroup) %>% 
        summarise(n = n()
                  , PropertyDamageAmt.Sum = sum(PropertyDamageAmt) 
                  , PropertyDamageAmt.Mean = mean(PropertyDamageAmt)
                  , PropertyDamageAmt.Median = median(PropertyDamageAmt)
                  , PropertyDamageAmt.Min_ = min(PropertyDamageAmt)
                  , PropertyDamageAmt.Max = max(PropertyDamageAmt)   ) %>% 
        arrange(desc(PropertyDamageAmt.Sum))


cost_CropDamage <- 
        sd1 %>% 
        group_by(EnvTypeGroup) %>% 
        summarise(n_date = n()
                  , CropDamageAmt.Sum = sum(CropDamageAmt) 
                  , CropDamageAmt.Mean = mean(CropDamageAmt)
                  , CropDamageAmt.Median = median(CropDamageAmt)
                  , CropDamageAmt.Min = min(CropDamageAmt)
                  , CropDamageAmt.Max = max(CropDamageAmt)   ) %>% 
        arrange(desc(CropDamageAmt.Sum))

cost_CropDamage_High <- 
        sd1 %>% 
        filter(reliability == "High")  %>% 
        group_by(EnvTypeGroup) %>% 
        summarise(n_date = n()
                  , CropDamageAmt.Sum = sum(CropDamageAmt) 
                  , CropDamageAmt.Mean = mean(CropDamageAmt)
                  , CropDamageAmt.Median = median(CropDamageAmt)
                  , CropDamageAmt.Min = min(CropDamageAmt)
                  , CropDamageAmt.Max = max(CropDamageAmt)   ) %>% 
        arrange(desc(CropDamageAmt.Sum))
```


Total property damages from 1996 - 2011 (assuming millions for ambiguous cost units) are as follows:


```r
PropertyDamage <- xtable(cost_PropertyDamage[cost_PropertyDamage$PropertyDamageAmt.Sum != 0,], digits = c(0,0,0,0,0,0,0,0) ) 
print(PropertyDamage, type="html", include.rownames=FALSE)
```

<!-- html table generated in R 3.1.3 by xtable 1.8-0 package -->
<!-- Thu Nov 19 17:07:55 2015 -->
<table border=1>
<tr> <th> EnvTypeGroup </th> <th> n </th> <th> PropertyDamageAmt.Sum </th> <th> PropertyDamageAmt.Mean </th> <th> PropertyDamageAmt.Median </th> <th> PropertyDamageAmt.Min_ </th> <th> PropertyDamageAmt.Max </th>  </tr>
  <tr> <td> Hurricane </td> <td align="right"> 273 </td> <td align="right"> 81719 </td> <td align="right"> 299 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 16930 </td> </tr>
  <tr> <td> Coastal Flooding </td> <td align="right"> 2351 </td> <td align="right"> 48347 </td> <td align="right"> 21 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 31300 </td> </tr>
  <tr> <td> Flooding </td> <td align="right"> 78796 </td> <td align="right"> 44359 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 3000 </td> </tr>
  <tr> <td> Tornado/Waterspout </td> <td align="right"> 32617 </td> <td align="right"> 24623 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 2800 </td> </tr>
  <tr> <td> Hail </td> <td align="right"> 208217 </td> <td align="right"> 14595 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1800 </td> </tr>
  <tr> <td> Wind </td> <td align="right"> 235508 </td> <td align="right"> 13338 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1300 </td> </tr>
  <tr> <td> Fire </td> <td align="right"> 4199 </td> <td align="right"> 7761 </td> <td align="right"> 2 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1500 </td> </tr>
  <tr> <td> Tropical Storm </td> <td align="right"> 682 </td> <td align="right"> 7642 </td> <td align="right"> 11 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 5150 </td> </tr>
  <tr> <td> Wintery Mix </td> <td align="right"> 37349 </td> <td align="right"> 5889 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 750 </td> </tr>
  <tr> <td> Drought </td> <td align="right"> 2514 </td> <td align="right"> 1046 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 645 </td> </tr>
  <tr> <td> Lightning </td> <td align="right"> 13203 </td> <td align="right"> 743 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 15 </td> </tr>
  <tr> <td> Rain </td> <td align="right"> 11656 </td> <td align="right"> 599 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 70 </td> </tr>
  <tr> <td> Blizzard </td> <td align="right"> 2636 </td> <td align="right"> 526 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 102 </td> </tr>
  <tr> <td> Landslide </td> <td align="right"> 620 </td> <td align="right"> 327 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 56 </td> </tr>
  <tr> <td> Tsunami </td> <td align="right"> 20 </td> <td align="right"> 144 </td> <td align="right"> 7 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 81 </td> </tr>
  <tr> <td> Cold </td> <td align="right"> 4083 </td> <td align="right"> 51 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 8 </td> </tr>
  <tr> <td> Fog </td> <td align="right"> 1728 </td> <td align="right"> 20 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 2 </td> </tr>
  <tr> <td> Thunderstorm </td> <td align="right"> 12183 </td> <td align="right"> 16 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 8 </td> </tr>
  <tr> <td> Heat </td> <td align="right"> 2760 </td> <td align="right"> 9 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 3 </td> </tr>
  <tr> <td> Dust Storm </td> <td align="right"> 557 </td> <td align="right"> 6 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1 </td> </tr>
  <tr> <td> Avalanche </td> <td align="right"> 378 </td> <td align="right"> 4 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1 </td> </tr>
  <tr> <td> Tropical Depression </td> <td align="right"> 60 </td> <td align="right"> 2 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1 </td> </tr>
  <tr> <td> Dam Failure </td> <td align="right"> 4 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1 </td> </tr>
  <tr> <td> Seiche </td> <td align="right"> 21 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1 </td> </tr>
  <tr> <td> Volcano </td> <td align="right"> 30 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> </tr>
  <tr> <td> Low Tide </td> <td align="right"> 174 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> </tr>
  <tr> <td> Rip Current </td> <td align="right"> 734 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> </tr>
  <tr> <td> Unknown </td> <td align="right"> 126 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> </tr>
  <tr> <td> Marine Mishap </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> </tr>
  <tr> <td> Heavy Seas </td> <td align="right"> 15 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> </tr>
   </table>


Retaining only the records with high reliabiltiy cost estimates doesn't signnificantly change the relative rankings on property damage: 


```r
PropertyDamageHigh <- xtable(cost_PropertyDamage_High[cost_PropertyDamage_High$PropertyDamageAmt.Sum != 0,], digits = c(0,0,0,0,0,0,0,0) )
print(PropertyDamageHigh, type="html", include.rownames=FALSE)
```

<!-- html table generated in R 3.1.3 by xtable 1.8-0 package -->
<!-- Thu Nov 19 17:07:55 2015 -->
<table border=1>
<tr> <th> EnvTypeGroup </th> <th> n </th> <th> PropertyDamageAmt.Sum </th> <th> PropertyDamageAmt.Mean </th> <th> PropertyDamageAmt.Median </th> <th> PropertyDamageAmt.Min_ </th> <th> PropertyDamageAmt.Max </th>  </tr>
  <tr> <td> Hurricane </td> <td align="right"> 105 </td> <td align="right"> 36472 </td> <td align="right"> 347 </td> <td align="right"> 11 </td> <td align="right"> 0 </td> <td align="right"> 5880 </td> </tr>
  <tr> <td> Flooding </td> <td align="right"> 34976 </td> <td align="right"> 24703 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 2000 </td> </tr>
  <tr> <td> Tornado/Waterspout </td> <td align="right"> 12608 </td> <td align="right"> 16036 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 2800 </td> </tr>
  <tr> <td> Hail </td> <td align="right"> 79215 </td> <td align="right"> 7602 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1800 </td> </tr>
  <tr> <td> Wind </td> <td align="right"> 101314 </td> <td align="right"> 6591 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 929 </td> </tr>
  <tr> <td> Coastal Flooding </td> <td align="right"> 1071 </td> <td align="right"> 4894 </td> <td align="right"> 5 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 4000 </td> </tr>
  <tr> <td> Fire </td> <td align="right"> 1818 </td> <td align="right"> 3547 </td> <td align="right"> 2 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1040 </td> </tr>
  <tr> <td> Wintery Mix </td> <td align="right"> 21030 </td> <td align="right"> 2082 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 750 </td> </tr>
  <tr> <td> Tropical Storm </td> <td align="right"> 412 </td> <td align="right"> 1045 </td> <td align="right"> 3 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 530 </td> </tr>
  <tr> <td> Lightning </td> <td align="right"> 4154 </td> <td align="right"> 313 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 15 </td> </tr>
  <tr> <td> Rain </td> <td align="right"> 5253 </td> <td align="right"> 274 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 70 </td> </tr>
  <tr> <td> Drought </td> <td align="right"> 1399 </td> <td align="right"> 234 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 23 </td> </tr>
  <tr> <td> Landslide </td> <td align="right"> 320 </td> <td align="right"> 150 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 48 </td> </tr>
  <tr> <td> Tsunami </td> <td align="right"> 19 </td> <td align="right"> 144 </td> <td align="right"> 8 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 81 </td> </tr>
  <tr> <td> Blizzard </td> <td align="right"> 1739 </td> <td align="right"> 39 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 6 </td> </tr>
  <tr> <td> Cold </td> <td align="right"> 2351 </td> <td align="right"> 22 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 5 </td> </tr>
  <tr> <td> Dust Storm </td> <td align="right"> 253 </td> <td align="right"> 4 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1 </td> </tr>
  <tr> <td> Heat </td> <td align="right"> 1334 </td> <td align="right"> 3 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 2 </td> </tr>
  <tr> <td> Fog </td> <td align="right"> 921 </td> <td align="right"> 3 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1 </td> </tr>
  <tr> <td> Avalanche </td> <td align="right"> 154 </td> <td align="right"> 2 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1 </td> </tr>
  <tr> <td> Tropical Depression </td> <td align="right"> 27 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1 </td> </tr>
  <tr> <td> Thunderstorm </td> <td align="right"> 5816 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> </tr>
  <tr> <td> Low Tide </td> <td align="right"> 174 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> </tr>
  <tr> <td> Seiche </td> <td align="right"> 7 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> </tr>
  <tr> <td> Rip Current </td> <td align="right"> 277 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> </tr>
   </table>


Total crop damages from 1996 - 2011 (assuming millions for ambiguous cost units) are as follows:


```r
CropDamage <- xtable(cost_CropDamage[cost_CropDamage$CropDamageAmt.Sum != 0,], digits = c(0,0,0,0,0,0,0,0) ) 
print(CropDamage, type="html", include.rownames=FALSE)
```

<!-- html table generated in R 3.1.3 by xtable 1.8-0 package -->
<!-- Thu Nov 19 17:07:55 2015 -->
<table border=1>
<tr> <th> EnvTypeGroup </th> <th> n_date </th> <th> CropDamageAmt.Sum </th> <th> CropDamageAmt.Mean </th> <th> CropDamageAmt.Median </th> <th> CropDamageAmt.Min </th> <th> CropDamageAmt.Max </th>  </tr>
  <tr> <td> Drought </td> <td align="right"> 2514 </td> <td align="right"> 13368 </td> <td align="right"> 5 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1000 </td> </tr>
  <tr> <td> Flooding </td> <td align="right"> 78796 </td> <td align="right"> 6316 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 500 </td> </tr>
  <tr> <td> Hurricane </td> <td align="right"> 273 </td> <td align="right"> 5350 </td> <td align="right"> 20 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1510 </td> </tr>
  <tr> <td> Cold </td> <td align="right"> 4083 </td> <td align="right"> 2726 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 596 </td> </tr>
  <tr> <td> Hail </td> <td align="right"> 208217 </td> <td align="right"> 2497 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 70 </td> </tr>
  <tr> <td> Wind </td> <td align="right"> 235508 </td> <td align="right"> 1716 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 175 </td> </tr>
  <tr> <td> Rain </td> <td align="right"> 11656 </td> <td align="right"> 740 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 200 </td> </tr>
  <tr> <td> Tropical Storm </td> <td align="right"> 682 </td> <td align="right"> 678 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 200 </td> </tr>
  <tr> <td> Heat </td> <td align="right"> 2760 </td> <td align="right"> 493 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 492 </td> </tr>
  <tr> <td> Fire </td> <td align="right"> 4199 </td> <td align="right"> 402 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 90 </td> </tr>
  <tr> <td> Tornado/Waterspout </td> <td align="right"> 32617 </td> <td align="right"> 283 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 70 </td> </tr>
  <tr> <td> Wintery Mix </td> <td align="right"> 37349 </td> <td align="right"> 114 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 65 </td> </tr>
  <tr> <td> Landslide </td> <td align="right"> 620 </td> <td align="right"> 20 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 20 </td> </tr>
  <tr> <td> Blizzard </td> <td align="right"> 2636 </td> <td align="right"> 7 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 7 </td> </tr>
  <tr> <td> Lightning </td> <td align="right"> 13203 </td> <td align="right"> 7 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 3 </td> </tr>
  <tr> <td> Dust Storm </td> <td align="right"> 557 </td> <td align="right"> 3 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 2 </td> </tr>
  <tr> <td> Unknown </td> <td align="right"> 126 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> </tr>
  <tr> <td> Coastal Flooding </td> <td align="right"> 2351 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1 </td> </tr>
  <tr> <td> Thunderstorm </td> <td align="right"> 12183 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> </tr>
  <tr> <td> Tsunami </td> <td align="right"> 20 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> </tr>
   </table>


Retaining only the records with high reliabiltiy cost extimates DOES signnificantly change the relative rankings for crop damages. Drought is now # 12 in the rankings: 


```r
CropDamageHigh <- xtable(cost_CropDamage_High[cost_CropDamage$CropDamageAmt.Sum != 0,], digits = c(0,0,0,0,0,0,0,0) ) 
print(CropDamageHigh, type="html", include.rownames=FALSE)
```

<!-- html table generated in R 3.1.3 by xtable 1.8-0 package -->
<!-- Thu Nov 19 17:07:55 2015 -->
<table border=1>
<tr> <th> EnvTypeGroup </th> <th> n_date </th> <th> CropDamageAmt.Sum </th> <th> CropDamageAmt.Mean </th> <th> CropDamageAmt.Median </th> <th> CropDamageAmt.Min </th> <th> CropDamageAmt.Max </th>  </tr>
  <tr> <td> Flooding </td> <td align="right"> 34976 </td> <td align="right"> 6100 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 500 </td> </tr>
  <tr> <td> Hurricane </td> <td align="right"> 105 </td> <td align="right"> 5297 </td> <td align="right"> 50 </td> <td align="right"> 2 </td> <td align="right"> 0 </td> <td align="right"> 1510 </td> </tr>
  <tr> <td> Hail </td> <td align="right"> 79215 </td> <td align="right"> 1730 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 70 </td> </tr>
  <tr> <td> Drought </td> <td align="right"> 1399 </td> <td align="right"> 1635 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 344 </td> </tr>
  <tr> <td> Wind </td> <td align="right"> 101314 </td> <td align="right"> 1617 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 175 </td> </tr>
  <tr> <td> Cold </td> <td align="right"> 2351 </td> <td align="right"> 935 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 286 </td> </tr>
  <tr> <td> Heat </td> <td align="right"> 1334 </td> <td align="right"> 492 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 492 </td> </tr>
  <tr> <td> Tropical Storm </td> <td align="right"> 412 </td> <td align="right"> 451 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 102 </td> </tr>
  <tr> <td> Fire </td> <td align="right"> 1818 </td> <td align="right"> 285 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 80 </td> </tr>
  <tr> <td> Tornado/Waterspout </td> <td align="right"> 12608 </td> <td align="right"> 278 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 70 </td> </tr>
  <tr> <td> Wintery Mix </td> <td align="right"> 21030 </td> <td align="right"> 108 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 65 </td> </tr>
  <tr> <td> Rain </td> <td align="right"> 5253 </td> <td align="right"> 61 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 20 </td> </tr>
  <tr> <td> Landslide </td> <td align="right"> 320 </td> <td align="right"> 20 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 20 </td> </tr>
  <tr> <td> Blizzard </td> <td align="right"> 1739 </td> <td align="right"> 7 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 7 </td> </tr>
  <tr> <td> Lightning </td> <td align="right"> 4154 </td> <td align="right"> 5 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 3 </td> </tr>
  <tr> <td> Dust Storm </td> <td align="right"> 253 </td> <td align="right"> 2 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 2 </td> </tr>
  <tr> <td> Coastal Flooding </td> <td align="right"> 1071 </td> <td align="right"> 1 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 1 </td> </tr>
  <tr> <td> Thunderstorm </td> <td align="right"> 5816 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> </tr>
  <tr> <td> Tsunami </td> <td align="right"> 19 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> </tr>
  <tr> <td> Avalanche </td> <td align="right"> 154 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> <td align="right"> 0 </td> </tr>
   </table>




### Conclusion

The United States population health is impacted by weather events in terms of both fatalities and injuries.

The most fatalities occur due to tornados. Heat is the second most prevelant cause of fatalities.

More injuries are incurred as a result of tornados more than any other event group.

The greatest economic consequences are more difficult to determine due to the inconsistency of the cost unit coding. If ambiguous units are assumed to be in millions, extremes of water- drought and flooding, with drought being more than twice as expensive as flooding in the crop damage rankings.

For property damages, hurricanes, flooding and coastal flooding are the most expensive event groups.

Local emergency management personal should allocate resources for early warning systems to allow people to seek shelter before tornados, hurricanes and high wind events to reduce the possibility of injury and death from these events. . 
 
### Appendix

Additional information can be found at these websites:

https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf

https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf


#### EVTYPE Grouping Table
The mapping table needs to be extracted from the HTML/PDF and created locally to reproduce this analysis.


```r
print(xtable(envTypeMapping) ,  type="html" , include.rownames=FALSE)                                                     
```

<!-- html table generated in R 3.1.3 by xtable 1.8-0 package -->
<!-- Thu Nov 19 17:07:55 2015 -->
<table border=1>
<tr> <th> ENVTYPE </th> <th> EnvTypeGroup </th>  </tr>
  <tr> <td> AVALANCE </td> <td> Avalanche </td> </tr>
  <tr> <td> AVALANCHE </td> <td> Avalanche </td> </tr>
  <tr> <td> HEAVY SNOW/BLIZZARD/AVALANCHE </td> <td> Avalanche </td> </tr>
  <tr> <td> BLIZZARD </td> <td> Blizzard </td> </tr>
  <tr> <td> BLIZZARD AND EXTREME WIND CHIL </td> <td> Blizzard </td> </tr>
  <tr> <td> BLIZZARD AND HEAVY SNOW </td> <td> Blizzard </td> </tr>
  <tr> <td> Blizzard Summary </td> <td> Blizzard </td> </tr>
  <tr> <td> BLIZZARD WEATHER </td> <td> Blizzard </td> </tr>
  <tr> <td> BLIZZARD/FREEZING RAIN </td> <td> Blizzard </td> </tr>
  <tr> <td> BLIZZARD/HEAVY SNOW </td> <td> Blizzard </td> </tr>
  <tr> <td> BLIZZARD/HIGH WIND </td> <td> Blizzard </td> </tr>
  <tr> <td> BLIZZARD/WINTER STORM </td> <td> Blizzard </td> </tr>
  <tr> <td> BLOWING DUST </td> <td> Blizzard </td> </tr>
  <tr> <td> BLOWING SNOW </td> <td> Blizzard </td> </tr>
  <tr> <td> GROUND BLIZZARD </td> <td> Blizzard </td> </tr>
  <tr> <td> HIGH WIND/ BLIZZARD </td> <td> Blizzard </td> </tr>
  <tr> <td> HIGH WIND/BLIZZARD </td> <td> Blizzard </td> </tr>
  <tr> <td> HIGH WIND/BLIZZARD/FREEZING RA </td> <td> Blizzard </td> </tr>
  <tr> <td>    HIGH SURF ADVISORY </td> <td> Coastal Flooding </td> </tr>
  <tr> <td>  COASTAL FLOOD </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> ASTRONOMICAL HIGH TIDE </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> BEACH EROSIN </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> Beach Erosion </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> BEACH EROSION </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> BEACH EROSION/COASTAL FLOOD </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> BEACH FLOOD </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> BLOW-OUT TIDE </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> BLOW-OUT TIDES </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> COASTAL  FLOODING/EROSION </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> COASTAL EROSION </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> COASTAL FLOOD </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> Coastal Flood </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> COASTAL FLOODING </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> Coastal Flooding </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> coastal flooding </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> COASTAL FLOODING/EROSION </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> COASTAL STORM </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> Coastal Storm </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> COASTAL SURGE </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> COASTAL/TIDAL FLOOD </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> COASTALFLOOD </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> COASTALSTORM </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> CSTL FLOODING/EROSION </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> Erosion/Cstl Flood </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> HAZARDOUS SURF </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> HEAVY SURF </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> Heavy Surf </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> Heavy surf and wind </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> HEAVY SURF COASTAL FLOODING </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> HEAVY SURF/HIGH SURF </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> HEAVY SWELLS </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> HIGH SEAS </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> HIGH SURF </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> High Surf </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> HIGH SURF ADVISORIES </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> HIGH SURF ADVISORY </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> HIGH TIDES </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> HIGH WATER </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> HIGH WIND AND HIGH TIDES </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> HIGH WIND/SEAS </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> HIGH WINDS/COASTAL FLOOD </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> STORM SURGE </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> STORM SURGE/TIDE </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> TIDAL FLOOD </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> Tidal Flooding </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> TIDAL FLOODING </td> <td> Coastal Flooding </td> </tr>
  <tr> <td> AGRICULTURAL FREEZE </td> <td> Cold </td> </tr>
  <tr> <td> BITTER WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td> BITTER WIND CHILL TEMPERATURES </td> <td> Cold </td> </tr>
  <tr> <td> COLD </td> <td> Cold </td> </tr>
  <tr> <td> Cold </td> <td> Cold </td> </tr>
  <tr> <td> Cold and Frost </td> <td> Cold </td> </tr>
  <tr> <td> COLD AND FROST </td> <td> Cold </td> </tr>
  <tr> <td> COLD AND SNOW </td> <td> Cold </td> </tr>
  <tr> <td> COLD AND WET CONDITIONS </td> <td> Cold </td> </tr>
  <tr> <td> Cold Temperature </td> <td> Cold </td> </tr>
  <tr> <td> COLD TEMPERATURES </td> <td> Cold </td> </tr>
  <tr> <td> COLD WAVE </td> <td> Cold </td> </tr>
  <tr> <td> COLD WEATHER </td> <td> Cold </td> </tr>
  <tr> <td> COLD WIND CHILL TEMPERATURES </td> <td> Cold </td> </tr>
  <tr> <td> COLD/WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td> COLD/WINDS </td> <td> Cold </td> </tr>
  <tr> <td> COOL AND WET </td> <td> Cold </td> </tr>
  <tr> <td> COOL SPELL </td> <td> Cold </td> </tr>
  <tr> <td> DAMAGING FREEZE </td> <td> Cold </td> </tr>
  <tr> <td> Damaging Freeze </td> <td> Cold </td> </tr>
  <tr> <td> EARLY FREEZE </td> <td> Cold </td> </tr>
  <tr> <td> EARLY FROST </td> <td> Cold </td> </tr>
  <tr> <td> Early Frost </td> <td> Cold </td> </tr>
  <tr> <td> Excessive Cold </td> <td> Cold </td> </tr>
  <tr> <td> Extended Cold </td> <td> Cold </td> </tr>
  <tr> <td> EXTREME COLD </td> <td> Cold </td> </tr>
  <tr> <td> Extreme Cold </td> <td> Cold </td> </tr>
  <tr> <td> EXTREME COLD/WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td> EXTREME WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td> EXTREME WIND CHILL/BLOWING SNO </td> <td> Cold </td> </tr>
  <tr> <td> EXTREME WIND CHILLS </td> <td> Cold </td> </tr>
  <tr> <td> EXTREME WINDCHILL </td> <td> Cold </td> </tr>
  <tr> <td> EXTREME WINDCHILL TEMPERATURES </td> <td> Cold </td> </tr>
  <tr> <td> EXTREME/RECORD COLD </td> <td> Cold </td> </tr>
  <tr> <td> FIRST FROST </td> <td> Cold </td> </tr>
  <tr> <td> FOG AND COLD TEMPERATURES </td> <td> Cold </td> </tr>
  <tr> <td> FREEZE </td> <td> Cold </td> </tr>
  <tr> <td> Freeze </td> <td> Cold </td> </tr>
  <tr> <td> FROST </td> <td> Cold </td> </tr>
  <tr> <td> Frost </td> <td> Cold </td> </tr>
  <tr> <td> Frost/Freeze </td> <td> Cold </td> </tr>
  <tr> <td> FROST/FREEZE </td> <td> Cold </td> </tr>
  <tr> <td> FROST\FREEZE </td> <td> Cold </td> </tr>
  <tr> <td> HARD FREEZE </td> <td> Cold </td> </tr>
  <tr> <td> HIGH WIND/LOW WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td> HIGH WIND/WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td> HIGH WIND/WIND CHILL/BLIZZARD </td> <td> Cold </td> </tr>
  <tr> <td> HIGH WINDS AND WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td> HYPERTHERMIA/EXPOSURE </td> <td> Cold </td> </tr>
  <tr> <td> HYPOTHERMIA </td> <td> Cold </td> </tr>
  <tr> <td> Hypothermia/Exposure </td> <td> Cold </td> </tr>
  <tr> <td> HYPOTHERMIA/EXPOSURE </td> <td> Cold </td> </tr>
  <tr> <td> LATE FREEZE </td> <td> Cold </td> </tr>
  <tr> <td> LOW TEMPERATURE </td> <td> Cold </td> </tr>
  <tr> <td> LOW TEMPERATURE RECORD </td> <td> Cold </td> </tr>
  <tr> <td> LOW WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td> PROLONG COLD </td> <td> Cold </td> </tr>
  <tr> <td> Prolong Cold </td> <td> Cold </td> </tr>
  <tr> <td> PROLONG COLD/SNOW </td> <td> Cold </td> </tr>
  <tr> <td> RECORD  COLD </td> <td> Cold </td> </tr>
  <tr> <td> RECORD COLD </td> <td> Cold </td> </tr>
  <tr> <td> Record Cold </td> <td> Cold </td> </tr>
  <tr> <td> RECORD COLD AND HIGH WIND </td> <td> Cold </td> </tr>
  <tr> <td> RECORD COLD/FROST </td> <td> Cold </td> </tr>
  <tr> <td> RECORD COOL </td> <td> Cold </td> </tr>
  <tr> <td> RECORD LOW </td> <td> Cold </td> </tr>
  <tr> <td> SEVERE COLD </td> <td> Cold </td> </tr>
  <tr> <td> Temperature record </td> <td> Cold </td> </tr>
  <tr> <td> Unseasonable Cold </td> <td> Cold </td> </tr>
  <tr> <td> UNSEASONABLY COLD </td> <td> Cold </td> </tr>
  <tr> <td> UNSEASONABLY COOL </td> <td> Cold </td> </tr>
  <tr> <td> UNSEASONABLY COOL &amp; WET </td> <td> Cold </td> </tr>
  <tr> <td> UNSEASONAL LOW TEMP </td> <td> Cold </td> </tr>
  <tr> <td> UNUSUALLY COLD </td> <td> Cold </td> </tr>
  <tr> <td> WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td> WIND CHILL/HIGH WIND </td> <td> Cold </td> </tr>
  <tr> <td> DAM BREAK </td> <td> Dam Failure </td> </tr>
  <tr> <td> DAM FAILURE </td> <td> Dam Failure </td> </tr>
  <tr> <td> ABNORMALLY DRY </td> <td> Drought </td> </tr>
  <tr> <td> BELOW NORMAL PRECIPITATION </td> <td> Drought </td> </tr>
  <tr> <td> DRIEST MONTH </td> <td> Drought </td> </tr>
  <tr> <td> DROUGHT </td> <td> Drought </td> </tr>
  <tr> <td> DROUGHT/EXCESSIVE HEAT </td> <td> Drought </td> </tr>
  <tr> <td> DRY </td> <td> Drought </td> </tr>
  <tr> <td> DRY CONDITIONS </td> <td> Drought </td> </tr>
  <tr> <td> DRY PATTERN </td> <td> Drought </td> </tr>
  <tr> <td> DRY SPELL </td> <td> Drought </td> </tr>
  <tr> <td> DRY WEATHER </td> <td> Drought </td> </tr>
  <tr> <td> DRYNESS </td> <td> Drought </td> </tr>
  <tr> <td> EXCESSIVE HEAT/DROUGHT </td> <td> Drought </td> </tr>
  <tr> <td> EXCESSIVELY DRY </td> <td> Drought </td> </tr>
  <tr> <td> HEAT DROUGHT </td> <td> Drought </td> </tr>
  <tr> <td> HEAT WAVE DROUGHT </td> <td> Drought </td> </tr>
  <tr> <td> Record dry month </td> <td> Drought </td> </tr>
  <tr> <td> RECORD DRYNESS </td> <td> Drought </td> </tr>
  <tr> <td> RECORD LOW RAINFALL </td> <td> Drought </td> </tr>
  <tr> <td> UNSEASONABLY DRY </td> <td> Drought </td> </tr>
  <tr> <td> VERY DRY </td> <td> Drought </td> </tr>
  <tr> <td> WARM DRY CONDITIONS </td> <td> Drought </td> </tr>
  <tr> <td> DROWNING </td> <td> Drowning </td> </tr>
  <tr> <td> DUST DEVEL </td> <td> Dust Storm </td> </tr>
  <tr> <td> DUST DEVIL </td> <td> Dust Storm </td> </tr>
  <tr> <td> Dust Devil </td> <td> Dust Storm </td> </tr>
  <tr> <td> DUST DEVIL WATERSPOUT </td> <td> Dust Storm </td> </tr>
  <tr> <td> DUST STORM </td> <td> Dust Storm </td> </tr>
  <tr> <td> DUST STORM/HIGH WINDS </td> <td> Dust Storm </td> </tr>
  <tr> <td> DUSTSTORM </td> <td> Dust Storm </td> </tr>
  <tr> <td> HIGH WINDS DUST STORM </td> <td> Dust Storm </td> </tr>
  <tr> <td> SAHARAN DUST </td> <td> Dust Storm </td> </tr>
  <tr> <td> Saharan Dust </td> <td> Dust Storm </td> </tr>
  <tr> <td> BRUSH FIRE </td> <td> Fire </td> </tr>
  <tr> <td> BRUSH FIRES </td> <td> Fire </td> </tr>
  <tr> <td> DENSE SMOKE </td> <td> Fire </td> </tr>
  <tr> <td> FOREST FIRES </td> <td> Fire </td> </tr>
  <tr> <td> GRASS FIRES </td> <td> Fire </td> </tr>
  <tr> <td> RED FLAG FIRE WX </td> <td> Fire </td> </tr>
  <tr> <td> SMOKE </td> <td> Fire </td> </tr>
  <tr> <td> WILD FIRES </td> <td> Fire </td> </tr>
  <tr> <td> WILD/FOREST FIRE </td> <td> Fire </td> </tr>
  <tr> <td> WILD/FOREST FIRES </td> <td> Fire </td> </tr>
  <tr> <td> WILDFIRE </td> <td> Fire </td> </tr>
  <tr> <td> WILDFIRES </td> <td> Fire </td> </tr>
  <tr> <td>  FLASH FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> BREAKUP FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td> FLASH FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> FLASH FLOOD - HEAVY RAIN </td> <td> Flooding </td> </tr>
  <tr> <td> FLASH FLOOD FROM ICE JAMS </td> <td> Flooding </td> </tr>
  <tr> <td> FLASH FLOOD WINDS </td> <td> Flooding </td> </tr>
  <tr> <td> FLASH FLOOD/ </td> <td> Flooding </td> </tr>
  <tr> <td> FLASH FLOOD/ FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> FLASH FLOOD/ STREET </td> <td> Flooding </td> </tr>
  <tr> <td> FLASH FLOOD/FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> FLASH FLOOD/HEAVY RAIN </td> <td> Flooding </td> </tr>
  <tr> <td> FLASH FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td> FLASH FLOODING/FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> FLASH FLOODING/THUNDERSTORM WI </td> <td> Flooding </td> </tr>
  <tr> <td> FLASH FLOODS </td> <td> Flooding </td> </tr>
  <tr> <td> FLASH FLOOODING </td> <td> Flooding </td> </tr>
  <tr> <td> FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> Flood </td> <td> Flooding </td> </tr>
  <tr> <td> FLOOD &amp; HEAVY RAIN </td> <td> Flooding </td> </tr>
  <tr> <td> FLOOD FLASH </td> <td> Flooding </td> </tr>
  <tr> <td> FLOOD FLOOD/FLASH </td> <td> Flooding </td> </tr>
  <tr> <td> FLOOD WATCH/ </td> <td> Flooding </td> </tr>
  <tr> <td> FLOOD/FLASH </td> <td> Flooding </td> </tr>
  <tr> <td> FLOOD/FLASH FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> Flood/Flash Flood </td> <td> Flooding </td> </tr>
  <tr> <td> FLOOD/FLASH FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td> FLOOD/FLASH/FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> FLOOD/FLASHFLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> FLOOD/RAIN/WIND </td> <td> Flooding </td> </tr>
  <tr> <td> FLOOD/RAIN/WINDS </td> <td> Flooding </td> </tr>
  <tr> <td> FLOOD/RIVER FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> Flood/Strong Wind </td> <td> Flooding </td> </tr>
  <tr> <td> FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td> FLOODING/HEAVY RAIN </td> <td> Flooding </td> </tr>
  <tr> <td> FLOODS </td> <td> Flooding </td> </tr>
  <tr> <td> HIGH WINDS/FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td> HIGHWAY FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td> LAKE FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> LAKESHORE FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> LOCAL FLASH FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> LOCAL FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> MAJOR FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> MINOR FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> MINOR FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td> Minor Flooding </td> <td> Flooding </td> </tr>
  <tr> <td> RAPIDLY RISING WATER </td> <td> Flooding </td> </tr>
  <tr> <td> RIVER AND STREAM FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> RIVER FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> RIVER FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td> River Flooding </td> <td> Flooding </td> </tr>
  <tr> <td> RURAL FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> SMALL STREAM </td> <td> Flooding </td> </tr>
  <tr> <td> SMALL STREAM AND </td> <td> Flooding </td> </tr>
  <tr> <td> SMALL STREAM AND URBAN FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> SMALL STREAM AND URBAN FLOODIN </td> <td> Flooding </td> </tr>
  <tr> <td> SMALL STREAM FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> SMALL STREAM FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td> SMALL STREAM URBAN FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> SMALL STREAM/URBAN FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> Sml Stream Fld </td> <td> Flooding </td> </tr>
  <tr> <td> SNOWMELT FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td> STREAM FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td> STREET FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> STREET FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN AND SMALL </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN AND SMALL STREAM </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN AND SMALL STREAM FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN AND SMALL STREAM FLOODIN </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> Urban flood </td> <td> Flooding </td> </tr>
  <tr> <td> Urban Flood </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN FLOOD LANDSLIDE </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td> Urban Flooding </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN FLOODS </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN SMALL </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN SMALL STREAM FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN/SMALL </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN/SMALL FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN/SMALL STREAM </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN/SMALL STREAM  FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN/SMALL STREAM FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN/SMALL STREAM FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN/SMALL STRM FLDG </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN/SML STREAM FLD </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN/SML STREAM FLDG </td> <td> Flooding </td> </tr>
  <tr> <td> URBAN/STREET FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td> DENSE FOG </td> <td> Fog </td> </tr>
  <tr> <td> FOG </td> <td> Fog </td> </tr>
  <tr> <td> PATCHY DENSE FOG </td> <td> Fog </td> </tr>
  <tr> <td> DEEP HAIL </td> <td> Hail </td> </tr>
  <tr> <td> HAIL </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 0.75 </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 0.88 </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 075 </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 088 </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 1.00 </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 1.75 </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 1.75) </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 100 </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 125 </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 150 </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 175 </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 200 </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 225 </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 275 </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 450 </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 75 </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 80 </td> <td> Hail </td> </tr>
  <tr> <td> HAIL 88 </td> <td> Hail </td> </tr>
  <tr> <td> HAIL ALOFT </td> <td> Hail </td> </tr>
  <tr> <td> HAIL DAMAGE </td> <td> Hail </td> </tr>
  <tr> <td> HAIL FLOODING </td> <td> Hail </td> </tr>
  <tr> <td> HAIL STORM </td> <td> Hail </td> </tr>
  <tr> <td> Hail(0.75) </td> <td> Hail </td> </tr>
  <tr> <td> HAIL/ICY ROADS </td> <td> Hail </td> </tr>
  <tr> <td> HAIL/WIND </td> <td> Hail </td> </tr>
  <tr> <td> HAIL/WINDS </td> <td> Hail </td> </tr>
  <tr> <td> HAILSTORM </td> <td> Hail </td> </tr>
  <tr> <td> HAILSTORMS </td> <td> Hail </td> </tr>
  <tr> <td> LATE SEASON HAIL </td> <td> Hail </td> </tr>
  <tr> <td> MARINE HAIL </td> <td> Hail </td> </tr>
  <tr> <td> NON SEVERE HAIL </td> <td> Hail </td> </tr>
  <tr> <td> SMALL HAIL </td> <td> Hail </td> </tr>
  <tr> <td> small hail </td> <td> Hail </td> </tr>
  <tr> <td> Small Hail </td> <td> Hail </td> </tr>
  <tr> <td> THUNDERSTORM HAIL </td> <td> Hail </td> </tr>
  <tr> <td> ABNORMAL WARMTH </td> <td> Heat </td> </tr>
  <tr> <td> DRY HOT WEATHER </td> <td> Heat </td> </tr>
  <tr> <td> EXCESSIVE HEAT </td> <td> Heat </td> </tr>
  <tr> <td> EXTREME HEAT </td> <td> Heat </td> </tr>
  <tr> <td> HEAT </td> <td> Heat </td> </tr>
  <tr> <td> HEAT WAVE </td> <td> Heat </td> </tr>
  <tr> <td> Heat Wave </td> <td> Heat </td> </tr>
  <tr> <td> HEAT WAVES </td> <td> Heat </td> </tr>
  <tr> <td> HEAT/DROUGHT </td> <td> Heat </td> </tr>
  <tr> <td> Heatburst </td> <td> Heat </td> </tr>
  <tr> <td> HIGH TEMPERATURE RECORD </td> <td> Heat </td> </tr>
  <tr> <td> Hot and Dry </td> <td> Heat </td> </tr>
  <tr> <td> HOT PATTERN </td> <td> Heat </td> </tr>
  <tr> <td> HOT SPELL </td> <td> Heat </td> </tr>
  <tr> <td> HOT WEATHER </td> <td> Heat </td> </tr>
  <tr> <td> HOT/DRY PATTERN </td> <td> Heat </td> </tr>
  <tr> <td> PROLONG WARMTH </td> <td> Heat </td> </tr>
  <tr> <td> RECORD HEAT </td> <td> Heat </td> </tr>
  <tr> <td> Record Heat </td> <td> Heat </td> </tr>
  <tr> <td> RECORD HEAT WAVE </td> <td> Heat </td> </tr>
  <tr> <td> RECORD HIGH </td> <td> Heat </td> </tr>
  <tr> <td> Record High </td> <td> Heat </td> </tr>
  <tr> <td> RECORD HIGH TEMPERATURE </td> <td> Heat </td> </tr>
  <tr> <td> RECORD HIGH TEMPERATURES </td> <td> Heat </td> </tr>
  <tr> <td> Record temperature </td> <td> Heat </td> </tr>
  <tr> <td> RECORD TEMPERATURE </td> <td> Heat </td> </tr>
  <tr> <td> RECORD TEMPERATURES </td> <td> Heat </td> </tr>
  <tr> <td> Record Temperatures </td> <td> Heat </td> </tr>
  <tr> <td> RECORD WARM </td> <td> Heat </td> </tr>
  <tr> <td> RECORD WARM TEMPS. </td> <td> Heat </td> </tr>
  <tr> <td> RECORD WARMTH </td> <td> Heat </td> </tr>
  <tr> <td> Record Warmth </td> <td> Heat </td> </tr>
  <tr> <td> RECORD/EXCESSIVE HEAT </td> <td> Heat </td> </tr>
  <tr> <td> RED FLAG CRITERIA </td> <td> Heat </td> </tr>
  <tr> <td> UNSEASONABLY HOT </td> <td> Heat </td> </tr>
  <tr> <td> UNSEASONABLY WARM </td> <td> Heat </td> </tr>
  <tr> <td> UNSEASONABLY WARM &amp; WET </td> <td> Heat </td> </tr>
  <tr> <td> UNSEASONABLY WARM AND DRY </td> <td> Heat </td> </tr>
  <tr> <td> UNSEASONABLY WARM YEAR </td> <td> Heat </td> </tr>
  <tr> <td> UNSEASONABLY WARM/WET </td> <td> Heat </td> </tr>
  <tr> <td> UNUSUAL WARMTH </td> <td> Heat </td> </tr>
  <tr> <td> UNUSUAL/RECORD WARMTH </td> <td> Heat </td> </tr>
  <tr> <td> UNUSUALLY WARM </td> <td> Heat </td> </tr>
  <tr> <td> VERY WARM </td> <td> Heat </td> </tr>
  <tr> <td> WARM WEATHER </td> <td> Heat </td> </tr>
  <tr> <td> HEAVY SEAS </td> <td> Heavy Seas </td> </tr>
  <tr> <td> HIGH  SWELLS </td> <td> Heavy Seas </td> </tr>
  <tr> <td> HIGH SWELLS </td> <td> Heavy Seas </td> </tr>
  <tr> <td> HIGH WAVES </td> <td> Heavy Seas </td> </tr>
  <tr> <td> HIGH WIND AND SEAS </td> <td> Heavy Seas </td> </tr>
  <tr> <td> ROGUE WAVE </td> <td> Heavy Seas </td> </tr>
  <tr> <td> ROUGH SEAS </td> <td> Heavy Seas </td> </tr>
  <tr> <td> ROUGH SURF </td> <td> Heavy Seas </td> </tr>
  <tr> <td> HURRICANE </td> <td> Hurricane </td> </tr>
  <tr> <td> Hurricane Edouard </td> <td> Hurricane </td> </tr>
  <tr> <td> HURRICANE EMILY </td> <td> Hurricane </td> </tr>
  <tr> <td> HURRICANE ERIN </td> <td> Hurricane </td> </tr>
  <tr> <td> HURRICANE FELIX </td> <td> Hurricane </td> </tr>
  <tr> <td> HURRICANE GORDON </td> <td> Hurricane </td> </tr>
  <tr> <td> HURRICANE OPAL </td> <td> Hurricane </td> </tr>
  <tr> <td> HURRICANE OPAL/HIGH WINDS </td> <td> Hurricane </td> </tr>
  <tr> <td> HURRICANE/TYPHOON </td> <td> Hurricane </td> </tr>
  <tr> <td> HURRICANE-GENERATED SWELLS </td> <td> Hurricane </td> </tr>
  <tr> <td> REMNANTS OF FLOYD </td> <td> Hurricane </td> </tr>
  <tr> <td> TYPHOON </td> <td> Hurricane </td> </tr>
  <tr> <td> LACK OF SNOW </td> <td> Lack of Snow </td> </tr>
  <tr> <td> FLASH FLOOD LANDSLIDES </td> <td> Landslide </td> </tr>
  <tr> <td> FLASH FLOOD/LANDSLIDE </td> <td> Landslide </td> </tr>
  <tr> <td> LANDSLIDE </td> <td> Landslide </td> </tr>
  <tr> <td> LANDSLIDE/URBAN FLOOD </td> <td> Landslide </td> </tr>
  <tr> <td> LANDSLIDES </td> <td> Landslide </td> </tr>
  <tr> <td> Landslump </td> <td> Landslide </td> </tr>
  <tr> <td> LANDSLUMP </td> <td> Landslide </td> </tr>
  <tr> <td> MUD SLIDE </td> <td> Landslide </td> </tr>
  <tr> <td> MUD SLIDES </td> <td> Landslide </td> </tr>
  <tr> <td> MUD SLIDES URBAN FLOODING </td> <td> Landslide </td> </tr>
  <tr> <td> MUD/ROCK SLIDE </td> <td> Landslide </td> </tr>
  <tr> <td> MUDSLIDE </td> <td> Landslide </td> </tr>
  <tr> <td> Mudslide </td> <td> Landslide </td> </tr>
  <tr> <td> MUDSLIDE/LANDSLIDE </td> <td> Landslide </td> </tr>
  <tr> <td> MUDSLIDES </td> <td> Landslide </td> </tr>
  <tr> <td> Mudslides </td> <td> Landslide </td> </tr>
  <tr> <td> ROCK SLIDE </td> <td> Landslide </td> </tr>
  <tr> <td>  LIGHTNING </td> <td> Lightning </td> </tr>
  <tr> <td> LIGHTING </td> <td> Lightning </td> </tr>
  <tr> <td> LIGHTNING </td> <td> Lightning </td> </tr>
  <tr> <td> LIGHTNING  WAUSEON </td> <td> Lightning </td> </tr>
  <tr> <td> LIGHTNING AND HEAVY RAIN </td> <td> Lightning </td> </tr>
  <tr> <td> LIGHTNING AND THUNDERSTORM WIN </td> <td> Lightning </td> </tr>
  <tr> <td> LIGHTNING AND WINDS </td> <td> Lightning </td> </tr>
  <tr> <td> LIGHTNING DAMAGE </td> <td> Lightning </td> </tr>
  <tr> <td> LIGHTNING FIRE </td> <td> Lightning </td> </tr>
  <tr> <td> LIGHTNING INJURY </td> <td> Lightning </td> </tr>
  <tr> <td> LIGHTNING THUNDERSTORM WINDS </td> <td> Lightning </td> </tr>
  <tr> <td> LIGHTNING THUNDERSTORM WINDSS </td> <td> Lightning </td> </tr>
  <tr> <td> LIGHTNING. </td> <td> Lightning </td> </tr>
  <tr> <td> LIGHTNING/HEAVY RAIN </td> <td> Lightning </td> </tr>
  <tr> <td> LIGNTNING </td> <td> Lightning </td> </tr>
  <tr> <td> ASTRONOMICAL LOW TIDE </td> <td> Low Tide </td> </tr>
  <tr> <td> Marine Accident </td> <td> Marine Mishap </td> </tr>
  <tr> <td> MARINE MISHAP </td> <td> Marine Mishap </td> </tr>
  <tr> <td> Mild and Dry Pattern </td> <td> Mild </td> </tr>
  <tr> <td> MILD PATTERN </td> <td> Mild </td> </tr>
  <tr> <td> MILD/DRY PATTERN </td> <td> Mild </td> </tr>
  <tr> <td> MONTHLY TEMPERATURE </td> <td> Mild </td> </tr>
  <tr> <td> No Severe Weather </td> <td> Mild </td> </tr>
  <tr> <td> NONE </td> <td> Mild </td> </tr>
  <tr> <td> NORTHERN LIGHTS </td> <td> Northern Lights </td> </tr>
  <tr> <td> ABNORMALLY WET </td> <td> Rain </td> </tr>
  <tr> <td> EARLY RAIN </td> <td> Rain </td> </tr>
  <tr> <td> EXCESSIVE PRECIPITATION </td> <td> Rain </td> </tr>
  <tr> <td> EXCESSIVE RAIN </td> <td> Rain </td> </tr>
  <tr> <td> EXCESSIVE RAINFALL </td> <td> Rain </td> </tr>
  <tr> <td> EXCESSIVE WETNESS </td> <td> Rain </td> </tr>
  <tr> <td> EXTREMELY WET </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY PRECIPATATION </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY PRECIPITATION </td> <td> Rain </td> </tr>
  <tr> <td> Heavy Precipitation </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY RAIN </td> <td> Rain </td> </tr>
  <tr> <td> Heavy Rain </td> <td> Rain </td> </tr>
  <tr> <td> Heavy rain </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY RAIN AND FLOOD </td> <td> Rain </td> </tr>
  <tr> <td> Heavy Rain and Wind </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY RAIN EFFECTS </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY RAIN/FLOODING </td> <td> Rain </td> </tr>
  <tr> <td> Heavy Rain/High Surf </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY RAIN/LIGHTNING </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY RAIN/MUDSLIDES/FLOOD </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY RAIN/SEVERE WEATHER </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY RAIN/SMALL STREAM URBAN </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY RAIN/URBAN FLOOD </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY RAIN/WIND </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY RAIN; URBAN FLOOD WINDS; </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY RAINFALL </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY RAINS </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY RAINS/FLOODING </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY SHOWER </td> <td> Rain </td> </tr>
  <tr> <td> HEAVY SHOWERS </td> <td> Rain </td> </tr>
  <tr> <td> HIGH WINDS HEAVY RAINS </td> <td> Rain </td> </tr>
  <tr> <td> HIGH WINDS/HEAVY RAIN </td> <td> Rain </td> </tr>
  <tr> <td> HVY RAIN </td> <td> Rain </td> </tr>
  <tr> <td> LOCALLY HEAVY RAIN </td> <td> Rain </td> </tr>
  <tr> <td> MONTHLY PRECIPITATION </td> <td> Rain </td> </tr>
  <tr> <td> Monthly Rainfall </td> <td> Rain </td> </tr>
  <tr> <td> MONTHLY RAINFALL </td> <td> Rain </td> </tr>
  <tr> <td> NORMAL PRECIPITATION </td> <td> Rain </td> </tr>
  <tr> <td> PROLONGED RAIN </td> <td> Rain </td> </tr>
  <tr> <td> RAIN </td> <td> Rain </td> </tr>
  <tr> <td> RAIN (HEAVY) </td> <td> Rain </td> </tr>
  <tr> <td> RAIN AND WIND </td> <td> Rain </td> </tr>
  <tr> <td> Rain Damage </td> <td> Rain </td> </tr>
  <tr> <td> RAIN/WIND </td> <td> Rain </td> </tr>
  <tr> <td> RAINSTORM </td> <td> Rain </td> </tr>
  <tr> <td> RECORD PRECIPITATION </td> <td> Rain </td> </tr>
  <tr> <td> RECORD RAINFALL </td> <td> Rain </td> </tr>
  <tr> <td> RECORD/EXCESSIVE RAINFALL </td> <td> Rain </td> </tr>
  <tr> <td> TORRENTIAL RAIN </td> <td> Rain </td> </tr>
  <tr> <td> Torrential Rainfall </td> <td> Rain </td> </tr>
  <tr> <td> TSTM HEAVY RAIN </td> <td> Rain </td> </tr>
  <tr> <td> UNSEASONABLY WET </td> <td> Rain </td> </tr>
  <tr> <td> UNSEASONAL RAIN </td> <td> Rain </td> </tr>
  <tr> <td> Wet Month </td> <td> Rain </td> </tr>
  <tr> <td> WET WEATHER </td> <td> Rain </td> </tr>
  <tr> <td> Wet Year </td> <td> Rain </td> </tr>
  <tr> <td> RIP CURRENT </td> <td> Rip Current </td> </tr>
  <tr> <td> RIP CURRENTS </td> <td> Rip Current </td> </tr>
  <tr> <td> RIP CURRENTS HEAVY SURF </td> <td> Rip Current </td> </tr>
  <tr> <td> RIP CURRENTS/HEAVY SURF </td> <td> Rip Current </td> </tr>
  <tr> <td> SEICHE </td> <td> Seiche </td> </tr>
  <tr> <td>  TSTM WIND </td> <td> Thunderstorm </td> </tr>
  <tr> <td>  TSTM WIND (G45) </td> <td> Thunderstorm </td> </tr>
  <tr> <td> DRY MICROBURST </td> <td> Thunderstorm </td> </tr>
  <tr> <td> DRY MICROBURST 50 </td> <td> Thunderstorm </td> </tr>
  <tr> <td> DRY MICROBURST 53 </td> <td> Thunderstorm </td> </tr>
  <tr> <td> DRY MICROBURST 58 </td> <td> Thunderstorm </td> </tr>
  <tr> <td> DRY MICROBURST 61 </td> <td> Thunderstorm </td> </tr>
  <tr> <td> DRY MICROBURST 84 </td> <td> Thunderstorm </td> </tr>
  <tr> <td> DRY MICROBURST WINDS </td> <td> Thunderstorm </td> </tr>
  <tr> <td> DRY MIRCOBURST WINDS </td> <td> Thunderstorm </td> </tr>
  <tr> <td> MARINE THUNDERSTORM WIND </td> <td> Thunderstorm </td> </tr>
  <tr> <td> MARINE TSTM WIND </td> <td> Thunderstorm </td> </tr>
  <tr> <td> Metro Storm </td> <td> Thunderstorm </td> </tr>
  <tr> <td> SEVERE THUNDERSTORM </td> <td> Thunderstorm </td> </tr>
  <tr> <td> SEVERE THUNDERSTORM WINDS </td> <td> Thunderstorm </td> </tr>
  <tr> <td> SEVERE THUNDERSTORMS </td> <td> Thunderstorm </td> </tr>
  <tr> <td> SEVERE TURBULENCE </td> <td> Thunderstorm </td> </tr>
  <tr> <td>  WATERSPOUT </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> COLD AIR FUNNEL </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> COLD AIR FUNNELS </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> COLD AIR TORNADO </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> FUNNEL </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> FUNNEL CLOUD </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> Funnel Cloud </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> FUNNEL CLOUD. </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> FUNNEL CLOUD/HAIL </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> FUNNEL CLOUDS </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> FUNNELS </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> LANDSPOUT </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> LARGE WALL CLOUD </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> ROTATING WALL CLOUD </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> TORNADO </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> TORNADO DEBRIS </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> TORNADO F0 </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> TORNADO F1 </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> TORNADO F2 </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> TORNADO F3 </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> TORNADO/WATERSPOUT </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> TORNADOES </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> TORNADOES </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> TORNADOS </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> TORNDAO </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> WALL CLOUD </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> WALL CLOUD/FUNNEL CLOUD </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> WATER SPOUT </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> WATERSPOUT </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> WATERSPOUT- </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> WATERSPOUT FUNNEL CLOUD </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> WATERSPOUT TORNADO </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> WATERSPOUT/ </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> WATERSPOUT/ TORNADO </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> WATERSPOUT/TORNADO </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> WATERSPOUTS </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> WATERSPOUT-TORNADO </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> WAYTERSPOUT </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td> TROPICAL DEPRESSION </td> <td> Tropical Depression </td> </tr>
  <tr> <td> TROPICAL STORM </td> <td> Tropical Storm </td> </tr>
  <tr> <td> TROPICAL STORM ALBERTO </td> <td> Tropical Storm </td> </tr>
  <tr> <td> TROPICAL STORM DEAN </td> <td> Tropical Storm </td> </tr>
  <tr> <td> TROPICAL STORM GORDON </td> <td> Tropical Storm </td> </tr>
  <tr> <td> TROPICAL STORM JERRY </td> <td> Tropical Storm </td> </tr>
  <tr> <td> TSUNAMI </td> <td> Tsunami </td> </tr>
  <tr> <td> ? </td> <td> Unknown </td> </tr>
  <tr> <td> APACHE COUNTY </td> <td> Unknown </td> </tr>
  <tr> <td> EXCESSIVE </td> <td> Unknown </td> </tr>
  <tr> <td> OTHER </td> <td> Unknown </td> </tr>
  <tr> <td> Other </td> <td> Unknown </td> </tr>
  <tr> <td> SOUTHEAST </td> <td> Unknown </td> </tr>
  <tr> <td> Summary August 10 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary August 11 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary August 17 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary August 21 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary August 2-3 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary August 28 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary August 4 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary August 7 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary August 9 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary Jan 17 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary July 23-24 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary June 18-19 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary June 5-6 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary June 6 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of April 12 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of April 13 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of April 21 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of April 27 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of April 3rd </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of August 1 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of July 11 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of July 2 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of July 22 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of July 26 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of July 29 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of July 3 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of June 10 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of June 11 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of June 12 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of June 13 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of June 15 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of June 16 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of June 18 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of June 23 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of June 24 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of June 3 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of June 30 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of June 4 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of June 6 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of March 14 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of March 23 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of March 24 </td> <td> Unknown </td> </tr>
  <tr> <td> SUMMARY OF MARCH 24-25 </td> <td> Unknown </td> </tr>
  <tr> <td> SUMMARY OF MARCH 27 </td> <td> Unknown </td> </tr>
  <tr> <td> SUMMARY OF MARCH 29 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of May 10 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of May 13 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of May 14 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of May 22 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of May 22 am </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of May 22 pm </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of May 26 am </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of May 26 pm </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of May 31 am </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of May 31 pm </td> <td> Unknown </td> </tr>
  <tr> <td> Summary of May 9-10 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary Sept. 25-26 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary September 20 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary September 23 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary September 3 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary September 4 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary: Nov. 16 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary: Nov. 6-7 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary: Oct. 20-21 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary: October 31 </td> <td> Unknown </td> </tr>
  <tr> <td> Summary: Sept. 18 </td> <td> Unknown </td> </tr>
  <tr> <td> VOG </td> <td> Volcano </td> </tr>
  <tr> <td> Volcanic Ash </td> <td> Volcano </td> </tr>
  <tr> <td> VOLCANIC ASH </td> <td> Volcano </td> </tr>
  <tr> <td> Volcanic Ash Plume </td> <td> Volcano </td> </tr>
  <tr> <td> VOLCANIC ASHFALL </td> <td> Volcano </td> </tr>
  <tr> <td> VOLCANIC ERUPTION </td> <td> Volcano </td> </tr>
  <tr> <td>  WIND </td> <td> Wind </td> </tr>
  <tr> <td> DOWNBURST </td> <td> Wind </td> </tr>
  <tr> <td> DOWNBURST WINDS </td> <td> Wind </td> </tr>
  <tr> <td> Gradient wind </td> <td> Wind </td> </tr>
  <tr> <td> GRADIENT WIND </td> <td> Wind </td> </tr>
  <tr> <td> gradient wind </td> <td> Wind </td> </tr>
  <tr> <td> GRADIENT WINDS </td> <td> Wind </td> </tr>
  <tr> <td> GUSTNADO </td> <td> Wind </td> </tr>
  <tr> <td> GUSTNADO AND </td> <td> Wind </td> </tr>
  <tr> <td> GUSTY LAKE WIND </td> <td> Wind </td> </tr>
  <tr> <td> GUSTY THUNDERSTORM WIND </td> <td> Wind </td> </tr>
  <tr> <td> GUSTY THUNDERSTORM WINDS </td> <td> Wind </td> </tr>
  <tr> <td> Gusty Wind </td> <td> Wind </td> </tr>
  <tr> <td> GUSTY WIND </td> <td> Wind </td> </tr>
  <tr> <td> GUSTY WIND/HAIL </td> <td> Wind </td> </tr>
  <tr> <td> GUSTY WIND/HVY RAIN </td> <td> Wind </td> </tr>
  <tr> <td> Gusty wind/rain </td> <td> Wind </td> </tr>
  <tr> <td> GUSTY WINDS </td> <td> Wind </td> </tr>
  <tr> <td> Gusty Winds </td> <td> Wind </td> </tr>
  <tr> <td> Gusty winds </td> <td> Wind </td> </tr>
  <tr> <td> HIGH </td> <td> Wind </td> </tr>
  <tr> <td> HIGH  WINDS </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WIND </td> <td> Wind </td> </tr>
  <tr> <td> High Wind </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WIND (G40) </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WIND 48 </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WIND 63 </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WIND 70 </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WIND DAMAGE </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WINDS </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WINDS 55 </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WINDS 57 </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WINDS 58 </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WINDS 63 </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WINDS 66 </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WINDS 67 </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WINDS 73 </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WINDS 76 </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WINDS 80 </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WINDS 82 </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WINDS/ </td> <td> Wind </td> </tr>
  <tr> <td> HIGH WINDS/COLD </td> <td> Wind </td> </tr>
  <tr> <td> MARINE HIGH WIND </td> <td> Wind </td> </tr>
  <tr> <td> MARINE STRONG WIND </td> <td> Wind </td> </tr>
  <tr> <td> MICROBURST </td> <td> Wind </td> </tr>
  <tr> <td> Microburst </td> <td> Wind </td> </tr>
  <tr> <td> MICROBURST WINDS </td> <td> Wind </td> </tr>
  <tr> <td> NON TSTM WIND </td> <td> Wind </td> </tr>
  <tr> <td> NON-SEVERE WIND DAMAGE </td> <td> Wind </td> </tr>
  <tr> <td> NON-TSTM WIND </td> <td> Wind </td> </tr>
  <tr> <td> STORM FORCE WINDS </td> <td> Wind </td> </tr>
  <tr> <td> STRONG WIND </td> <td> Wind </td> </tr>
  <tr> <td> Strong Wind </td> <td> Wind </td> </tr>
  <tr> <td> STRONG WIND GUST </td> <td> Wind </td> </tr>
  <tr> <td> STRONG WINDS </td> <td> Wind </td> </tr>
  <tr> <td> Strong Winds </td> <td> Wind </td> </tr>
  <tr> <td> Strong winds </td> <td> Wind </td> </tr>
  <tr> <td> THUDERSTORM WINDS </td> <td> Wind </td> </tr>
  <tr> <td> THUNDEERSTORM WINDS </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERESTORM WINDS </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM  WINDS </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM DAMAGE </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM DAMAGE TO </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM W INDS </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND </td> <td> Wind </td> </tr>
  <tr> <td> Thunderstorm Wind </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND (G40) </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND 50 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND 52 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND 56 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND 59 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND 59 MPH </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND 59 MPH. </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND 60 MPH </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND 65 MPH </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND 65MPH </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND 69 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND 98 MPH </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND G50 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND G51 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND G52 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND G55 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND G60 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND G61 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND TREES </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND. </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND/ TREE </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND/ TREES </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND/AWNING </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND/HAIL </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WIND/LIGHTNING </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS      LE CEN </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS 13 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS 2 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS 50 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS 52 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS 53 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS 60 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS 61 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS 62 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS 63 MPH </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS AND </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS FUNNEL CLOU </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS G </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS G60 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS HAIL </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS HEAVY RAIN </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS LIGHTNING </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS SMALL STREA </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS URBAN FLOOD </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS. </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS/ FLOOD </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS/ HAIL </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS/FLASH FLOOD </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS/FLOODING </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS/FUNNEL CLOU </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS/HAIL </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS/HEAVY RAIN </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDS53 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDSHAIL </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINDSS </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORM WINS </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORMS </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORMS WIND </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORMS WINDS </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORMW </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORMW 50 </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORMW WINDS </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTORMWINDS </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTROM WIND </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERSTROM WINDS </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERTORM WINDS </td> <td> Wind </td> </tr>
  <tr> <td> THUNDERTSORM WIND </td> <td> Wind </td> </tr>
  <tr> <td> THUNDESTORM WINDS </td> <td> Wind </td> </tr>
  <tr> <td> THUNERSTORM WINDS </td> <td> Wind </td> </tr>
  <tr> <td> TSTM </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND </td> <td> Wind </td> </tr>
  <tr> <td> Tstm Wind </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND  (G45) </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND (41) </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND (G35) </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND (G40) </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND (G45) </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND 40 </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND 45 </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND 50 </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND 51 </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND 52 </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND 55 </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND 65) </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND AND LIGHTNING </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND DAMAGE </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND G45 </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND G58 </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WIND/HAIL </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WINDS </td> <td> Wind </td> </tr>
  <tr> <td> TSTM WND </td> <td> Wind </td> </tr>
  <tr> <td> TSTMW </td> <td> Wind </td> </tr>
  <tr> <td> TUNDERSTORM WIND </td> <td> Wind </td> </tr>
  <tr> <td> WAKE LOW WIND </td> <td> Wind </td> </tr>
  <tr> <td> wet micoburst </td> <td> Wind </td> </tr>
  <tr> <td> WET MICROBURST </td> <td> Wind </td> </tr>
  <tr> <td> Whirlwind </td> <td> Wind </td> </tr>
  <tr> <td> WHIRLWIND </td> <td> Wind </td> </tr>
  <tr> <td> WIND </td> <td> Wind </td> </tr>
  <tr> <td> Wind </td> <td> Wind </td> </tr>
  <tr> <td> WIND ADVISORY </td> <td> Wind </td> </tr>
  <tr> <td> WIND AND WAVE </td> <td> Wind </td> </tr>
  <tr> <td> WIND DAMAGE </td> <td> Wind </td> </tr>
  <tr> <td> Wind Damage </td> <td> Wind </td> </tr>
  <tr> <td> WIND GUSTS </td> <td> Wind </td> </tr>
  <tr> <td> WIND STORM </td> <td> Wind </td> </tr>
  <tr> <td> WIND/HAIL </td> <td> Wind </td> </tr>
  <tr> <td> WINDS </td> <td> Wind </td> </tr>
  <tr> <td> WND </td> <td> Wind </td> </tr>
  <tr> <td> ACCUMULATED SNOWFALL </td> <td> Wintery Mix </td> </tr>
  <tr> <td> blowing snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Blowing Snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td> BLOWING SNOW &amp; EXTREME WIND CH </td> <td> Wintery Mix </td> </tr>
  <tr> <td> BLOWING SNOW- EXTREME WIND CHI </td> <td> Wintery Mix </td> </tr>
  <tr> <td> BLOWING SNOW/EXTREME WIND CHIL </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Drifting Snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td> EARLY SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Early snowfall </td> <td> Wintery Mix </td> </tr>
  <tr> <td> EARLY SNOWFALL </td> <td> Wintery Mix </td> </tr>
  <tr> <td> EXCESSIVE SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> FALLING SNOW/ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td> FIRST SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> FREEZING DRIZZLE </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Freezing Drizzle </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Freezing drizzle </td> <td> Wintery Mix </td> </tr>
  <tr> <td> FREEZING DRIZZLE AND FREEZING </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Freezing Fog </td> <td> Wintery Mix </td> </tr>
  <tr> <td> FREEZING FOG </td> <td> Wintery Mix </td> </tr>
  <tr> <td> FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Freezing Rain </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Freezing rain </td> <td> Wintery Mix </td> </tr>
  <tr> <td> FREEZING RAIN AND SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td> FREEZING RAIN AND SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> FREEZING RAIN SLEET AND </td> <td> Wintery Mix </td> </tr>
  <tr> <td> FREEZING RAIN SLEET AND LIGHT </td> <td> Wintery Mix </td> </tr>
  <tr> <td> FREEZING RAIN/SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td> FREEZING RAIN/SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Freezing Spray </td> <td> Wintery Mix </td> </tr>
  <tr> <td> GLAZE </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Glaze </td> <td> Wintery Mix </td> </tr>
  <tr> <td> GLAZE ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td> GLAZE/ICE STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY LAKE SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY MIX </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY RAIN/SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW   FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW &amp; ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW AND </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW AND HIGH WINDS </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW AND ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW AND ICE STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW AND STRONG WINDS </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW ANDBLOWING SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Heavy snow shower </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW SQUALLS </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW/BLIZZARD </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW/BLOWING SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW/FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW/HIGH </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW/HIGH WIND </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW/HIGH WINDS </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW/HIGH WINDS &amp; FLOOD </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW/HIGH WINDS/FREEZING </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW/ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW/ICE STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW/SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW/SQUALLS </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW/WIND </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW/WINTER STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOWPACK </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY SNOW-SQUALLS </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HEAVY WET SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HIGH WIND AND HEAVY SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HIGH WIND/HEAVY SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> HIGH WINDS/SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td> ICE AND SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> ICE FLOES </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Ice Fog </td> <td> Wintery Mix </td> </tr>
  <tr> <td> ICE JAM </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Ice jam flood (minor </td> <td> Wintery Mix </td> </tr>
  <tr> <td> ICE JAM FLOODING </td> <td> Wintery Mix </td> </tr>
  <tr> <td> ICE ON ROAD </td> <td> Wintery Mix </td> </tr>
  <tr> <td> ICE PELLETS </td> <td> Wintery Mix </td> </tr>
  <tr> <td> ICE ROADS </td> <td> Wintery Mix </td> </tr>
  <tr> <td> ICE STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td> ICE STORM AND SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> ICE STORM/FLASH FLOOD </td> <td> Wintery Mix </td> </tr>
  <tr> <td> ICE/SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Ice/Snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td> ICE/STRONG WINDS </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Icestorm/Blizzard </td> <td> Wintery Mix </td> </tr>
  <tr> <td> ICY ROADS </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Icy Roads </td> <td> Wintery Mix </td> </tr>
  <tr> <td> LAKE EFFECT SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Lake Effect Snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td> LAKE-EFFECT SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> LATE SEASON SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Late Season Snowfall </td> <td> Wintery Mix </td> </tr>
  <tr> <td> LATE SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Late-season Snowfall </td> <td> Wintery Mix </td> </tr>
  <tr> <td> LIGHT FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td> LIGHT SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Light snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Light Snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td> LIGHT SNOW AND SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Light Snow/Flurries </td> <td> Wintery Mix </td> </tr>
  <tr> <td> LIGHT SNOW/FREEZING PRECIP </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Light Snowfall </td> <td> Wintery Mix </td> </tr>
  <tr> <td> MIXED PRECIP </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Mixed Precipitation </td> <td> Wintery Mix </td> </tr>
  <tr> <td> MIXED PRECIPITATION </td> <td> Wintery Mix </td> </tr>
  <tr> <td> MODERATE SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> MODERATE SNOWFALL </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Monthly Snowfall </td> <td> Wintery Mix </td> </tr>
  <tr> <td> MONTHLY SNOWFALL </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Mountain Snows </td> <td> Wintery Mix </td> </tr>
  <tr> <td> NEAR RECORD SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> PATCHY ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td> RAIN/SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Record May Snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td> RECORD SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> RECORD SNOW/COLD </td> <td> Wintery Mix </td> </tr>
  <tr> <td> RECORD SNOWFALL </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Record Winter Snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Seasonal Snowfall </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SLEET &amp; FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SLEET STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SLEET/FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SLEET/ICE STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SLEET/RAIN/SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SLEET/SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW ACCUMULATION </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Snow Accumulation </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW ADVISORY </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW AND COLD </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW AND HEAVY SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW AND ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Snow and Ice </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW AND ICE STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Snow and sleet </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW AND SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW AND WIND </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW DROUGHT </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW- HIGH WIND- WIND CHILL </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW SHOWERS </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW SQUALL </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW SQUALLS </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Snow Squalls </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Snow squalls </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW/ BITTER COLD </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW/ ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW/BLOWING SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW/COLD </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW/FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW/HEAVY SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW/HIGH WINDS </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW/ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW/ICE STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW/RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW/RAIN/SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW/SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW/SLEET/FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW/SLEET/RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOW\COLD </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOWFALL RECORD </td> <td> Wintery Mix </td> </tr>
  <tr> <td> SNOWSTORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td> THUNDERSNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Thundersnow shower </td> <td> Wintery Mix </td> </tr>
  <tr> <td> UNUSUALLY LATE SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> WET SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td> WINTER MIX </td> <td> Wintery Mix </td> </tr>
  <tr> <td> WINTER STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td> WINTER STORM HIGH WINDS </td> <td> Wintery Mix </td> </tr>
  <tr> <td> WINTER STORM/HIGH WIND </td> <td> Wintery Mix </td> </tr>
  <tr> <td> WINTER STORM/HIGH WINDS </td> <td> Wintery Mix </td> </tr>
  <tr> <td> WINTER STORMS </td> <td> Wintery Mix </td> </tr>
  <tr> <td> WINTER WEATHER </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Winter Weather </td> <td> Wintery Mix </td> </tr>
  <tr> <td> WINTER WEATHER MIX </td> <td> Wintery Mix </td> </tr>
  <tr> <td> WINTER WEATHER/MIX </td> <td> Wintery Mix </td> </tr>
  <tr> <td> WINTERY MIX </td> <td> Wintery Mix </td> </tr>
  <tr> <td> WINTRY MIX </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Wintry Mix </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Wintry mix </td> <td> Wintery Mix </td> </tr>
  <tr> <td> Black Ice </td> <td> Wintery Mix </td> </tr>
  <tr> <td> BLACK ICE </td> <td> Wintery Mix </td> </tr>
   </table>
