# Health and Economic Impacts of U.S. Storms
treepruner  
November 15, 2015  



rm(list = ls())


```r
library(lubridate)
library(dplyr)
library(sqldf)
library(xtable)
library(ggplot2)
library(mosaic)
```


### Synopsis

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage. The file contains data from 1950 - 2011.

The United States population health is impacted by weather events in terms of both fatalities and injuries.

The most fatalities occur due to tornados. Heat is the second most prevelant cause of fatalities.

More injuries are incurred as a result of tornados more than any other event group.

The greatest economic consequences are more difficult to determine due to the inconsistency of the cost unit coding. If ambiguous units are assumed to be in millions, extremes of water- drought and flooding are each more than twice as expensive as the next event group in the crop damage rankings.

For property damages, hurricanes, tornados, and flooding are the most expensive event groups.


### Data Processing

#### Load Data



```r
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

download.file(fileURL, "./proj2/repdata%2Fdata%2FStormData.csv.bz2", method = "curl")
```

```
## Warning: running command 'curl "https://d396qusza40orc.cloudfront.net/
## repdata%2Fdata%2FStormData.csv.bz2" -o "./proj2/repdata%2Fdata
## %2FStormData.csv.bz2"' had status 127
```

```
## Warning in download.file(fileURL, "./proj2/repdata%2Fdata
## %2FStormData.csv.bz2", : download had nonzero exit status
```

```r
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

##### Dates

lubridate was used to convert the dates that are factors into date fields.


```r
sd$BGN_DATE <- mdy_hms(sd$BGN_DATE)
sd$END_DATE <- mdy_hms(sd$END_DATE)
```


##### Group EVTYPE Codes

There are 985 unique EVTYPE codes, many of which are very similar. Rankings using this EVTYPE as it stands would be too disaggregated to be useful.

The EVTYPE codes were grouped together into 38 groups. This mapping was created by extracting the 985 unique EVTYPE codes and manually assigning them to a group. The storm data file was joined to the mapping file on EVTYPE with sqldf and the new EnvTypeGroup code was added.

To make this reproducible by others, the entire mapping file has been read in to this documen in the appendix:


```r
envTypeMapping <- read.csv("./proj2/envTypeList.csv",  sep = ",")[, 2:3]

sd <- sqldf( 
        "select s.*, e.envTypeGroup   
         from sd s join envTypeMapping e on s.EVTYPE = e.ENVTYPE")
```

```
## Loading required package: tcltk
```


##### Duplicates 

Check for duplicates. Determine if the REFNUM identifies a unique row. Remove duplicate REFNUMs. This removed records that were exactly the same in all fields so that only 1 record out of the multiples remained in the file. 

There is another pair of records, REFNUM 605943 and 567221 that although they have different REFNUMs, appear to be for the same event. The REMARKS refer to millions in damage, so REFNUM 605943 which referred to billions in PROPDMGEXP, was removed.


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

# thes code also works and was actually faster
# sd1 <- sd[!duplicated(sd),] 
```

##### Economic Damage Fields

The PROPDMG field is the numerical component of the property damage and the PROPDMGEXP is the units, however, the units field is dirty. The numerical field was multiplied as follows: 
 * Bb = billions
 * Mm = millions 
 * Kk = thousands
 * Hh = hundreds
 * ?  = 0
 * anything else = millions
 
The CROPDMG field is the numerical component of the crop damage and the CROPDMGEXP is the units, however, the units field is also dirty. The numerical field was multiplied as follows: 
 * Bb = billions
 * Mm = millions 
 * Kk = thousands
 * Hh = hundreds
 * ?  = 0
 * anything else = millions

Another field was added to indicate the reliability of the numbers. The assumption about "anything else"  and "?" was coded as low reliability. 

The REMARKS have cost numbers that do not always match the cost fields for that record. A future analysis should compare these fields and either adjust the  reliability and/or update the damage numbers.

The events in this file range from 1950 to 2011. To truely be comparable and understandable to the modern audience, the damage dollar amounts should be adjusted using an appropriate price index into current dollars in order to properly rank the financial impact of each event. This was deemed out of scope for our assignment.



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
                when upper(PROPDMGEXP) = 'B' then PROPDMG * 1000000000
                when upper(PROPDMGEXP) = 'M' then PROPDMG * 1000000
                when upper(PROPDMGEXP) = 'K' then PROPDMG * 1000
                when upper(PROPDMGEXP) = 'H' then PROPDMG * 100
                when upper(PROPDMGEXP) = '?' then PROPDMG * 0
           else PROPDMG * 1000000
           end as PropertyDamageAmt
        , CROPDMG
        , CROPDMGEXP
        , case 
                when upper(CROPDMGEXP) = 'B' then CROPDMG * 1000000000
                when upper(CROPDMGEXP) = 'M' then CROPDMG * 1000000
                when upper(CROPDMGEXP) = 'K' then CROPDMG * 1000
                when upper(CROPDMGEXP) = 'H' then CROPDMG * 100
                when upper(CROPDMGEXP) = '?' then CROPDMG * 0
           else CROPDMG * 1000000
           end as CropDamageAmt
        , case 
                when upper(PROPDMGEXP) not in ('B', 'M', 'K', 'H') then 'Low'
                when upper(CROPDMGEXP) not in ('B', 'M', 'K', 'H') then 'Low'
          else 'High'
          end as reliability
        , REMARKS   
         from sd
         where REFNUM <> 605943 "          )

saveRDS(sd, "./proj2/sd.rds")
```
         
### Results by Event Group

#### Population Health Stastics by Event Group



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


The fatalities from 1950 - 2011 are ranked as follows:


```r
fatalities <- xtable(pop_fatalities[pop_fatalities$Fatalities.Count != 0,] )
print(fatalities, type="html")
```

<!-- html table generated in R 3.1.3 by xtable 1.8-0 package -->
<!-- Wed Nov 18 13:41:09 2015 -->
<table border=1>
<tr> <th>  </th> <th> EnvTypeGroup </th> <th> n </th> <th> Fatalities.Count </th> <th> Fatalities.Mean </th> <th> Fatalities.Median </th> <th> Fatalities.Min </th> <th> Fatalities.Max </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> Tornado/Waterspout </td> <td align="right"> 71548 </td> <td align="right"> 5639.00 </td> <td align="right"> 0.08 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 158.00 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> Heat </td> <td align="right"> 3013 </td> <td align="right"> 3172.00 </td> <td align="right"> 1.05 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 583.00 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> Flooding </td> <td align="right"> 85229 </td> <td align="right"> 1548.00 </td> <td align="right"> 0.02 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 20.00 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> Wind </td> <td align="right"> 351097 </td> <td align="right"> 1151.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 11.00 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> Lightning </td> <td align="right"> 15770 </td> <td align="right"> 817.00 </td> <td align="right"> 0.05 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 5.00 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> Rip Current </td> <td align="right"> 777 </td> <td align="right"> 577.00 </td> <td align="right"> 0.74 </td> <td align="right"> 1.00 </td> <td align="right"> 0.00 </td> <td align="right"> 6.00 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> Wintery Mix </td> <td align="right"> 40059 </td> <td align="right"> 556.00 </td> <td align="right"> 0.01 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 10.00 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> Cold </td> <td align="right"> 4292 </td> <td align="right"> 485.00 </td> <td align="right"> 0.11 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 14.00 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> Avalanche </td> <td align="right"> 388 </td> <td align="right"> 225.00 </td> <td align="right"> 0.58 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 6.00 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> Coastal Flooding </td> <td align="right"> 2494 </td> <td align="right"> 203.00 </td> <td align="right"> 0.08 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 11.00 </td> </tr>
  <tr> <td align="right"> 11 </td> <td> Hurricane </td> <td align="right"> 301 </td> <td align="right"> 135.00 </td> <td align="right"> 0.45 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 15.00 </td> </tr>
  <tr> <td align="right"> 12 </td> <td> Blizzard </td> <td align="right"> 2755 </td> <td align="right"> 102.00 </td> <td align="right"> 0.04 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 8.00 </td> </tr>
  <tr> <td align="right"> 13 </td> <td> Rain </td> <td align="right"> 11954 </td> <td align="right"> 101.00 </td> <td align="right"> 0.01 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 19.00 </td> </tr>
  <tr> <td align="right"> 14 </td> <td> Fire </td> <td align="right"> 4260 </td> <td align="right"> 90.00 </td> <td align="right"> 0.02 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 14.00 </td> </tr>
  <tr> <td align="right"> 15 </td> <td> Fog </td> <td align="right"> 1834 </td> <td align="right"> 80.00 </td> <td align="right"> 0.04 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 11.00 </td> </tr>
  <tr> <td align="right"> 16 </td> <td> Tropical Storm </td> <td align="right"> 697 </td> <td align="right"> 66.00 </td> <td align="right"> 0.09 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 22.00 </td> </tr>
  <tr> <td align="right"> 17 </td> <td> Landslide </td> <td align="right"> 652 </td> <td align="right"> 44.00 </td> <td align="right"> 0.07 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 14.00 </td> </tr>
  <tr> <td align="right"> 18 </td> <td> Tsunami </td> <td align="right">  20 </td> <td align="right"> 33.00 </td> <td align="right"> 1.65 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 32.00 </td> </tr>
  <tr> <td align="right"> 19 </td> <td> Dust Storm </td> <td align="right"> 585 </td> <td align="right"> 24.00 </td> <td align="right"> 0.04 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 10.00 </td> </tr>
  <tr> <td align="right"> 20 </td> <td> Thunderstorm </td> <td align="right"> 12232 </td> <td align="right"> 22.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 5.00 </td> </tr>
  <tr> <td align="right"> 21 </td> <td> Heavy Seas </td> <td align="right">  20 </td> <td align="right"> 20.00 </td> <td align="right"> 1.00 </td> <td align="right"> 0.50 </td> <td align="right"> 0.00 </td> <td align="right"> 4.00 </td> </tr>
  <tr> <td align="right"> 22 </td> <td> Hail </td> <td align="right"> 289281 </td> <td align="right"> 15.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 4.00 </td> </tr>
  <tr> <td align="right"> 23 </td> <td> Marine Mishap </td> <td align="right">   3 </td> <td align="right"> 8.00 </td> <td align="right"> 2.67 </td> <td align="right"> 1.00 </td> <td align="right"> 1.00 </td> <td align="right"> 6.00 </td> </tr>
  <tr> <td align="right"> 24 </td> <td> Drought </td> <td align="right"> 2599 </td> <td align="right"> 6.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 4.00 </td> </tr>
  <tr> <td align="right"> 25 </td> <td> Drowning </td> <td align="right">   1 </td> <td align="right"> 1.00 </td> <td align="right"> 1.00 </td> <td align="right"> 1.00 </td> <td align="right"> 1.00 </td> <td align="right"> 1.00 </td> </tr>
   </table>

Fatalities by Year


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


Top fatality event groups by year show that the data may be incomplete or have quality issues since there are no flooding or heat related deaths prior to 1990.


```r
ggplot( data = pop_fatalities_yr
        , aes(x = Year
        , y = Fatalities.Count , colour = EnvTypeGroup)) +
        geom_point() + 
        facet_grid(EnvTypeGroup ~ .) +
        geom_line()  + 
        theme(legend.position = "bottom")
```

![](PA2_template_files/figure-html/Figure 1-1.png) 





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


The injuries from  1950 - 2011 are ranked as follows:


```r
injuries <- xtable(pop_injuries[pop_injuries$Injuries.Count != 0,] )
print(injuries, type="html")
```

<!-- html table generated in R 3.1.3 by xtable 1.8-0 package -->
<!-- Wed Nov 18 13:41:12 2015 -->
<table border=1>
<tr> <th>  </th> <th> EnvTypeGroup </th> <th> n </th> <th> Injuries.Count </th> <th> Injuries.Mean </th> <th> Injuries.Median </th> <th> Injuries.Min </th> <th> Injuries.Max </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> Tornado/Waterspout </td> <td align="right"> 71548 </td> <td align="right"> 91439.00 </td> <td align="right"> 1.28 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1700.00 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> Wind </td> <td align="right"> 351097 </td> <td align="right"> 11384.00 </td> <td align="right"> 0.03 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 89.00 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> Heat </td> <td align="right"> 3013 </td> <td align="right"> 9228.00 </td> <td align="right"> 3.06 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 519.00 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> Flooding </td> <td align="right"> 85229 </td> <td align="right"> 8673.00 </td> <td align="right"> 0.10 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 800.00 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> Wintery Mix </td> <td align="right"> 40059 </td> <td align="right"> 5585.00 </td> <td align="right"> 0.14 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1568.00 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> Lightning </td> <td align="right"> 15770 </td> <td align="right"> 5232.00 </td> <td align="right"> 0.33 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 51.00 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> Fire </td> <td align="right"> 4260 </td> <td align="right"> 1608.00 </td> <td align="right"> 0.38 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 150.00 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> Hail </td> <td align="right"> 289281 </td> <td align="right"> 1371.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 109.00 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> Hurricane </td> <td align="right"> 301 </td> <td align="right"> 1333.00 </td> <td align="right"> 4.43 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 780.00 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> Fog </td> <td align="right"> 1834 </td> <td align="right"> 1076.00 </td> <td align="right"> 0.59 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 78.00 </td> </tr>
  <tr> <td align="right"> 11 </td> <td> Blizzard </td> <td align="right"> 2755 </td> <td align="right"> 818.00 </td> <td align="right"> 0.30 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 385.00 </td> </tr>
  <tr> <td align="right"> 12 </td> <td> Rip Current </td> <td align="right"> 777 </td> <td align="right"> 529.00 </td> <td align="right"> 0.68 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 35.00 </td> </tr>
  <tr> <td align="right"> 13 </td> <td> Dust Storm </td> <td align="right"> 585 </td> <td align="right"> 483.00 </td> <td align="right"> 0.83 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 40.00 </td> </tr>
  <tr> <td align="right"> 14 </td> <td> Tropical Storm </td> <td align="right"> 697 </td> <td align="right"> 383.00 </td> <td align="right"> 0.55 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 200.00 </td> </tr>
  <tr> <td align="right"> 15 </td> <td> Cold </td> <td align="right"> 4292 </td> <td align="right"> 324.00 </td> <td align="right"> 0.08 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 129.00 </td> </tr>
  <tr> <td align="right"> 16 </td> <td> Coastal Flooding </td> <td align="right"> 2494 </td> <td align="right"> 306.00 </td> <td align="right"> 0.12 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 55.00 </td> </tr>
  <tr> <td align="right"> 17 </td> <td> Rain </td> <td align="right"> 11954 </td> <td align="right"> 280.00 </td> <td align="right"> 0.02 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 32.00 </td> </tr>
  <tr> <td align="right"> 18 </td> <td> Avalanche </td> <td align="right"> 388 </td> <td align="right"> 171.00 </td> <td align="right"> 0.44 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 11.00 </td> </tr>
  <tr> <td align="right"> 19 </td> <td> Tsunami </td> <td align="right">  20 </td> <td align="right"> 129.00 </td> <td align="right"> 6.45 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 129.00 </td> </tr>
  <tr> <td align="right"> 20 </td> <td> Thunderstorm </td> <td align="right"> 12232 </td> <td align="right"> 63.00 </td> <td align="right"> 0.01 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 13.00 </td> </tr>
  <tr> <td align="right"> 21 </td> <td> Landslide </td> <td align="right"> 652 </td> <td align="right"> 55.00 </td> <td align="right"> 0.08 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 10.00 </td> </tr>
  <tr> <td align="right"> 22 </td> <td> Heavy Seas </td> <td align="right">  20 </td> <td align="right"> 28.00 </td> <td align="right"> 1.40 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 20.00 </td> </tr>
  <tr> <td align="right"> 23 </td> <td> Drought </td> <td align="right"> 2599 </td> <td align="right"> 19.00 </td> <td align="right"> 0.01 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 15.00 </td> </tr>
  <tr> <td align="right"> 24 </td> <td> Marine Mishap </td> <td align="right">   3 </td> <td align="right"> 7.00 </td> <td align="right"> 2.33 </td> <td align="right"> 2.00 </td> <td align="right"> 0.00 </td> <td align="right"> 5.00 </td> </tr>
  <tr> <td align="right"> 25 </td> <td> Unknown </td> <td align="right"> 131 </td> <td align="right"> 4.00 </td> <td align="right"> 0.03 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 4.00 </td> </tr>
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


Top fatality event groups by year show that the data may be incomplete or have quality issues since several of the high count event groups have no injuries prior to 1990


```r
ggplot( data = pop_injuries_yr
        , aes(x = Year
        , y = Injuries.Count, colour = EnvTypeGroup)) +
        geom_point() + 
        facet_grid(EnvTypeGroup ~ .) +
        geom_line() + 
        theme(legend.position = "bottom")
```

![](PA2_template_files/figure-html/Figure 2-1.png) 




#### Economic Impacts


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


Total property damages from 1950 - 2011 (assuming millions for ambiguous cost units) are as follows:


```r
PropertyDamage <- xtable(cost_PropertyDamage[cost_PropertyDamage$PropertyDamageAmt.Sum != 0,] )
print(PropertyDamage, type="html")
```

<!-- html table generated in R 3.1.3 by xtable 1.8-0 package -->
<!-- Wed Nov 18 13:41:16 2015 -->
<table border=1>
<tr> <th>  </th> <th> EnvTypeGroup </th> <th> n </th> <th> PropertyDamageAmt.Sum </th> <th> PropertyDamageAmt.Mean </th> <th> PropertyDamageAmt.Median </th> <th> PropertyDamageAmt.Min_ </th> <th> PropertyDamageAmt.Max </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> Hurricane </td> <td align="right"> 301 </td> <td align="right"> 85356410010.00 </td> <td align="right"> 283576112.99 </td> <td align="right"> 1000000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 16930000000.00 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> Tornado/Waterspout </td> <td align="right"> 71548 </td> <td align="right"> 57301564030.00 </td> <td align="right"> 800882.82 </td> <td align="right"> 500.00 </td> <td align="right"> 0.00 </td> <td align="right"> 2800000000.00 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> Flooding </td> <td align="right"> 85229 </td> <td align="right"> 52738382960.00 </td> <td align="right"> 618784.49 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 5000000000.00 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> Coastal Flooding </td> <td align="right"> 2494 </td> <td align="right"> 48676894560.00 </td> <td align="right"> 19517600.06 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 31300000000.00 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> Wind </td> <td align="right"> 351097 </td> <td align="right"> 22496800370.00 </td> <td align="right"> 64075.74 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1300000000.00 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> Hail </td> <td align="right"> 289281 </td> <td align="right"> 16297248720.00 </td> <td align="right"> 56337.09 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1800000000.00 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> Wintery Mix </td> <td align="right"> 40059 </td> <td align="right"> 11843739500.00 </td> <td align="right"> 295657.39 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 5000000000.00 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> Fire </td> <td align="right"> 4260 </td> <td align="right"> 8496728500.00 </td> <td align="right"> 1994537.21 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1500000000.00 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> Tropical Storm </td> <td align="right"> 697 </td> <td align="right"> 7714390550.00 </td> <td align="right"> 11067992.18 </td> <td align="right"> 5000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 5150000000.00 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> Rain </td> <td align="right"> 11954 </td> <td align="right"> 3243854190.00 </td> <td align="right"> 271361.40 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 2500000000.00 </td> </tr>
  <tr> <td align="right"> 11 </td> <td> Thunderstorm </td> <td align="right"> 12232 </td> <td align="right"> 1228283000.00 </td> <td align="right"> 100415.55 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1200000000.00 </td> </tr>
  <tr> <td align="right"> 12 </td> <td> Lightning </td> <td align="right"> 15770 </td> <td align="right"> 1100442280.00 </td> <td align="right"> 69780.74 </td> <td align="right"> 5000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 37000000.00 </td> </tr>
  <tr> <td align="right"> 13 </td> <td> Drought </td> <td align="right"> 2599 </td> <td align="right"> 1046306000.00 </td> <td align="right"> 402580.22 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 645150000.00 </td> </tr>
  <tr> <td align="right"> 14 </td> <td> Blizzard </td> <td align="right"> 2755 </td> <td align="right"> 659883950.00 </td> <td align="right"> 239522.30 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 102000000.00 </td> </tr>
  <tr> <td align="right"> 15 </td> <td> Landslide </td> <td align="right"> 652 </td> <td align="right"> 327452100.00 </td> <td align="right"> 502227.15 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 55900000.00 </td> </tr>
  <tr> <td align="right"> 16 </td> <td> Cold </td> <td align="right"> 4292 </td> <td align="right"> 155034400.00 </td> <td align="right"> 36121.71 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 50000000.00 </td> </tr>
  <tr> <td align="right"> 17 </td> <td> Tsunami </td> <td align="right">  20 </td> <td align="right"> 144062000.00 </td> <td align="right"> 7203100.00 </td> <td align="right"> 115000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 81000000.00 </td> </tr>
  <tr> <td align="right"> 18 </td> <td> Fog </td> <td align="right"> 1834 </td> <td align="right"> 22829500.00 </td> <td align="right"> 12447.93 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1500000.00 </td> </tr>
  <tr> <td align="right"> 19 </td> <td> Heat </td> <td align="right"> 3013 </td> <td align="right"> 20125750.00 </td> <td align="right"> 6679.64 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 3800000.00 </td> </tr>
  <tr> <td align="right"> 20 </td> <td> Avalanche </td> <td align="right"> 388 </td> <td align="right"> 8721800.00 </td> <td align="right"> 22478.87 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 5000000.00 </td> </tr>
  <tr> <td align="right"> 21 </td> <td> Dust Storm </td> <td align="right"> 585 </td> <td align="right"> 6318130.00 </td> <td align="right"> 10800.22 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 640000.00 </td> </tr>
  <tr> <td align="right"> 22 </td> <td> Tropical Depression </td> <td align="right">  60 </td> <td align="right"> 1737000.00 </td> <td align="right"> 28950.00 </td> <td align="right"> 6500.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1000000.00 </td> </tr>
  <tr> <td align="right"> 23 </td> <td> Dam Failure </td> <td align="right">   5 </td> <td align="right"> 1002000.00 </td> <td align="right"> 200400.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1000000.00 </td> </tr>
  <tr> <td align="right"> 24 </td> <td> Seiche </td> <td align="right">  21 </td> <td align="right"> 980000.00 </td> <td align="right"> 46666.67 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 750000.00 </td> </tr>
  <tr> <td align="right"> 25 </td> <td> Volcano </td> <td align="right">  30 </td> <td align="right"> 500000.00 </td> <td align="right"> 16666.67 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 300000.00 </td> </tr>
  <tr> <td align="right"> 26 </td> <td> Low Tide </td> <td align="right"> 174 </td> <td align="right"> 320000.00 </td> <td align="right"> 1839.08 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 200000.00 </td> </tr>
  <tr> <td align="right"> 27 </td> <td> Rip Current </td> <td align="right"> 777 </td> <td align="right"> 163000.00 </td> <td align="right"> 209.78 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 80000.00 </td> </tr>
  <tr> <td align="right"> 28 </td> <td> Unknown </td> <td align="right"> 131 </td> <td align="right"> 65500.00 </td> <td align="right"> 500.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 50000.00 </td> </tr>
  <tr> <td align="right"> 29 </td> <td> Heavy Seas </td> <td align="right">  20 </td> <td align="right"> 65000.00 </td> <td align="right"> 3250.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 50000.00 </td> </tr>
  <tr> <td align="right"> 30 </td> <td> Marine Mishap </td> <td align="right">   3 </td> <td align="right"> 50000.00 </td> <td align="right"> 16666.67 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 50000.00 </td> </tr>
   </table>

Retaining only the records with high reliabiltiy cost estimates doesn't signnificantly change the relative rankings on property damage: 


```r
PropertyDamageHigh <- xtable(cost_PropertyDamage_High[cost_PropertyDamage_High$PropertyDamageAmt.Sum != 0,] )
print(PropertyDamageHigh, type="html")
```

<!-- html table generated in R 3.1.3 by xtable 1.8-0 package -->
<!-- Wed Nov 18 13:41:16 2015 -->
<table border=1>
<tr> <th>  </th> <th> EnvTypeGroup </th> <th> n </th> <th> PropertyDamageAmt.Sum </th> <th> PropertyDamageAmt.Mean </th> <th> PropertyDamageAmt.Median </th> <th> PropertyDamageAmt.Min_ </th> <th> PropertyDamageAmt.Max </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> Hurricane </td> <td align="right"> 114 </td> <td align="right"> 38996883000.00 </td> <td align="right"> 342077921.05 </td> <td align="right"> 12750000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 5880000000.00 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> Flooding </td> <td align="right"> 35599 </td> <td align="right"> 30832150730.00 </td> <td align="right"> 866095.98 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 5000000000.00 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> Tornado/Waterspout </td> <td align="right"> 12823 </td> <td align="right"> 16172053240.00 </td> <td align="right"> 1261175.48 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 2800000000.00 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> Hail </td> <td align="right"> 80178 </td> <td align="right"> 7992338190.00 </td> <td align="right"> 99682.43 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1800000000.00 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> Wind </td> <td align="right"> 102195 </td> <td align="right"> 7092587390.00 </td> <td align="right"> 69402.49 </td> <td align="right"> 700.00 </td> <td align="right"> 0.00 </td> <td align="right"> 929000000.00 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> Coastal Flooding </td> <td align="right"> 1073 </td> <td align="right"> 4919461060.00 </td> <td align="right"> 4584772.66 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 4000000000.00 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> Fire </td> <td align="right"> 1824 </td> <td align="right"> 3552827470.00 </td> <td align="right"> 1947822.08 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1040000000.00 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> Wintery Mix </td> <td align="right"> 21063 </td> <td align="right"> 2228640500.00 </td> <td align="right"> 105808.31 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 750000000.00 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> Tropical Storm </td> <td align="right"> 421 </td> <td align="right"> 1062091350.00 </td> <td align="right"> 2522782.30 </td> <td align="right"> 5000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 530470000.00 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> Rain </td> <td align="right"> 5269 </td> <td align="right"> 347421930.00 </td> <td align="right"> 65936.98 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 70000000.00 </td> </tr>
  <tr> <td align="right"> 11 </td> <td> Lightning </td> <td align="right"> 4168 </td> <td align="right"> 315273980.00 </td> <td align="right"> 75641.55 </td> <td align="right"> 10000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 15000000.00 </td> </tr>
  <tr> <td align="right"> 12 </td> <td> Drought </td> <td align="right"> 1401 </td> <td align="right"> 233921000.00 </td> <td align="right"> 166967.17 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 23000000.00 </td> </tr>
  <tr> <td align="right"> 13 </td> <td> Landslide </td> <td align="right"> 320 </td> <td align="right"> 150303500.00 </td> <td align="right"> 469698.44 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 48000000.00 </td> </tr>
  <tr> <td align="right"> 14 </td> <td> Tsunami </td> <td align="right">  19 </td> <td align="right"> 144062000.00 </td> <td align="right"> 7582210.53 </td> <td align="right"> 150000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 81000000.00 </td> </tr>
  <tr> <td align="right"> 15 </td> <td> Blizzard </td> <td align="right"> 1742 </td> <td align="right"> 94981000.00 </td> <td align="right"> 54524.11 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 50000000.00 </td> </tr>
  <tr> <td align="right"> 16 </td> <td> Cold </td> <td align="right"> 2351 </td> <td align="right"> 22063000.00 </td> <td align="right"> 9384.52 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 5000000.00 </td> </tr>
  <tr> <td align="right"> 17 </td> <td> Dust Storm </td> <td align="right"> 254 </td> <td align="right"> 3830130.00 </td> <td align="right"> 15079.25 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 640000.00 </td> </tr>
  <tr> <td align="right"> 18 </td> <td> Heat </td> <td align="right"> 1339 </td> <td align="right"> 3218200.00 </td> <td align="right"> 2403.44 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1500000.00 </td> </tr>
  <tr> <td align="right"> 19 </td> <td> Fog </td> <td align="right"> 921 </td> <td align="right"> 2874000.00 </td> <td align="right"> 3120.52 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 700000.00 </td> </tr>
  <tr> <td align="right"> 20 </td> <td> Avalanche </td> <td align="right"> 154 </td> <td align="right"> 2385800.00 </td> <td align="right"> 15492.21 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1100000.00 </td> </tr>
  <tr> <td align="right"> 21 </td> <td> Tropical Depression </td> <td align="right">  27 </td> <td align="right"> 1302000.00 </td> <td align="right"> 48222.22 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1000000.00 </td> </tr>
  <tr> <td align="right"> 22 </td> <td> Thunderstorm </td> <td align="right"> 5819 </td> <td align="right"> 1200400.00 </td> <td align="right"> 206.29 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 500000.00 </td> </tr>
  <tr> <td align="right"> 23 </td> <td> Low Tide </td> <td align="right"> 174 </td> <td align="right"> 320000.00 </td> <td align="right"> 1839.08 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 200000.00 </td> </tr>
  <tr> <td align="right"> 24 </td> <td> Seiche </td> <td align="right">   7 </td> <td align="right"> 100000.00 </td> <td align="right"> 14285.71 </td> <td align="right"> 10000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 50000.00 </td> </tr>
  <tr> <td align="right"> 25 </td> <td> Rip Current </td> <td align="right"> 277 </td> <td align="right"> 1000.00 </td> <td align="right"> 3.61 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1000.00 </td> </tr>
   </table>

Total crop damages from 1950 - 2011 (assuming millions for ambiguous cost units) are as follows:


```r
CropDamage <- xtable(cost_CropDamage[cost_CropDamage$CropDamageAmt.Sum != 0,] )
print(CropDamage, type="html")
```

<!-- html table generated in R 3.1.3 by xtable 1.8-0 package -->
<!-- Wed Nov 18 13:41:16 2015 -->
<table border=1>
<tr> <th>  </th> <th> EnvTypeGroup </th> <th> n_date </th> <th> CropDamageAmt.Sum </th> <th> CropDamageAmt.Mean </th> <th> CropDamageAmt.Median </th> <th> CropDamageAmt.Min </th> <th> CropDamageAmt.Max </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> Drought </td> <td align="right"> 2599 </td> <td align="right"> 13972621780.00 </td> <td align="right"> 5376153.05 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1000000000.00 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> Flooding </td> <td align="right"> 85229 </td> <td align="right"> 12350638200.00 </td> <td align="right"> 144911.22 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 5000000000.00 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> Hurricane </td> <td align="right"> 301 </td> <td align="right"> 5516117800.00 </td> <td align="right"> 18325972.76 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1510000000.00 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> Wintery Mix </td> <td align="right"> 40059 </td> <td align="right"> 5209241400.00 </td> <td align="right"> 130039.23 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 5000000000.00 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> Cold </td> <td align="right"> 4292 </td> <td align="right"> 3428826500.00 </td> <td align="right"> 798887.81 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 596000000.00 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> Hail </td> <td align="right"> 289281 </td> <td align="right"> 3069937600.00 </td> <td align="right"> 10612.30 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 70000000.00 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> Wind </td> <td align="right"> 351097 </td> <td align="right"> 2070730850.00 </td> <td align="right"> 5897.89 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 175000000.00 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> Rain </td> <td align="right"> 11954 </td> <td align="right"> 948515800.00 </td> <td align="right"> 79347.15 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 200000000.00 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> Heat </td> <td align="right"> 3013 </td> <td align="right"> 904423500.00 </td> <td align="right"> 300173.75 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 492400000.00 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> Tropical Storm </td> <td align="right"> 697 </td> <td align="right"> 694896000.00 </td> <td align="right"> 996981.35 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 200000000.00 </td> </tr>
  <tr> <td align="right"> 11 </td> <td> Tornado/Waterspout </td> <td align="right"> 71548 </td> <td align="right"> 574961360.00 </td> <td align="right"> 8036.02 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 70000000.00 </td> </tr>
  <tr> <td align="right"> 12 </td> <td> Fire </td> <td align="right"> 4260 </td> <td align="right"> 403281630.00 </td> <td align="right"> 94667.05 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 90000000.00 </td> </tr>
  <tr> <td align="right"> 13 </td> <td> Blizzard </td> <td align="right"> 2755 </td> <td align="right"> 112060000.00 </td> <td align="right"> 40675.14 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 50000000.00 </td> </tr>
  <tr> <td align="right"> 14 </td> <td> Thunderstorm </td> <td align="right"> 12232 </td> <td align="right"> 46265000.00 </td> <td align="right"> 3782.29 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 26000000.00 </td> </tr>
  <tr> <td align="right"> 15 </td> <td> Landslide </td> <td align="right"> 652 </td> <td align="right"> 20017000.00 </td> <td align="right"> 30700.92 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 20000000.00 </td> </tr>
  <tr> <td align="right"> 16 </td> <td> Lightning </td> <td align="right"> 15770 </td> <td align="right"> 12092090.00 </td> <td align="right"> 766.78 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 3500000.00 </td> </tr>
  <tr> <td align="right"> 17 </td> <td> Dust Storm </td> <td align="right"> 585 </td> <td align="right"> 3600000.00 </td> <td align="right"> 6153.85 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1500000.00 </td> </tr>
  <tr> <td align="right"> 18 </td> <td> Unknown </td> <td align="right"> 131 </td> <td align="right"> 1034400.00 </td> <td align="right"> 7896.18 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 34480.00 </td> </tr>
  <tr> <td align="right"> 19 </td> <td> Coastal Flooding </td> <td align="right"> 2494 </td> <td align="right"> 911000.00 </td> <td align="right"> 365.28 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 750000.00 </td> </tr>
  <tr> <td align="right"> 20 </td> <td> Tsunami </td> <td align="right">  20 </td> <td align="right"> 20000.00 </td> <td align="right"> 1000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 20000.00 </td> </tr>
   </table>


Retaining only the records with high reliabiltiy cost extimates DOES signnificantly change the relative rankings for crop damages. Drought is now # 12 in the rankings: 


```r
CropDamageHigh <- xtable(cost_CropDamage_High[cost_CropDamage$CropDamageAmt.Sum != 0,] )
print(CropDamageHigh, type="html")
```

<!-- html table generated in R 3.1.3 by xtable 1.8-0 package -->
<!-- Wed Nov 18 13:41:16 2015 -->
<table border=1>
<tr> <th>  </th> <th> EnvTypeGroup </th> <th> n_date </th> <th> CropDamageAmt.Sum </th> <th> CropDamageAmt.Mean </th> <th> CropDamageAmt.Median </th> <th> CropDamageAmt.Min </th> <th> CropDamageAmt.Max </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> Flooding </td> <td align="right"> 35599 </td> <td align="right"> 11701327150.00 </td> <td align="right"> 328698.20 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 5000000000.00 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> Hurricane </td> <td align="right"> 114 </td> <td align="right"> 5333117800.00 </td> <td align="right"> 46781735.09 </td> <td align="right"> 2240000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1510000000.00 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> Wintery Mix </td> <td align="right"> 21063 </td> <td align="right"> 5203007900.00 </td> <td align="right"> 247021.22 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 5000000000.00 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> Hail </td> <td align="right"> 80178 </td> <td align="right"> 2029417950.00 </td> <td align="right"> 25311.41 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 70000000.00 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> Wind </td> <td align="right"> 102195 </td> <td align="right"> 1846417650.00 </td> <td align="right"> 18067.59 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 175000000.00 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> Drought </td> <td align="right"> 1401 </td> <td align="right"> 1652746000.00 </td> <td align="right"> 1179690.22 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 344000000.00 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> Cold </td> <td align="right"> 2351 </td> <td align="right"> 935231000.00 </td> <td align="right"> 397801.36 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 286000000.00 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> Heat </td> <td align="right"> 1339 </td> <td align="right"> 493285000.00 </td> <td align="right"> 368398.06 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 492400000.00 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> Tropical Storm </td> <td align="right"> 421 </td> <td align="right"> 468261000.00 </td> <td align="right"> 1112258.91 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 101500000.00 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> Tornado/Waterspout </td> <td align="right"> 12823 </td> <td align="right"> 353383710.00 </td> <td align="right"> 27558.58 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 70000000.00 </td> </tr>
  <tr> <td align="right"> 11 </td> <td> Fire </td> <td align="right"> 1824 </td> <td align="right"> 285822100.00 </td> <td align="right"> 156700.71 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 80000000.00 </td> </tr>
  <tr> <td align="right"> 12 </td> <td> Rain </td> <td align="right"> 5269 </td> <td align="right"> 126948800.00 </td> <td align="right"> 24093.53 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 50000000.00 </td> </tr>
  <tr> <td align="right"> 13 </td> <td> Blizzard </td> <td align="right"> 1742 </td> <td align="right"> 112060000.00 </td> <td align="right"> 64328.36 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 50000000.00 </td> </tr>
  <tr> <td align="right"> 14 </td> <td> Thunderstorm </td> <td align="right"> 5819 </td> <td align="right"> 46059000.00 </td> <td align="right"> 7915.28 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 26000000.00 </td> </tr>
  <tr> <td align="right"> 15 </td> <td> Landslide </td> <td align="right"> 320 </td> <td align="right"> 20017000.00 </td> <td align="right"> 62553.12 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 20000000.00 </td> </tr>
  <tr> <td align="right"> 16 </td> <td> Lightning </td> <td align="right"> 4168 </td> <td align="right"> 5512150.00 </td> <td align="right"> 1322.49 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 3000000.00 </td> </tr>
  <tr> <td align="right"> 17 </td> <td> Dust Storm </td> <td align="right"> 254 </td> <td align="right"> 2850000.00 </td> <td align="right"> 11220.47 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1500000.00 </td> </tr>
  <tr> <td align="right"> 18 </td> <td> Coastal Flooding </td> <td align="right"> 1073 </td> <td align="right"> 911000.00 </td> <td align="right"> 849.02 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 750000.00 </td> </tr>
  <tr> <td align="right"> 19 </td> <td> Tsunami </td> <td align="right">  19 </td> <td align="right"> 20000.00 </td> <td align="right"> 1052.63 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 20000.00 </td> </tr>
  <tr> <td align="right"> 20 </td> <td> Avalanche </td> <td align="right"> 154 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> </tr>
   </table>




### Conclusion

The United States population health is impacted by weather events in terms of both fatalities and injuries.

The most fatalities occur due to tornados. Heat is the second most prevelant cause of fatalities.

More injuries are incurred as a result of tornados more than any other event group.

The greatest economic consequences are more difficult to determine due to the inconsistency of the cost unit coding. If ambiguous units are assumed to be in millions, extremes of water- drought and flooding are each more than twice as expensive as the next event group in the crop damage rankings.

For property damages, hurricanes, tornados, and flooding are the most expensive event groups.


 
### Appendix

Additional information can be found at these websites:

https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf

https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf


#### EVTYPE Grouping Table
The mapping table needs to be extracted from the HTML/PDF and created locally to reproduce the results.


```r
print(xtable(envTypeMapping) ,  type="html")                                                     
```

<!-- html table generated in R 3.1.3 by xtable 1.8-0 package -->
<!-- Wed Nov 18 13:41:16 2015 -->
<table border=1>
<tr> <th>  </th> <th> ENVTYPE </th> <th> EnvTypeGroup </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> AVALANCE </td> <td> Avalanche </td> </tr>
  <tr> <td align="right"> 2 </td> <td> AVALANCHE </td> <td> Avalanche </td> </tr>
  <tr> <td align="right"> 3 </td> <td> HEAVY SNOW/BLIZZARD/AVALANCHE </td> <td> Avalanche </td> </tr>
  <tr> <td align="right"> 4 </td> <td> BLIZZARD </td> <td> Blizzard </td> </tr>
  <tr> <td align="right"> 5 </td> <td> BLIZZARD AND EXTREME WIND CHIL </td> <td> Blizzard </td> </tr>
  <tr> <td align="right"> 6 </td> <td> BLIZZARD AND HEAVY SNOW </td> <td> Blizzard </td> </tr>
  <tr> <td align="right"> 7 </td> <td> Blizzard Summary </td> <td> Blizzard </td> </tr>
  <tr> <td align="right"> 8 </td> <td> BLIZZARD WEATHER </td> <td> Blizzard </td> </tr>
  <tr> <td align="right"> 9 </td> <td> BLIZZARD/FREEZING RAIN </td> <td> Blizzard </td> </tr>
  <tr> <td align="right"> 10 </td> <td> BLIZZARD/HEAVY SNOW </td> <td> Blizzard </td> </tr>
  <tr> <td align="right"> 11 </td> <td> BLIZZARD/HIGH WIND </td> <td> Blizzard </td> </tr>
  <tr> <td align="right"> 12 </td> <td> BLIZZARD/WINTER STORM </td> <td> Blizzard </td> </tr>
  <tr> <td align="right"> 13 </td> <td> BLOWING DUST </td> <td> Blizzard </td> </tr>
  <tr> <td align="right"> 14 </td> <td> BLOWING SNOW </td> <td> Blizzard </td> </tr>
  <tr> <td align="right"> 15 </td> <td> GROUND BLIZZARD </td> <td> Blizzard </td> </tr>
  <tr> <td align="right"> 16 </td> <td> HIGH WIND/ BLIZZARD </td> <td> Blizzard </td> </tr>
  <tr> <td align="right"> 17 </td> <td> HIGH WIND/BLIZZARD </td> <td> Blizzard </td> </tr>
  <tr> <td align="right"> 18 </td> <td> HIGH WIND/BLIZZARD/FREEZING RA </td> <td> Blizzard </td> </tr>
  <tr> <td align="right"> 19 </td> <td>    HIGH SURF ADVISORY </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 20 </td> <td>  COASTAL FLOOD </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 21 </td> <td> ASTRONOMICAL HIGH TIDE </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 22 </td> <td> BEACH EROSIN </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 23 </td> <td> Beach Erosion </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 24 </td> <td> BEACH EROSION </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 25 </td> <td> BEACH EROSION/COASTAL FLOOD </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 26 </td> <td> BEACH FLOOD </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 27 </td> <td> BLOW-OUT TIDE </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 28 </td> <td> BLOW-OUT TIDES </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 29 </td> <td> COASTAL  FLOODING/EROSION </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 30 </td> <td> COASTAL EROSION </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 31 </td> <td> COASTAL FLOOD </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 32 </td> <td> Coastal Flood </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 33 </td> <td> COASTAL FLOODING </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 34 </td> <td> Coastal Flooding </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 35 </td> <td> coastal flooding </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 36 </td> <td> COASTAL FLOODING/EROSION </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 37 </td> <td> COASTAL STORM </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 38 </td> <td> Coastal Storm </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 39 </td> <td> COASTAL SURGE </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 40 </td> <td> COASTAL/TIDAL FLOOD </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 41 </td> <td> COASTALFLOOD </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 42 </td> <td> COASTALSTORM </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 43 </td> <td> CSTL FLOODING/EROSION </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 44 </td> <td> Erosion/Cstl Flood </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 45 </td> <td> HAZARDOUS SURF </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 46 </td> <td> HEAVY SURF </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 47 </td> <td> Heavy Surf </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 48 </td> <td> Heavy surf and wind </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 49 </td> <td> HEAVY SURF COASTAL FLOODING </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 50 </td> <td> HEAVY SURF/HIGH SURF </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 51 </td> <td> HEAVY SWELLS </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 52 </td> <td> HIGH SEAS </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 53 </td> <td> HIGH SURF </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 54 </td> <td> High Surf </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 55 </td> <td> HIGH SURF ADVISORIES </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 56 </td> <td> HIGH SURF ADVISORY </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 57 </td> <td> HIGH TIDES </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 58 </td> <td> HIGH WATER </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 59 </td> <td> HIGH WIND AND HIGH TIDES </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 60 </td> <td> HIGH WIND/SEAS </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 61 </td> <td> HIGH WINDS/COASTAL FLOOD </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 62 </td> <td> STORM SURGE </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 63 </td> <td> STORM SURGE/TIDE </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 64 </td> <td> TIDAL FLOOD </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 65 </td> <td> Tidal Flooding </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 66 </td> <td> TIDAL FLOODING </td> <td> Coastal Flooding </td> </tr>
  <tr> <td align="right"> 67 </td> <td> AGRICULTURAL FREEZE </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 68 </td> <td> BITTER WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 69 </td> <td> BITTER WIND CHILL TEMPERATURES </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 70 </td> <td> COLD </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 71 </td> <td> Cold </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 72 </td> <td> Cold and Frost </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 73 </td> <td> COLD AND FROST </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 74 </td> <td> COLD AND SNOW </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 75 </td> <td> COLD AND WET CONDITIONS </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 76 </td> <td> Cold Temperature </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 77 </td> <td> COLD TEMPERATURES </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 78 </td> <td> COLD WAVE </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 79 </td> <td> COLD WEATHER </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 80 </td> <td> COLD WIND CHILL TEMPERATURES </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 81 </td> <td> COLD/WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 82 </td> <td> COLD/WINDS </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 83 </td> <td> COOL AND WET </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 84 </td> <td> COOL SPELL </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 85 </td> <td> DAMAGING FREEZE </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 86 </td> <td> Damaging Freeze </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 87 </td> <td> EARLY FREEZE </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 88 </td> <td> EARLY FROST </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 89 </td> <td> Early Frost </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 90 </td> <td> Excessive Cold </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 91 </td> <td> Extended Cold </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 92 </td> <td> EXTREME COLD </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 93 </td> <td> Extreme Cold </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 94 </td> <td> EXTREME COLD/WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 95 </td> <td> EXTREME WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 96 </td> <td> EXTREME WIND CHILL/BLOWING SNO </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 97 </td> <td> EXTREME WIND CHILLS </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 98 </td> <td> EXTREME WINDCHILL </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 99 </td> <td> EXTREME WINDCHILL TEMPERATURES </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 100 </td> <td> EXTREME/RECORD COLD </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 101 </td> <td> FIRST FROST </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 102 </td> <td> FOG AND COLD TEMPERATURES </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 103 </td> <td> FREEZE </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 104 </td> <td> Freeze </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 105 </td> <td> FROST </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 106 </td> <td> Frost </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 107 </td> <td> Frost/Freeze </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 108 </td> <td> FROST/FREEZE </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 109 </td> <td> FROST\FREEZE </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 110 </td> <td> HARD FREEZE </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 111 </td> <td> HIGH WIND/LOW WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 112 </td> <td> HIGH WIND/WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 113 </td> <td> HIGH WIND/WIND CHILL/BLIZZARD </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 114 </td> <td> HIGH WINDS AND WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 115 </td> <td> HYPERTHERMIA/EXPOSURE </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 116 </td> <td> HYPOTHERMIA </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 117 </td> <td> Hypothermia/Exposure </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 118 </td> <td> HYPOTHERMIA/EXPOSURE </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 119 </td> <td> LATE FREEZE </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 120 </td> <td> LOW TEMPERATURE </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 121 </td> <td> LOW TEMPERATURE RECORD </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 122 </td> <td> LOW WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 123 </td> <td> PROLONG COLD </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 124 </td> <td> Prolong Cold </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 125 </td> <td> PROLONG COLD/SNOW </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 126 </td> <td> RECORD  COLD </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 127 </td> <td> RECORD COLD </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 128 </td> <td> Record Cold </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 129 </td> <td> RECORD COLD AND HIGH WIND </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 130 </td> <td> RECORD COLD/FROST </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 131 </td> <td> RECORD COOL </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 132 </td> <td> RECORD LOW </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 133 </td> <td> SEVERE COLD </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 134 </td> <td> Temperature record </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 135 </td> <td> Unseasonable Cold </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 136 </td> <td> UNSEASONABLY COLD </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 137 </td> <td> UNSEASONABLY COOL </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 138 </td> <td> UNSEASONABLY COOL &amp; WET </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 139 </td> <td> UNSEASONAL LOW TEMP </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 140 </td> <td> UNUSUALLY COLD </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 141 </td> <td> WIND CHILL </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 142 </td> <td> WIND CHILL/HIGH WIND </td> <td> Cold </td> </tr>
  <tr> <td align="right"> 143 </td> <td> DAM BREAK </td> <td> Dam Failure </td> </tr>
  <tr> <td align="right"> 144 </td> <td> DAM FAILURE </td> <td> Dam Failure </td> </tr>
  <tr> <td align="right"> 145 </td> <td> ABNORMALLY DRY </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 146 </td> <td> BELOW NORMAL PRECIPITATION </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 147 </td> <td> DRIEST MONTH </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 148 </td> <td> DROUGHT </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 149 </td> <td> DROUGHT/EXCESSIVE HEAT </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 150 </td> <td> DRY </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 151 </td> <td> DRY CONDITIONS </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 152 </td> <td> DRY PATTERN </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 153 </td> <td> DRY SPELL </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 154 </td> <td> DRY WEATHER </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 155 </td> <td> DRYNESS </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 156 </td> <td> EXCESSIVE HEAT/DROUGHT </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 157 </td> <td> EXCESSIVELY DRY </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 158 </td> <td> HEAT DROUGHT </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 159 </td> <td> HEAT WAVE DROUGHT </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 160 </td> <td> Record dry month </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 161 </td> <td> RECORD DRYNESS </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 162 </td> <td> RECORD LOW RAINFALL </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 163 </td> <td> UNSEASONABLY DRY </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 164 </td> <td> VERY DRY </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 165 </td> <td> WARM DRY CONDITIONS </td> <td> Drought </td> </tr>
  <tr> <td align="right"> 166 </td> <td> DROWNING </td> <td> Drowning </td> </tr>
  <tr> <td align="right"> 167 </td> <td> DUST DEVEL </td> <td> Dust Storm </td> </tr>
  <tr> <td align="right"> 168 </td> <td> DUST DEVIL </td> <td> Dust Storm </td> </tr>
  <tr> <td align="right"> 169 </td> <td> Dust Devil </td> <td> Dust Storm </td> </tr>
  <tr> <td align="right"> 170 </td> <td> DUST DEVIL WATERSPOUT </td> <td> Dust Storm </td> </tr>
  <tr> <td align="right"> 171 </td> <td> DUST STORM </td> <td> Dust Storm </td> </tr>
  <tr> <td align="right"> 172 </td> <td> DUST STORM/HIGH WINDS </td> <td> Dust Storm </td> </tr>
  <tr> <td align="right"> 173 </td> <td> DUSTSTORM </td> <td> Dust Storm </td> </tr>
  <tr> <td align="right"> 174 </td> <td> HIGH WINDS DUST STORM </td> <td> Dust Storm </td> </tr>
  <tr> <td align="right"> 175 </td> <td> SAHARAN DUST </td> <td> Dust Storm </td> </tr>
  <tr> <td align="right"> 176 </td> <td> Saharan Dust </td> <td> Dust Storm </td> </tr>
  <tr> <td align="right"> 177 </td> <td> BRUSH FIRE </td> <td> Fire </td> </tr>
  <tr> <td align="right"> 178 </td> <td> BRUSH FIRES </td> <td> Fire </td> </tr>
  <tr> <td align="right"> 179 </td> <td> DENSE SMOKE </td> <td> Fire </td> </tr>
  <tr> <td align="right"> 180 </td> <td> FOREST FIRES </td> <td> Fire </td> </tr>
  <tr> <td align="right"> 181 </td> <td> GRASS FIRES </td> <td> Fire </td> </tr>
  <tr> <td align="right"> 182 </td> <td> RED FLAG FIRE WX </td> <td> Fire </td> </tr>
  <tr> <td align="right"> 183 </td> <td> SMOKE </td> <td> Fire </td> </tr>
  <tr> <td align="right"> 184 </td> <td> WILD FIRES </td> <td> Fire </td> </tr>
  <tr> <td align="right"> 185 </td> <td> WILD/FOREST FIRE </td> <td> Fire </td> </tr>
  <tr> <td align="right"> 186 </td> <td> WILD/FOREST FIRES </td> <td> Fire </td> </tr>
  <tr> <td align="right"> 187 </td> <td> WILDFIRE </td> <td> Fire </td> </tr>
  <tr> <td align="right"> 188 </td> <td> WILDFIRES </td> <td> Fire </td> </tr>
  <tr> <td align="right"> 189 </td> <td>  FLASH FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 190 </td> <td> BREAKUP FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 191 </td> <td> FLASH FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 192 </td> <td> FLASH FLOOD - HEAVY RAIN </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 193 </td> <td> FLASH FLOOD FROM ICE JAMS </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 194 </td> <td> FLASH FLOOD WINDS </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 195 </td> <td> FLASH FLOOD/ </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 196 </td> <td> FLASH FLOOD/ FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 197 </td> <td> FLASH FLOOD/ STREET </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 198 </td> <td> FLASH FLOOD/FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 199 </td> <td> FLASH FLOOD/HEAVY RAIN </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 200 </td> <td> FLASH FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 201 </td> <td> FLASH FLOODING/FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 202 </td> <td> FLASH FLOODING/THUNDERSTORM WI </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 203 </td> <td> FLASH FLOODS </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 204 </td> <td> FLASH FLOOODING </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 205 </td> <td> FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 206 </td> <td> Flood </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 207 </td> <td> FLOOD &amp; HEAVY RAIN </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 208 </td> <td> FLOOD FLASH </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 209 </td> <td> FLOOD FLOOD/FLASH </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 210 </td> <td> FLOOD WATCH/ </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 211 </td> <td> FLOOD/FLASH </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 212 </td> <td> FLOOD/FLASH FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 213 </td> <td> Flood/Flash Flood </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 214 </td> <td> FLOOD/FLASH FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 215 </td> <td> FLOOD/FLASH/FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 216 </td> <td> FLOOD/FLASHFLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 217 </td> <td> FLOOD/RAIN/WIND </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 218 </td> <td> FLOOD/RAIN/WINDS </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 219 </td> <td> FLOOD/RIVER FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 220 </td> <td> Flood/Strong Wind </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 221 </td> <td> FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 222 </td> <td> FLOODING/HEAVY RAIN </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 223 </td> <td> FLOODS </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 224 </td> <td> HIGH WINDS/FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 225 </td> <td> HIGHWAY FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 226 </td> <td> LAKE FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 227 </td> <td> LAKESHORE FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 228 </td> <td> LOCAL FLASH FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 229 </td> <td> LOCAL FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 230 </td> <td> MAJOR FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 231 </td> <td> MINOR FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 232 </td> <td> MINOR FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 233 </td> <td> Minor Flooding </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 234 </td> <td> RAPIDLY RISING WATER </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 235 </td> <td> RIVER AND STREAM FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 236 </td> <td> RIVER FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 237 </td> <td> RIVER FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 238 </td> <td> River Flooding </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 239 </td> <td> RURAL FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 240 </td> <td> SMALL STREAM </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 241 </td> <td> SMALL STREAM AND </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 242 </td> <td> SMALL STREAM AND URBAN FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 243 </td> <td> SMALL STREAM AND URBAN FLOODIN </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 244 </td> <td> SMALL STREAM FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 245 </td> <td> SMALL STREAM FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 246 </td> <td> SMALL STREAM URBAN FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 247 </td> <td> SMALL STREAM/URBAN FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 248 </td> <td> Sml Stream Fld </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 249 </td> <td> SNOWMELT FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 250 </td> <td> STREAM FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 251 </td> <td> STREET FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 252 </td> <td> STREET FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 253 </td> <td> URBAN AND SMALL </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 254 </td> <td> URBAN AND SMALL STREAM </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 255 </td> <td> URBAN AND SMALL STREAM FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 256 </td> <td> URBAN AND SMALL STREAM FLOODIN </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 257 </td> <td> URBAN FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 258 </td> <td> Urban flood </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 259 </td> <td> Urban Flood </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 260 </td> <td> URBAN FLOOD LANDSLIDE </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 261 </td> <td> URBAN FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 262 </td> <td> Urban Flooding </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 263 </td> <td> URBAN FLOODS </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 264 </td> <td> URBAN SMALL </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 265 </td> <td> URBAN SMALL STREAM FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 266 </td> <td> URBAN/SMALL </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 267 </td> <td> URBAN/SMALL FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 268 </td> <td> URBAN/SMALL STREAM </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 269 </td> <td> URBAN/SMALL STREAM  FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 270 </td> <td> URBAN/SMALL STREAM FLOOD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 271 </td> <td> URBAN/SMALL STREAM FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 272 </td> <td> URBAN/SMALL STRM FLDG </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 273 </td> <td> URBAN/SML STREAM FLD </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 274 </td> <td> URBAN/SML STREAM FLDG </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 275 </td> <td> URBAN/STREET FLOODING </td> <td> Flooding </td> </tr>
  <tr> <td align="right"> 276 </td> <td> DENSE FOG </td> <td> Fog </td> </tr>
  <tr> <td align="right"> 277 </td> <td> FOG </td> <td> Fog </td> </tr>
  <tr> <td align="right"> 278 </td> <td> PATCHY DENSE FOG </td> <td> Fog </td> </tr>
  <tr> <td align="right"> 279 </td> <td> DEEP HAIL </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 280 </td> <td> HAIL </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 281 </td> <td> HAIL 0.75 </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 282 </td> <td> HAIL 0.88 </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 283 </td> <td> HAIL 075 </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 284 </td> <td> HAIL 088 </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 285 </td> <td> HAIL 1.00 </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 286 </td> <td> HAIL 1.75 </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 287 </td> <td> HAIL 1.75) </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 288 </td> <td> HAIL 100 </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 289 </td> <td> HAIL 125 </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 290 </td> <td> HAIL 150 </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 291 </td> <td> HAIL 175 </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 292 </td> <td> HAIL 200 </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 293 </td> <td> HAIL 225 </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 294 </td> <td> HAIL 275 </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 295 </td> <td> HAIL 450 </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 296 </td> <td> HAIL 75 </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 297 </td> <td> HAIL 80 </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 298 </td> <td> HAIL 88 </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 299 </td> <td> HAIL ALOFT </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 300 </td> <td> HAIL DAMAGE </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 301 </td> <td> HAIL FLOODING </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 302 </td> <td> HAIL STORM </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 303 </td> <td> Hail(0.75) </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 304 </td> <td> HAIL/ICY ROADS </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 305 </td> <td> HAIL/WIND </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 306 </td> <td> HAIL/WINDS </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 307 </td> <td> HAILSTORM </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 308 </td> <td> HAILSTORMS </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 309 </td> <td> LATE SEASON HAIL </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 310 </td> <td> MARINE HAIL </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 311 </td> <td> NON SEVERE HAIL </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 312 </td> <td> SMALL HAIL </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 313 </td> <td> small hail </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 314 </td> <td> Small Hail </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 315 </td> <td> THUNDERSTORM HAIL </td> <td> Hail </td> </tr>
  <tr> <td align="right"> 316 </td> <td> ABNORMAL WARMTH </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 317 </td> <td> DRY HOT WEATHER </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 318 </td> <td> EXCESSIVE HEAT </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 319 </td> <td> EXTREME HEAT </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 320 </td> <td> HEAT </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 321 </td> <td> HEAT WAVE </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 322 </td> <td> Heat Wave </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 323 </td> <td> HEAT WAVES </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 324 </td> <td> HEAT/DROUGHT </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 325 </td> <td> Heatburst </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 326 </td> <td> HIGH TEMPERATURE RECORD </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 327 </td> <td> Hot and Dry </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 328 </td> <td> HOT PATTERN </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 329 </td> <td> HOT SPELL </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 330 </td> <td> HOT WEATHER </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 331 </td> <td> HOT/DRY PATTERN </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 332 </td> <td> PROLONG WARMTH </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 333 </td> <td> RECORD HEAT </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 334 </td> <td> Record Heat </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 335 </td> <td> RECORD HEAT WAVE </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 336 </td> <td> RECORD HIGH </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 337 </td> <td> Record High </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 338 </td> <td> RECORD HIGH TEMPERATURE </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 339 </td> <td> RECORD HIGH TEMPERATURES </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 340 </td> <td> Record temperature </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 341 </td> <td> RECORD TEMPERATURE </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 342 </td> <td> RECORD TEMPERATURES </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 343 </td> <td> Record Temperatures </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 344 </td> <td> RECORD WARM </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 345 </td> <td> RECORD WARM TEMPS. </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 346 </td> <td> RECORD WARMTH </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 347 </td> <td> Record Warmth </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 348 </td> <td> RECORD/EXCESSIVE HEAT </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 349 </td> <td> RED FLAG CRITERIA </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 350 </td> <td> UNSEASONABLY HOT </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 351 </td> <td> UNSEASONABLY WARM </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 352 </td> <td> UNSEASONABLY WARM &amp; WET </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 353 </td> <td> UNSEASONABLY WARM AND DRY </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 354 </td> <td> UNSEASONABLY WARM YEAR </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 355 </td> <td> UNSEASONABLY WARM/WET </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 356 </td> <td> UNUSUAL WARMTH </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 357 </td> <td> UNUSUAL/RECORD WARMTH </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 358 </td> <td> UNUSUALLY WARM </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 359 </td> <td> VERY WARM </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 360 </td> <td> WARM WEATHER </td> <td> Heat </td> </tr>
  <tr> <td align="right"> 361 </td> <td> HEAVY SEAS </td> <td> Heavy Seas </td> </tr>
  <tr> <td align="right"> 362 </td> <td> HIGH  SWELLS </td> <td> Heavy Seas </td> </tr>
  <tr> <td align="right"> 363 </td> <td> HIGH SWELLS </td> <td> Heavy Seas </td> </tr>
  <tr> <td align="right"> 364 </td> <td> HIGH WAVES </td> <td> Heavy Seas </td> </tr>
  <tr> <td align="right"> 365 </td> <td> HIGH WIND AND SEAS </td> <td> Heavy Seas </td> </tr>
  <tr> <td align="right"> 366 </td> <td> ROGUE WAVE </td> <td> Heavy Seas </td> </tr>
  <tr> <td align="right"> 367 </td> <td> ROUGH SEAS </td> <td> Heavy Seas </td> </tr>
  <tr> <td align="right"> 368 </td> <td> ROUGH SURF </td> <td> Heavy Seas </td> </tr>
  <tr> <td align="right"> 369 </td> <td> HURRICANE </td> <td> Hurricane </td> </tr>
  <tr> <td align="right"> 370 </td> <td> Hurricane Edouard </td> <td> Hurricane </td> </tr>
  <tr> <td align="right"> 371 </td> <td> HURRICANE EMILY </td> <td> Hurricane </td> </tr>
  <tr> <td align="right"> 372 </td> <td> HURRICANE ERIN </td> <td> Hurricane </td> </tr>
  <tr> <td align="right"> 373 </td> <td> HURRICANE FELIX </td> <td> Hurricane </td> </tr>
  <tr> <td align="right"> 374 </td> <td> HURRICANE GORDON </td> <td> Hurricane </td> </tr>
  <tr> <td align="right"> 375 </td> <td> HURRICANE OPAL </td> <td> Hurricane </td> </tr>
  <tr> <td align="right"> 376 </td> <td> HURRICANE OPAL/HIGH WINDS </td> <td> Hurricane </td> </tr>
  <tr> <td align="right"> 377 </td> <td> HURRICANE/TYPHOON </td> <td> Hurricane </td> </tr>
  <tr> <td align="right"> 378 </td> <td> HURRICANE-GENERATED SWELLS </td> <td> Hurricane </td> </tr>
  <tr> <td align="right"> 379 </td> <td> REMNANTS OF FLOYD </td> <td> Hurricane </td> </tr>
  <tr> <td align="right"> 380 </td> <td> TYPHOON </td> <td> Hurricane </td> </tr>
  <tr> <td align="right"> 381 </td> <td> LACK OF SNOW </td> <td> Lack of Snow </td> </tr>
  <tr> <td align="right"> 382 </td> <td> FLASH FLOOD LANDSLIDES </td> <td> Landslide </td> </tr>
  <tr> <td align="right"> 383 </td> <td> FLASH FLOOD/LANDSLIDE </td> <td> Landslide </td> </tr>
  <tr> <td align="right"> 384 </td> <td> LANDSLIDE </td> <td> Landslide </td> </tr>
  <tr> <td align="right"> 385 </td> <td> LANDSLIDE/URBAN FLOOD </td> <td> Landslide </td> </tr>
  <tr> <td align="right"> 386 </td> <td> LANDSLIDES </td> <td> Landslide </td> </tr>
  <tr> <td align="right"> 387 </td> <td> Landslump </td> <td> Landslide </td> </tr>
  <tr> <td align="right"> 388 </td> <td> LANDSLUMP </td> <td> Landslide </td> </tr>
  <tr> <td align="right"> 389 </td> <td> MUD SLIDE </td> <td> Landslide </td> </tr>
  <tr> <td align="right"> 390 </td> <td> MUD SLIDES </td> <td> Landslide </td> </tr>
  <tr> <td align="right"> 391 </td> <td> MUD SLIDES URBAN FLOODING </td> <td> Landslide </td> </tr>
  <tr> <td align="right"> 392 </td> <td> MUD/ROCK SLIDE </td> <td> Landslide </td> </tr>
  <tr> <td align="right"> 393 </td> <td> MUDSLIDE </td> <td> Landslide </td> </tr>
  <tr> <td align="right"> 394 </td> <td> Mudslide </td> <td> Landslide </td> </tr>
  <tr> <td align="right"> 395 </td> <td> MUDSLIDE/LANDSLIDE </td> <td> Landslide </td> </tr>
  <tr> <td align="right"> 396 </td> <td> MUDSLIDES </td> <td> Landslide </td> </tr>
  <tr> <td align="right"> 397 </td> <td> Mudslides </td> <td> Landslide </td> </tr>
  <tr> <td align="right"> 398 </td> <td> ROCK SLIDE </td> <td> Landslide </td> </tr>
  <tr> <td align="right"> 399 </td> <td>  LIGHTNING </td> <td> Lightning </td> </tr>
  <tr> <td align="right"> 400 </td> <td> LIGHTING </td> <td> Lightning </td> </tr>
  <tr> <td align="right"> 401 </td> <td> LIGHTNING </td> <td> Lightning </td> </tr>
  <tr> <td align="right"> 402 </td> <td> LIGHTNING  WAUSEON </td> <td> Lightning </td> </tr>
  <tr> <td align="right"> 403 </td> <td> LIGHTNING AND HEAVY RAIN </td> <td> Lightning </td> </tr>
  <tr> <td align="right"> 404 </td> <td> LIGHTNING AND THUNDERSTORM WIN </td> <td> Lightning </td> </tr>
  <tr> <td align="right"> 405 </td> <td> LIGHTNING AND WINDS </td> <td> Lightning </td> </tr>
  <tr> <td align="right"> 406 </td> <td> LIGHTNING DAMAGE </td> <td> Lightning </td> </tr>
  <tr> <td align="right"> 407 </td> <td> LIGHTNING FIRE </td> <td> Lightning </td> </tr>
  <tr> <td align="right"> 408 </td> <td> LIGHTNING INJURY </td> <td> Lightning </td> </tr>
  <tr> <td align="right"> 409 </td> <td> LIGHTNING THUNDERSTORM WINDS </td> <td> Lightning </td> </tr>
  <tr> <td align="right"> 410 </td> <td> LIGHTNING THUNDERSTORM WINDSS </td> <td> Lightning </td> </tr>
  <tr> <td align="right"> 411 </td> <td> LIGHTNING. </td> <td> Lightning </td> </tr>
  <tr> <td align="right"> 412 </td> <td> LIGHTNING/HEAVY RAIN </td> <td> Lightning </td> </tr>
  <tr> <td align="right"> 413 </td> <td> LIGNTNING </td> <td> Lightning </td> </tr>
  <tr> <td align="right"> 414 </td> <td> ASTRONOMICAL LOW TIDE </td> <td> Low Tide </td> </tr>
  <tr> <td align="right"> 415 </td> <td> Marine Accident </td> <td> Marine Mishap </td> </tr>
  <tr> <td align="right"> 416 </td> <td> MARINE MISHAP </td> <td> Marine Mishap </td> </tr>
  <tr> <td align="right"> 417 </td> <td> Mild and Dry Pattern </td> <td> Mild </td> </tr>
  <tr> <td align="right"> 418 </td> <td> MILD PATTERN </td> <td> Mild </td> </tr>
  <tr> <td align="right"> 419 </td> <td> MILD/DRY PATTERN </td> <td> Mild </td> </tr>
  <tr> <td align="right"> 420 </td> <td> MONTHLY TEMPERATURE </td> <td> Mild </td> </tr>
  <tr> <td align="right"> 421 </td> <td> No Severe Weather </td> <td> Mild </td> </tr>
  <tr> <td align="right"> 422 </td> <td> NONE </td> <td> Mild </td> </tr>
  <tr> <td align="right"> 423 </td> <td> NORTHERN LIGHTS </td> <td> Northern Lights </td> </tr>
  <tr> <td align="right"> 424 </td> <td> ABNORMALLY WET </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 425 </td> <td> EARLY RAIN </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 426 </td> <td> EXCESSIVE PRECIPITATION </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 427 </td> <td> EXCESSIVE RAIN </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 428 </td> <td> EXCESSIVE RAINFALL </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 429 </td> <td> EXCESSIVE WETNESS </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 430 </td> <td> EXTREMELY WET </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 431 </td> <td> HEAVY PRECIPATATION </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 432 </td> <td> HEAVY PRECIPITATION </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 433 </td> <td> Heavy Precipitation </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 434 </td> <td> HEAVY RAIN </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 435 </td> <td> Heavy Rain </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 436 </td> <td> Heavy rain </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 437 </td> <td> HEAVY RAIN AND FLOOD </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 438 </td> <td> Heavy Rain and Wind </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 439 </td> <td> HEAVY RAIN EFFECTS </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 440 </td> <td> HEAVY RAIN/FLOODING </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 441 </td> <td> Heavy Rain/High Surf </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 442 </td> <td> HEAVY RAIN/LIGHTNING </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 443 </td> <td> HEAVY RAIN/MUDSLIDES/FLOOD </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 444 </td> <td> HEAVY RAIN/SEVERE WEATHER </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 445 </td> <td> HEAVY RAIN/SMALL STREAM URBAN </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 446 </td> <td> HEAVY RAIN/URBAN FLOOD </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 447 </td> <td> HEAVY RAIN/WIND </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 448 </td> <td> HEAVY RAIN; URBAN FLOOD WINDS; </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 449 </td> <td> HEAVY RAINFALL </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 450 </td> <td> HEAVY RAINS </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 451 </td> <td> HEAVY RAINS/FLOODING </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 452 </td> <td> HEAVY SHOWER </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 453 </td> <td> HEAVY SHOWERS </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 454 </td> <td> HIGH WINDS HEAVY RAINS </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 455 </td> <td> HIGH WINDS/HEAVY RAIN </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 456 </td> <td> HVY RAIN </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 457 </td> <td> LOCALLY HEAVY RAIN </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 458 </td> <td> MONTHLY PRECIPITATION </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 459 </td> <td> Monthly Rainfall </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 460 </td> <td> MONTHLY RAINFALL </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 461 </td> <td> NORMAL PRECIPITATION </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 462 </td> <td> PROLONGED RAIN </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 463 </td> <td> RAIN </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 464 </td> <td> RAIN (HEAVY) </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 465 </td> <td> RAIN AND WIND </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 466 </td> <td> Rain Damage </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 467 </td> <td> RAIN/WIND </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 468 </td> <td> RAINSTORM </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 469 </td> <td> RECORD PRECIPITATION </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 470 </td> <td> RECORD RAINFALL </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 471 </td> <td> RECORD/EXCESSIVE RAINFALL </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 472 </td> <td> TORRENTIAL RAIN </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 473 </td> <td> Torrential Rainfall </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 474 </td> <td> TSTM HEAVY RAIN </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 475 </td> <td> UNSEASONABLY WET </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 476 </td> <td> UNSEASONAL RAIN </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 477 </td> <td> Wet Month </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 478 </td> <td> WET WEATHER </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 479 </td> <td> Wet Year </td> <td> Rain </td> </tr>
  <tr> <td align="right"> 480 </td> <td> RIP CURRENT </td> <td> Rip Current </td> </tr>
  <tr> <td align="right"> 481 </td> <td> RIP CURRENTS </td> <td> Rip Current </td> </tr>
  <tr> <td align="right"> 482 </td> <td> RIP CURRENTS HEAVY SURF </td> <td> Rip Current </td> </tr>
  <tr> <td align="right"> 483 </td> <td> RIP CURRENTS/HEAVY SURF </td> <td> Rip Current </td> </tr>
  <tr> <td align="right"> 484 </td> <td> SEICHE </td> <td> Seiche </td> </tr>
  <tr> <td align="right"> 485 </td> <td>  TSTM WIND </td> <td> Thunderstorm </td> </tr>
  <tr> <td align="right"> 486 </td> <td>  TSTM WIND (G45) </td> <td> Thunderstorm </td> </tr>
  <tr> <td align="right"> 487 </td> <td> DRY MICROBURST </td> <td> Thunderstorm </td> </tr>
  <tr> <td align="right"> 488 </td> <td> DRY MICROBURST 50 </td> <td> Thunderstorm </td> </tr>
  <tr> <td align="right"> 489 </td> <td> DRY MICROBURST 53 </td> <td> Thunderstorm </td> </tr>
  <tr> <td align="right"> 490 </td> <td> DRY MICROBURST 58 </td> <td> Thunderstorm </td> </tr>
  <tr> <td align="right"> 491 </td> <td> DRY MICROBURST 61 </td> <td> Thunderstorm </td> </tr>
  <tr> <td align="right"> 492 </td> <td> DRY MICROBURST 84 </td> <td> Thunderstorm </td> </tr>
  <tr> <td align="right"> 493 </td> <td> DRY MICROBURST WINDS </td> <td> Thunderstorm </td> </tr>
  <tr> <td align="right"> 494 </td> <td> DRY MIRCOBURST WINDS </td> <td> Thunderstorm </td> </tr>
  <tr> <td align="right"> 495 </td> <td> MARINE THUNDERSTORM WIND </td> <td> Thunderstorm </td> </tr>
  <tr> <td align="right"> 496 </td> <td> MARINE TSTM WIND </td> <td> Thunderstorm </td> </tr>
  <tr> <td align="right"> 497 </td> <td> Metro Storm </td> <td> Thunderstorm </td> </tr>
  <tr> <td align="right"> 498 </td> <td> SEVERE THUNDERSTORM </td> <td> Thunderstorm </td> </tr>
  <tr> <td align="right"> 499 </td> <td> SEVERE THUNDERSTORM WINDS </td> <td> Thunderstorm </td> </tr>
  <tr> <td align="right"> 500 </td> <td> SEVERE THUNDERSTORMS </td> <td> Thunderstorm </td> </tr>
  <tr> <td align="right"> 501 </td> <td> SEVERE TURBULENCE </td> <td> Thunderstorm </td> </tr>
  <tr> <td align="right"> 502 </td> <td>  WATERSPOUT </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 503 </td> <td> COLD AIR FUNNEL </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 504 </td> <td> COLD AIR FUNNELS </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 505 </td> <td> COLD AIR TORNADO </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 506 </td> <td> FUNNEL </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 507 </td> <td> FUNNEL CLOUD </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 508 </td> <td> Funnel Cloud </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 509 </td> <td> FUNNEL CLOUD. </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 510 </td> <td> FUNNEL CLOUD/HAIL </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 511 </td> <td> FUNNEL CLOUDS </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 512 </td> <td> FUNNELS </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 513 </td> <td> LANDSPOUT </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 514 </td> <td> LARGE WALL CLOUD </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 515 </td> <td> ROTATING WALL CLOUD </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 516 </td> <td> TORNADO </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 517 </td> <td> TORNADO DEBRIS </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 518 </td> <td> TORNADO F0 </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 519 </td> <td> TORNADO F1 </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 520 </td> <td> TORNADO F2 </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 521 </td> <td> TORNADO F3 </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 522 </td> <td> TORNADO/WATERSPOUT </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 523 </td> <td> TORNADOES </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 524 </td> <td> TORNADOES </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 525 </td> <td> TORNADOS </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 526 </td> <td> TORNDAO </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 527 </td> <td> WALL CLOUD </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 528 </td> <td> WALL CLOUD/FUNNEL CLOUD </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 529 </td> <td> WATER SPOUT </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 530 </td> <td> WATERSPOUT </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 531 </td> <td> WATERSPOUT- </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 532 </td> <td> WATERSPOUT FUNNEL CLOUD </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 533 </td> <td> WATERSPOUT TORNADO </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 534 </td> <td> WATERSPOUT/ </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 535 </td> <td> WATERSPOUT/ TORNADO </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 536 </td> <td> WATERSPOUT/TORNADO </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 537 </td> <td> WATERSPOUTS </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 538 </td> <td> WATERSPOUT-TORNADO </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 539 </td> <td> WAYTERSPOUT </td> <td> Tornado/Waterspout </td> </tr>
  <tr> <td align="right"> 540 </td> <td> TROPICAL DEPRESSION </td> <td> Tropical Depression </td> </tr>
  <tr> <td align="right"> 541 </td> <td> TROPICAL STORM </td> <td> Tropical Storm </td> </tr>
  <tr> <td align="right"> 542 </td> <td> TROPICAL STORM ALBERTO </td> <td> Tropical Storm </td> </tr>
  <tr> <td align="right"> 543 </td> <td> TROPICAL STORM DEAN </td> <td> Tropical Storm </td> </tr>
  <tr> <td align="right"> 544 </td> <td> TROPICAL STORM GORDON </td> <td> Tropical Storm </td> </tr>
  <tr> <td align="right"> 545 </td> <td> TROPICAL STORM JERRY </td> <td> Tropical Storm </td> </tr>
  <tr> <td align="right"> 546 </td> <td> TSUNAMI </td> <td> Tsunami </td> </tr>
  <tr> <td align="right"> 547 </td> <td> ? </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 548 </td> <td> APACHE COUNTY </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 549 </td> <td> EXCESSIVE </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 550 </td> <td> OTHER </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 551 </td> <td> Other </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 552 </td> <td> SOUTHEAST </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 553 </td> <td> Summary August 10 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 554 </td> <td> Summary August 11 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 555 </td> <td> Summary August 17 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 556 </td> <td> Summary August 21 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 557 </td> <td> Summary August 2-3 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 558 </td> <td> Summary August 28 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 559 </td> <td> Summary August 4 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 560 </td> <td> Summary August 7 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 561 </td> <td> Summary August 9 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 562 </td> <td> Summary Jan 17 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 563 </td> <td> Summary July 23-24 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 564 </td> <td> Summary June 18-19 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 565 </td> <td> Summary June 5-6 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 566 </td> <td> Summary June 6 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 567 </td> <td> Summary of April 12 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 568 </td> <td> Summary of April 13 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 569 </td> <td> Summary of April 21 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 570 </td> <td> Summary of April 27 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 571 </td> <td> Summary of April 3rd </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 572 </td> <td> Summary of August 1 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 573 </td> <td> Summary of July 11 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 574 </td> <td> Summary of July 2 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 575 </td> <td> Summary of July 22 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 576 </td> <td> Summary of July 26 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 577 </td> <td> Summary of July 29 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 578 </td> <td> Summary of July 3 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 579 </td> <td> Summary of June 10 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 580 </td> <td> Summary of June 11 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 581 </td> <td> Summary of June 12 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 582 </td> <td> Summary of June 13 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 583 </td> <td> Summary of June 15 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 584 </td> <td> Summary of June 16 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 585 </td> <td> Summary of June 18 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 586 </td> <td> Summary of June 23 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 587 </td> <td> Summary of June 24 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 588 </td> <td> Summary of June 3 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 589 </td> <td> Summary of June 30 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 590 </td> <td> Summary of June 4 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 591 </td> <td> Summary of June 6 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 592 </td> <td> Summary of March 14 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 593 </td> <td> Summary of March 23 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 594 </td> <td> Summary of March 24 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 595 </td> <td> SUMMARY OF MARCH 24-25 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 596 </td> <td> SUMMARY OF MARCH 27 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 597 </td> <td> SUMMARY OF MARCH 29 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 598 </td> <td> Summary of May 10 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 599 </td> <td> Summary of May 13 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 600 </td> <td> Summary of May 14 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 601 </td> <td> Summary of May 22 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 602 </td> <td> Summary of May 22 am </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 603 </td> <td> Summary of May 22 pm </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 604 </td> <td> Summary of May 26 am </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 605 </td> <td> Summary of May 26 pm </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 606 </td> <td> Summary of May 31 am </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 607 </td> <td> Summary of May 31 pm </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 608 </td> <td> Summary of May 9-10 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 609 </td> <td> Summary Sept. 25-26 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 610 </td> <td> Summary September 20 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 611 </td> <td> Summary September 23 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 612 </td> <td> Summary September 3 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 613 </td> <td> Summary September 4 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 614 </td> <td> Summary: Nov. 16 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 615 </td> <td> Summary: Nov. 6-7 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 616 </td> <td> Summary: Oct. 20-21 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 617 </td> <td> Summary: October 31 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 618 </td> <td> Summary: Sept. 18 </td> <td> Unknown </td> </tr>
  <tr> <td align="right"> 619 </td> <td> VOG </td> <td> Volcano </td> </tr>
  <tr> <td align="right"> 620 </td> <td> Volcanic Ash </td> <td> Volcano </td> </tr>
  <tr> <td align="right"> 621 </td> <td> VOLCANIC ASH </td> <td> Volcano </td> </tr>
  <tr> <td align="right"> 622 </td> <td> Volcanic Ash Plume </td> <td> Volcano </td> </tr>
  <tr> <td align="right"> 623 </td> <td> VOLCANIC ASHFALL </td> <td> Volcano </td> </tr>
  <tr> <td align="right"> 624 </td> <td> VOLCANIC ERUPTION </td> <td> Volcano </td> </tr>
  <tr> <td align="right"> 625 </td> <td>  WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 626 </td> <td> DOWNBURST </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 627 </td> <td> DOWNBURST WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 628 </td> <td> Gradient wind </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 629 </td> <td> GRADIENT WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 630 </td> <td> gradient wind </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 631 </td> <td> GRADIENT WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 632 </td> <td> GUSTNADO </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 633 </td> <td> GUSTNADO AND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 634 </td> <td> GUSTY LAKE WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 635 </td> <td> GUSTY THUNDERSTORM WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 636 </td> <td> GUSTY THUNDERSTORM WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 637 </td> <td> Gusty Wind </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 638 </td> <td> GUSTY WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 639 </td> <td> GUSTY WIND/HAIL </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 640 </td> <td> GUSTY WIND/HVY RAIN </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 641 </td> <td> Gusty wind/rain </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 642 </td> <td> GUSTY WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 643 </td> <td> Gusty Winds </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 644 </td> <td> Gusty winds </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 645 </td> <td> HIGH </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 646 </td> <td> HIGH  WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 647 </td> <td> HIGH WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 648 </td> <td> High Wind </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 649 </td> <td> HIGH WIND (G40) </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 650 </td> <td> HIGH WIND 48 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 651 </td> <td> HIGH WIND 63 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 652 </td> <td> HIGH WIND 70 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 653 </td> <td> HIGH WIND DAMAGE </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 654 </td> <td> HIGH WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 655 </td> <td> HIGH WINDS 55 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 656 </td> <td> HIGH WINDS 57 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 657 </td> <td> HIGH WINDS 58 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 658 </td> <td> HIGH WINDS 63 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 659 </td> <td> HIGH WINDS 66 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 660 </td> <td> HIGH WINDS 67 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 661 </td> <td> HIGH WINDS 73 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 662 </td> <td> HIGH WINDS 76 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 663 </td> <td> HIGH WINDS 80 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 664 </td> <td> HIGH WINDS 82 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 665 </td> <td> HIGH WINDS/ </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 666 </td> <td> HIGH WINDS/COLD </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 667 </td> <td> MARINE HIGH WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 668 </td> <td> MARINE STRONG WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 669 </td> <td> MICROBURST </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 670 </td> <td> Microburst </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 671 </td> <td> MICROBURST WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 672 </td> <td> NON TSTM WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 673 </td> <td> NON-SEVERE WIND DAMAGE </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 674 </td> <td> NON-TSTM WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 675 </td> <td> STORM FORCE WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 676 </td> <td> STRONG WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 677 </td> <td> Strong Wind </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 678 </td> <td> STRONG WIND GUST </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 679 </td> <td> STRONG WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 680 </td> <td> Strong Winds </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 681 </td> <td> Strong winds </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 682 </td> <td> THUDERSTORM WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 683 </td> <td> THUNDEERSTORM WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 684 </td> <td> THUNDERESTORM WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 685 </td> <td> THUNDERSTORM </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 686 </td> <td> THUNDERSTORM  WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 687 </td> <td> THUNDERSTORM DAMAGE </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 688 </td> <td> THUNDERSTORM DAMAGE TO </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 689 </td> <td> THUNDERSTORM W INDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 690 </td> <td> THUNDERSTORM WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 691 </td> <td> Thunderstorm Wind </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 692 </td> <td> THUNDERSTORM WIND (G40) </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 693 </td> <td> THUNDERSTORM WIND 50 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 694 </td> <td> THUNDERSTORM WIND 52 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 695 </td> <td> THUNDERSTORM WIND 56 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 696 </td> <td> THUNDERSTORM WIND 59 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 697 </td> <td> THUNDERSTORM WIND 59 MPH </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 698 </td> <td> THUNDERSTORM WIND 59 MPH. </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 699 </td> <td> THUNDERSTORM WIND 60 MPH </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 700 </td> <td> THUNDERSTORM WIND 65 MPH </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 701 </td> <td> THUNDERSTORM WIND 65MPH </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 702 </td> <td> THUNDERSTORM WIND 69 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 703 </td> <td> THUNDERSTORM WIND 98 MPH </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 704 </td> <td> THUNDERSTORM WIND G50 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 705 </td> <td> THUNDERSTORM WIND G51 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 706 </td> <td> THUNDERSTORM WIND G52 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 707 </td> <td> THUNDERSTORM WIND G55 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 708 </td> <td> THUNDERSTORM WIND G60 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 709 </td> <td> THUNDERSTORM WIND G61 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 710 </td> <td> THUNDERSTORM WIND TREES </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 711 </td> <td> THUNDERSTORM WIND. </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 712 </td> <td> THUNDERSTORM WIND/ TREE </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 713 </td> <td> THUNDERSTORM WIND/ TREES </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 714 </td> <td> THUNDERSTORM WIND/AWNING </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 715 </td> <td> THUNDERSTORM WIND/HAIL </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 716 </td> <td> THUNDERSTORM WIND/LIGHTNING </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 717 </td> <td> THUNDERSTORM WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 718 </td> <td> THUNDERSTORM WINDS      LE CEN </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 719 </td> <td> THUNDERSTORM WINDS 13 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 720 </td> <td> THUNDERSTORM WINDS 2 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 721 </td> <td> THUNDERSTORM WINDS 50 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 722 </td> <td> THUNDERSTORM WINDS 52 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 723 </td> <td> THUNDERSTORM WINDS 53 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 724 </td> <td> THUNDERSTORM WINDS 60 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 725 </td> <td> THUNDERSTORM WINDS 61 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 726 </td> <td> THUNDERSTORM WINDS 62 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 727 </td> <td> THUNDERSTORM WINDS 63 MPH </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 728 </td> <td> THUNDERSTORM WINDS AND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 729 </td> <td> THUNDERSTORM WINDS FUNNEL CLOU </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 730 </td> <td> THUNDERSTORM WINDS G </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 731 </td> <td> THUNDERSTORM WINDS G60 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 732 </td> <td> THUNDERSTORM WINDS HAIL </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 733 </td> <td> THUNDERSTORM WINDS HEAVY RAIN </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 734 </td> <td> THUNDERSTORM WINDS LIGHTNING </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 735 </td> <td> THUNDERSTORM WINDS SMALL STREA </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 736 </td> <td> THUNDERSTORM WINDS URBAN FLOOD </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 737 </td> <td> THUNDERSTORM WINDS. </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 738 </td> <td> THUNDERSTORM WINDS/ FLOOD </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 739 </td> <td> THUNDERSTORM WINDS/ HAIL </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 740 </td> <td> THUNDERSTORM WINDS/FLASH FLOOD </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 741 </td> <td> THUNDERSTORM WINDS/FLOODING </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 742 </td> <td> THUNDERSTORM WINDS/FUNNEL CLOU </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 743 </td> <td> THUNDERSTORM WINDS/HAIL </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 744 </td> <td> THUNDERSTORM WINDS/HEAVY RAIN </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 745 </td> <td> THUNDERSTORM WINDS53 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 746 </td> <td> THUNDERSTORM WINDSHAIL </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 747 </td> <td> THUNDERSTORM WINDSS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 748 </td> <td> THUNDERSTORM WINS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 749 </td> <td> THUNDERSTORMS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 750 </td> <td> THUNDERSTORMS WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 751 </td> <td> THUNDERSTORMS WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 752 </td> <td> THUNDERSTORMW </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 753 </td> <td> THUNDERSTORMW 50 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 754 </td> <td> THUNDERSTORMW WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 755 </td> <td> THUNDERSTORMWINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 756 </td> <td> THUNDERSTROM WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 757 </td> <td> THUNDERSTROM WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 758 </td> <td> THUNDERTORM WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 759 </td> <td> THUNDERTSORM WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 760 </td> <td> THUNDESTORM WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 761 </td> <td> THUNERSTORM WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 762 </td> <td> TSTM </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 763 </td> <td> TSTM WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 764 </td> <td> Tstm Wind </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 765 </td> <td> TSTM WIND  (G45) </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 766 </td> <td> TSTM WIND (41) </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 767 </td> <td> TSTM WIND (G35) </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 768 </td> <td> TSTM WIND (G40) </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 769 </td> <td> TSTM WIND (G45) </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 770 </td> <td> TSTM WIND 40 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 771 </td> <td> TSTM WIND 45 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 772 </td> <td> TSTM WIND 50 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 773 </td> <td> TSTM WIND 51 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 774 </td> <td> TSTM WIND 52 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 775 </td> <td> TSTM WIND 55 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 776 </td> <td> TSTM WIND 65) </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 777 </td> <td> TSTM WIND AND LIGHTNING </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 778 </td> <td> TSTM WIND DAMAGE </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 779 </td> <td> TSTM WIND G45 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 780 </td> <td> TSTM WIND G58 </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 781 </td> <td> TSTM WIND/HAIL </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 782 </td> <td> TSTM WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 783 </td> <td> TSTM WND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 784 </td> <td> TSTMW </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 785 </td> <td> TUNDERSTORM WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 786 </td> <td> WAKE LOW WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 787 </td> <td> wet micoburst </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 788 </td> <td> WET MICROBURST </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 789 </td> <td> Whirlwind </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 790 </td> <td> WHIRLWIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 791 </td> <td> WIND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 792 </td> <td> Wind </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 793 </td> <td> WIND ADVISORY </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 794 </td> <td> WIND AND WAVE </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 795 </td> <td> WIND DAMAGE </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 796 </td> <td> Wind Damage </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 797 </td> <td> WIND GUSTS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 798 </td> <td> WIND STORM </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 799 </td> <td> WIND/HAIL </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 800 </td> <td> WINDS </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 801 </td> <td> WND </td> <td> Wind </td> </tr>
  <tr> <td align="right"> 802 </td> <td> ACCUMULATED SNOWFALL </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 803 </td> <td> blowing snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 804 </td> <td> Blowing Snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 805 </td> <td> BLOWING SNOW &amp; EXTREME WIND CH </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 806 </td> <td> BLOWING SNOW- EXTREME WIND CHI </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 807 </td> <td> BLOWING SNOW/EXTREME WIND CHIL </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 808 </td> <td> Drifting Snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 809 </td> <td> EARLY SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 810 </td> <td> Early snowfall </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 811 </td> <td> EARLY SNOWFALL </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 812 </td> <td> EXCESSIVE SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 813 </td> <td> FALLING SNOW/ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 814 </td> <td> FIRST SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 815 </td> <td> FREEZING DRIZZLE </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 816 </td> <td> Freezing Drizzle </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 817 </td> <td> Freezing drizzle </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 818 </td> <td> FREEZING DRIZZLE AND FREEZING </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 819 </td> <td> Freezing Fog </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 820 </td> <td> FREEZING FOG </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 821 </td> <td> FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 822 </td> <td> Freezing Rain </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 823 </td> <td> Freezing rain </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 824 </td> <td> FREEZING RAIN AND SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 825 </td> <td> FREEZING RAIN AND SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 826 </td> <td> FREEZING RAIN SLEET AND </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 827 </td> <td> FREEZING RAIN SLEET AND LIGHT </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 828 </td> <td> FREEZING RAIN/SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 829 </td> <td> FREEZING RAIN/SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 830 </td> <td> Freezing Spray </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 831 </td> <td> GLAZE </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 832 </td> <td> Glaze </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 833 </td> <td> GLAZE ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 834 </td> <td> GLAZE/ICE STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 835 </td> <td> HEAVY LAKE SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 836 </td> <td> HEAVY MIX </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 837 </td> <td> HEAVY RAIN/SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 838 </td> <td> HEAVY SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 839 </td> <td> HEAVY SNOW   FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 840 </td> <td> HEAVY SNOW &amp; ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 841 </td> <td> HEAVY SNOW AND </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 842 </td> <td> HEAVY SNOW AND HIGH WINDS </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 843 </td> <td> HEAVY SNOW AND ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 844 </td> <td> HEAVY SNOW AND ICE STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 845 </td> <td> HEAVY SNOW AND STRONG WINDS </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 846 </td> <td> HEAVY SNOW ANDBLOWING SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 847 </td> <td> Heavy snow shower </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 848 </td> <td> HEAVY SNOW SQUALLS </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 849 </td> <td> HEAVY SNOW/BLIZZARD </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 850 </td> <td> HEAVY SNOW/BLOWING SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 851 </td> <td> HEAVY SNOW/FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 852 </td> <td> HEAVY SNOW/HIGH </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 853 </td> <td> HEAVY SNOW/HIGH WIND </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 854 </td> <td> HEAVY SNOW/HIGH WINDS </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 855 </td> <td> HEAVY SNOW/HIGH WINDS &amp; FLOOD </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 856 </td> <td> HEAVY SNOW/HIGH WINDS/FREEZING </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 857 </td> <td> HEAVY SNOW/ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 858 </td> <td> HEAVY SNOW/ICE STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 859 </td> <td> HEAVY SNOW/SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 860 </td> <td> HEAVY SNOW/SQUALLS </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 861 </td> <td> HEAVY SNOW/WIND </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 862 </td> <td> HEAVY SNOW/WINTER STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 863 </td> <td> HEAVY SNOWPACK </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 864 </td> <td> HEAVY SNOW-SQUALLS </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 865 </td> <td> HEAVY WET SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 866 </td> <td> HIGH WIND AND HEAVY SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 867 </td> <td> HIGH WIND/HEAVY SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 868 </td> <td> HIGH WINDS/SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 869 </td> <td> ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 870 </td> <td> ICE AND SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 871 </td> <td> ICE FLOES </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 872 </td> <td> Ice Fog </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 873 </td> <td> ICE JAM </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 874 </td> <td> Ice jam flood (minor </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 875 </td> <td> ICE JAM FLOODING </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 876 </td> <td> ICE ON ROAD </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 877 </td> <td> ICE PELLETS </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 878 </td> <td> ICE ROADS </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 879 </td> <td> ICE STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 880 </td> <td> ICE STORM AND SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 881 </td> <td> ICE STORM/FLASH FLOOD </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 882 </td> <td> ICE/SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 883 </td> <td> Ice/Snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 884 </td> <td> ICE/STRONG WINDS </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 885 </td> <td> Icestorm/Blizzard </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 886 </td> <td> ICY ROADS </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 887 </td> <td> Icy Roads </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 888 </td> <td> LAKE EFFECT SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 889 </td> <td> Lake Effect Snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 890 </td> <td> LAKE-EFFECT SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 891 </td> <td> LATE SEASON SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 892 </td> <td> Late Season Snowfall </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 893 </td> <td> LATE SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 894 </td> <td> Late-season Snowfall </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 895 </td> <td> LIGHT FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 896 </td> <td> LIGHT SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 897 </td> <td> Light snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 898 </td> <td> Light Snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 899 </td> <td> LIGHT SNOW AND SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 900 </td> <td> Light Snow/Flurries </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 901 </td> <td> LIGHT SNOW/FREEZING PRECIP </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 902 </td> <td> Light Snowfall </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 903 </td> <td> MIXED PRECIP </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 904 </td> <td> Mixed Precipitation </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 905 </td> <td> MIXED PRECIPITATION </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 906 </td> <td> MODERATE SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 907 </td> <td> MODERATE SNOWFALL </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 908 </td> <td> Monthly Snowfall </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 909 </td> <td> MONTHLY SNOWFALL </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 910 </td> <td> Mountain Snows </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 911 </td> <td> NEAR RECORD SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 912 </td> <td> PATCHY ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 913 </td> <td> RAIN/SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 914 </td> <td> Record May Snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 915 </td> <td> RECORD SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 916 </td> <td> RECORD SNOW/COLD </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 917 </td> <td> RECORD SNOWFALL </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 918 </td> <td> Record Winter Snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 919 </td> <td> Seasonal Snowfall </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 920 </td> <td> SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 921 </td> <td> SLEET &amp; FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 922 </td> <td> SLEET STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 923 </td> <td> SLEET/FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 924 </td> <td> SLEET/ICE STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 925 </td> <td> SLEET/RAIN/SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 926 </td> <td> SLEET/SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 927 </td> <td> SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 928 </td> <td> Snow </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 929 </td> <td> SNOW ACCUMULATION </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 930 </td> <td> Snow Accumulation </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 931 </td> <td> SNOW ADVISORY </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 932 </td> <td> SNOW AND COLD </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 933 </td> <td> SNOW AND HEAVY SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 934 </td> <td> SNOW AND ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 935 </td> <td> Snow and Ice </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 936 </td> <td> SNOW AND ICE STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 937 </td> <td> Snow and sleet </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 938 </td> <td> SNOW AND SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 939 </td> <td> SNOW AND WIND </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 940 </td> <td> SNOW DROUGHT </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 941 </td> <td> SNOW FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 942 </td> <td> SNOW- HIGH WIND- WIND CHILL </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 943 </td> <td> SNOW SHOWERS </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 944 </td> <td> SNOW SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 945 </td> <td> SNOW SQUALL </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 946 </td> <td> SNOW SQUALLS </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 947 </td> <td> Snow Squalls </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 948 </td> <td> Snow squalls </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 949 </td> <td> SNOW/ BITTER COLD </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 950 </td> <td> SNOW/ ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 951 </td> <td> SNOW/BLOWING SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 952 </td> <td> SNOW/COLD </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 953 </td> <td> SNOW/FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 954 </td> <td> SNOW/HEAVY SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 955 </td> <td> SNOW/HIGH WINDS </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 956 </td> <td> SNOW/ICE </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 957 </td> <td> SNOW/ICE STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 958 </td> <td> SNOW/RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 959 </td> <td> SNOW/RAIN/SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 960 </td> <td> SNOW/SLEET </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 961 </td> <td> SNOW/SLEET/FREEZING RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 962 </td> <td> SNOW/SLEET/RAIN </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 963 </td> <td> SNOW\COLD </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 964 </td> <td> SNOWFALL RECORD </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 965 </td> <td> SNOWSTORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 966 </td> <td> THUNDERSNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 967 </td> <td> Thundersnow shower </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 968 </td> <td> UNUSUALLY LATE SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 969 </td> <td> WET SNOW </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 970 </td> <td> WINTER MIX </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 971 </td> <td> WINTER STORM </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 972 </td> <td> WINTER STORM HIGH WINDS </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 973 </td> <td> WINTER STORM/HIGH WIND </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 974 </td> <td> WINTER STORM/HIGH WINDS </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 975 </td> <td> WINTER STORMS </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 976 </td> <td> WINTER WEATHER </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 977 </td> <td> Winter Weather </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 978 </td> <td> WINTER WEATHER MIX </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 979 </td> <td> WINTER WEATHER/MIX </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 980 </td> <td> WINTERY MIX </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 981 </td> <td> WINTRY MIX </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 982 </td> <td> Wintry Mix </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 983 </td> <td> Wintry mix </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 984 </td> <td> Black Ice </td> <td> Wintery Mix </td> </tr>
  <tr> <td align="right"> 985 </td> <td> BLACK ICE </td> <td> Wintery Mix </td> </tr>
   </table>
