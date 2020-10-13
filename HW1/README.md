HW1-EDA
================
Lin Wang
9/1/2020

# Steps

## 1\. Read in the data, conduct EDA checklist 2-4

``` r
ca2004 <- data.table::fread("/Users/Hope/OneDrive/Documents/20 Fall/PM566/HW/cadata2004.csv")
ca2019 <- data.table::fread("/Users/Hope/OneDrive/Documents/20 Fall/PM566/HW/cadata2019.csv")
```

### Check the dimensions, headers, footers, variable names and variable types

``` r
dim(ca2004)
```

    ## [1] 19233    20

``` r
dim(ca2019)
```

    ## [1] 53328    20

``` r
head(ca2004)
```

    ##          Date Source  Site ID POC Daily Mean PM2.5 Concentration    UNITS
    ## 1: 01/01/2004    AQS 60010007   1                           11.0 ug/m3 LC
    ## 2: 01/02/2004    AQS 60010007   1                           12.2 ug/m3 LC
    ## 3: 01/03/2004    AQS 60010007   1                           16.5 ug/m3 LC
    ## 4: 01/04/2004    AQS 60010007   1                           18.1 ug/m3 LC
    ## 5: 01/05/2004    AQS 60010007   1                           11.5 ug/m3 LC
    ## 6: 01/06/2004    AQS 60010007   1                           32.5 ug/m3 LC
    ##    DAILY_AQI_VALUE Site Name DAILY_OBS_COUNT PERCENT_COMPLETE
    ## 1:              46 Livermore               1              100
    ## 2:              51 Livermore               1              100
    ## 3:              60 Livermore               1              100
    ## 4:              64 Livermore               1              100
    ## 5:              48 Livermore               1              100
    ## 6:              94 Livermore               1              100
    ##    AQS_PARAMETER_CODE                     AQS_PARAMETER_DESC CBSA_CODE
    ## 1:              88502 Acceptable PM2.5 AQI & Speciation Mass     41860
    ## 2:              88502 Acceptable PM2.5 AQI & Speciation Mass     41860
    ## 3:              88502 Acceptable PM2.5 AQI & Speciation Mass     41860
    ## 4:              88101               PM2.5 - Local Conditions     41860
    ## 5:              88502 Acceptable PM2.5 AQI & Speciation Mass     41860
    ## 6:              88502 Acceptable PM2.5 AQI & Speciation Mass     41860
    ##                            CBSA_NAME STATE_CODE      STATE COUNTY_CODE  COUNTY
    ## 1: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 2: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 3: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 4: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 5: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 6: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ##    SITE_LATITUDE SITE_LONGITUDE
    ## 1:      37.68753      -121.7842
    ## 2:      37.68753      -121.7842
    ## 3:      37.68753      -121.7842
    ## 4:      37.68753      -121.7842
    ## 5:      37.68753      -121.7842
    ## 6:      37.68753      -121.7842

``` r
head(ca2019)
```

    ##          Date Source  Site ID POC Daily Mean PM2.5 Concentration    UNITS
    ## 1: 01/01/2019    AQS 60010007   3                            5.7 ug/m3 LC
    ## 2: 01/02/2019    AQS 60010007   3                           11.9 ug/m3 LC
    ## 3: 01/03/2019    AQS 60010007   3                           20.1 ug/m3 LC
    ## 4: 01/04/2019    AQS 60010007   3                           28.8 ug/m3 LC
    ## 5: 01/05/2019    AQS 60010007   3                           11.2 ug/m3 LC
    ## 6: 01/06/2019    AQS 60010007   3                            2.7 ug/m3 LC
    ##    DAILY_AQI_VALUE Site Name DAILY_OBS_COUNT PERCENT_COMPLETE
    ## 1:              24 Livermore               1              100
    ## 2:              50 Livermore               1              100
    ## 3:              68 Livermore               1              100
    ## 4:              86 Livermore               1              100
    ## 5:              47 Livermore               1              100
    ## 6:              11 Livermore               1              100
    ##    AQS_PARAMETER_CODE       AQS_PARAMETER_DESC CBSA_CODE
    ## 1:              88101 PM2.5 - Local Conditions     41860
    ## 2:              88101 PM2.5 - Local Conditions     41860
    ## 3:              88101 PM2.5 - Local Conditions     41860
    ## 4:              88101 PM2.5 - Local Conditions     41860
    ## 5:              88101 PM2.5 - Local Conditions     41860
    ## 6:              88101 PM2.5 - Local Conditions     41860
    ##                            CBSA_NAME STATE_CODE      STATE COUNTY_CODE  COUNTY
    ## 1: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 2: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 3: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 4: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 5: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 6: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ##    SITE_LATITUDE SITE_LONGITUDE
    ## 1:      37.68753      -121.7842
    ## 2:      37.68753      -121.7842
    ## 3:      37.68753      -121.7842
    ## 4:      37.68753      -121.7842
    ## 5:      37.68753      -121.7842
    ## 6:      37.68753      -121.7842

``` r
tail(ca2004)
```

    ##          Date Source  Site ID POC Daily Mean PM2.5 Concentration    UNITS
    ## 1: 12/14/2004    AQS 61131003   1                             11 ug/m3 LC
    ## 2: 12/17/2004    AQS 61131003   1                             16 ug/m3 LC
    ## 3: 12/20/2004    AQS 61131003   1                             17 ug/m3 LC
    ## 4: 12/23/2004    AQS 61131003   1                              9 ug/m3 LC
    ## 5: 12/26/2004    AQS 61131003   1                             24 ug/m3 LC
    ## 6: 12/29/2004    AQS 61131003   1                              9 ug/m3 LC
    ##    DAILY_AQI_VALUE            Site Name DAILY_OBS_COUNT PERCENT_COMPLETE
    ## 1:              46 Woodland-Gibson Road               1              100
    ## 2:              59 Woodland-Gibson Road               1              100
    ## 3:              61 Woodland-Gibson Road               1              100
    ## 4:              38 Woodland-Gibson Road               1              100
    ## 5:              76 Woodland-Gibson Road               1              100
    ## 6:              38 Woodland-Gibson Road               1              100
    ##    AQS_PARAMETER_CODE       AQS_PARAMETER_DESC CBSA_CODE
    ## 1:              88101 PM2.5 - Local Conditions     40900
    ## 2:              88101 PM2.5 - Local Conditions     40900
    ## 3:              88101 PM2.5 - Local Conditions     40900
    ## 4:              88101 PM2.5 - Local Conditions     40900
    ## 5:              88101 PM2.5 - Local Conditions     40900
    ## 6:              88101 PM2.5 - Local Conditions     40900
    ##                                  CBSA_NAME STATE_CODE      STATE COUNTY_CODE
    ## 1: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 2: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 3: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 4: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 5: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 6: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ##    COUNTY SITE_LATITUDE SITE_LONGITUDE
    ## 1:   Yolo      38.66121      -121.7327
    ## 2:   Yolo      38.66121      -121.7327
    ## 3:   Yolo      38.66121      -121.7327
    ## 4:   Yolo      38.66121      -121.7327
    ## 5:   Yolo      38.66121      -121.7327
    ## 6:   Yolo      38.66121      -121.7327

``` r
tail(ca2019)
```

    ##          Date Source  Site ID POC Daily Mean PM2.5 Concentration    UNITS
    ## 1: 11/11/2019    AQS 61131003   1                           13.5 ug/m3 LC
    ## 2: 11/17/2019    AQS 61131003   1                           18.1 ug/m3 LC
    ## 3: 11/29/2019    AQS 61131003   1                           12.5 ug/m3 LC
    ## 4: 12/17/2019    AQS 61131003   1                           23.8 ug/m3 LC
    ## 5: 12/23/2019    AQS 61131003   1                            1.0 ug/m3 LC
    ## 6: 12/29/2019    AQS 61131003   1                            9.1 ug/m3 LC
    ##    DAILY_AQI_VALUE            Site Name DAILY_OBS_COUNT PERCENT_COMPLETE
    ## 1:              54 Woodland-Gibson Road               1              100
    ## 2:              64 Woodland-Gibson Road               1              100
    ## 3:              52 Woodland-Gibson Road               1              100
    ## 4:              76 Woodland-Gibson Road               1              100
    ## 5:               4 Woodland-Gibson Road               1              100
    ## 6:              38 Woodland-Gibson Road               1              100
    ##    AQS_PARAMETER_CODE       AQS_PARAMETER_DESC CBSA_CODE
    ## 1:              88101 PM2.5 - Local Conditions     40900
    ## 2:              88101 PM2.5 - Local Conditions     40900
    ## 3:              88101 PM2.5 - Local Conditions     40900
    ## 4:              88101 PM2.5 - Local Conditions     40900
    ## 5:              88101 PM2.5 - Local Conditions     40900
    ## 6:              88101 PM2.5 - Local Conditions     40900
    ##                                  CBSA_NAME STATE_CODE      STATE COUNTY_CODE
    ## 1: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 2: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 3: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 4: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 5: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 6: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ##    COUNTY SITE_LATITUDE SITE_LONGITUDE
    ## 1:   Yolo      38.66121      -121.7327
    ## 2:   Yolo      38.66121      -121.7327
    ## 3:   Yolo      38.66121      -121.7327
    ## 4:   Yolo      38.66121      -121.7327
    ## 5:   Yolo      38.66121      -121.7327
    ## 6:   Yolo      38.66121      -121.7327

``` r
str(ca2004)
```

    ## Classes 'data.table' and 'data.frame':   19233 obs. of  20 variables:
    ##  $ Date                          : chr  "01/01/2004" "01/02/2004" "01/03/2004" "01/04/2004" ...
    ##  $ Source                        : chr  "AQS" "AQS" "AQS" "AQS" ...
    ##  $ Site ID                       : int  60010007 60010007 60010007 60010007 60010007 60010007 60010007 60010007 60010007 60010007 ...
    ##  $ POC                           : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Daily Mean PM2.5 Concentration: num  11 12.2 16.5 18.1 11.5 32.5 14 29.9 21 15.7 ...
    ##  $ UNITS                         : chr  "ug/m3 LC" "ug/m3 LC" "ug/m3 LC" "ug/m3 LC" ...
    ##  $ DAILY_AQI_VALUE               : int  46 51 60 64 48 94 55 88 70 59 ...
    ##  $ Site Name                     : chr  "Livermore" "Livermore" "Livermore" "Livermore" ...
    ##  $ DAILY_OBS_COUNT               : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ PERCENT_COMPLETE              : num  100 100 100 100 100 100 100 100 100 100 ...
    ##  $ AQS_PARAMETER_CODE            : int  88502 88502 88502 88101 88502 88502 88101 88502 88502 88101 ...
    ##  $ AQS_PARAMETER_DESC            : chr  "Acceptable PM2.5 AQI & Speciation Mass" "Acceptable PM2.5 AQI & Speciation Mass" "Acceptable PM2.5 AQI & Speciation Mass" "PM2.5 - Local Conditions" ...
    ##  $ CBSA_CODE                     : int  41860 41860 41860 41860 41860 41860 41860 41860 41860 41860 ...
    ##  $ CBSA_NAME                     : chr  "San Francisco-Oakland-Hayward, CA" "San Francisco-Oakland-Hayward, CA" "San Francisco-Oakland-Hayward, CA" "San Francisco-Oakland-Hayward, CA" ...
    ##  $ STATE_CODE                    : int  6 6 6 6 6 6 6 6 6 6 ...
    ##  $ STATE                         : chr  "California" "California" "California" "California" ...
    ##  $ COUNTY_CODE                   : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ COUNTY                        : chr  "Alameda" "Alameda" "Alameda" "Alameda" ...
    ##  $ SITE_LATITUDE                 : num  37.7 37.7 37.7 37.7 37.7 ...
    ##  $ SITE_LONGITUDE                : num  -122 -122 -122 -122 -122 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
str(ca2019)
```

    ## Classes 'data.table' and 'data.frame':   53328 obs. of  20 variables:
    ##  $ Date                          : chr  "01/01/2019" "01/02/2019" "01/03/2019" "01/04/2019" ...
    ##  $ Source                        : chr  "AQS" "AQS" "AQS" "AQS" ...
    ##  $ Site ID                       : int  60010007 60010007 60010007 60010007 60010007 60010007 60010007 60010007 60010007 60010007 ...
    ##  $ POC                           : int  3 3 3 3 3 3 3 3 3 3 ...
    ##  $ Daily Mean PM2.5 Concentration: num  5.7 11.9 20.1 28.8 11.2 2.7 2.8 7 3.1 7.1 ...
    ##  $ UNITS                         : chr  "ug/m3 LC" "ug/m3 LC" "ug/m3 LC" "ug/m3 LC" ...
    ##  $ DAILY_AQI_VALUE               : int  24 50 68 86 47 11 12 29 13 30 ...
    ##  $ Site Name                     : chr  "Livermore" "Livermore" "Livermore" "Livermore" ...
    ##  $ DAILY_OBS_COUNT               : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ PERCENT_COMPLETE              : num  100 100 100 100 100 100 100 100 100 100 ...
    ##  $ AQS_PARAMETER_CODE            : int  88101 88101 88101 88101 88101 88101 88101 88101 88101 88101 ...
    ##  $ AQS_PARAMETER_DESC            : chr  "PM2.5 - Local Conditions" "PM2.5 - Local Conditions" "PM2.5 - Local Conditions" "PM2.5 - Local Conditions" ...
    ##  $ CBSA_CODE                     : int  41860 41860 41860 41860 41860 41860 41860 41860 41860 41860 ...
    ##  $ CBSA_NAME                     : chr  "San Francisco-Oakland-Hayward, CA" "San Francisco-Oakland-Hayward, CA" "San Francisco-Oakland-Hayward, CA" "San Francisco-Oakland-Hayward, CA" ...
    ##  $ STATE_CODE                    : int  6 6 6 6 6 6 6 6 6 6 ...
    ##  $ STATE                         : chr  "California" "California" "California" "California" ...
    ##  $ COUNTY_CODE                   : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ COUNTY                        : chr  "Alameda" "Alameda" "Alameda" "Alameda" ...
    ##  $ SITE_LATITUDE                 : num  37.7 37.7 37.7 37.7 37.7 ...
    ##  $ SITE_LONGITUDE                : num  -122 -122 -122 -122 -122 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
# check key variables
summary(ca2004$`Daily Mean PM2.5 Concentration`)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   -0.10    6.00   10.10   13.12   16.30  251.00

``` r
summary(ca2019$`Daily Mean PM2.5 Concentration`)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -2.200   4.000   6.500   7.742   9.900 120.900

  - There are 19233 rows and 20 columns in 2004 dataset, while 53328
    rows and 20 columns in 2019 dataset.Both datasets contain 20
    variables which includes daily mean PM2.5 concentration, site
    location, state and county name. When we tail the datasets, it looks
    like the date they documented is not consecutive. And we summarize
    the mean PM2.5 concentration for both datasets. The results show the
    minimum value of daily mean PM2.5 concentration are -0.1 for 2004
    and -2.2 for 2019, and the maximum value is 251 for 2004, which
    seems suspicious.

## 2\. Combine dataset, create new column, rename the key variables

``` r
library(dplyr)
ca2004 = mutate(ca2004, Year=2004) 
ca2019 = mutate(ca2019, Year=2019)
pm <- rbind(ca2004, ca2019) %>%
  rename(PM=`Daily Mean PM2.5 Concentration`, lat=SITE_LATITUDE, lon=SITE_LONGITUDE, State=STATE, County=COUNTY, Site=`Site Name`, ID=`Site ID`, AQI=`DAILY_AQI_VALUE`)
head(pm)
```

    ##          Date Source       ID POC   PM    UNITS AQI      Site DAILY_OBS_COUNT
    ## 1: 01/01/2004    AQS 60010007   1 11.0 ug/m3 LC  46 Livermore               1
    ## 2: 01/02/2004    AQS 60010007   1 12.2 ug/m3 LC  51 Livermore               1
    ## 3: 01/03/2004    AQS 60010007   1 16.5 ug/m3 LC  60 Livermore               1
    ## 4: 01/04/2004    AQS 60010007   1 18.1 ug/m3 LC  64 Livermore               1
    ## 5: 01/05/2004    AQS 60010007   1 11.5 ug/m3 LC  48 Livermore               1
    ## 6: 01/06/2004    AQS 60010007   1 32.5 ug/m3 LC  94 Livermore               1
    ##    PERCENT_COMPLETE AQS_PARAMETER_CODE                     AQS_PARAMETER_DESC
    ## 1:              100              88502 Acceptable PM2.5 AQI & Speciation Mass
    ## 2:              100              88502 Acceptable PM2.5 AQI & Speciation Mass
    ## 3:              100              88502 Acceptable PM2.5 AQI & Speciation Mass
    ## 4:              100              88101               PM2.5 - Local Conditions
    ## 5:              100              88502 Acceptable PM2.5 AQI & Speciation Mass
    ## 6:              100              88502 Acceptable PM2.5 AQI & Speciation Mass
    ##    CBSA_CODE                         CBSA_NAME STATE_CODE      State
    ## 1:     41860 San Francisco-Oakland-Hayward, CA          6 California
    ## 2:     41860 San Francisco-Oakland-Hayward, CA          6 California
    ## 3:     41860 San Francisco-Oakland-Hayward, CA          6 California
    ## 4:     41860 San Francisco-Oakland-Hayward, CA          6 California
    ## 5:     41860 San Francisco-Oakland-Hayward, CA          6 California
    ## 6:     41860 San Francisco-Oakland-Hayward, CA          6 California
    ##    COUNTY_CODE  County      lat       lon Year
    ## 1:           1 Alameda 37.68753 -121.7842 2004
    ## 2:           1 Alameda 37.68753 -121.7842 2004
    ## 3:           1 Alameda 37.68753 -121.7842 2004
    ## 4:           1 Alameda 37.68753 -121.7842 2004
    ## 5:           1 Alameda 37.68753 -121.7842 2004
    ## 6:           1 Alameda 37.68753 -121.7842 2004

## 3\. Create a basic map

``` r
yr.pal=colorFactor(c('yellow','red'), domain = pm$Year)
leaflet(pm) %>%
  addProviderTiles("OpenStreetMap") %>%
  addCircles(lat = ~lat, lng = ~lon, opacity = 1, fillOpacity = 1, radius = 500, color = ~yr.pal(Year)) %>%
  addLegend(position = "bottomright", pal = yr.pal, values = pm$Year, title = "Year", opacity = 1)
```

<!--html_preserve-->

<div id="htmlwidget-c96dca26d3b2556ccdcd" class="leaflet html-widget" style="width:672px;height:480px;">

</div>


<!--/html_preserve-->

  - The map colors the circle differently by year, and indicate
    locations of the sites throughout California. We can see there are
    more sites in 2019 than in 2004, and most sites are located near the
    west coast, the yellow circle represents sites in 2004 while red one
    represents that in 2019, and circles overlap each other.

## 4\. Check and validate any missing or implausible values of PM2.5

``` r
mean(is.na(pm))
```

    ## [1] 0.003550379

``` r
pm2 <- pm[pm$PM>0]
summary(pm2$PM)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.100   4.500   7.300   9.221  11.300 251.000

``` r
tail(pm2[order(PM)])
```

    ##          Date Source       ID POC    PM    UNITS AQI
    ## 1: 07/15/2004    AQS 60431001   3 110.4 ug/m3 LC 179
    ## 2: 10/11/2019    AQS 60371201   3 120.9 ug/m3 LC 185
    ## 3: 07/21/2004    AQS 60431001   3 122.5 ug/m3 LC 186
    ## 4: 07/20/2004    AQS 60431001   3 148.4 ug/m3 LC 199
    ## 5: 07/19/2004    AQS 60431001   3 170.4 ug/m3 LC 221
    ## 6: 07/18/2004    AQS 60431001   3 251.0 ug/m3 LC 301
    ##                                          Site DAILY_OBS_COUNT PERCENT_COMPLETE
    ## 1: Yosemite NP-Yosemite Village Vistor Center               1              100
    ## 2:                                     Reseda               1              100
    ## 3: Yosemite NP-Yosemite Village Vistor Center               1              100
    ## 4: Yosemite NP-Yosemite Village Vistor Center               1              100
    ## 5: Yosemite NP-Yosemite Village Vistor Center               1              100
    ## 6: Yosemite NP-Yosemite Village Vistor Center               1              100
    ##    AQS_PARAMETER_CODE                     AQS_PARAMETER_DESC CBSA_CODE
    ## 1:              88502 Acceptable PM2.5 AQI & Speciation Mass        NA
    ## 2:              88502 Acceptable PM2.5 AQI & Speciation Mass     31080
    ## 3:              88502 Acceptable PM2.5 AQI & Speciation Mass        NA
    ## 4:              88502 Acceptable PM2.5 AQI & Speciation Mass        NA
    ## 5:              88502 Acceptable PM2.5 AQI & Speciation Mass        NA
    ## 6:              88502 Acceptable PM2.5 AQI & Speciation Mass        NA
    ##                             CBSA_NAME STATE_CODE      State COUNTY_CODE
    ## 1:                                             6 California          43
    ## 2: Los Angeles-Long Beach-Anaheim, CA          6 California          37
    ## 3:                                             6 California          43
    ## 4:                                             6 California          43
    ## 5:                                             6 California          43
    ## 6:                                             6 California          43
    ##         County      lat       lon Year
    ## 1:    Mariposa 37.74871 -119.5871 2004
    ## 2: Los Angeles 34.19925 -118.5328 2019
    ## 3:    Mariposa 37.74871 -119.5871 2004
    ## 4:    Mariposa 37.74871 -119.5871 2004
    ## 5:    Mariposa 37.74871 -119.5871 2004
    ## 6:    Mariposa 37.74871 -119.5871 2004

``` r
table(pm2$Year)
```

    ## 
    ##  2004  2019 
    ## 19221 52956

  - The proportion of missing value is low(0.00355), so we choose to
    ignore it.
  - The negative value for PM2.5 is invalid, so it should be excluded.
  - We can see the maximum pm2.5 is 251 ug/m3 at Yosemite village vistor
    center on 7/18/2004, it is likely because of the mountain fire
    activity. Today(9/9/20)’s live data show PM2.5 level is at 320
    ug/m3(1-hr avg) and 135 ug/m3(24-hr avg) in Yosemite National Park.
    So this data is convinced probably.

## 5\. EDA at Three spatial levels

### State level

``` r
pmn1=mean(ca2004$`Daily Mean PM2.5 Concentration`, na.rm = TRUE)
pmn2=mean(ca2019$`Daily Mean PM2.5 Concentration`, na.rm = TRUE)
pmn1
```

    ## [1] 13.12497

``` r
pmn2
```

    ## [1] 7.742194

``` r
ggplot(pm2)+
  geom_boxplot(mapping = aes(group=Year, y=PM, color=Year))+
  facet_wrap(~Year, nrow = 2)
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# take a look at different month
pm_s=mutate(pm2, month=substr(pm2$Date, 1, 2))
ggplot(pm_s)+
  geom_boxplot(mapping = aes(x=month, y=PM, color=Year))+
  facet_wrap(~Year, nrow = 2)
```

![](README_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

  - The mean of daily concentration of PM2.5 have decreased in
    California from 2004 to 2019(13.12497 to 7.742194).
  - From the boxplot above, we can also see the concentration of PM2.5
    is higher in 2004 than in 2019, and especially high in July.

### County level

``` r
pm_c <- group_by(pm2, Year, County) %>% 
  summarise(PM = mean(PM, na.rm = TRUE))
```

    ## `summarise()` regrouping output by 'Year' (override with `.groups` argument)

``` r
summary(pm_c$PM)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.648   5.712   7.694   8.622  10.986  19.655

``` r
pm_c=mutate(pm_c, xyear=as.numeric(as.character(Year))) 
ggplot(pm_c)+
  geom_line(mapping = aes(x=xyear, y=PM, color=factor(County)))
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

  - We calculate the mean PM2.5 concentration for different counties,
    and plot it with consecutive year to see the difference. In general,
    the concentration of PM2.5 has decreased among most counties from
    2004 to 2019 with a few counties increasing

### Site in Los Angeles

``` r
# take a look at Azusa site in LA county
pm_la <- filter(pm2, County == "Los Angeles" & Site == "Azusa") %>%
  select(Year, PM, Date)
pm_la
```

    ##      Year   PM       Date
    ##   1: 2004 18.0 01/01/2004
    ##   2: 2004 20.4 01/02/2004
    ##   3: 2004  8.0 01/03/2004
    ##   4: 2004 23.6 01/07/2004
    ##   5: 2004 28.3 01/08/2004
    ##  ---                     
    ## 396: 2019  7.6 12/02/2019
    ## 397: 2019  5.3 12/08/2019
    ## 398: 2019 18.6 12/14/2019
    ## 399: 2019  2.5 12/20/2019
    ## 400: 2019  1.6 12/26/2019

``` r
ggplot(pm_la)+
  geom_point(mapping = aes(x=Date, y=PM, color=Year))
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

  - There are larger variation and higher values of PM2.5 in 2004 than
    that in 2019. Overall, a similar pattern appears for the PM2.5 under
    30 ug/m3 in 2004 and 2019. We can conclude that daily PM2.5 has
    decreased from 2004 to 2019 at Azusa site.