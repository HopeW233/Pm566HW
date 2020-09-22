HW2
================
Lin Wang
9/19/2020

## Data Wrangling

1.  After merging the data, make sure you don’t have any duplicates by
    counting the number of rows. Make sure it matches.

<!-- end list -->

``` r
dat1 <- read.csv("C:/Users/Hope/Documents/PM566HW/chs_individual.txt")
dim(dat1)
```

    ## [1] 1200   23

``` r
dat2 <- read.csv("C:/Users/Hope/Documents/PM566HW/chs_regional.txt")
dim(dat2)
```

    ## [1] 12 27

``` r
dat <- merge(
  x = dat1, y = dat2, by.x = "townname", by.y = "townname", all.x = TRUE, all.y = FALSE
) 

# take a look at data after merging
head(dat)
```

    ##   townname sid male race hispanic    agepft height weight      bmi asthma
    ## 1   Alpine 841    1    W        1 10.548939    150     78 15.75758      0
    ## 2   Alpine 835    0    W        0 10.099932    143     69 15.33749      0
    ## 3   Alpine 838    0    O        1  9.486653    133     62 15.93183      0
    ## 4   Alpine 840    0    W        0  9.965777    146     78 16.63283      0
    ## 5   Alpine 865    0    W        0 10.039699    162    140 24.24797      1
    ## 6   Alpine 867    0    W        1  9.957563    141     94 21.49151      0
    ##   active_asthma father_asthma mother_asthma wheeze hayfever allergy educ_parent
    ## 1             0             0             0      0        0       0           5
    ## 2             0             0             0      0        0       1           3
    ## 3             0             0             0      0        0       0           4
    ## 4             0             0             0      0        0       0          NA
    ## 5             1             0             0      1        0       1           3
    ## 6             0            NA             0      0        0       0           5
    ##   smoke pets gasstove      fev      fvc     mmef pm25_mass pm25_so4 pm25_no3
    ## 1     0    1        0 2251.505 2594.649 2445.151      8.74     1.73     1.59
    ## 2     0    1        0 2529.276 2826.316 3406.579      8.74     1.73     1.59
    ## 3    NA    1        0 1737.793 1963.545 2133.110      8.74     1.73     1.59
    ## 4    NA    0       NA 2466.791 2638.221 3466.464      8.74     1.73     1.59
    ## 5     0    1        1 2583.934 3567.541 2071.475      8.74     1.73     1.59
    ## 6     0    1        1 1973.115 2154.098 2690.164      8.74     1.73     1.59
    ##   pm25_nh4 pm25_oc pm25_ec pm25_om pm10_oc pm10_ec pm10_tc formic acetic  hcl
    ## 1     0.88    2.54    0.48    3.04    3.25    0.49    3.75   1.03   2.49 0.41
    ## 2     0.88    2.54    0.48    3.04    3.25    0.49    3.75   1.03   2.49 0.41
    ## 3     0.88    2.54    0.48    3.04    3.25    0.49    3.75   1.03   2.49 0.41
    ## 4     0.88    2.54    0.48    3.04    3.25    0.49    3.75   1.03   2.49 0.41
    ## 5     0.88    2.54    0.48    3.04    3.25    0.49    3.75   1.03   2.49 0.41
    ## 6     0.88    2.54    0.48    3.04    3.25    0.49    3.75   1.03   2.49 0.41
    ##   hno3 o3_max o3106 o3_24   no2  pm10 no_24hr pm2_5_fr iacid oacid total_acids
    ## 1 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28  2.39  3.52         5.5
    ## 2 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28  2.39  3.52         5.5
    ## 3 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28  2.39  3.52         5.5
    ## 4 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28  2.39  3.52         5.5
    ## 5 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28  2.39  3.52         5.5
    ## 6 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28  2.39  3.52         5.5
    ##         lon      lat
    ## 1 -116.7664 32.83505
    ## 2 -116.7664 32.83505
    ## 3 -116.7664 32.83505
    ## 4 -116.7664 32.83505
    ## 5 -116.7664 32.83505
    ## 6 -116.7664 32.83505

``` r
summary(dat)
```

    ##    townname              sid              male            race          
    ##  Length:1200        Min.   :   1.0   Min.   :0.0000   Length:1200       
    ##  Class :character   1st Qu.: 528.8   1st Qu.:0.0000   Class :character  
    ##  Mode  :character   Median :1041.5   Median :0.0000   Mode  :character  
    ##                     Mean   :1037.5   Mean   :0.4917                     
    ##                     3rd Qu.:1554.2   3rd Qu.:1.0000                     
    ##                     Max.   :2053.0   Max.   :1.0000                     
    ##                                                                         
    ##     hispanic          agepft           height        weight      
    ##  Min.   :0.0000   Min.   : 8.961   Min.   :114   Min.   : 42.00  
    ##  1st Qu.:0.0000   1st Qu.: 9.610   1st Qu.:135   1st Qu.: 65.00  
    ##  Median :0.0000   Median : 9.906   Median :139   Median : 74.00  
    ##  Mean   :0.4342   Mean   : 9.924   Mean   :139   Mean   : 79.33  
    ##  3rd Qu.:1.0000   3rd Qu.:10.177   3rd Qu.:143   3rd Qu.: 89.00  
    ##  Max.   :1.0000   Max.   :12.731   Max.   :165   Max.   :207.00  
    ##                   NA's   :89       NA's   :89    NA's   :89      
    ##       bmi            asthma       active_asthma  father_asthma    
    ##  Min.   :11.30   Min.   :0.0000   Min.   :0.00   Min.   :0.00000  
    ##  1st Qu.:15.78   1st Qu.:0.0000   1st Qu.:0.00   1st Qu.:0.00000  
    ##  Median :17.48   Median :0.0000   Median :0.00   Median :0.00000  
    ##  Mean   :18.50   Mean   :0.1463   Mean   :0.19   Mean   :0.08318  
    ##  3rd Qu.:20.35   3rd Qu.:0.0000   3rd Qu.:0.00   3rd Qu.:0.00000  
    ##  Max.   :41.27   Max.   :1.0000   Max.   :1.00   Max.   :1.00000  
    ##  NA's   :89      NA's   :31                      NA's   :106      
    ##  mother_asthma        wheeze          hayfever         allergy      
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000  
    ##  Mean   :0.1023   Mean   :0.3313   Mean   :0.1747   Mean   :0.2929  
    ##  3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:1.0000  
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
    ##  NA's   :56       NA's   :71       NA's   :118      NA's   :63      
    ##   educ_parent        smoke             pets           gasstove     
    ##  Min.   :1.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:2.000   1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:1.0000  
    ##  Median :3.000   Median :0.0000   Median :1.0000   Median :1.0000  
    ##  Mean   :2.797   Mean   :0.1638   Mean   :0.7667   Mean   :0.7815  
    ##  3rd Qu.:3.000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
    ##  Max.   :5.000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
    ##  NA's   :64      NA's   :40                        NA's   :33      
    ##       fev              fvc            mmef          pm25_mass     
    ##  Min.   : 984.8   Min.   : 895   Min.   : 757.6   Min.   : 5.960  
    ##  1st Qu.:1809.0   1st Qu.:2041   1st Qu.:1994.0   1st Qu.: 7.615  
    ##  Median :2022.7   Median :2293   Median :2401.5   Median :10.545  
    ##  Mean   :2031.3   Mean   :2324   Mean   :2398.8   Mean   :14.362  
    ##  3rd Qu.:2249.7   3rd Qu.:2573   3rd Qu.:2793.8   3rd Qu.:20.988  
    ##  Max.   :3323.7   Max.   :3698   Max.   :4935.9   Max.   :29.970  
    ##  NA's   :95       NA's   :97     NA's   :106                      
    ##     pm25_so4        pm25_no3         pm25_nh4         pm25_oc      
    ##  Min.   :0.790   Min.   : 0.730   Min.   :0.4100   Min.   : 1.450  
    ##  1st Qu.:1.077   1st Qu.: 1.538   1st Qu.:0.7375   1st Qu.: 2.520  
    ##  Median :1.815   Median : 2.525   Median :1.1350   Median : 4.035  
    ##  Mean   :1.876   Mean   : 4.488   Mean   :1.7642   Mean   : 4.551  
    ##  3rd Qu.:2.605   3rd Qu.: 7.338   3rd Qu.:2.7725   3rd Qu.: 5.350  
    ##  Max.   :3.230   Max.   :12.200   Max.   :4.2500   Max.   :11.830  
    ##                                                                    
    ##     pm25_ec          pm25_om          pm10_oc          pm10_ec      
    ##  Min.   :0.1300   Min.   : 1.740   Min.   : 1.860   Min.   :0.1400  
    ##  1st Qu.:0.4000   1st Qu.: 3.020   1st Qu.: 3.228   1st Qu.:0.4100  
    ##  Median :0.5850   Median : 4.840   Median : 5.170   Median :0.5950  
    ##  Mean   :0.7358   Mean   : 5.460   Mean   : 5.832   Mean   :0.7525  
    ##  3rd Qu.:1.1750   3rd Qu.: 6.418   3rd Qu.: 6.855   3rd Qu.:1.1975  
    ##  Max.   :1.3600   Max.   :14.200   Max.   :15.160   Max.   :1.3900  
    ##                                                                     
    ##     pm10_tc           formic          acetic           hcl        
    ##  Min.   : 1.990   Min.   :0.340   Min.   :0.750   Min.   :0.2200  
    ##  1st Qu.: 3.705   1st Qu.:0.720   1st Qu.:2.297   1st Qu.:0.3250  
    ##  Median : 6.505   Median :1.105   Median :2.910   Median :0.4350  
    ##  Mean   : 6.784   Mean   :1.332   Mean   :3.010   Mean   :0.4208  
    ##  3rd Qu.: 8.430   3rd Qu.:1.765   3rd Qu.:4.000   3rd Qu.:0.4625  
    ##  Max.   :16.440   Max.   :2.770   Max.   :5.140   Max.   :0.7300  
    ##                                                                   
    ##       hno3           o3_max          o3106           o3_24      
    ##  Min.   :0.430   Min.   :38.27   Min.   :28.22   Min.   :18.22  
    ##  1st Qu.:1.593   1st Qu.:49.93   1st Qu.:41.90   1st Qu.:23.31  
    ##  Median :2.455   Median :64.05   Median :46.74   Median :27.59  
    ##  Mean   :2.367   Mean   :60.16   Mean   :47.76   Mean   :30.23  
    ##  3rd Qu.:3.355   3rd Qu.:67.69   3rd Qu.:55.24   3rd Qu.:32.39  
    ##  Max.   :4.070   Max.   :84.44   Max.   :67.01   Max.   :57.76  
    ##                                                                 
    ##       no2             pm10          no_24hr         pm2_5_fr    
    ##  Min.   : 4.60   Min.   :18.40   Min.   : 2.05   Min.   : 9.01  
    ##  1st Qu.:12.12   1st Qu.:20.71   1st Qu.: 4.74   1st Qu.:10.28  
    ##  Median :16.40   Median :29.64   Median :12.68   Median :22.23  
    ##  Mean   :18.99   Mean   :32.64   Mean   :16.21   Mean   :19.79  
    ##  3rd Qu.:23.24   3rd Qu.:39.16   3rd Qu.:26.90   3rd Qu.:27.73  
    ##  Max.   :37.97   Max.   :70.39   Max.   :42.95   Max.   :31.55  
    ##                                  NA's   :100     NA's   :300    
    ##      iacid           oacid        total_acids          lon        
    ##  Min.   :0.760   Min.   :1.090   Min.   : 1.520   Min.   :-120.7  
    ##  1st Qu.:1.835   1st Qu.:2.978   1st Qu.: 4.930   1st Qu.:-118.8  
    ##  Median :2.825   Median :4.135   Median : 6.370   Median :-117.7  
    ##  Mean   :2.788   Mean   :4.342   Mean   : 6.708   Mean   :-118.3  
    ##  3rd Qu.:3.817   3rd Qu.:5.982   3rd Qu.: 9.395   3rd Qu.:-117.4  
    ##  Max.   :4.620   Max.   :7.400   Max.   :11.430   Max.   :-116.8  
    ##                                                                   
    ##       lat       
    ##  Min.   :32.84  
    ##  1st Qu.:33.93  
    ##  Median :34.10  
    ##  Mean   :34.20  
    ##  3rd Qu.:34.65  
    ##  Max.   :35.49  
    ## 

``` r
# dealing with NAs
dat <- as.data.table(dat)

dat[, bmi := fcoalesce(bmi, mean(bmi, na.rm = TRUE)),
    by = .(male, hispanic)]

dat[, fev := fcoalesce(fev, mean(fev, na.rm = TRUE)),
    by = .(male, hispanic)]
```

  - There are 1200 rows and 49 columns in dat with some missing values.

<!-- end list -->

2.  Create a new categorical variable named “obesity\_level” using the
    BMI measurement (underweight BMI\<14; normal BMI 14-22; overweight
    BMI 22-24; obese BMI\>24). To make sure the variable is rightly
    coded, create a summary table that contains the minimum BMI, maximum
    BMI, and the total number of observations per category.

<!-- end list -->

``` r
# create variable
dat[bmi<14, obesity_level := "underweight"]
dat[bmi>=14 & bmi<22, obesity_level := "normal"]
dat[bmi>=22 & bmi<24, obesity_level := "overweight"]
dat[bmi>24, obesity_level := "obese"]

# summary table of obesity level
n <- count(dat, obesity_level)

merge(n, dat[, .(
  bmi_min = min(bmi, na.rm = TRUE),
  bmi_max = max(bmi, na.rm = TRUE)
),
by = obesity_level][order(obesity_level)])
```

    ##    obesity_level   n  bmi_min  bmi_max
    ## 1:        normal 975 14.00380 21.96387
    ## 2:         obese 103 24.00647 41.26613
    ## 3:    overweight  87 22.02353 23.99650
    ## 4:   underweight  35 11.29640 13.98601

  - The obesity\_level is coded successfully.

<!-- end list -->

3.  Create another categorical variable named “smoke\_gas\_exposure”
    that summarizes “Second Hand Smoke” and “Gas Stove.” The variable
    should have four categories in total.

<!-- end list -->

``` r
# create variable
dat[smoke == 0 & gasstove == 0, smoke_gas_exposure := 1]
dat[smoke == 0 & gasstove == 1, smoke_gas_exposure := 2]
dat[smoke == 1 & gasstove == 0, smoke_gas_exposure := 3]
dat[smoke == 1 & gasstove == 1, smoke_gas_exposure := 4]
dat[is.na(smoke) | is.na(gasstove), smoke_gas_exposure := NA]

# dropping NAs
# dat <- dat[!is.na(smoke_gas_exposure)]

# summary table of smoke_gas_exposure
count(dat, smoke_gas_exposure)
```

    ##    smoke_gas_exposure   n
    ## 1:                  1 214
    ## 2:                  2 739
    ## 3:                  3  36
    ## 4:                  4 151
    ## 5:                 NA  60

  - The smoke\_gas\_exposure variable has 4 categories which reflects
    the exposure degree to smoke and gas.

<!-- end list -->

4.  Create four summary tables showing the average (or proportion, if
    binary) and sd of “Forced expiratory volume in 1 second (ml)” and
    asthma indicator by town, sex, obesity level, and
    “smoke\_gas\_exposure.”

<!-- end list -->

``` r
# FEV & asthma by town
unique(dat[, .(
  fev_avg = mean(fev, na.rm = TRUE),
  fev_sd = sd(fev, na.rm = TRUE)
  ),
  by = townname][order(townname)])
```

    ##          townname  fev_avg   fev_sd
    ##  1:        Alpine 2087.101 291.1768
    ##  2:    Atascadero 2075.897 324.0935
    ##  3: Lake Elsinore 2038.849 303.6956
    ##  4:  Lake Gregory 2084.700 319.9593
    ##  5:     Lancaster 2003.044 317.1298
    ##  6:        Lompoc 2034.354 351.0454
    ##  7:    Long Beach 1985.861 319.4625
    ##  8:     Mira Loma 1985.202 324.9634
    ##  9:     Riverside 1989.881 277.5065
    ## 10:     San Dimas 2026.794 318.7845
    ## 11:   Santa Maria 2025.750 312.1725
    ## 12:        Upland 2024.266 343.1637

``` r
# FEV & asthma by sex
unique(dat[, .(
  fev_avg = mean(fev, na.rm = TRUE),
  fev_sd = sd(fev, na.rm = TRUE)
  ),
  by = male][order(male)])
```

    ##    male  fev_avg   fev_sd
    ## 1:    0 1958.911 311.9181
    ## 2:    1 2103.787 307.5123

``` r
# FEV & asthma by obesity level
unique(dat[, .(
  fev_avg = mean(fev, na.rm = TRUE),
  fev_sd = sd(fev, na.rm = TRUE)
  ),
  by = obesity_level][order(obesity_level)])
```

    ##    obesity_level  fev_avg   fev_sd
    ## 1:        normal 1999.794 295.1964
    ## 2:         obese 2266.154 325.4710
    ## 3:    overweight 2224.322 317.4261
    ## 4:   underweight 1698.327 303.3983

``` r
# FEV & asthma by smoke_gas_exposure
unique(dat[, .(
  fev_avg = mean(fev, na.rm = TRUE),
  fev_sd = sd(fev, na.rm = TRUE)
  ),
  by = smoke_gas_exposure][order(smoke_gas_exposure)])
```

    ##    smoke_gas_exposure  fev_avg   fev_sd
    ## 1:                  1 2055.356 330.4169
    ## 2:                  2 2025.989 317.6305
    ## 3:                  3 2055.714 295.6475
    ## 4:                  4 2019.867 298.9728
    ## 5:                 NA 2001.878 340.2592

## EDA

The primary questions: - What is the association between BMI and FEV? -
What is the association between smoke and gas exposure and FEV? - What
is the association between PM2.5 exposure and FEV?

1.  Facet plot showing scatterplots with regression lines of BMI vs FEV
    by “townname”.

2.  Stacked histograms of FEV by BMI category and FEV by smoke/gas
    exposure. Use different color schemes than the ggplot default.

3.  Barchart of BMI by smoke/gas exposure.

4.  Statistical summary graphs of FEV by BMI and FEV by smoke/gas
    exposure category.

5.  A leaflet map showing the concentrations of PM2.5 mass in each of
    the CHS communities.

6.  Choose a visualization to examine whether PM2.5 mass is associated
    with FEV.
