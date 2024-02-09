Lab 05 - Data Wrangling
================
Jing Yu
02-07-2024

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/JSC370-2024/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages),
    `mgcv`, `ggplot2`, `leaflet`, `kableExtra`.

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-41. For overview type 'help("mgcv-package")'.

``` r
library(ggplot2)
library(leaflet)
```

    ## Warning: package 'leaflet' was built under R version 4.2.3

``` r
library(kableExtra)
```

    ## Warning: package 'kableExtra' was built under R version 4.2.3

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
fn <- "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz"
if (!file.exists("met_all_2023.gz"))
  download.file(fn, destfile = "met_all_2023.gz")
met <- data.table::fread("met_all_2023.gz")
```

2.  Load the met data from
    <https://github.com/JSC370/JSC370-2024/main/data/met_all_2023.gz> or
    (Use
    <https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE, LAT, LON)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

# Read in the met data and fix lat, lon, temp
met$lat <- met$lat/1000
met$lon <- met$lon/1000
met$temp <- met$temp/10
met$wind.sp <- met$wind.sp/10
met$atm.press <- met$atm.press/10
```

3.  Merge the data as we did during the lecture. Use the `merge()` code
    and you can also try the tidy way with `left_join()`

``` r
met <- merge(x = met, y = stations, 
             by.x  = "USAFID", by.y  = "USAF", 
             all.x = TRUE, all.y = FALSE)
```

## Question 1: Identifying Representative Stations

Across all weather stations, which stations have the median values of
temperature, wind speed, and atmospheric pressure? Using the
`quantile()` function, identify these three stations. Do they coincide?

``` r
# Calculate median values for temperature, wind speed, and atmospheric pressure
medians <- met[, .(temp_median = quantile(temp, 0.5, na.rm = TRUE),
                   wind.sp_median = quantile(wind.sp, 0.5, na.rm = TRUE),
                   atm.press_median = quantile(atm.press, 0.5, na.rm = TRUE)
                   )]

# Median by station
station_med <- met[, .(temp = quantile(temp, 0.5, na.rm = TRUE),
                       wind.sp = quantile(wind.sp, 0.5, na.rm = TRUE),
                       atm.press = quantile(atm.press, 0.5, na.rm = TRUE)
                       ),
                   by = .(USAFID, STATE)]

# Identify stations with median values
station_med[, temp_dist:= abs(temp - medians$temp_median)]
station_med_temp <- station_med[temp_dist == 0]

station_med[, wind.sp_dist:= abs(wind.sp - medians$wind.sp_median)]
station_med_wind.sp <- station_med[wind.sp_dist == 0]

station_med[, atm.press_dist:= abs(atm.press - medians$atm.press_median)]
station_med_atm.press <- station_med[atm.press_dist == 0]
```

Next identify the stations have these median values.

``` r
coincide <- station_med[temp_dist==0 & wind.sp_dist == 0 & atm.press_dist == 0]
coincide
```

    ##    USAFID STATE temp wind.sp atm.press temp_dist wind.sp_dist atm.press_dist
    ## 1: 723119    SC 21.7     3.1    1011.7         0            0              0

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Identifying Representative Stations per State

Now let’s find the weather stations by state with closest temperature
and wind speed based on the euclidean distance from these medians.

``` r
# Calculate median values for temperature and wind speed for each state
state_medians <- met[, .(temp_median = quantile(temp, 0.5, na.rm = TRUE),
                         wind.sp_median = quantile(wind.sp, 0.5, na.rm = TRUE)),
                     by = .(STATE)]

# Join state medians with station_med
station_med <- merge(station_med, state_medians, by = "STATE", all.x = TRUE)

# Calculate Euclidean distance
station_med[, euclidean_dist := sqrt((temp_median - temp)^2 + (wind.sp_median - wind.sp)^2)]

# Representative Stations per State (containing ties)
station_med <- station_med[, .(USAFID = USAFID[which(euclidean_dist == min(euclidean_dist, na.rm = TRUE))]),
            by = .(STATE)]
station_med
```

    ##      STATE USAFID
    ##   1:    AL 720265
    ##   2:    AR 722188
    ##   3:    AR 723405
    ##   4:    AR 743312
    ##   5:    AZ 722728
    ##  ---             
    ## 100:    WI 726415
    ## 101:    WI 726457
    ## 102:    WI 726509
    ## 103:    WV 724177
    ## 104:    WY 720521

Knit the doc and save it on GitHub.

## Question 3: In the Geographic Center?

For each state, identify which station is closest to the geographic
mid-point (median) of the state. Combining these with the stations you
identified in the previous question, use `leaflet()` to visualize all
~100 points in the same figure, applying different colors for the
geographic median and the temperature and wind speed median.

``` r
# Calculate median latitude and longitude for each state
state_midpoints <- met[, .(median_lat = quantile(lat, 0.5, na.rm = TRUE),
                           median_lon = quantile(lon, 0.5, na.rm = TRUE)),
                      by = .(STATE)]

# Calculate median values for lon and lat for each station
station_midpoints <- met[, .(lon = quantile(lon, 0.5, na.rm = TRUE),
                             lat = quantile(lat, 0.5, na.rm = TRUE)), 
                         by = .(USAFID, STATE)]

# Join state midpoints with met
station_geo <- merge(station_midpoints, state_midpoints, by = "STATE", all.x = TRUE)

# Calculate distance between each station and its state's midpoint
station_geo[, midpoint_dist := sqrt((lat - median_lat)^2 + (lon - median_lon)^2)]

# Find closest station to each state's midpoint
station_geo <- station_geo[, .(USAFID = USAFID[which(midpoint_dist == min(midpoint_dist, na.rm = TRUE))]),by =.(STATE)]
station_geo
```

    ##     STATE USAFID
    ##  1:    AL 722300
    ##  2:    AR 723429
    ##  3:    AZ 722783
    ##  4:    CA 745046
    ##  5:    CO 726396
    ##  6:    CT 720545
    ##  7:    DE 724088
    ##  8:    FL 722213
    ##  9:    GA 722175
    ## 10:    IA 725466
    ## 11:    ID 726810
    ## 12:    IL 724397
    ## 13:    IN 720736
    ## 14:    KS 724506
    ## 15:    KY 720448
    ## 16:    LA 720468
    ## 17:    MA 744907
    ## 18:    MD 724060
    ## 19:    ME 726073
    ## 20:    MI 725405
    ## 21:    MN 726550
    ## 22:    MO 720869
    ## 23:    MS 722350
    ## 24:    MT 727755
    ## 25:    NC 722201
    ## 26:    ND 720867
    ## 27:    NE 725513
    ## 28:    NH 726050
    ## 29:    NJ 722247
    ## 30:    NM 722683
    ## 31:    NV 724770
    ## 32:    NY 725145
    ## 33:    OH 720928
    ## 34:    OK 722187
    ## 35:    OR 726945
    ## 36:    PA 725118
    ## 37:    RI 725074
    ## 38:    SC 747900
    ## 39:    SD 726530
    ## 40:    TN 721031
    ## 41:    TX 722570
    ## 42:    UT 725724
    ## 43:    VA 720498
    ## 44:    VT 726114
    ## 45:    WA 727930
    ## 46:    WI 726465
    ## 47:    WV 720328
    ## 48:    WY 726720
    ##     STATE USAFID

``` r
# Combining these with the stations identified in the previous question
station_med <- merge(station_med, station_midpoints, 
                     by = c("USAFID", "STATE"), 
                     all.x = TRUE, all.y = FALSE)

station_geo <- merge(station_geo, station_midpoints, 
                     by = c("USAFID", "STATE"), 
                     all.x = TRUE, all.y = FALSE)

map <- leaflet(station_geo) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat, 
                   color = "blue", radius = 5, 
                   popup = ~paste("USAFID:", USAFID, "<br>", "STATE", STATE)) %>%
  addCircleMarkers(data = station_med, ~lon, ~lat, 
                   color = "red", radius = 5, 
                   popup = ~paste("USAFID:", USAFID, "<br>", "STATE", STATE))
map
```

<div class="leaflet html-widget html-fill-item" id="htmlwidget-ffeec03042bfd623a883" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-ffeec03042bfd623a883">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]},{"method":"addCircleMarkers","args":[[39,37.578,30.558,37.4,41.384,41.066,48.39,38.947,40.28,35.38,32.633,35.357,35.582,28.821,40.624,33.178,32.32,31.133,33.45,33.466,35.257,39.173,39.133,40.483,38.068,39.601,41.597,40.218,41.701,43.322,41.691,40.893,40.219,43.205,44.533,44.533,39.05,44.783,43.767,45.544,43.062,43.567,44.5,47.517,47.445,42.212,36.985,33.967],[-80.274,-84.77,-92.099,-77.517,-72.506,-86.182,-100.024,-92.683,-83.115,-86.246,-83.6,-96.943,-79.101,-81.81,-74.669,-86.782,-90.078,-97.717,-105.516,-111.721,-93.095,-76.684,-75.467,-88.95,-97.861,-116.005,-71.412,-76.855,-74.795,-84.688,-93.566,-97.997,-111.723,-71.503,-69.667,-72.615,-105.516,-89.667,-99.318,-94.052,-108.447,-116.24,-123.283,-111.183,-122.314,-71.114,-120.11,-80.467],5,null,null,{"interactive":true,"className":"","stroke":true,"color":"blue","weight":5,"opacity":0.5,"fill":true,"fillColor":"blue","fillOpacity":0.2},null,null,["USAFID: 720328 <br> STATE WV","USAFID: 720448 <br> STATE KY","USAFID: 720468 <br> STATE LA","USAFID: 720498 <br> STATE VA","USAFID: 720545 <br> STATE CT","USAFID: 720736 <br> STATE IN","USAFID: 720867 <br> STATE ND","USAFID: 720869 <br> STATE MO","USAFID: 720928 <br> STATE OH","USAFID: 721031 <br> STATE TN","USAFID: 722175 <br> STATE GA","USAFID: 722187 <br> STATE OK","USAFID: 722201 <br> STATE NC","USAFID: 722213 <br> STATE FL","USAFID: 722247 <br> STATE NJ","USAFID: 722300 <br> STATE AL","USAFID: 722350 <br> STATE MS","USAFID: 722570 <br> STATE TX","USAFID: 722683 <br> STATE NM","USAFID: 722783 <br> STATE AZ","USAFID: 723429 <br> STATE AR","USAFID: 724060 <br> STATE MD","USAFID: 724088 <br> STATE DE","USAFID: 724397 <br> STATE IL","USAFID: 724506 <br> STATE KS","USAFID: 724770 <br> STATE NV","USAFID: 725074 <br> STATE RI","USAFID: 725118 <br> STATE PA","USAFID: 725145 <br> STATE NY","USAFID: 725405 <br> STATE MI","USAFID: 725466 <br> STATE IA","USAFID: 725513 <br> STATE NE","USAFID: 725724 <br> STATE UT","USAFID: 726050 <br> STATE NH","USAFID: 726073 <br> STATE ME","USAFID: 726114 <br> STATE VT","USAFID: 726396 <br> STATE CO","USAFID: 726465 <br> STATE WI","USAFID: 726530 <br> STATE SD","USAFID: 726550 <br> STATE MN","USAFID: 726720 <br> STATE WY","USAFID: 726810 <br> STATE ID","USAFID: 726945 <br> STATE OR","USAFID: 727755 <br> STATE MT","USAFID: 727930 <br> STATE WA","USAFID: 744907 <br> STATE MA","USAFID: 745046 <br> STATE CA","USAFID: 747900 <br> STATE SC"],null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addCircleMarkers","args":[[31.397,32.915,36.018,43.677,40.947,40.435,41.828,40.333,44.883,41.444,40.617,29.976,33.25,34,33.828,34.4,40.225,40.204,48.884,48.929,47.29,48.39,47.796,46.768,32.214,35.178,43.621,25.91,44.074,26.918,26,41.584,35.541,35.417,35.668,35.212,32.516,30.833,32.338,32.359,29.339,29.36,33.45,31.421,33.938,35.317,35.554,36.77,34.917,38.721,39.366,38.69,39.404,38.23,38.817,39.374,39.717,39.417,40.859,41.164,41.921,42.468,41.53,43.111,42.264,41.407,41.276,41.674,41.691,41.019,40.722,41.367,41.299,38.142,40.6,40.789,40.219,41.194,42.545,42.167,43.642,42.9,45.462,44.739,42.617,44.267,43.426,43.389,46.447,45.15,44.018,45.807,43.595,44.843,46.87,46.782,47.646,46.117,34.048,40.948,39.78,32.483,26.079,33.317],[-84.895,-85.963,-75.671,-92.18,-91.511,-75.382,-94.16,-82.517,-72.233,-106.827,-74.25,-92.084,-81.383,-80.367,-79.122,-80.117,-83.352,-84.532,-99.621,-103.297,-101.581,-100.024,-103.254,-100.894,-83.128,-86.066,-96.216,-80.283,-93.553,-81.994,-80.241,-95.339,-78.39,-80.151,-95.949,-91.737,-84.942,-93.333,-90.221,-95.404,-98.472,-99.174,-105.516,-110.846,-118.386,-77.633,-87.179,-90.322,-92.15,-77.515,-75.078,-75.362,-77.975,-85.663,-92.215,-99.83,-104.75,-118.716,-74.056,-73.127,-71.491,-71.295,-71.283,-76.104,-84.456,-95.047,-91.673,-93.022,-93.566,-93.359,-95.026,-91.15,-93.114,-76.429,-98.426,-99.771,-111.723,-112.016,-113.768,-120.4,-70.304,-72.267,-69.596,-85.568,-89.033,-88.517,-88.703,-99.843,-95.212,-93.217,-92.831,-108.546,-118.957,-117.809,-68.017,-100.757,-101.44,-122.893,-94.402,-87.183,-90.238,-81.737,-80.162,-79.317],5,null,null,{"interactive":true,"className":"","stroke":true,"color":"red","weight":5,"opacity":0.5,"fill":true,"fillColor":"red","fillOpacity":0.2},null,null,["USAFID: 720257 <br> STATE GA","USAFID: 720265 <br> STATE AL","USAFID: 720282 <br> STATE NC","USAFID: 720283 <br> STATE MN","USAFID: 720309 <br> STATE IA","USAFID: 720324 <br> STATE PA","USAFID: 720412 <br> STATE IA","USAFID: 720414 <br> STATE OH","USAFID: 720493 <br> STATE VT","USAFID: 720521 <br> STATE WY","USAFID: 720581 <br> STATE NJ","USAFID: 720587 <br> STATE LA","USAFID: 720602 <br> STATE SC","USAFID: 720611 <br> STATE SC","USAFID: 720613 <br> STATE SC","USAFID: 720633 <br> STATE SC","USAFID: 720651 <br> STATE OH","USAFID: 720713 <br> STATE OH","USAFID: 720853 <br> STATE ND","USAFID: 720861 <br> STATE ND","USAFID: 720866 <br> STATE ND","USAFID: 720867 <br> STATE ND","USAFID: 720868 <br> STATE ND","USAFID: 720871 <br> STATE ND","USAFID: 720962 <br> STATE GA","USAFID: 720974 <br> STATE TN","USAFID: 722006 <br> STATE MN","USAFID: 722024 <br> STATE FL","USAFID: 722032 <br> STATE MN","USAFID: 722034 <br> STATE FL","USAFID: 722037 <br> STATE FL","USAFID: 722097 <br> STATE IA","USAFID: 722131 <br> STATE NC","USAFID: 722148 <br> STATE NC","USAFID: 722164 <br> STATE OK","USAFID: 722188 <br> STATE AR","USAFID: 722255 <br> STATE GA","USAFID: 722334 <br> STATE LA","USAFID: 722354 <br> STATE MS","USAFID: 722448 <br> STATE TX","USAFID: 722523 <br> STATE TX","USAFID: 722533 <br> STATE TX","USAFID: 722683 <br> STATE NM","USAFID: 722728 <br> STATE AZ","USAFID: 722950 <br> STATE CA","USAFID: 723067 <br> STATE NC","USAFID: 723249 <br> STATE TN","USAFID: 723300 <br> STATE MO","USAFID: 723405 <br> STATE AR","USAFID: 724036 <br> STATE VA","USAFID: 724075 <br> STATE NJ","USAFID: 724093 <br> STATE DE","USAFID: 724177 <br> STATE WV","USAFID: 724235 <br> STATE KY","USAFID: 724450 <br> STATE MO","USAFID: 724655 <br> STATE KS","USAFID: 724695 <br> STATE CO","USAFID: 724885 <br> STATE NV","USAFID: 725025 <br> STATE NJ","USAFID: 725040 <br> STATE CT","USAFID: 725054 <br> STATE RI","USAFID: 725059 <br> STATE MA","USAFID: 725079 <br> STATE RI","USAFID: 725190 <br> STATE NY","USAFID: 725395 <br> STATE MI","USAFID: 725453 <br> STATE IA","USAFID: 725454 <br> STATE IA","USAFID: 725464 <br> STATE IA","USAFID: 725466 <br> STATE IA","USAFID: 725469 <br> STATE IA","USAFID: 725479 <br> STATE IA","USAFID: 725487 <br> STATE IA","USAFID: 725493 <br> STATE IA","USAFID: 725514 <br> STATE MD","USAFID: 725525 <br> STATE NE","USAFID: 725624 <br> STATE NE","USAFID: 725724 <br> STATE UT","USAFID: 725750 <br> STATE UT","USAFID: 725867 <br> STATE ID","USAFID: 725976 <br> STATE OR","USAFID: 726060 <br> STATE ME","USAFID: 726165 <br> STATE NH","USAFID: 726190 <br> STATE ME","USAFID: 726387 <br> STATE MI","USAFID: 726415 <br> STATE WI","USAFID: 726457 <br> STATE WI","USAFID: 726509 <br> STATE WI","USAFID: 726518 <br> STATE SD","USAFID: 726561 <br> STATE MN","USAFID: 726577 <br> STATE MN","USAFID: 726596 <br> STATE MN","USAFID: 726770 <br> STATE MT","USAFID: 726830 <br> STATE OR","USAFID: 726886 <br> STATE OR","USAFID: 727120 <br> STATE ME","USAFID: 727640 <br> STATE ND","USAFID: 727677 <br> STATE ND","USAFID: 727924 <br> STATE WA","USAFID: 743312 <br> STATE AR","USAFID: 744660 <br> STATE IN","USAFID: 744666 <br> STATE IL","USAFID: 747805 <br> STATE GA","USAFID: 747830 <br> STATE FL","USAFID: 747918 <br> STATE SC"],null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[25.91,48.929],"lng":[-123.283,-68.017]}},"evals":[],"jsHooks":[]}</script>

Knit the doc and save it on GitHub.

## Question 4: Summary Table with `kableExtra`

Generate a summary table using `kable` where the rows are each state and
the columns represent average temperature broken down by low, median,
and high elevation stations.

Use the following breakdown for elevation:

- Low: elev \< 93
- Mid: elev \>= 93 and elev \< 401
- High: elev \>= 401

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, first create a
  lazy table. Filter out values of atmospheric pressure outside of the
  range 1000 to 1020. Examine the association between temperature (y)
  and atmospheric pressure (x). Create a scatterplot of the two
  variables using ggplot2. Add both a linear regression line and a
  smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.

## Deliverables

- .Rmd file (this file)

- link to the .md file (with all outputs) in your GitHub repository
