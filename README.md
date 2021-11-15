Scott Creek CZU Fire AGU Fall 2021 Poster (WY2022)
================
15 November, 2021

-   [Introduction](#introduction)
-   [Readme File Purpose](#readme-file-purpose)
-   [Workflow Steps](#workflow-steps)
-   [Pebble Counts](#pebble-counts)
-   [Final Poster Output](#final-poster-output)

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Introduction

<img align="Right" width="300" height="300" src="Figures/CZU_perim_SCWatershed_crop_20210426.jpg">

In August 2020, the CZU Lightning Complex fire burned more than 350 km2
(86,500 acres) of coastal forests and hills in the Santa Cruz Mountains
region (Red outline in right figure; Santa Cruz and San Mateo counties,
California). Among the watersheds severely affected by wildfire was
Scott Creek (Yellow outline in right figure), a small (70 km2) coastal
basin \~80 km south of San Francisco Bay. The Scott Creek watershed is
of special management concern as it supports the southernmost extant
population of coho salmon (*Oncorhynchus kisutch*; Central California
Coast \[CCC\] evolutionarily significant unit) in North America, as well
as federally threatened CCC steelhead (anadramous *O. mykiss*). Scott
Creek is also the location of a salmonid life cycle monitoring station
operated jointly by NOAA’s Southwest Fisheries Science Center and the
University of California, Santa Cruz. Extensive physical, chemical, and
biological monitoring conducted throughout the Scott Creek watershed
since 2002 provides a unique opportunity to rigorously examine the
direct and indirect effects of wildfire on salmonid productivity and
carrying capacity.

## Readme File Purpose

This readme file consists of an overview of the datasets, goals, and
data visualizations used in the AGU Fall 2021 Poster (December 2021;
WY2022). The goal was to visualize conditions 1 year after the 2020 CZU
Fire in the Scott Creek watershed (Santa Cruz, CA). Datasets include:
pebble counts, and \[Add more details here\]. Each dataset has a unique
scipt (.R file) where the data wrangling, analysis, and data
visualization occurs. The “source” function has been included in this
readme file to incorperate the individual scripts if desired (they are
currently commented out for simplicity).

<br>

**Dataset Descriptions**

The <span style="color:purple">*Data*</span> folder contains the
datasets used in the poster.

1.  The <span
    style="color:purple">*Scott\_Creek\_Pebble\_20211115.csv*</span>
    datafile consists of pebble counts at 23 transects accrost the
    watershed (6 eFishing sites with 3 transects each, and 5 additional
    pebble count only transects) which were repeated twice (Autumn 2020
    & Summer 2021). AC and MA entered the data. There was some sort of
    typo which hopefully is now fixed.

<br>

**Plot Theme Notes**

-   The colors “\#011a27” (dark blue) and “\#e6df44” (yellow) were used
    in a preivious poster to designate data collected before and after
    the fire, respectively.

<br>

## Workflow Steps

**Pebble Count Dataset**

1.  Decide on which pebble count rounds to present (likely round 1 and
    3),

2.  Generate pebble count summaries (% Fines, D50).

3.  Plot summaries (percent fines boxplots).

<br>

## Pebble Counts

**Goals**:

1.  Visualize the sediment distributions at each site/transect. (Data
    needs to be manipulated from size class counts (summaries) to
    individual pebbles).

2.  Estimate the change in the amount of surface fines at each transect
    (reported as % change over time). Note: The cuttoff for “fines” in
    the litterature is a bit ambiguous. For now we will stick with
    “fines” meaning &lt;6mm (though &lt;8mm could also been used).

3.  Compare how different surface summary statistics (i.e. Dx) changed 1
    year post fire at 1.A each site and 1.B longitudinally along the
    mainstem and Big Creek.

-   There are some example papers vizualizing pebble count data in the
    CZU fire google drive [pebble count
    folder](https://drive.google.com/drive/u/1/folders/1MwYFVTyhN1_3NMqhlwIAu8DBYOm8KzSo).

**Script**: 1\_Pebble\_Count\_Data.R

**Dataset name**: pc

**Variable descriptions**:

-   Date = Sample date (format = YYYY-MM-DD).

-   Site = Site name (“\_\_\_ eFishing” or “PCX-\_\_”).

-   Transect = Transect number (1,2,3 for eFishing sites or blank for
    PCX sites).

-   Round = Survey round (1 = Autumn 2020, 2 = Winter 2021).

-   Long\_Station = Longitude Station (1 = Lower eFishing, 2 = PCX-1, 3
    = PCX-2, 4 = PCX-4, 5 = PCX-5, 6 = Upper eFishing, 7 = Dog eFishing,
    8 = PCX-3, 9 = Big Creek eFishing, 10 = Powerhouse eFishing, 11 =
    Little Creek eFishing).

-   Size\_class\_mm = Size class (mm), gevelometer hole the pebble *did
    not* fit though.

-   Category\_total = Total number of pebbles in the size class.

-   Percent\_finer = Cumulative percent finer for each size class. This
    is used for calculating the Dx statistic.

<br>

**Cumulative percent finer plots for the eFishing sites**

Each eFishing site plot: each panel (facet) is a transect and line color
denotes the two survey rounds (tan is before winter, blue is after first
flush). The starting place and slope of each line gives you an idea of
the sediment distribution. “Shifts in the lower end of the pebble count
cumulative frequency curves are indicative of significant increases in
streambed fines” (Potyondy and Hardy 1994). If the blue line starts
above the tan line, the surface sediment became finer after the first
flush.

Potyondy and Hardy (1994) reccomend, “statistical comparison of particle
size distributions can be done with 2 x 2 contingency tables (number of
pebbles less than 6 mm versus the number of pebbles greater than or
equal to 6 mm) and the likelihood ratio Chi-square statistic to compare
one frequency distribution with another (King and Potyondy, 1993).”

<br>

**Percent fines (&lt;6mm) and change in percent fines along the
mainstem**

<br>

**Boxplots and stat tests of percent surface fines (before and i year
post fire)**

<br>

**Sediment Distribution Plots**

Distribution plots can help answer the question, “Are the data
symmetrically distributed or are they concentrated in one or more size
classes?” To look at distributions the data need to have a row for each
observation (“tidy” data format).

Before we can plot the distribution, the pebble count input data needs
to be “expanded” from size class *totals* to *individual* pebble counts
by duplicating rows of values.

<br>

**Find gransize at percentiles**

Colin Nicol has generously shared his code to help with this. He created
a function which interpolates a straight line between the two points
nearest to the desired percentile `Dx`. Using the data provided, the
function looks for the minimum grain size where the percent finer is
greater than `Dx`. Then it calculates the slope between those two lines.
From here, it uses the slope and the `rise` to get to 50% to calculate a
`run` (distance on the x-axis `grain size` we need to move from the
known point to `D50`).

``` r
#Pebble Count Summary Stat code from C. Nicol :)

calculate_dx <- function(dx, size, prcnt_finer) {
  # Purpose: Calculate the grainsize for a given percentile
  # Method: Interpolate a straight line between the two bounding points
  # Input:  df - Dataframe with percent finer than data, dx - desired output percentile
  
  # Output: Grainsize at dx
  
  dx <- dx/100
  
  data.frame(size = size, prcnt_finer = prcnt_finer) %>%
    mutate(abovex = min(size[prcnt_finer > dx]),
           lessx = max(size[prcnt_finer <= dx])) %>%
    filter(size %in% c(abovex, lessx)) %>%
    mutate(slope = (max(prcnt_finer) - min(prcnt_finer)) / (max(size) - min(size))) %>%
    filter(size == lessx) %>%
    mutate(run = (dx - prcnt_finer) / slope,
           dx_size = size + run,
           dx = dx * 100) %>%
    pull(dx_size)
  
}

#Call in what you want to calculate. In this case its the median grain size (D50) of the test dataset (one site). 

# calculate_dx(50, pc.test$Size_class_mm, pc.test$Percent_finer)

#Note, it will give you an error if the first category (i.e. <2mm) is greater than the percential you want to calculate.

# calculate_dx(16, pc.test$Size_class_mm, pc.test$Percent_finer) #error generated becuase you want to calculate the D16 and <2mm is 40% of the sample.

#Once we have this function set up, loop through D16, D50 and D84 for the one site.
# dxs <- c(16, 50, 84) # choose which percentiles to calculate (e.g. D16, D50, D84)
# 
# names(dxs) <- paste0('d', dxs)
# 
# sapply(dxs, calculate_dx, size = pc.test$Size_class_mm, prcnt_finer = pc.test$Percent_finer)



# Calculate summary stats for multiple transects all at once
#Test dataset
# pc.test <- pc.test %>%
#   summarize(
#     d50 = calculate_dx(50, size = pc.test$Size_class_mm, prcnt_finer = pc.test$Percent_finer),
#     d84 = calculate_dx(84, size = pc.test$Size_class_mm, prcnt_finer = pc.test$Percent_finer))

####START HERE####
#NOT WORKING FOR THE WHOLE DATASET!
####

# pc.x <- pc %>%
#   # group_by(Site, Transect, Round) %>% # Creates a unique grouping variable
#   summarize(
#             # d16 = calculate_dx(16, size = pc.test$Size_class_mm, prcnt_finer = pc.test$Percent_finer),
#     d50 = calculate_dx(50, size = pc$Size_class_mm, prcnt_finer = pc$Percent_finer),
#     d84 = calculate_dx(84, size = pc$Size_class_mm, prcnt_finer = pc$Percent_finer))
```

<br>

## Final Poster Output

The <span style="color:purple">*Final\_Poster*</span> folder contains
the poster presented at the [AGU Fall 2021
Meeting](https://https://www.agu.org/Fall-Meeting) inlcuding the pptx
file, pdf version, and jpeg file (shown below).
