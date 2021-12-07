###############################################
##
## Script name: 3_WQ_Data.R
##
## Visualize turbidity, precipitation, and Flow time-series for WY21 (one year post-wildfire).
##
## Note: See https://github.com/RMBond/ScottCreek-WY22_CZUFire_Poster for project information.
##
###############################################

#Load packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork)
# library(xts)

#1. Read in WQ data ####

wq.dat <- read.csv("Data/Scott_Creek_Weir_Hydrolab_20210101_to_20210622.csv", sep = ",", header = T) #7884 obs 56 var.

#2. Do some data wrangling ####

wq <- wq.dat %>% #Start formatting columns into r "tidy" structure.
  mutate(Date = mdy(Date)) %>% #R sees dates as Year-Month-Day this tells it where to get that info from the date column.
  mutate(Time = hms(Time)) %>% # Tells r this column is Hours:Min:Sec
  mutate(TS = as.POSIXct(Date + Time)) %>% #making a new column with timestamp by combining Date and Time
  select(Date, TS, TurbSC_NTU, Dep100_m)#pair down dataset to what we need turbidity and depth only.

#Check the data structure
#Note: Date = format Date and TS = format POSIXct (time series). Everything else should be numbers.

str(wq)

#3. Make individual plots ####

#Set data limits - This will help cut the time series to the dates we want to look at.
limits = as.POSIXct(c("2020-09-30 00:00:00","2021-09-30 00:00:00"))

#* Water Depth Plot ####

depth.plot <- ggplot(wq, aes(x = TS, y = Dep100_m)) +
  geom_point() +
  scale_x_datetime(name = "",
                   date_breaks = "1 month", date_labels = ("%b %y"), 
                   limits = limits, expand = c(0,0)) +
  scale_y_continuous(name = "Stage (m)", limits = c(1.5, 3.5), expand = c(0,0)) +
  theme_classic() +
   theme(axis.text.x = element_blank())


#* Turbidity Plot ####

turb.plot <- ggplot(wq, aes(x = TS, y = TurbSC_NTU)) +
  geom_point() +
  scale_x_datetime(name = "",
                   date_breaks = "1 month", date_labels = ("%b %y"),
                   limits = limits, expand = c(0,0)) +
  scale_y_continuous(name = "Turbidity (NTU)", limits = c(0, 3100), expand = c(0,0)) +
  theme_classic() #+
   # theme(axis.text.x = element_blank())


#4. Milti-plot output####

#Using the "Patchwork" package to pull individual plots together into one multi-plot
#Note the notation below only works with patchwork. 

depth.plot / turb.plot 

#Save the output
#Note: its a good practice to keep the ggsave function "commented out" so you do not override preivious files.

# ggsave("Figures/WQ_20210413_6x4.jpg", width = 6, height = 4, units = "in", dpi = 650, device = "jpg")


#5. Precipitation Bar graph####

precip.dat <- read.csv("Data/Precip_20211207.csv", sep = ",", header = T) #Daily precipitation dataset for WY21. 365 obs 2 var

precip <- precip.dat %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(Date2 = as.POSIXct(Date)) %>% 
  mutate(rain_cm = Inches * 2.54)

str(precip)

#bar graph
precip.daily.plot <- ggplot(precip, aes(x = Date2, y = rain_cm)) +
  geom_col(fill = "black") +
  scale_x_datetime(name = "",
                   date_breaks = "1 month", date_labels = ("%b %d"),
                    expand = c(0,0)) + #limits = limits,
  scale_y_continuous(name = "Precipitation (cm)", limits = c(0, 8), expand = c(0,0)) +
  theme_classic() +
   theme(axis.text.x = element_blank())


precip.daily.plot / depth.plot / turb.plot

# ggsave("Figures/WQ_20211207_6x7.jpg", width = 6, height = 7, units = "in", dpi = 650, device = "jpg")
