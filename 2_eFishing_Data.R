###############################################
##
## Script name: 2_eFishing_Data.R
##
## Goals: 1. Generate population estimates and CI using FSA package.
##
##        2. Plot population estimates over time.
##
## Note: See https://github.com/RMBond/ScottCreek-WY21_CZUFire_Poster for project information.
##
###############################################

#Load packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(FSA) #install.packages("FSA") # this will take a while.
library(patchwork)

#1. Read in NEW (Autumn WY21) data ####
#Notes: is only has the three sites (reported by Katie Kobayashi).

fish.dat <- read.csv("Data/WY21_Autumn_fish_20211206.csv", sep = ",", header = T) #3 obs of 7 var. TOTAL SALMONIDS


#2. Do some data wrangling ####

fish <- fish.dat %>% #Start formatting columns into r "tidy" structure.
  mutate(Date = mdy(Date)) #R sees dates as Year-Month-Day this tells it where to get that info from the date column.

#3 pass sampling
fish.3p <- fish %>% 
  filter(P4 == 0) %>% #remove 4 pass samples, 3 obs
  select(!P4) #remove pass 4 column. 
  

#Check the data structure

str(fish)

#3. Single species K-pass efishing depletion estimates ####

#Starting with one sample - FOR 3 PASS
#*3.1 number of fish captured during each pass ####
# ct <- unlist(fish.test[5:7]) #2 SPP

fish.test <- fish %>% 
  filter(Date == "2021-08-30")

ct <- unlist(fish.test[3:5]) #TOTAL FISH

#*3.2 calculates number of passes ####
k <- length(ct) 

#*3.3 calculates total fish captured across all passes
Total <- sum(ct) 
i <- seq(1,k)
X <- sum((k - i)*ct)
pr1 <- removal(ct)
summary(pr1)
confint(pr1)

#_______________________________________________

#Try to make a function out of the above calculations FOR 3 PASS

calculate_pop <- function(ct) {
  # Purpose: Calculate the population size and confidence intervals for an eFishing sample
  # Input:  ct - Dataframe with 3 pass depletion counts, pop - desired output pop size
  
  ct <- unlist(fish.test[3:5])
  k <- length(ct) 
  Total <- sum(ct) 
  i <- seq(1,k)
  X <- sum((k - i)*ct)
  pop <- removal(ct)
  
  data.frame(summary(pop), confint(pop))
  
}

calculate_pop(fish.test)
  
#_______________________________________________
#Make for loop cycle through all rows - FOR 3 PASS
# NOTE: I used this to generate the .csv file used in the next step. 
#       I wasn't able to figure out how to convert the print output into 
#       a dataframe. 

for (i in 1:nrow(fish)) {
  ct <- unlist(fish[i,3:5])
  k <- length(ct) 
  Total <- sum(ct) 
  i <- seq(1,k)
  X <- sum((k - i)*ct)
  pop <- removal(ct)
  
  print(data.frame(summary(pop), confint(pop)))
  
}

#_____________________________

#EXPERIMENTAL - not working FOR 3 PASS
#for loop works now I need to figure out how to save the output (not just print it).
# 
# results <- data.frame(pop = vector(mode = "numeric", length = nrow(fish)),
#                       ci = vector(mode = "numeric", length = nrow(fish)))
# 
# 
# for (i in 1:nrow(fish)) {
#   ct <- unlist(fish[i,5:7])
#   k <- length(ct) 
#   Total <- sum(ct) 
#   i <- seq(1,k)
#   X <- sum((k - i)*ct)
#   pop <- removal(ct)
#   
#   results$pop[i] <- summary(pop[i])
#   results$ci[i] <- confint(pop[i])
#   
# }

#4. Plot efishing depletion estimates ####

#Read in population estimate file
# fish.pop.dat <- read.csv("Data/eFishing_pop_20210408.csv", sep = ",", header = T) #23 obs of 8 var. BY SPECIES

fish.pop.dat <- read.csv("Data/eFishing_totalpop_20211206.csv", sep = ",", header = T) #18 obs of 7 var. TOTAL SALMONIDS


#Do some data wrangling
fish.pop <- fish.pop.dat %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(Year = year(Date)) %>% #make a year column
  mutate(Fire = ifelse(Year > 2019, T,F)) # make a column to note fire, used for color in ggplot

#Check data structure
str(fish.pop)

# #Start plotting - 2 SPP
# ggplot(fish.pop, aes(x = Year, y = Pop_Estimate, color = Species, group = Site)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = X95_LCI, ymax = X95_UCI)) +
#   facet_grid(Site ~.) +
#   theme_classic()

#BarPlot - 2 SPP
# ggplot(fish.pop, aes(x = Year, y = Pop_Estimate, fill = Species, group = Species)) +
#   # geom_errorbar(aes(ymin = Pop_Estimate, ymax = X95_UCI, width = 0.5)) +
#   geom_col(position = "dodge") +
#   facet_grid(Site ~.) +
#   geom_vline(xintercept = 2019.5, linetype = "dashed") +
#   scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "Total abundance (# /100 m)") +
#   theme_classic() +
#   theme(legend.position = "bottom",
#         legend.title = element_blank())

#____________________________________________________

# Focusing on SH and the last 3 years of data
# fish.pop.sh <- fish.pop %>% 
#   filter(Species == "Steelhead") %>% 
#   filter(Year > 2017) %>% 
#   mutate(Fire = ifelse(Year == 2020, T,F))

# #create facet labels
# dat_text <- data.frame(
#   label = c("a", "b", "c"),
#   Site = c("BC eFishing", "LS eFishing", "US eFishing"),
#   Site_label = c("Big Creek", "Lower Mainstem", "Upper Mainstem"),
#   x     = c(2017.7,2017.7,2017.7),
#   y     = c(290,290,290))


# ggplot(fish.pop.sh, aes(x = Year, y = Pop_Estimate, color = Fire)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = X95_LCI, ymax = X95_UCI)) +
#   facet_grid(Site ~.) +
#   scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "Population Estimate") +
#   scale_color_manual(values = c("#a6611a", "#018571"), labels = c("Before", "After")) +
#   theme_classic() +
#   theme(legend.position = "bottom",
#         legend.title = element_blank()) 
#   # geom_text(data  = dat_text, mapping = aes(x = x, y = y, label = label), 
#   #           inherit.aes = FALSE, size = 3.5)  #Add annotation to plot

# ggsave("Figures/eFishing_20210408_6x6.jpg", width = 6, height = 6, units = "in", dpi = 650, device = "jpg")

# #BarPlot SH only
# 
# ggplot(fish.pop.sh, aes(x = Year, y = Pop_Estimate, fill = Fire)) +
#   geom_errorbar(aes(ymin = X95_LCI, ymax = X95_UCI, width = 0.5)) +
#   geom_col() +
#   facet_grid(Site ~.) +
#   geom_vline(xintercept = 2019.5, linetype = "dashed") +
#   scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "Steelhead Abundance (# /100 m)") +
#   scale_fill_manual(values = c("#a6611a", "#018571"), labels = c("Before", "After")) +
#   theme_classic() +
#   theme(legend.position = "bottom",
#         legend.title = element_blank()) 

# ggsave("Figures/eFishing_bar_20210408_3x5.jpg", width = 3, height = 5, units = "in", dpi = 650, device = "jpg")

#____________________________________________________


# Focusing on last 3 years of data

fish.pop.3y <- fish.pop %>%
  filter(Year > 2017)


#Code for odering facets

fish.pop.3y$Site = factor(fish.pop.3y$Site,levels = c("US eFishing","BC eFishing","LS eFishing"),ordered = TRUE)

fish.pop.3y_3color <- fish.pop.3y %>% 
  mutate(Fire = ifelse(Year < 2020, 0, ifelse(Year > 2020, 2,1))) # make a column to note fire, used for color in ggplot


#BarPlot TOTAL SALMONIDS
#Note 3 colors: Dark blue = historical data, light blue = before winter 21 (but after the fire), gold = after winter 21

ggplot(fish.pop.3y_3color, aes(x = Year, y = Pop_Estimate, fill = as.factor(Fire))) +
  geom_errorbar(aes(ymin = X95_LCI, ymax = X95_UCI, width = 0.5)) +
  geom_col() +
  facet_grid(Site ~.) +
  # geom_vline(xintercept = 2019.5, linetype = "dashed") +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Salmonid Abundance (# /100 m)") +
  scale_fill_manual(values = c("#011a27", "#33658a", "#f6ae2d"), labels = c("Pre-CZU Wildfire", "Before", "After")) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) 


# ggsave("Figures/eFishing_totalbar_20211206_3x5.jpg", width = 3, height = 5, units = "in", dpi = 650, device = "jpg")

#BarPlot TOTAL SALMONIDS - all years

ggplot(fish.pop, aes(x = Year, y = Pop_Estimate, fill = Fire)) +
  geom_errorbar(aes(ymin = X95_LCI, ymax = X95_UCI, width = 0.5)) +
  geom_col() +
  facet_grid(Site ~.) +
  geom_vline(xintercept = 2019.5, linetype = "dashed") +
  scale_x_continuous(name = "Year", breaks = seq(2013,2021,1)) +
  scale_y_continuous(name = "Salmonid Abundance (# /100 m)") +
  scale_fill_manual(values = c("#011a27", "#f6ae2d"), labels = c("Before", "After")) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) 

#BarPlot TOTAL SALMONIDS - Landscpae layout
# Goal: need to make three seperate plots and stick together to make the above 3 panel facet plot into landscape
# starting with fish.pop.3y

fish.pop.3y.US <- fish.pop.3y %>% 
  filter(Site == "US eFishing")

fish.pop.3y.BC <- fish.pop.3y %>% 
  filter(Site == "BC eFishing")

fish.pop.3y.LS <- fish.pop.3y %>% 
  filter(Site == "LS eFishing")


#BarPlot Upper Mainstem

plot.UM <- ggplot(fish.pop.3y.US, aes(x = Year, y = Pop_Estimate, fill = Fire)) +
  geom_errorbar(aes(ymin = X95_LCI, ymax = X95_UCI, width = 0.5)) +
  geom_col() +
  geom_vline(xintercept = 2019.5, linetype = "dashed") +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "Salmonid Abundance (# /100 m)", limits = c(0,500)) +
  scale_fill_manual(values = c("#011a27", "#f6ae2d"), labels = c("Before", "After")) +
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_blank()) +
  ggtitle("Upper Mainstem")
  

#BarPlot Big Creek

plot.BC <- ggplot(fish.pop.3y.BC, aes(x = Year, y = Pop_Estimate, fill = Fire)) +
  geom_errorbar(aes(ymin = X95_LCI, ymax = X95_UCI, width = 0.5)) +
  geom_col() +
  geom_vline(xintercept = 2019.5, linetype = "dashed") +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", limits = c(0,500)) +
  scale_fill_manual(values = c("#011a27", "#f6ae2d"), labels = c("Before", "After")) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle("Big Creek")


#BarPlot Lower Mainstem

plot.LM <- ggplot(fish.pop.3y.LS, aes(x = Year, y = Pop_Estimate, fill = Fire)) +
  geom_errorbar(aes(ymin = X95_LCI, ymax = X95_UCI, width = 0.5)) +
  geom_col() +
  geom_vline(xintercept = 2019.5, linetype = "dashed") +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", limits = c(0,500)) +
  scale_fill_manual(values = c("#011a27", "#f6ae2d"), labels = c("Before", "After")) +
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_blank()) +
  ggtitle("Lower Mainstem")

#stich together
plot.UM + plot.BC + plot.LM

# ggsave("Figures/eFishing_totalbar_20211206_7x3.jpg", width = 7, height = 3, units = "in", dpi = 650, device = "jpg")

#____________________________________________________

