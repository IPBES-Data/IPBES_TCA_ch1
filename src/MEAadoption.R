rm(list=ls())

# TFC assess
# Paradoxes

# librarues
library(stringr)
#library(data.table)
#library(foreign)
library(tidyverse)
library(dplyr)
library(readxl)
library(readr)

#library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
#library(scales)   # to access breaks/formatting functions
#library(gridExtra) # for arranging plots

# library(patchwork) # To display 2 charts together
# library(hrbrthemes)
# library(RColorBrewer) # for the color palette
# 
# # Packages
# library(car)
# library(geomtextpath)

# dirs
dir_git <- 'C:/Users/yanis/Documents/scripts/IPBES-Data/IPBES_TCA_ch1'


# Increased participation in international treaties but maintained biodiversity decline----

## Load data 
# Our World in Data (features figure 1.7 of the DEVELOPMENT AND GLOBALIZATION FACTS AND FIGURES 2016)
owd_treaties= read_csv(file.path(dir_git, 'data/ourworldindata/number-of-parties-env-agreements.csv'))
owd_treaties$max_parties_OWD = apply(owd_treaties[,4:16], 1, max)#max across rows

# Calc percentage 
owd_treaties = owd_treaties %>% 
  mutate(perc_max_parties_OWD = max_parties_OWD*100/206)#percentage out of 193 countries + 2 non-UN memeber states + 11 territories
names(owd_treaties)
write_csv(owd_treaties,file.path(dir_git, 'data/ourworldindata/processed-parties-env-agreements.csv'))

# other conventions
basel = read_xlsx(file.path(dir_git, 'data/other_conventions/basel.xlsx'), sheet = 'year')
basel2 = basel %>% 
  mutate(year = if_else(is.na(year_sig),
                        true = year_prot,
                        false = year_sig)) %>% 
  group_by(year) %>% 
  summarize(basel = n())
  
ozone_montreal = read_xlsx(file.path(dir_git, 'data/other_conventions/ozone_montreal.xlsx'), sheet = 'year')
ozone_montreal2 = ozone_montreal %>% 
  mutate(year = if_else(is.na(year_sig),
                        true = year_prot,
                        false = year_sig)) %>% 
  group_by(year) %>% 
  summarize(ozone_montreal = n())
minamata = read_xlsx(file.path(dir_git, 'data/other_conventions/minamata.xlsx'), sheet = 'year')
minamata2 = minamata %>% 
  mutate(year = if_else(is.na(year_sig),
                        true = year_prot,
                        false = year_sig)) %>% 
  group_by(year) %>% 
  summarize(minamata = n())

nagoya = read_xlsx(file.path(dir_git, 'data/other_conventions/nagoya.xlsx'), sheet = 'year')
nagoya2 = nagoya %>% 
  mutate(year = if_else(is.na(year_sig),
                        true = year_prot,
                        false = year_sig)) %>% 
  filter(!is.na(year)) %>% 
  group_by(year) %>% 
  summarize(nagoya = n())


paris = read_xlsx(file.path(dir_git, 'data/other_conventions/paris_agree.xlsx'), sheet = 'year')
paris2 = paris %>% 
  mutate(year = if_else(is.na(year_sig),
                        true = year_prot,
                        false = year_sig)) %>% 
  group_by(year) %>% 
  summarize(paris = n())

UNCLOS = read_xlsx(file.path(dir_git, 'data/other_conventions/UNCLOS.xlsx'), sheet = 'year')
UNCLOS2 = UNCLOS %>% 
  mutate(year = if_else(is.na(year_sig),
                        true = year_prot,
                        false = year_sig)) %>% 
  filter(!is.na(year)) %>% 
  group_by(year) %>% 
  summarize(UNCLOS = n())
UNECE = read_xlsx(file.path(dir_git, 'data/other_conventions/UNECE.xlsx'), sheet = 'year')
UNECE2 = UNECE %>% 
  group_by(year) %>% 
  summarize(UNECE = n())
africa = read_xlsx(file.path(dir_git, 'data/other_conventions/africa_conservation.xlsx'), sheet = 'year')
africa2 = africa %>% 
  group_by(year) %>% 
  summarize(africa = n())

#join other conventions

other_treaties = basel2 %>% 
  full_join(minamata2, by = 'year') %>% 
  full_join(nagoya2, by = 'year') %>% 
  full_join(ozone_montreal2, by = 'year') %>% 
  full_join(paris2, by = 'year') %>% 
  full_join(UNCLOS2, by = 'year') %>% 
  full_join(UNECE2, by = 'year') %>% 
  mutate(across(.cols = everything(), \(x) replace_na(x, 0))) %>% 
  mutate(across(.cols = everything(), \(x) as.numeric(x))) 



# join OWD with other conventions
all_treaties = other_treaties %>% 
  full_join(owd_treaties, by = c('year'='Year')) %>% 
  dplyr::select(-Entity, -Code)
all_treaties$max_parties = apply(all_treaties[,2:21], 1, max)#max across rows
all_treaties$perc_max_parties = all_treaties$max_parties*100/206 #percentage out of 193 countries + 2 non-UN memeber states + 11 territories
write_csv(all_treaties,file.path(dir_git, '/outputs/all-processed-parties-env-agreements.csv'))


# plot
world_treaties <- ggplot(owd_treaties, aes(x=Year, y=perc_max_parties)) +
  geom_line(color="#69b3a2", size=2) +
  ggtitle("Percentage parties in MEA") +
  theme_ipsum()
world_treaties

## Living Planet Index ()
lpi= read_csv(file.path(dir_git, 'data/LPI/Global.csv'))
lpi2 = lpi %>% 
  mutate(percent = 100) %>% 
  mutate(across(LPI_final:CI_high, ~ .*percent))
names(lpi)
write_csv(lpi2, file.path(dir_git, 'data/LPI/Global_processed.csv'))

# plot
world_lpi <- ggplot(lpi, aes(x=Year, y=living_planet_index_average)) +
  geom_line(color="#1E91D6", size=2) +
  ggtitle("Average decline in monitored wildlife populations (LPI)") +
  theme_ipsum()
world_lpi

world_treaties + world_lpi

# Join datasets to display them together
treaties_lpi = lpi %>% 
  inner_join(treaties, by = 'Year') %>% 
  dplyr::select(Year, perc_max_parties, LPI_final, CI_low, CI_high )

### Plot with 2 Y axis ----
# sec.axis() which builds a second Y axis based on the first one, applying a mathematical transformation.

# Value used to transform the data
coeff <- 1

temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(treaties_lpi, aes(x=Year)) +
  
  geom_line( aes(y=perc_max_parties), size=2, color=temperatureColor) + 
  geom_line( aes(y=living_planet_index_average / coeff), size=2, color=priceColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Percentage of parties in MEAs",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Average change in monitored wildlife populations (LPI)")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +
  
  ggtitle("Decline in biodiversity and increased participation in MEAs")

### Plot with one Y axis----

ggplot(treaties_lpi, aes(x = Year)) + 
  geom_line(aes(y = perc_max_parties, colour = "Percentage of countries in \nMultilateral Environmental Agreements (in %)"), size=1.5, linetype = "dashed") + 
  geom_line(aes(y = LPI_final, colour = "Average change in monitored wildlife populations \n(Living Planet Index, in %)"), size=1.5) +
  geom_line(aes(y=CI_low),linetype="dotted", size = 1) + 
  geom_line(aes(y=CI_high),linetype="dotted", size = 1)+
  labs(x = NULL, y = NULL, color = NULL) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #scale_color_manual(values = rep("black", 20)) +
  scale_colour_grey() +
  annotate("text", x = 2016, y = 31.62, label = "LPI", color = 'black',size = 3) +
  annotate("text", x = 2017, y = 26.28, label = "Lower CI", color = 'black',size = 3) +
  annotate("text", x = 2017, y = 38.06, label = "Upper CI", color = 'black',size = 3)

  # annotate("text", x = 2010, y = 75, label = "Maximum of 197 parties \nacross agreements", color = 'black',size = 3) +
  # geom_segment(aes(x = 2015, y = 95.6, xend = 2010, yend = 85), colour='black', size=0.5, arrow = arrow(length = unit(0.08, "cm"))) +
  # annotate("text", x = 2005, y = 15, label = "Wildlife populations have declined\nby ~60% between 1970 and 2014", color = 'black', size = 3) +
  # geom_segment(aes(x = 2015, y = 30, xend = 2010, yend = 25), colour='black', size=0.5, arrow = arrow(length = unit(0.08, "cm")))

