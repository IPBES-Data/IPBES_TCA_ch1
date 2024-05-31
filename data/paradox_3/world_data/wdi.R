rm(list=ls())

# TFC assess
# Paradoxes

# librarues
library(stringr)
#library(data.table)
#library(foreign)
library(tidyverse)
library(dplyr)
#library(readxl)
library(readr)

library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
#library(scales)   # to access breaks/formatting functions
#library(gridExtra) # for arranging plots

library(patchwork) # To display 2 charts together
library(hrbrthemes)


# dirs

dir <- 'G:/.shortcut-targets-by-id/18yX-16J7W2Kyq4Mn3YbU_HTjslZyr4hE/IPBES Task Force Knowledge and Data/_DATA/_TSU Internal/_ Weekly reports/Files - Yanina/TfC/'

data = read_csv(paste0(dir,'world_data/ece4cf76-ad92-440b-bf09-14b1e143bf24_Data.csv'))
data = data[c(1:80),]
data[,5:67]=sapply(data[,5:67],as.numeric)
names(data)

# world
world_data = data %>% 
  filter(`Country Name`=='World') %>% 
  dplyr::select(-`Country Name`, -`Country Code`,-`Series Code`) %>% 
  pivot_longer(cols = -`Series Name`, names_to = 'year') %>% 
  pivot_wider(names_from = `Series Name`, values_from = value) %>% 
  mutate(year = word(year,1)) %>% 
  filter(!is.na(`Adjusted savings: natural resources depletion (% of GNI)`))
world_data$year = as.numeric(world_data$year)
names(world_data)
# [1] "year"                                                    
# [2] "Adjusted savings: natural resources depletion (% of GNI)"
# [3] "GNI (current US$)"                                       
# [4] "GNI (constant 2015 US$)"                                 
# [5] "GDP (constant 2015 US$)"                                 
# [6] "GDP (current US$)"                                       
# [7] "GDP growth (annual %)"                                   
# [8] "GNI growth (annual %)"                                   
# [9] "Adjusted net national income (annual % growth)"          
# [10] "Adjusted net national income (constant 2015 US$)"        
# [11] "Adjusted net national income (current US$)"

names(world_data) <- c("year","Natural_resources_depletion","GNI_current_usd","GNI_2015usd",                                 
                  "GDP_2015usd", "GDP_current_usd","GDP_growth","GNI_growth",                                  
                  "Adjusted_net_national_income","Adjusted_net_national_income_2015usd",        
                  "Adjusted_net_national_income_current_usd")

low_data = data %>% 
  filter(`Country Name`=='Low income') %>% 
  dplyr::select(-`Country Name`, -`Country Code`,-`Series Code`) %>% 
  pivot_longer(cols = -`Series Name`, names_to = 'year') %>% 
  pivot_wider(names_from = `Series Name`, values_from = value) %>% 
  mutate(year = word(year,1)) #%>% 
  #filter(!is.na(`Adjusted savings: natural resources depletion (% of GNI)`))
low_data$year = as.numeric(low_data$year)
names(low_data) <- c("year","Natural_resources_depletion","GNI_current_usd","GNI_2015usd",                                 
                  "GDP_2015usd", "GDP_current_usd","GDP_growth","GNI_growth",                                  
                  "Adjusted_net_national_income","Adjusted_net_national_income_2015usd",        
                  "Adjusted_net_national_income_current_usd")

high_data = data %>% 
  filter(`Country Name`=='High income') %>% 
  dplyr::select(-`Country Name`, -`Country Code`,-`Series Code`) %>% 
  pivot_longer(cols = -`Series Name`, names_to = 'year') %>% 
  pivot_wider(names_from = `Series Name`, values_from = value) %>% 
  mutate(year = word(year,1)) #%>% 
#filter(!is.na(`Adjusted savings: natural resources depletion (% of GNI)`))
high_data$year = as.numeric(high_data$year)
names(high_data) <- c("year","Natural_resources_depletion","GNI_current_usd","GNI_2015usd",                                 
                "GDP_2015usd", "GDP_current_usd","GDP_growth","GNI_growth",                                  
                "Adjusted_net_national_income","Adjusted_net_national_income_2015usd",        
                "Adjusted_net_national_income_current_usd")




# Most basic line chart
world <- ggplot(world_data, aes(x=year, y=Natural_resources_depletion)) +
  geom_line(color="#69b3a2", size=2) +
  ggtitle("Natural resources depletion (% of GNI) WORLD") +
  theme_ipsum()
world


low <- ggplot(low_data, aes(x=year, y=Natural_resources_depletion)) +
  geom_line(color="#69b3a2", size=2) +
  ggtitle("Natural resources depletion (% of GNI) LOW") +
  theme_ipsum()
low


high <- ggplot(high_data, aes(x=year, y=Natural_resources_depletion)) +
  geom_line(color="#69b3a2", size=2) +
  ggtitle("Natural resources depletion (% of GNI) HIGH") +
  theme_ipsum()
high

world + low + high



p2 <- ggplot(world, aes(x=year, y=`GNI_current_usd`)) +
  geom_line(color="grey",size=2) +
  ggtitle("GNI (current US$)") +
  theme_ipsum()
p2

p3 <- ggplot(world, aes(x=year, y=`Adjusted_net_national_income_current_usd`)) +
  geom_line(color="grey",size=2) +
  ggtitle("Adjusted net national income (current US$)") +
  theme_ipsum()
p3

# Display both charts side by side thanks to the patchwork package
p2 + p3

# Adding a second Y axis with sec.axis(): the idea
# sec.axis() does not allow to build an entirely new Y axis. It just builds a second Y axis 
# based on the first one, applying a mathematical transformation.

# Start with a usual ggplot2 call:
ggplot(world, aes(x=year, y=`GNI_current_usd`)) +
  
  # Custom the Y scales:
  scale_y_continuous(
    
    # Features of the first axis
    name = "First Axis",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*1, name="Second Axis")
  ) +
  
  theme_ipsum()
#Show 2 series on the same line chart thanks to sec.axis()
# We can use this sec.axis mathematical transformation to display 2 series that have a different range.

# Value used to transform the data
coeff <- 1

ggplot(world, aes(x=year)) +
  
  geom_line( aes(y=GNI_current_usd)) + 
  geom_line( aes(y=Adjusted_net_national_income_current_usd / coeff)) + # Divide by 10 to get the same range than the temperature
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "First Axis",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis")
  )

# Dual Y axis customization with ggplot2
# A feew usual tricks to make the chart looks better:
  
# ipsum theme to remove the black background and improve the general style, add a title, customize the Y axes to pair them with their related line.

# Value used to transform the data
coeff <- 1

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(world_data, aes(x=year)) +
  
  geom_line( aes(y=GNI_current_usd), size=2, color=temperatureColor) + 
  geom_line( aes(y=Adjusted_net_national_income_current_usd / coeff), size=2, color=priceColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "GNI (current US$)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Adjusted net national income (current US$)")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +
  
  ggtitle("Difference between GNI and adjusted NI grows (nature depletion)")





# plot 
qplot(x=year, y=`Adjusted savings: natural resources depletion (% of GNI)`,
      data=world, na.rm=TRUE,
      main="Adjusted savings: natural resources depletion (% of GNI)",
      xlab="Date", ylab="Natural resources depletion (% of GNI)")
