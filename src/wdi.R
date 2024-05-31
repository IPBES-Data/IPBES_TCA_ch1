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
library(RColorBrewer) # for the color palette

# Packages
library(car)
library(geomtextpath)

# dirs
dir_drive <- 'G:/.shortcut-targets-by-id/18yX-16J7W2Kyq4Mn3YbU_HTjslZyr4hE/IPBES Task Force Knowledge and Data/_DATA/_TSU Internal/_ Weekly reports/Files - Yanina/TfC/paradoxes_CH1'
dir_git <- 'C:/Users/yanis/Documents/scripts/IPBES-Data/IPBES_TCA_ch1'


# Fig 1.3: The difference between explicit environmentally harmful subsidies and environmental expenditures----

# environemntal harmful subsidies
ehs= read_csv(file.path(dir_git, 'data/IMF/Fossil_Fuel_Subsidies.csv'))
names(ehs)

source = ehs %>% distinct(Source)

#ehs %>% distinct(ISO3) %>% View()
ehs_global = ehs %>% 
  filter(Unit =='Percent of GDP') %>% # USD at constant 2021 prices
  filter(CTS_Name == 'Explicit' | CTS_Name == 'Implicit' | CTS_Name == 'Total Implicit and Explicit') %>% # totals
  dplyr::select(-"ObjectId",-"ISO2",-"Country",-"Indicator",-"Source",-"CTS_Code",-"CTS_Full_Descriptor",-"Unit") %>% 
  pivot_longer(cols = starts_with("F"), names_to = 'year') %>% 
  pivot_wider(names_from = ISO3, values_from = value) %>% 
  mutate(year = gsub('F', '',as.character(year)))%>% 
  mutate(global = rowMeans(across(where(is.numeric)))) %>% 
  dplyr::select(CTS_Name, year, global) %>% 
  pivot_wider(names_from = CTS_Name, values_from = global) %>% 
  mutate(year = as.numeric(year))
#write_csv(ehs_global,file.path(dir_git, 'IMF/Fossil_Fuel_Subsidies_YS.csv'))
names(ehs_global)

# environmental  subsidies
es = read_csv(file.path(dir_git, 'data/IMF/Environmental_Protection_Expenditures.csv'))
names(es)
es %>% distinct(CTS_Name)
es %>% distinct(CTS_Full_Descriptor)
#es %>% distinct(ISO3) %>% View()
source = es %>% distinct(Source)
es_global = es %>% 
  filter(Unit =='Percent of GDP') %>% # USD at constant 2021 prices
  filter(CTS_Name =='Environmental Protection Expenditures') %>% 
  dplyr::select(-"ObjectId",-"ISO2",-"Country",-"Indicator",-"Source",-"CTS_Code",-"CTS_Full_Descriptor",-"Unit") %>% 
  pivot_longer(cols = starts_with("F"), names_to = 'year') %>% 
  pivot_wider(names_from = ISO3, values_from = value) %>% 
  mutate(year = gsub('F', '',as.character(year)))%>% 
  mutate(global = rowMeans(across(where(is.numeric)),na.rm = TRUE)) %>% 
  dplyr::select(CTS_Name, year, global) %>% 
  #pivot_longer(cols = starts_with("F"), names_to = 'year') %>% 
  pivot_wider(names_from = CTS_Name, values_from = global) %>% 
  #mutate(total_expenditure = rowMeans(across(where(is.numeric)),na.rm = TRUE)) %>% 
  mutate(year = as.numeric(year))
names(es_global)
#es_global %>% distinct(CTS_Name)

### plot----
ehs_global_gr <- ggplot(ehs_global, aes(x=year, y=`Explicit`)) +
  geom_line(color="#69b3a2", size=2) +
  ggtitle("Explicit EHS (mean % GDP)") +
  theme_ipsum()
ehs_global_gr

es_global_gr <- ggplot(es_global, aes(x=year, y=`Environmental Protection Expenditures`)) +
  geom_line(color="#69b3a2", size=2) +
  ggtitle("Total Environmental Expenditures (mean % GDP)") +
  theme_ipsum()
es_global_gr

ehs_global_gr + es_global_gr

# Join data to plot together
ehs_es = left_join(es_global, ehs_global, by = 'year')
names(ehs_es)
ehs_es = ehs_es %>% 
  mutate(dif = Explicit/`Environmental Protection Expenditures`) %>% 
  filter(!is.na(Explicit))

ggplot(ehs_es, aes(x = year)) + 
  geom_line(aes(y = `Environmental Protection Expenditures`, colour = "Total Environmental Expenditures"), size=1.5, linetype = "dashed") + 
  geom_line(aes(y = `Explicit`, colour = "Explicit Environmentally Harmful Subsidies"), size=1.5) +
  labs(x = NULL, y = 'Mean % GDP', color = NULL) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  geom_vline(xintercept=2017,linetype=3, size = 1) + 
  scale_colour_grey() +
  ylim(0,2) +
  annotate("text", x = 2017, y = 2, label = "Projected data", color = 'black',size = 3, hjust = -0.1)
#  geom_segment(aes(x = 2015, y = 95.6, xend = 2010, yend = 85), colour='black', size=0.5, arrow = arrow(length = unit(0.08, "cm"))) +
# annotate("text", x = 2005, y = 15, label = "Wildlife populations have declined\nby ~60% between 1970 and 2014", color = 'black', size = 3) +
# geom_segment(aes(x = 2015, y = 30, xend = 2010, yend = 25), colour='black', size=0.5, arrow = arrow(length = unit(0.08, "cm")))

# calculate year mean and CIs; and data to plot together

ehs_es_2017 = ehs_es %>% 
  filter(year <= 2017) %>% 
  pivot
  t() %>% as.data.frame() %>%
  tibble::rownames_to_column("xyz")

ehs_es_2017 = ehs_es %>% 
  filter(year <= 2017) %>% 
  select(-year) %>% 
  mutate(mean_epe_2017 = mean(`Environmental Protection Expenditures`)) %>% 
  mutate(mean_ehs_2017 = mean(`Explicit`)) %>% 
  mutate(sd_epe_2017 = sd(`Environmental Protection Expenditures`)) %>% 
  mutate(sd_ehs_2017 = sd(`Explicit`)) %>% 
  mutate(expend = c('epe', 'eeh'))
  distinct(mean_epe_2017, .keep_all = TRUE) %>% 
  t() %>% as.data.frame()
  

# Most basic error bar
ggplot(ehs_es) +
  geom_bar( aes(x=V1, y= mean_epe_2017), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=V1, ymin=mean_epe_2017-sd_epe_2017, ymax=mean_epe_2017-sd_epe_2017), width=0.4, colour="orange", alpha=0.9, size=1.3)



## Display both charts together
sub_global = es_global %>% 
  left_join(ehs_global, by = 'year')

# Adding a second Y axis with sec.axis(): the idea
# sec.axis() does not allow to build an entirely new Y axis. It just builds a second Y axis 
# based on the first one, applying a mathematical transformation.

# Start with a usual ggplot2 call:
ggplot(sub_global, aes(x=year, y=`Explicit`)) +
  
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

ggplot(sub_global, aes(x=year)) +
  
  geom_line( aes(y=`Explicit`)) + 
  geom_vline(xintercept=5) +
  geom_line( aes(y=`Environmental Protection Expenditures` / coeff)) + # Divide by 10 to get the same range than the temperature
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "First Axis",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis")
  )

# Dual Y axis customization with ggplot2
# A few usual tricks to make the chart looks better:

# ipsum theme to remove the black background and improve the general style, add a title, customize the Y axes to pair them with their related line.

# Value used to transform the data
coeff <- 1

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(sub_global, aes(x=year)) +
  
  geom_line( aes(y=`Explicit`), size=2, color=temperatureColor) + 
  geom_vline(xintercept=2017,linetype=3) + 
  geom_line( aes(y=`Environmental Protection Expenditures` / coeff), size=2, color=priceColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Explicit EHS (mean % GDP)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Environmental Expenditure (mean %GDP)")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +
  
  ggtitle("Difference between explicit environmentaly harmful subsidies and environmental expenditures")





# Fig 1.1: Increased participation in international treaties but maintained biodiversity decline----

# Load data 
treaties= read_csv(file.path(dir_git, 'data/ourworldindata/number-of-parties-env-agreements.csv'))

# calc Max numer of parties across MEAs
treaties$max_parties = apply(treaties[,4:16], 1, max)#max across rows

# Calc percentage to match LPI 
treaties = treaties %>% 
  mutate(perc_max_parties = max_parties*100/206)#percentage out of 193 countries + 2 non-UN memeber states + 11 territories
names(treaties)
#write_csv(treaties,file.path(dir_git, 'data/ourworldindata/processed-parties-env-agreements.csv'))

# plot
world_treaties <- ggplot(treaties, aes(x=Year, y=perc_max_parties)) +
  geom_line(color="#69b3a2", size=2) +
  ggtitle("Percentage parties in MEA") +
  theme_ipsum()
world_treaties

# load LPI data
# lpi= read_csv(file.path(dir_git, 'data/ourworldindata/living-planet-index-by-region.csv'))
# lpi = filter(lpi, Entity == 'World')
# names(lpi)

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

# Load Human modification layer
lcc = read_csv('C:/Users/yanis/Documents/IPBES/human_modification_indic/anthrome_change/fraccover_classbased_100/global_lc_change.csv')

# Join datasets to display them together
treaties_lcc = lcc %>% 
  inner_join(treaties, by = c( 'year' = 'Year')) %>% 
  dplyr::select(Year = year, perc_max_parties, urban_mean, agri_mean, anthropo_mean, forest_mean )

ggplot(treaties_lcc, aes(x = Year)) + 
  geom_line(aes(y = perc_max_parties, colour = "Participation in Multilateral Environmental Agreements\n(percentage of parties)"), size=2) + 
  geom_line(aes(y = anthropo_mean, colour = "Average change in forest cover"), size=2) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  annotate("text", x = 2010, y = 75, label = "Maximum of 197 parties \nacross agreements", color = 'black',size = 3) +
  geom_segment(aes(x = 2015, y = 95.6, xend = 2010, yend = 85), colour='black', size=0.5, arrow = arrow(length = unit(0.08, "cm"))) +
  annotate("text", x = 2005, y = 15, label = "Wildlife populations have declined\nby ~60% between 1970 and 2014", color = 'black', size = 3) +
  geom_segment(aes(x = 2015, y = 30, xend = 2010, yend = 25), colour='black', size=0.5, arrow = arrow(length = unit(0.08, "cm")))

# Fig 1.2: Difference between Gross National Income and adjusted National Income growth (nature depletion)----
### data----
data = read_csv(file.path(dir_git,'data/paradox_3/world_data/ece4cf76-ad92-440b-bf09-14b1e143bf24_Data.csv'))
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

### Plots----

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

### Compare GNI and Adjusted NI----
names(world_data)

world_data = world_data %>% 
  mutate(GNI_2015usd_M = GNI_2015usd/1000000000) %>% 
  mutate(Adjusted_net_national_income_2015usd_M = Adjusted_net_national_income_2015usd/1000000000) %>% 
  mutate(GNI_current_usd_M = GNI_current_usd/1000000000) %>% 
  mutate(Adjusted_net_national_income_current_usd_M = Adjusted_net_national_income_current_usd/1000000000)

## Display both charts together

ggplot(filter(world_data,!is.na(GNI_2015usd)) , aes(x = year)) + 
#ggplot(world_data , aes(x = year)) + 
  geom_line(aes(y = Adjusted_net_national_income_2015usd_M, colour = "Adjusted net national income \n(2015 US$)"), size=1.5, linetype = "dashed") + 
  geom_line(aes(y = GNI_2015usd_M, colour = "Gross National Income \n(2015 US$)"), size=1.5) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  scale_y_continuous(labels = function(x) paste0(x, " B")) + # Add percent sign 
  #scale_color_manual(values = rep("black", 20)) +
  scale_colour_grey() 



#### Paradox #7----

### data----

##CO2 emisions
co2 = read_csv(file.path(dir,'paradox_7/co2/owid-co2-data.csv'))
co2 %>% dplyr::select(country) %>% distinct() %>% View()
co2_region_2004 = co2 %>% 
  filter(grepl('income',country)) %>% 
  filter(year ==2004) 

co2_region_2004_2 = co2_region_2004 %>% 
  filter(country!= 'High-income countries') %>% 
  bind_rows(summarise_all(., ~ if (is.numeric(.)) sum(.) else "lower-mid-income")) %>% 
  mutate(year=2004) %>% 
  rbind(co2_region_2004) %>% 
  distinct(country,.keep_all = TRUE)%>% 
  filter(country =='High-income countries' | country == "lower-mid-income")

ghg_2004 <- ggplot(co2_region_2004_2, aes(x=`country`, y=`ghg_per_capita`)) +
  geom_col(color="grey") +
  ggtitle("ghg per capita") +
  theme_ipsum()
ghg_2004

## health 
deaths = read_csv(file.path(dir,'paradox_7/people_affected_CC/f820451b-2c94-4539-8bbd-9d1676ae39fc.csv'))

  
deaths_region = deaths %>% 
  filter(Location != 'High income countries' & Location != 'Global') %>% 
  summarise(Location = "lower-mid-income",Value = sum(Value)) %>% 
  bind_rows(deaths, .) %>% 
  mutate(Period = 2004) %>% 
  dplyr::select(Period, Location, Value) %>% 
  filter(Location =='High income countries' | Location == "lower-mid-income")

  
cc_death <- ggplot(deaths_region, aes(x=Location, y=Value)) +
  geom_col(color="#69b3a2") +
  ggtitle("Climate change atribbutable deaths") +
  theme_ipsum()
cc_death


#### Paradox  potentially disappeared fraction (PDF)----
### data----

pdf_2019 = read_csv(file.path(dir_git,'/pdf/PDF footprint results 2019.csv'))%>% 
  dplyr::select("country","acronym","population"="population in 2019","production-based_PDF"="production-based PDF",
                "PDF_footprint"="PDF footprint","net_importer"="net-importer","percapite_production_based_PDF"="per capite production-based PDF",
                "percapita_PDF_footprint"="per capita PDF footprint","percapita_net_trade"="per capita net-trade 2019")
names(pdf_2019)


world_bank = read_csv(file.path(dir_git,'/the_world_bank/WDI_CSV/WDICSV.csv'))
names(world_bank)
wb_2019 = world_bank %>% 
  filter(grepl('GNP',`Indicator Code`) | grepl('income',`Indicator Name`) | `Indicator Code`== 'SP.POP.TOTL') %>% 
  dplyr::select("Country Name","Country Code","Indicator Name", "Indicator Code", "2019")

# GNI
gni_2019 = world_bank %>% 
  filter(grepl('GNP',`Indicator Code`)) %>% 
  filter(`Indicator Code`== 'NY.GNP.PCAP.PP.KD') %>% #GNI per capita, PPP (constant 2017 international $)
  dplyr::select("Country_name"="Country Name","Country_Code"="Country Code","Indicator_Name"="Indicator Name",
                "Indicator_Code"="Indicator Code","GNI_percapita_PPP" ="2019")

gni_2019_fix = wb_2019 %>% 
  filter(grepl('GNP',`Indicator Code`)) %>% 
  filter(`Indicator Code`== 'NY.GNP.MKTP.PP.CD') %>% #GNI, PPP (current international $)
  dplyr::select("Country_name"="Country Name","Country_Code"="Country Code","Indicator_Name"="Indicator Name",
                "Indicator_Code"="Indicator Code","GNI_PPP" ="2019") %>% 
  # join with population data for 2019
  left_join(filter(wb_2019, `Indicator Code`== 'SP.POP.TOTL'), by = c("Country_Code"="Country Code")) %>% 
  dplyr::select(-"Country Name", -"Indicator Name",-"Indicator Code") %>% 
  rename('population'='2019') %>% 
  mutate(GNI_percapita_PPP = GNI_PPP/population)
  
# INCOME
anni_2019 = world_bank %>% 
  filter(grepl('income',`Indicator Name`)) %>% 
  filter(`Indicator Code`== 'NY.ADJ.NNTY.PC.KD') %>% # 	Adjusted net national income per capita (constant 2015 US$)
  dplyr::select("Country_name"="Country Name","Country_Code"="Country Code","Indicator_Name"="Indicator Name",
                "Indicator_Code"="Indicator Code","Adj_netNationalIncome_percapita" ="2019")
anni_2019_fix = world_bank %>% 
  #filter(grepl('income',`Indicator Name`)) %>% 
  filter(`Indicator Code`== 'NY.ADJ.NNTY.CD') %>% # 	Adjusted net national income (current US$) %>% 
  dplyr::select("Country_name"="Country Name","Country_Code"="Country Code","Indicator_Name"="Indicator Name",
                "Indicator_Code"="Indicator Code","Adj_netNationalIncome" ="2019") %>% 
  # join with population data for 2019
  left_join(filter(wb_2019, `Indicator Code`== 'SP.POP.TOTL'), by = c("Country_Code"="Country Code")) %>% 
  dplyr::select(-"Country Name", -"Indicator Name",-"Indicator Code") %>% 
  rename('population'='2019') %>% 
  mutate(Adj_netNationalIncome_percapita = Adj_netNationalIncome/population)

gini_2019 = world_bank %>% 
  #filter(grepl('income',`Indicator Name`)) %>% 
  filter(`Indicator Code`== 'SI.POV.GINI') %>% # 	Gini index 
  dplyr::select("Country_name"="Country Name","Country_Code"="Country Code","Indicator_Name"="Indicator Name",
                "Indicator_Code"="Indicator Code","GINI" ="2019")

# joins

pdf_gni_anni_gini = pdf_2019 %>% 
  inner_join(gni_2019_fix, by = c('acronym'='Country_Code')) %>% 
  inner_join(anni_2019_fix, by = c('acronym'='Country_Code')) %>% 
  inner_join(gini_2019, by = c('acronym'='Country_Code')) %>% 
  dplyr::select("country","acronym","population","production-based_PDF","PDF_footprint",
                "net_importer","percapite_production_based_PDF","percapita_PDF_footprint","percapita_net_trade",
                "Indicator_Name"="Indicator_Name.x","Indicator_Code"="Indicator_Code.x",
                "GNI_PPP","GNI_percapita_PPP","Adj_netNationalIncome","Adj_netNationalIncome_percapita","GINI")
names(pdf_gni_anni_gini)
write_csv(pdf_gni_anni_gini, 'C:/Users/yanis/Documents/scripts/IPBES_TSU-DATA/TfC/paradoxes_CH1/output/pdf_gni_income_gini.csv')

# Scatter plots

data <- pdf_gni_anni_gini

my_colors <- brewer.pal(nlevels(as.factor(data$acronym)), "Set2")
scatterplotMatrix(~percapita_net_trade + GNI_percapita_PPP + Adj_netNationalIncome_percapita + GINI, data=data , 
                  reg.line="" , smoother="", 
                  smoother.args=list(col="grey") , cex=1.5 , 
                  pch=c(15,16,17)# , 
                  #main="Relation of wealth and the net terrestrial biodiversity loss"
)


# scatter plot + linear trend + confidence interval
p1 <- ggplot(data, aes(x = GNI_percapita_PPP, y = percapita_net_trade)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  scale_y_continuous(
    # Features of the first axis
    name = "Per capita net-trade") + 
  scale_x_continuous(
    # Features of the first axis
    name = "GNI per capita in PPP") +
  theme_ipsum()# +
  # 
  # theme(
  #   axis.title.y = element_text(color = temperatureColor, size=13),
  #   axis.title.x = element_text(color = priceColor, size=13)
  # ) +
  # 
  # ggtitle("Wealth and terrestrial biodiversity loss")
  
p1


p2 <- ggplot(data, aes(x = Adj_netNationalIncome_percapita, y = percapita_net_trade)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  scale_y_continuous(
    # Features of the first axis
    name = "Per capita net-trade") + 
  scale_x_continuous(
    # Features of the first axis
    name = "Income per capita") +
  theme_ipsum() #+
  # 
  # theme(
  #   axis.title.y = element_text(color = temperatureColor, size=13),
  #   axis.title.x = element_text(color = priceColor, size=13)
  # ) +
  # 
  # ggtitle("Wealth and terrestrial biodiversity loss")
p2

p3 <- ggplot(data, aes(x = GINI, y = percapita_net_trade)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  scale_y_continuous(
    # Features of the first axis
    name = "Per capita net-trade") + 
  scale_x_continuous(
    # Features of the first axis
    name = "GINI index") +
  theme_ipsum() #+
# 
# theme(
#   axis.title.y = element_text(color = temperatureColor, size=13),
#   axis.title.x = element_text(color = priceColor, size=13)
# ) +
# 
# ggtitle("Wealth and terrestrial biodiversity loss")

p3
