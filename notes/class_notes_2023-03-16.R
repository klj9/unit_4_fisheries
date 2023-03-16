#2023-03-16
#Fisheries Regional Database

library(tidyverse)
load('data/RAMLDB v4.495/R Data/DBdata[asmt][v4.495].RData')
head(area)

glimpse(stock)
glimpse(timeseries_values_views)
glimpse(taxonomy)

fish = timeseries_values_views %>%
  left_join(stock) %>%
  left_join(taxonomy) %>%
  select(stockid, stocklong, year, TCbest, tsn, scientificname,
         commonname, region, FisheryType, taxGroup)
glimpse(fish)
dim(timeseries_values_views)
dim(stock)
dim(fish)

glimpse(tsmetrics)
tsmetrics %>% filter(tsshort == "TCbest")

fish = fish %>%
  filter(stockid != "ACADREDGOMGB")#one fishery stood out - managers accidentally multiplied by 1000

ggplot() +
  geom_line(aes(x=year, y=TCbest, color=stockid), data=fish) +
  theme(legend.position = "none")

fish %>% filter(TCbest > 6000000)#now the max makes sense - peruvian anchovy

#Let's look at the collapse of cod populations in east coast canadian fisheries

fish %>%
  filter(scientificname=="Gadus morhua") %>%
  distinct(region)

cod_can = fish %>% #filter just the cod in east canada region, no NAs
  filter(scientificname=="Gadus morhua",
         region=="Canada East Coast",
         !is.na(TCbest))
head(cod_can)

ggplot(data=cod_can)+ #graph by catch
  geom_line(aes(x=year, y=TCbest, color=stockid))

cod_can_total = cod_can %>% #sum the total catch by year
  group_by(year) %>%
  summarize(total_catch = sum(TCbest))
head(cod_can_total)

ggplot(data=cod_can_total)+ #graph total
  geom_line(aes(x=year, y=total_catch))

#collapsed fishery = reaches <10% of max historical catch - when was it first considered collapse

#cumulative functions in base r
dat = c(1,3,6,2,3,9,-1)
dat_max = cummax(dat)#returns list of numbers - for each data point, what was the max so far
dat_sum = cumsum(dat)#returns cumulative sums

cod_collapse = cod_can_total %>%
  mutate(historical_max_catch = cummax(total_catch),#add column for cumulative max
         collapse = (total_catch <= 0.1*historical_max_catch))#add column for collapse (true/false)
tail(cod_collapse)

cod_collapse_year = cod_collapse %>%
  filter(collapse == TRUE) %>%
  summarize(year=min(year)) %>%
  pull(year)
#stock was first considered collapsed in 1993

ggplot()+
  geom_line(aes(x=year, y=total_catch, color=collapse), data=cod_collapse)+
  geom_vline(xintercept = cod_collapse_year)#access data point programmatically

#global stock collapse

collapse = fish %>%
  filter(!is.na(TCbest)) %>%
  group_by(stockid) %>%
  mutate(historical_max_catch = cummax(TCbest),
         current_collapse = TCbest < 0.1*historical_max_catch,
         collapsed_yet = cumsum(current_collapse) > 0) %>%
  ungroup()

glimpse(collapse)

collapse_year = collapse %>%
  group_by(stockid) %>%
  filter(collapsed_yet == TRUE) %>%
  summarize(year=min(year)) %>%
  pull(stockid, year)
head(collapse_year)
