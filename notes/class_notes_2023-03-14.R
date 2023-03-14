#2023-03-14
#Joins

library(tidyverse)

data1 = data.frame(ID = c(1,2),
                   X1 = c("a1", "a2"))
data2 = data.frame(ID = c(2,3),
                   X2 = c("b1", "b2"))

#MUTATING JOINS

#Left Join - preserves exactly the first ("left") dataset and adds in info from second dataset
  #mutating join - adds data
  #left join adds columns, not rows
data12 = left_join(data1, data2, by="ID")#automatically joins by common column, but can state explicitly
  #can join by multiple columns (eg. data and site)
data12 = data1 %>% #alternative way to do the same thing
  left_join(data2, by="ID")
data12

#Right Join - preserves exactly the second ("right") dataset and adds in info from first
data12 = data1 %>%
  right_join(data2, by="ID")
data12

#Inner Join - only keeps rows with IDs both datasets have in common
  #i.e. removes NAs
data12 = data1 %>%
  inner_join(data2, by = "ID")
data12

#Full Join - keeps all IDs from both datasets
data12 = data1 %>%
  full_join(data2, by = "ID")
data12
#always check dimensions before and after join

#FILTERING JOINS

#Semi Join - only keeps rows from data1 that have corresponding IDs in data2
data12 = data1 %>%
  semi_join(data2, by= "ID")
data12

#Anti Join - only keeps rows from data1 that don't have corresponding IDs in data2
  #useful for looking at what's missing
data12 = data1 %>%
  anti_join(data2, by = "ID")
data12

#SWITCHING BETWEEN LONG AND WIDE DATAFRAMES
  #sometimes useful or required to change format of dataset

survey = data.frame(quadrat_id = c(101,102,103,104),
                    barnacle_n = c(2,11,8,27),
                    chiton_n = c(1,0,0,2),
                    mussel_n = c(0,1,1,4))
survey #wide format - each species gets its own column
long = survey %>%
  pivot_longer(cols=c("barnacle_n", "chiton_n", "mussel_n"),
               names_to="taxon",
               values_to="count")
long #repeats quadrats for each taxon

wide = long %>%
  pivot_wider(names_from = taxon,
              values_from = count)
wide

ggplot(data=wide)+
  geom_point(aes(x=quadrat_id, y=barnacle_n), color="red")+
  geom_point(aes(x=quadrat_id, y=chiton_n), color="blue")+
  geom_point(aes(x=quadrat_id, y=mussel_n), color="green")+
  labs(y="Count",
       x="Quadrat ID")
  
  

ggplot(data=long)+
  geom_point(aes(x=quadrat_id, y=count, col=taxon))
  