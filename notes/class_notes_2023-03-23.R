#2023-03-23
# PCA

library(tidyverse)
library(palmerpenguins)

head(penguins)

#DATA PREP

#pca doesn't work with nas, so we'll drop them
pen_drop_na = penguins %>%
  drop_na()

#we can only use numeric variables in pca, but we'll keep in mind the factor variables
pen_num = pen_drop_na %>%
  select(ends_with("mm"), body_mass_g)
head(pen_num)

#we'll hold on to the metadata using a separate dataframe with the exact same row order
pen_meta = pen_drop_na %>%
  select(species, sex, island, year)
head(pen_meta)  

#RUN THE PCA

pen_pca = prcomp(pen_num, scale.=TRUE, center=TRUE)
#scale standardizes contributions of each data point
#center shifts to center over zero

#UNDERSTAND THE PCA

pen_pca #returns standard deviations - tells us how much each principal component explains variation in dataset
#returns loadings/rotation - tells us how much each column contributes to each principal component

summary(pen_pca) #returns importance of components - stdev, proportion of variance (how much each pc contributes to total variation)
#all the pcs together can perfectly reconstruct dataset, explain 100% of the variation

str(pen_pca) #tells you structure of the object - what you can access programmatically
pen_pca$sdev

dim(pen_num)
dim(pen_pca$x) #size of full pc matrix = size of original dataset, corresponds with individual sampled
head(pen_pca$x)

summary(pen_pca)$importance[2,] #grabs second row of importance table (proportion of variance)

#calculate proportion of variance ourselves from sdevs instead of grabbing from summary
(pen_pca$sdev)^2/sum((pen_pca$sdev)^2)

#VISUALIZE THE PCA

#scree plot - bar chart of importance for each pc
plot(pen_pca)#by default plots sdev sqaured, not proportion of variance (similar but not usually what is published)

pca_scree = data.frame(pc=c(1:4),
                       var=summary(pen_pca)$importance[2,])
ggplot(data=pca_scree, aes(x=pc, y=var))+
  geom_col()+
  geom_point()+
  geom_line()+
  xlab("Principal Component")+
  ylab("Proportion of Variance")+
  theme_bw()+
  ggtitle("Scree Plot")

#biplot - plot one pc against another
head(pen_pca$x)
pen_pca_meta = cbind(pen_pca$x, pen_meta)#rejoin igen(sp?) vectors with metadata
head(pen_pca_meta)

ggplot(data=pen_pca_meta)+
  geom_point(aes(x=PC1, y=PC2, color=species, shape=sex))+
  coord_fixed(ratio=1) #avoid visually biasing one axis over another
#distinct clusters emerge for each species, with diagonal pattern within each species by sex

library(devtools) #use to access other people's packages on github
install_github("vqv/ggbiplot") #install ggbiplot stored at github.com/vqv/ggbiplot

library(ggbiplot)

biplot(pen_pca) #plots points along with rotation vectors for each variable - ugly but useful
ggbiplot(pen_pca, scale=1, groups=pen_meta$species, ellipse=TRUE, alpha=0)+ #looks much nicer, and customizable using ggplot tools
  geom_point(aes(color=pen_meta$species, shape=pen_meta$sex))+ #erase points and add back in to manipulate both color and shape
  xlim(-2.5,3.5)
#useful for drawing conclusions - pc2 reflects almost exclusively differences in bill size while pc1 reflects bill and body size

#can also look at pc3 and pc4
ggbiplot(pen_pca, scale=1, group=pen_meta$species, ellipse=T, choices=c(3,4))
