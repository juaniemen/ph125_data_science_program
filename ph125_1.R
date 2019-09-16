update.packages("dslabs")
library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)
head(stars)
mean(stars$magnitude)
sd(stars$magnitude)
ggplot(stars, aes(magnitude)) + geom_density()
ggplot(stars, aes(temp)) + geom_density()
ggplot(stars, aes(temp, magnitude)) + geom_point() + geom_smooth(level=0.5) + 
  scale_y_reverse() + scale_x_log10() + scale_x_reverse() +
  geom_label(aes(label=star), data=filter(stars, star=="Sun"))

ggplot(stars, aes(temp, magnitude)) + geom_point(aes(color=type)) + geom_smooth(level=0.5) + 
  scale_y_reverse() + scale_x_log10() + scale_x_reverse() 

ggplot(stars, aes(temp, magnitude)) + geom_point(aes(color=type)) + geom_smooth(level=0.5) + 
  scale_y_reverse() + scale_x_log10() + scale_x_reverse() +
  geom_text(data=filter(stars, type=="G"),aes(label=star))


