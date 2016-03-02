#load ggplot2 and diamonds dataset
library(ggplot2)
data("diamonds")


#Create a histogram of diamonds prices, facet it by diamond color
#and use cut to color the histogram
ggplot(diamonds, aes(price)) +
  facet_wrap(~color) +
  geom_histogram(aes(color = cut))


#Create a scatterplot of diamond price vs. table and color
#the points by cut of the diamond
ggplot(data = diamonds, aes(x = table, y = price)) +
  geom_point(aes(color = cut))


#Create volume variable and scatterplot of diamonds price vs. 
#volume (x*y*z) and color the points by the clarity of diamonds.  
#Use scale on y-axis to take the log10 of price.  
#Omit the top 1% of diamond volumes.
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(aes(x = volume, y = price), data = diamonds) +
  geom_point(aes(color = clarity)) +
  xlim(0, quantile(diamonds$volume, .99))+
  scale_y_log10()


#Load pseudo-facebook dataset
dataFolder <- <your file path here>
pf <- read.csv(paste0(dataFolder, "pseudo_facebook.tsv"), sep = '\t')


#In pseudo-facebook dataset, create a new variable called prop_initiated
#which contains the proportion of friendships the user initiated
pf$prop_initiated <- pf$friendships_initiated/pf$friend_count


#Create new variable year_joined from the variable tenure
pf$year_joined <- floor(2014 - pf$tenure/365)

#Create new variable year_joined.bucket to create 4 cohorts of facebook users
pf$year_joined.bucket <- cut(pf$year_joined, c(2004, 2009, 2011, 2012, 2014))

#Create a line graph of the median proportion of friendships_initiated vs.
#tenure and color the line segment by year_joined.bucket
ggplot(aes(x=tenure, y=prop_initiated), data = pf) + 
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = median) +
  geom_smooth()

#Create a scatterplot of the price/carat ratio of diamonds.  Variable x is
#assigned to cut, the points are colored by diamond color and the plot is 
#faceted by clarity.
ggplot(aes(x = cut, y = price/carat), data = diamonds) + 
  geom_point(aes(color = color)) +
  facet_wrap(~clarity)