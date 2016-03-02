#load ggplot2, dplyr packages and diamonds dataset
library(ggplot2)
library(dplyr)
data("diamonds")

#create scatterplot of price vs. x
#as x increases, price quickly increases
ggplot(aes(x = x, y = price), data = diamonds) +
  geom_point()

#find the correlation between price and x, y, and z respectively
cor.test(diamonds$x, diamonds$price, method = "pearson" )
cor.test(diamonds$y, diamonds$price, method = "pearson" )
cor.test(diamonds$z, diamonds$price, method = "pearson" )

#create a scatterplot of price vs. depth
ggplot(aes(x = depth, y = price), data = diamonds) +
  geom_point()

#adjust price vs. depth scatterplot to make points
#more transparent and mark x-axis every 2 units
ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha = 0.01) + 
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 2))

#find correlation of depth vs. price
#the correlation coefficient is -.01, 
#which reflects a poor correlation
cor.test(diamonds$depth, diamonds$price, method = "pearson" )

#create scatterplot of price vs. depth, omiting top 1% of values 
ggplot(aes(x = carat, y = price), data = diamonds) +
  geom_point() +
  xlim(0, quantile(diamonds$carat, 0.99))+
  ylim(0, quantile(diamonds$price, 0.99))

#create new variable in dataset, volume
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
#create scatterplot of price vs. volume
#as volume increases, price increases exponentially
ggplot(aes(x=(x*y*z), y=price), data = diamonds) + 
  geom_point() + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))

#find correlation of price and volume, excluding diamonds 
#with volume of 0 or greater than or equal to 800
with(subset(diamonds, volume > 0 & volume < 800), cor.test(volume, price))

#based on the above subset, create a scatterplot of 
#volume vs. price, making points more transparent and 
#applying a linear model (which is not the best model,
#since an exponential model appears more appropriate)
ggplot(aes(x= volume, y=price), 
       data= subset(diamonds, volume > 0 & volume < 800)) + 
  geom_point(alpha = 1/10) + 
  geom_smooth(method = 'lm', color = 'red')

#create a new data frame containing information
#on diamonds by clarity
clarity_groups <- group_by(diamonds, clarity)
diamondsByClarity <- summarise(clarity_groups, 
                               mean_price = mean(price), 
                               median_price = median(as.numeric(price)), 
                               min_price = min(price),
                               max_price = max(price),
                               n = n())

#create summary data frames with 
#mean price by clarity and color
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))


#load gridExtra, create bar charts of mean price by clarity & color,
#save them to their own variables to be able to arrange side by side
#using grid.arrange
library(gridExtra)
p1 <- ggplot(data = diamonds_mp_by_clarity, aes(clarity)) +
  geom_bar(aes(weight = mean_price))
p2 <- ggplot(data = diamonds_mp_by_color, aes(color)) +
  geom_bar(aes(weight = mean_price))
grid.arrange(p1, p2, ncol=2)