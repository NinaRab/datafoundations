library(ggplot2)
data(diamonds)
summary(diamonds)
?diamonds
levels(diamonds$cut)
levels(diamonds$color)
levels(diamonds$clarity)
qplot(x=price, data = diamonds)
summary(diamonds$price)
sum(diamonds$price<500)
sum(diamonds$price<250)
sum(diamonds$price>=15000)

qplot(x=price, data = diamonds, binwidth = 50) +
  scale_x_continuous(limits = c(250, 19000), breaks = seq(0, 19000, 1000)) +
  scale_y_continuous(limits = c(0, 1500))

qplot(x=price, data = diamonds) +
  facet_wrap(~cut)

by(diamonds$price, diamonds$cut, max)
by(diamonds$price, diamonds$cut, min)
by(diamonds$price, diamonds$cut, summary)

qplot(x=price, data = diamonds) +
  facet_wrap(~cut, scales = "free_y")

summary(diamonds$price/diamonds$carat)
summary(log10(diamonds$price/diamonds$carat))

qplot(x=price/carat, data = diamonds, bins=500) +
  facet_wrap(~cut) +
  scale_x_log10() + 
  ylim(c(0,150))

qplot(x=color, y= price, data = diamonds, geom = 'boxplot') 
ggsave("priceboxplot.png")
by(diamonds$price, diamonds$color, summary)


qplot(x=color, y= price/carat, data = diamonds, geom = 'boxplot') 
ggsave("pricecaratboxplot.png")

qplot(x=carat, data = diamonds, binwidth = .005, geom = 'freqpoly')+
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, .1))

