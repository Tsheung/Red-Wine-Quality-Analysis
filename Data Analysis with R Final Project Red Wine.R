getwd()
setwd('C:/Users/Toby/Downloads')
wine <- read.csv2("redwine.csv", header = TRUE, sep = ",")

str(wine)
wine$fixed.acidity <- as.numeric(wine$fixed.acidity)
wine$volatile.acidity <- as.numeric(wine$volatile.acidity)
wine$citric.acid <- as.numeric(wine$citric.acid)
wine$residual.sugar <- as.numeric(wine$residual.sugar)
wine$chlorides <- as.numeric(wine$chlorides)
wine$free.sulfur.dioxide <- as.numeric(wine$free.sulfur.dioxide)
wine$total.sulfur.dioxide <- as.numeric(wine$total.sulfur.dioxide)
wine$density <- as.numeric(wine$density)
wine$pH <- as.numeric(wine$pH)
wine$sulphates <- as.numeric(wine$sulphates)
wine$alcohol <- as.numeric(wine$alcohol)

str(wine)
summary(wine)
library(ggplot2)

#Univariate of Each Variable
ggplot(aes(x = fixed.acidity), data = wine) +
  geom_histogram(binwidth = 5, color = "green", fill = "black") +
  ggtitle("Fixed Acidity of Red Wine")


ggplot(aes(x = volatile.acidity), data = wine) +
  geom_histogram(binwidth = 5, color = "green",  fill = "black") +
  ggtitle("Volatile Acidity of Red Wine")

ggplot(aes(x = citric.acid), data = wine) +
  geom_histogram(binwidth = 5, color = "green",  fill = "black") +
  ggtitle("Citric Acid of Red Wine")

ggplot(aes(x = residual.sugar), data = wine) +
  geom_histogram(binwidth = 5, color = "green",  fill = "black") +
  ggtitle("Residual Sugar of Red Wine")

ggplot(aes(x = chlorides), data = wine) +
  geom_histogram(binwidth = 5, color = "green",  fill = "black") +
  ggtitle("Chlorides of Red Wine")

ggplot(aes(x = free.sulfur.dioxide), data = wine) +
  geom_histogram(binwidth = 5, color = "green",  fill = "black") +
  ggtitle("Free Sulfur Dioxide of Red Wine")

ggplot(aes(x = total.sulfur.dioxide), data = wine) +
  geom_histogram(binwidth = 5, color = "green",  fill = "black") +
  ggtitle("Total Sulfur Dioxide of Red Wine")

ggplot(aes(x = density), data = wine) +
  geom_histogram(binwidth = 5, color = "green",  fill = "black") +
  ggtitle("Density of Red Wine")

ggplot(aes(x = pH), data = wine) +
  geom_histogram(binwidth = 5, color = "green",  fill = "black") +
  ggtitle("pH of Red Wine")

ggplot(aes(x = alcohol), data = wine) +
  geom_histogram(binwidth = 5, color = "green",  fill = "black") +
  ggtitle("Alcohol of Red Wine")

ggplot(aes(x = quality), data = wine) +
  geom_histogram(binwidth = 1, color = "green",
                 fill = 'black') +
  ggtitle("Quality of Red Wine")


#Bivariate Analysis:Correlation Matrix
library(ggcorrplot)
corr <- cor(wine)
head(round(corr,2))
ggcorrplot(corr,method = 'square')

#Examine the Plot Without Repeats

ggcorrplot(corr,hc.order = TRUE, type = "lower", 
           outline.col = 'white')

#Correlation on Corrplots
ggcorrplot(corr, hc.order = TRUE, type  = "lower",
           lab = TRUE)


#Volatile Acidity vs Quality 
library(ggplot2)
ggplot(aes(x = quality, y = volatile.acidity), data = wine) + geom_point(alpha = 0.30, position = 'jitter') +
  coord_cartesian(xlim = c(0,10))

boxplot(volatile.acidity~quality, data = wine,
main = "Volatile Acidity and Quality of Red Wine",
xlab = "Quality of Red Wine",
ylab = "Volatile Acidity")

with(wine, by(volatile.acidity,quality,summary))


#Alcohol and Quality 
ggplot(aes(x = quality, y = alcohol),data = wine) +
  geom_point(alpha = 0.50, position = 'jitter') +
  coord_cartesian(xlim = c(0,10))

cor.test(wine$quality, wine$alcohol, method = 'pearson')


boxplot(alcohol~quality, data = wine,
        main = "Alcohol and Quality of Red Wine",
        xlab = "Quality of Red Wine",
        ylab = "Alcohol")

with(wine, by(alcohol, quality, summary))


#Quality and Citric Acid
ggplot(aes(x = quality, y = citric.acid), data = wine) +
  geom_point(alpha = 0.5, position = 'jitter') +
  coord_cartesian(xlim = c(0,10))


boxplot(citric.acid~quality, data = wine,
        main = "Citric Acid and Quality of Red Wine",
        xlab = "Quality of Red Wine",
        ylab = "Citric Acid")

with(wine, by(citric.acid,quality, summary))



#Quality and Sulphates

ggplot(aes(x = quality, y = sulphates), data = wine) +
  geom_point(alpha = 0.5, position = 'jitter') +
  coord_cartesian(xlim = c(0,10))

boxplot(sulphates~quality, data = wine, 
        main = "Sulphates and Quality of Red Wine",
        xlab = "Quality of Red Wine", ylab = "Sulphates") 


with(wine, by(sulphates, quality, summary))


#Multivariate Volatile Acidity, Alcohol, Quality

ggplot(aes(x = volatile.acidity, y = alcohol, color = quality),
data = wine) + geom_point() +
scale_color_gradientn(colors = rainbow(5)) 


#Citric Acid, Sulphates, Quality 
ggplot(aes(x = sulphates, y = citric.acid, color = quality),
data = wine) + geom_point() +
scale_color_gradientn(colors = rainbow(5))






