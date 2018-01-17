head(hour)
# we want to predict the bike share data using regression 
bikes <- hour

# create a new dataset with dummy variables
bikes_dum <- bikes

bikes_dum$spring <- bikes_dum$season == 1
bikes_dum$summer <- bikes_dum$season == 2
bikes_dum$winter <- bikes_dum$season == 3

# run a lm model based solely on these dummy variables

model1 <- lm(cnt ~ spring+summer+winter, data=bikes_dum)
summary(model1)

# intercept = fall, the most amount of bikes!

# now build some r square change model to examine assumptions

model2 <- lm(bikes$cnt ~ bikes$temp+bikes$hum+bikes$windspeed)
summary(model2)


head(model2$residuals)
hist(model2$residuals)
scatter.smooth(1:length(model2$residuals),
               model2$residuals)
abline(h=0)
smoothScatter(1:length(model2$residuals),
              model2$residuals)
plot(model2)

# try do a pca to see the components 
head(hour)
x <- cbind(hour$dteday, hour$season, hour$yr, hour$mnth, hour$hr, hour$holiday, hour$weekday, hour$workingday,
           hour$weekday, hour$workingday, hour$weathersit, hour$temp, hour$atemp, hour$hum, hour$windspeed,
           hour$casual, hour$registered)
pca1 <- princomp(x, cor=TRUE)
summary(pca1)
loadings(pca1)
plot(pca1)
screeplot(pca1, type="line", main="scree plot" )
biplot(pca1)

# now do a factor analysis 
# fa1 <- factanal(x, 5, rotation="varimax")
myfa <- factanal(x, 5, scores="regression")
?factanal

# now try the decision tree
install.packages("rpart")
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")
?rpart
tree1 <- rpart(cnt ~ weekday+workingday+season+yr+mnth+hr+holiday+weathersit+temp+atemp+hum+windspeed, data=hour, method="anova")
tree1
plot2 <- rpart.plot(tree1, type=2, digits=3)
plot2

# now try random forest
install.packages("randomForest")
library(randomForest)

?randomForest
rf1 <- randomForest(x, ntree=500)
summary(rf1)

?rpart.plot

tree2 <- rpart(cnt ~., data=hour, method="anova")
tree2
plot1 <- rpart.plot(tree2, type=3, digits=3, fallen.leaves = TRUE)
plot1

# playing around with linear regression
lm2 <- lm(cnt ~ casual+registered, data=hour)
summary(lm2)

lm3 <- lm(hour$cnt ~ hour$dteday+hour$season+hour$yr+hour$mnth+hour$hr+hour$holiday+hour$weekday+hour$workingday+
            hour$weekday+hour$workingday+hour$weathersit+hour$temp+hour$atemp+hour$hum+hour$windspeed)
summary(lm3)
