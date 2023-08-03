install.packages("summarytools")
install.packages("ggplot2")
install.packages("GGally")
install.packages("dplyr")
# Libraries used for the program
  library(ggplot2)
  library(ggcorrplot)
  library(GGally) 
  library(summarytools)
library(dplyr)

# Loading the data
setwd("C:/Users/dupad/Documents/MTH781P Data analytics")
getwd()
housing = read.csv("housing.csv", header=TRUE)

# Checking missing values, total number of values & Summary of the loaded data
head(housing)
table(is.na(housing))
housing_summary<-dfSummary(housing)
view(housing_summary)


# Setting CR01 and CHAS as factor since its Categorical:
housing$CR01 <- factor(housing$CR01,levels = c(0,1),
                       labels = c("LessThanCrMedian","MoreThanCrMedian"))
housing$CHAS <- factor(housing$CHAS,levels = c(0,1),
                       labels = c('FarfromRiver','ClosetoRiver'))


# Bar Graphs : MEDV vs CR01
ggplot(housing,aes(x=housing$CR01,y=housing$MEDV))+geom_col()+ ggtitle("Bar Graphs : MEDV vs CR01")

# Boxplot : MEDV vs LSTAT
ggplot(housing, aes(x = housing_num$MEDV, y = housing_num$LSTAT)) + geom_boxplot()+ggtitle("Boxplot : MEDV vs LSTAT")



# Filtering the numeric values to new dataset
housing_num <-Filter(is.numeric,housing)
# Viewing the new dataset
View(housing_num)

# Correlation matrix of all the numeric variables
ggpairs(housing_num)


# ScatterPlot : DIS vs ZN
cor(housing_num$DIS,housing_num$ZN)
ggplot(housing_num, aes(x=housing_num$RM, y=housing_num$LSTAT)) + geom_point()+ggtitle("ScatterPlot : DIS vs ZN")


# ScatterPlot : LSTAT vs RM
ggplot(housing_num, aes(x=housing_num$MEDV, y=housing_num$DIS)) + 
  geom_point(shape = 23) + ggtitle("ScatterPlot : LSTAT vs RM")+ 
  xlab("Number of rooms") + ylab("Percentage of lower status of the population")

# ScatterPlot : DIS vs NOX
ggplot(housing_num, aes(x=housing_num$DIS, y=housing_num$NOX)) + 
  geom_point(shape = 25) + ggtitle("ScatterPlot : DIS vs NOX")+ 
  xlab("Weighted distances to 5 Boston employment centers") + 
  ylab("Nitric oxide concentration(parts per 10 million)")

# Bar Graphs : MEDV vs RAD 
cor(housing$RM,housing$MEDV)
ggplot(housing,aes(x=housing$RAD,y=housing$MEDV))+geom_col()+ ggtitle("Bar Graphs : MEDV vs RAD")

ggplot(housing_num, aes(x=housing_num$RAD, y=housing_num$TAX)) + geom_point()+ggtitle("ScatterPlot : MEDV vs RM")


boxplot(LSTAT ~ MEDV_grp3,las = 2, data = housing_num)

housing_num$MEDV_grp3 = cut(housing_num$MEDV, breaks = seq(0, 51, 10))
ggplot(housing_num, aes(x=MEDV_grp3, y=LSTAT)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.5)+ 
  scale_y_continuous(name = "Percentage of lower status of the population") + 
  scale_x_discrete(name = "Range of Median value of homes") + 
  ggtitle("MEDV vs LSTAT")

ggplot(data = housing_num, aes(x = MEDV_grp3, y = LSTAT)) + 
  geom_boxplot(fill = "#4271AE",alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "Percentage of lower status of the population") + 
  scale_x_discrete(name = "Range of Median value of homes") + ggtitle("MEDV vs LSTAT") + 
  theme(axis.line = element_line(colour = "black", size = 0.5))

cor_func <- function(data, mapping, method){
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  corr <- cor(x, y, method=method, use='complete.obs')
  
  
  ggally_text(
    label = as.character(round(corr, 2)), 
    mapping = aes(),
    xP = 0.5, yP = 0.5,
    color = 'black'
  )
}


my_fn <- function(data, mapping, pts=list(), smt=list(), ...){
  ggplot(data = data, mapping = mapping, ...) + 
    do.call(geom_point, pts) +
    do.call(geom_smooth, smt) 
}

ggpairs(housing_num,upper = list(continuous = wrap(cor_func,method='spearman')))

#Need to treat for Rooms Since its Not Rounded!!!!
rmint <- round(housing$RM,0)
summary(rmint)
table(rmint)

#Need to treat for DIS Since its Not Rounded!!!!
disR <- round(housing_num$DIS,1)
summary(disR)
table(disR)

ggplot(housing_num, aes(x=rmint,y=MEDV)) +
  geom_histogram() + ggtitle("Plot MEDV vs RM")+
  xlab("Number of rooms") + ylab("Median value of houses")

summary(housing_num$DIS)

housing_num$DIS_grp = cut(housing_num$DIS, breaks = seq(1, 13, 1))

ZN_grp1 = housing_num %>%
  group_by(DIS_grp) %>%
  summarise(ZN_1 = sum(ZN))
ZN_grp1


ZN_grp1 %>%
  ggplot(aes(x=DIS_grp, y=ZN_1)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) + 
  xlab("Distance to employemnt center") + ylab("Zoned residential land") + 
  ggtitle("Bar Graphs : ZN vs DIS") + theme_bw()


MEDV_grp2 = housing_num %>%
  group_by(round(housing_num$RM,0)) %>%
  summarise(MEDV_house2 = sum(MEDV))
MEDV_grp2


MEDV_grp2 %>%
  ggplot(aes(x=rmint, y=MEDV_house2)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) + facet_grid(~RM,scales = "free_x", space = "free") +
  xlab("Number of rooms") + ylab("Median value of house") + 
  ggtitle("Bar Graphs : MEDV vs RAD") + theme_bw()


cor(housing_num$CRIM,housing$MEDV)
cor(housing)

cor(housing$PTRATIO,housing$LSTAT)

plot(housing)
pairs(housing)

# correlation plots
install.packages("ggcorrplot")
library(ggcorrplot)
# calulate the correlations
r <- cor(housing, use="complete.obs")
round(r,2)
# visualizing data correlation in color code 
ggcorrplot(r, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)
#Need to treat for Rooms Since its Not Rounded!!!!
rmint <- round(housing$RM,0)
summary(rmint)
table(rmint)
#Room Statistics will provide important insights when discussing descriptive statistics

set.seed(12345)
#Do data partitioning
house_part <- createDataPartition(y = housing$MEDV, p = 0.80, list = FALSE)
house_training <- housing[house_part,]
house_testing <- housing[-house_part,]


#Question 2
#Perform a Regression Model of Medv on all of the variables of the dataset
#Test Normality of Target Variable
hist(house_training$MEDV)
qqnorm(house_training$MEDV)
#MEDV seems approximately normal
####################################
lmodel1<-lm(MEDV~+RM+LSTAT, data=house_training)
summary(lmodel1)
plot(lmodel1)

lmodel2<-lm(MEDV~+RM+LSTAT+CRIM+DIS, data=house_training)
summary(lmodel2)
plot(lmodel2)

lmodel3<-lm(MEDV~+RM+LSTAT+PTRATIO+DIS, data=house_training)
summary(lmodel3)
plot(lmodel3,which=1)
plot(lmodel3,which=2)

lmodel4<-lm(MEDV~+RM+LSTAT+PTRATIO+DIS+CHAS, data=house_training)
summary(lmodel4)
plot(lmodel4,which=1)

lmodel5<-lm(MEDV~+RM+LSTAT+PTRATIO+DIS+CHAS+NOX, data=house_training)
summary(lmodel5)
plot(lmodel5,which=1)

lmodel6<-lm(MEDV~+RM+LSTAT+PTRATIO+DIS+CHAS+NOX+RAD, data=house_training)
summary(lmodel6)
plot(lmodel6,which=1)

#####################################

linearModel1 <- lm(MEDV~.,data=housing)
summary(linearModel1)
plot(linearModel1,which = 2)

#Remove Variables with high p-values i.e. INDUS and AGE
linearModel2 <- lm(MEDV~.-INDUS-AGE,data=housing)
summary(linearModel2)
plot(linearModel2,which = 2)

#Remove Variables with very Low Coefficient Estimates i.e. ZN and TAX
LinearModel3 <- lm(MEDV~.-INDUS-AGE-ZN-TAX,data=housing)
summary(LinearModel3)
plot(LinearModel3,which = 2)

#After examining the correlation between the variable it makes sense to add
#an interaction variable between NOX and DIS
LinearModel4 <- lm(MEDV~.-INDUS-AGE-ZN-TAX+NOX*DIS,data=housing)
summary(LinearModel4)
plot(LinearModel4,which = 2)


#95% Confidence Intervals for Coefficients:
confint(linearModel2)
linearModel2$coefficients
#Plot Residuals of Linear Model
plot(x=linearModel2$fitted.values, y=linearModel2$residuals)
abline(h=0)
hist(linearModel2$residuals)
qqnorm(linearModel2$residuals)
qqline(linearModel2$residuals)
#We observe that RM has a particularly low p-value. We create a Polynomial Model
PolyModel1 <- lm(MEDV~.-INDUS-AGE+I(RM^2),data=housing)
summary(PolyModel1)
PolyModel2 <- lm(MEDV~.-INDUS-CR01-AGE+I(RM^2),data=housing)
summary(PolyModel2)
PolyModel3 <- lm(MEDV~.-INDUS-ZN+I(DIS^2)-AGE+I(RM^2),data=housing)
summary(PolyModel3)
plot(PolyModel3)
PolyModel4 <- lm(MEDV~.-INDUS-ZN+I(DIS^2)-AGE+I(RM^2)+I(LSTAT^2),data=housing)
summary(PolyModel4)
PolyModel5 <- lm(log(MEDV)~.-INDUS-ZN+I(DIS^2)-AGE+I(RM^2)+I(LSTAT^2),data=housing)
summary(PolyModel5)
#95% Confidence Intervals for Coefficients:
confint(PolyModel3)
PolyModel3$coefficients
#Plot Residuals of Polynomial Model
plot(x=PolyModel3$fitted.values, y=PolyModel3$residuals)
abline(h=0)
hist(PolyModel3$residuals)
qqnorm(PolyModel3$residuals)
qqline(PolyModel3$residuals)
#New Polynomial Residuals
plot(x=PolyModel4$fitted.values, y=PolyModel4$residuals)
abline(h=0)
hist(PolyModel4$residuals)
qqnorm(PolyModel4$residuals)
qqline(PolyModel4$residuals)
#Cross-Valuation of Residuals
# now calculate cross-validated residuals
# for entire dataset
cv_res1 = vector(length=n)
for(i in 1:n){
  fiti = lm(MEDV~.-INDUS-ZN+I(DIS^2)-AGE+I(RM^2)+I(LSTAT^2),data=housing[-i,])
  predi = predict(fiti, newdata=housing[i,])
  cv_res1[i] = housing$MEDV[i] - predi
}
# PRESS is sum of squared cross-validated residuals
PRESS1 = sum(cv_res1^2)
PRESS1
#other Model
cv_res2 = vector(length=n)
for(i in 1:n){
  fiti1 = lm(MEDV ~ DIS + RM + LSTAT, data=housing[-i,])
  predi1 = predict(fiti1,newdata=housing[i,])
  cv_res2[i] = housing$MEDV[i] - predi1
}
PRESS2 = sum(cv_res2^2)
PRESS2


