library("car")
library("caret")
library("gvlma")
library("predictmeans")

#Test Assumptions
scatter.smooth(x=heights$AM_Height, y=heights$PM_Height, 
  main="Morning by Evening Height")

#check to see if it's linear, and its clearly linear.

#Testing for Homoscedasticity
lmModHeights = lm(PM_Height~AM_Height, data=heights)
#use code to graph
par(mfrow=c(2,2))
plot(lmModHeights)

#look at top left box, The top left graph shows the fitted 
#values against the residuals, while the bottom left 
#shows the fitted values against the standardized residuals. 
#the red lines are  not linear. This raises a red flag. 
#You can declare this to be heteroscedastic 
#(the opposite of homoscedastic).

#Breusch-Pagan Test
lmtest::bptest(lmModHeights)

#p value is 0.6484,its greater the 0.05 makes this not significant and 
#there is homoscedasticity.
#we continue to double check work

#Non-Constant Variance Test
car::ncvTest(lmModHeights)

#p value is 0.6967, still not significant, this assumption
#is also meet

#Homogeneity of Variance
gvlma(lmModHeights)

#although this is not always accurate for the top half of info listed,
#its p value is always accurate. triple checking work. 
#p value is 0.6823 for Heteroscedasticity

#ALL ASSUMPTIONS ARE MET

#Screening for outliers in x space (Cooks D)
CookD(lmModHeights, group=NULL, plot=TRUE, idn=3, newwd=TRUE)

#3,4,12 are the outliners

#Leverage values
lev = hat(model.matrix(lmModHeights))
plot(lev)

heights[lev>.2,]

######Going by leverage values, only 3 is really an issue
###### I got a little lost here. i dont see what this means...

#Screening for outliers in y space
car::outlierTest(lmModHeights)

#This test was significant, so it's likely there is at 
#least one outlier

#Screening for outliers in x and y space (influential points)
summary(influence.measures(lmModHeights))

#3,11,37. We'll remove them and see what the data looks like
#with/without outliners
heightsNoO <- heights[c(1,2,5,6,7,8,9,10,13,14,15,16,17,18,19,20,
21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41),]
lmModHeightsNoO = lm(PM_Height~AM_Height, data=heightsNoO)

#Look at the model summaries for each
summary(lmModHeights)
summary(lmModHeightsNoO)

#morning height and evening height has a variance of 99%, makes sense 
#bc no one grows inches to feet over night
#has similiar data, without outliners, can keep it as is. 
