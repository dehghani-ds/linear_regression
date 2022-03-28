setwd("---/w12")
library("ggplot2")    #Create Elegant Data Visualizations 
library("moments")    #Moments, skewness, kurtosis and related tests
library("car")        #Required for Calculations Related to Regression
library("corrplot")  
data = read.csv("---/CS_03.csv")
dim(data)
str(data)
names(data)
summary(data)
# cast data to proper dataType
col_should_changet_2_factor = c("FuelType","MetColor","Automatic","Doors")
data[col_should_changet_2_factor] = lapply(data[col_should_changet_2_factor], as.factor)

# getting scence of countinuos features
par(mfrow=c(2,3))
par(mar=c(1,1,3,3))
for (i in c(1,2,3,5,8,10)) {
  hist(data[,i],xlab = "",main=paste("Histogram of ",
                                     names(data[i])))
}
dev.off()
boxplot(data$Price)

#---- overview of continuous data ----

#because we are running linear regression correlation analysis not bad
#first have some assumption : more km lead to car be cheaper
cor(data$Price,data$KM) #=-0.51 not bad 
# we can have correlation table and see if there is any relation between 
# features two by two
cor_table = round(cor(data[,c(1,2,3,5,8,10)]),2)
cor_table
# of course the first row which is price that is out response variable
# is the most important row of this table but we can take look at table
# and find if there is some relation between features because it is lead
# to multicollinearity(can't fulfill VIF test)

#if we have numerous features this plot can help us 
corrplot(cor_table)

# keep in mind that correlation analysis show linear relation
# what if there is nonlinear relation. here is when scatter plot
# came for the saviour
dev.off()
par(mfrow=c(2,3))
for (i in c(2,3,5,8,10)) {
  plot(data[,i],data[,1],main = paste("features: ",names(data[i])))
}

#----overview of categorical data----
# in each group of categorical data we should have at least 30 observation
# because our modeling should see variety of group to learn well
# best function is table 
table(data$FuelType) # lack of data in CNG group
table(data$MetColor)
table(data$Automatic)
table(data$Doors) # problem lack of data in 2 doors car
# in this case we cant do any thing about our data and cant over sample 
# on group that we suffer from lack of data but at least we know our 
# model weakness we can get to this conclusion that this model may have
# problem or miss predict on CNG car's

# t-test and ANOVA exam to find out is there any relationship 
#between this categorical features and response variable

# two category = t-test
t.test(data$Price~data$MetColor)# p-value = 0.0013
dev.off()
boxplot(data$Price~data$MetColor)#??????

t.test(data$Price~data$Automatic)#p-value = 0.40
boxplot(data$Price~data$Automatic)#??????

#----dividing data to test and train ----

dim(data)
View(data)
#choose 70% to 30% for train and test division
set.seed(1234)
index_train = sample(nrow(data),0.7*nrow(data))
train_data = data[index_train,]
test_data =data[-index_train,]

# use summary function after dividing data to train and test to see
# if you divide proper or not for example mean shouldn't have much
# difference in these group or other statistic indexes
summary(train_data)
summary(test_data)

#---- model1 ----

m1=lm(data=train_data,formula = train_data$Price~train_data$KM)
summary(m1)

# in model summary at first we should look at F test which is called 
# F-statistic as we know f test show us if there is any linear relation
# between any features with response variable or not (h0 hypothesis is
# there is none linear relation between any features and response) 
# here p-value is less than 0.05 so h0 rejected and in conclusion 
# we may have relation with one of our features but because we only 
# use one features to build linear model so the feature (KM) has
# relation and we don't need t test

# next we should take look at R squared this parameter show us how
# well our model fit to data. in this model R^2 is not high because
# first we have only one feature and second we are in science category
# for example when we are in engineering desired r^2  in larger than
# 0.7~08 but in other category 0.2~0.3 maybe enough its totally depend
# on field of study (should take look at literature) in this case 0.8
# on the other hand when we use lot of features to predict response 
# R-squared may increase but not show accurate value so we can use
# R-squared adjusted instead

# next take look at coefficient data which show t test result and
# beta coefficients that we use in equation


#----check if our regression model follow linear regression assumptions----

# first: our errors should follow normal distribution and mean =0 and
# sd is constant

m1$residuals
hist(m1$residuals,probability = T)
lines(density(m1$residuals),col="red")

#QQ plot\
qqnorm(m1$residuals,main = "QQ norm plot price~KM")
qqline(m1$residuals,col="red")

#normality tests
anscombe.test(m1$residuals)
jarque.test(m1$residuals)

# because both p-value<0.05 so normality assumption rejected :(

# see diagnostic plot
 plot(m1)
 #first plot show us hetroscedasticity
 #second plot show us qq-plot
 #third plot show us same info as first one
 #forth plot show us influencer data (leverage)
 
 # we can use cook's distance with followed method
 cooks.distance(m1)
 # cook's distance should be under 1
 sum(cooks.distance(m1)>1)
 # its 0 so we have not outlier data
 
 #in conclusion we our errors doesn't follow normal distribution
 # and we have hetroscedasticity problem

 # one of the reason that we have hetroscedasticity problem is that 
 # we may not have linear relation and its maybe nonlinear relation
 # so lets take look again at scatter plot price and KM
 plot(train_data$Price,train_data$KM)
 
 # as we can see there is one curve in the scatter plot so in conclusion
 # we may have nonlinear relation lets modeling in quadratic function
 
 #---- quadratic regression ----
 m2= lm(train_data$Price~train_data$KM+I(train_data$KM^2))

 #see t-test answer
 summary(m2) 
 
 hist(m2$residuals,probability = T)
 lines(density(m2$residuals),col="red")
 
 qqnorm(m2$residuals)
 qqline(m2$residuals,col="red")

 anscombe.test(m2$residuals)
 jarque.test(m2$residuals)
 
 #again normality of error distribution rejected
 
 plot(m2)
 
 #a little rally in hetroscedasticity problem
 
 #Linear vs Quadratic Regression
 ggplot(train_data, aes(KM, Price)) +
   geom_point() +
   geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
   geom_smooth(method = "lm", formula = y ~ x, color = "red") +
   ggtitle("Price vs KM")
 
 # we may have another assumptions which is in qqnorm plot and 
 # residuals vs fitted plot we can see three data that was shown
 # these data making trouble we may thinking about omitting these
 # data 
 ##note : its better to not remove data from modeling but we can 
 # do it if its necessary just one role: don't omit more than 1% data
 
 # lets do it:
 removed_list_from_train = rownames(train_data[c(605,901,142),])

 train_data_2 = train_data[-which(rownames(train_data)%in%removed_list_from_train),] 

 m2_2= lm(Price~KM+I(KM^2),data = train_data_2)
 summary(m2_2)

 #our model has been fitted better because r-squared raised
 
 hist(m2_2$residuals)
 qqnorm(m2_2$residuals) 
 qqline(m2_2$residuals,col='red') 
 
 jarque.test(m2_2$residuals)
 anscombe.test(m2_2$residuals)

 plot(m2_2) 
 
 #normality of errors still rejected but we cant remove more data
 #and we can see in qq-plot that our performance get better in this
 #new modeling. on the other hand we have only one features and we 
 #cant get better result.
 dev.off()

 #get coefficient of our modeling 
 coef(m2)
 coef(m2_2)

#check multicollinearity problem
 
 car::vif(m2)

 #greater than 10 so we have multicollinearity problem 
 #but this case is special because we make this decision 
 #that use km and km^2 in polynomial regression this problem
 #inevitable (we may have this problem ween we want to add
 # interaction effect X1*X2). in these cases we can improve
 # our model by scaling the features (scaling=(X-mean(X))/sd)
 
 train_data$km_scaled = scale(train_data$KM)
 
 m2_3= lm(Price~km_scaled+I(km_scaled^2),data = train_data)

 summary(m2_3)

 coef(m2_3) 
 
 car::vif(m2_3)

 #VIF<10 so t test result can be referenced
 
 
 #----Model 3: add other features----
 
 head(train_data)
 
 #we can easily add categorical features to regression if its factor data type 
 #r make dummy variable itself
 m3=lm(Price~km_scaled+I(km_scaled^2)+Age+FuelType+
          HP+MetColor+Automatic+CC+Doors+Weight,
       data = train_data) 
 summary(m3)
 # in summary regression r show n-1 coefficient for n group categorical 
 # variable (as we discussed in lecture)
 #R-squared = 0.7746 & Adjusted R-squared = 0.7714
 
 #we should use diagnostic plot a lot instead of using all before steps 
 # because there is a lot of steps and we want to model a lot
 # so relax and just use diagnostic plot 
  
 plot(m3)
 
 #now we know that there is some problem we have with some data and
 #some features need to be removed from our model
 #we should take this journey slowly and step by step specially in removing 
 #features because they effect each other so remove then one by one
 
 #in summary m3 we can find highest  p-value for t test
 #Doors features have 0.7 for p-value and we have no assumption about 
 #doors features effect on price so it is good candidate to remove from
 #our model
 
 #we can use ANNOVA test to be sure that there is no relation between car Doors
 #and price but we choose simple way and just check that is there difference 
 #between price mean in Doors category if there isn't we can easily remove 
 # this features
 
 tapply(train_data$Price,train_data$Doors,mean)
 boxplot(train_data$Price~train_data$Doors)
 
 # there is no such difference or pattern
 # final decision: remove Doors features
 
 # another  features that have high p-value is HP
 # it is continues and we can calculate correlation
 cor(train_data$Price,train_data$HP) #0.19 ->very low
 plot(train_data$HP,train_data$Price) # :(
 
 #another features that have high p-vlaue in t-test m3 is
 # MetColor 
 
 tapply(train_data$Price,train_data$MetColor,mean)
 boxplot(train_data$Price~train_data$MetColor)
 
 # the other feature that have high p-value is fuel Type
 # but one of its group is high the other one has low 
 # p-value in this case we should wait and be patient and don't
 # remove it suddenly
 
 #so we remove HP & MetColor & Doors features and of course we do not
 #remove marginally fitted feature which shown by . instead of * in
 #summary model
 
#----refactor model 3----
 m3 = lm(Price~km_scaled+I(km_scaled^2)+Age+
            FuelType+Automatic+CC+Weight,data = train_data)
summary(m3) 
#R-squared:  0.7739,	Adjusted R-squared:  0.772
 # there is no such dramatic change in R-squared and adjusted
 # R-squared. this is another evidence show us removed features
 # useless for predicting price value

plot(m3)
# not so much difference in diagnostic plot so we are going
# with this features

# we still have problem on fuel type one of the category p-value
# is high -> we know that the distribution of car based on fuel type
# is: CNG Diesel Petrol  one solution that may help us is combine two
#     17    132   1176
# CNG and diesel group to one group so the group change to petrol and 
# not petrol. this solution first help us in lack of CNG data sample 
# and may help us in p-value. lets do it

ifelse(data$FuelType=='Petrol',1,0)
train_data$IfPetrol = ifelse(train_data$FuelType=='Petrol',T,F)
table(train_data$IfPetrol) 

#----Model 4: new modeling with change in fuel type category---- 

m4 = lm(Price~km_scaled+I(km_scaled)+Age+
           IfPetrol+Automatic+CC+Weight,data = train_data)
summary(m4) 
 # wow better p-value on if petrol true but our intercept p-value 
 # raised a little. not pass the range but just get bigger. r squared 
 # and adjusted r squared not change a lot

#lets take look ta errors(residuals)

hist(m4$residuals,probability = T)
lines(density(m4$residuals),col="red")

qqnorm(m4$residuals) 
qline(m4$residuals,col='red') 
#we can see some data in tale of qq-norm plot that have a lot of
#distance from qq-lines these are trouble maker data

#lets take look at skewness ant kurtosis number
skewness(m4$residuals)
kurtosis(m4$residuals)

#test for normality
anscombe.test(m4$residuals)
jarque.test(m4$residuals)
#both test reject normality assumption because p-value<0.05

#lets take look at diagnostic plot and see which data making this trouble
plot(m4)

# as we can see there are some data in the middle that bring our residual
# vs fitted line down the most trouble makers one which has been showed
# is 82,83,491
# in qq-norm the most trouble makers one is 82 491 112
# and in cook's distance plot 112 has pass 1 border and we can 
# see 82 and 491 haven't reached border but shown in plot which
# is mean marginally pass
# in conclusion there is 4 data that make residual staying away from 
# normal distribution so with 1% rule of thumb that say we can omit
# trouble maker data we consider removing these data: 82,83,112,493

train_data_3 = train_data[-which(rownames(train_data)%in%c(82,83,112,493)),]

m4_1 = lm(Price~km_scaled+I(km_scaled^2)+Age+
           IfPetrol+Automatic+CC+Weight,data = train_data_3)

summary(m4_1)
#R-squared raised 
plot(m4_1)
# still some other data making trouble in qq plot and residual vs fitted plot
# but cook's distance completely Ok 
#bad data 82,284,293
train_data_3 = train_data[-which(rownames(train_data)%in%c(82,83,112,493,284,293)),]
m4_2 = lm(Price~km_scaled+I(km_scaled^2)+Age+
             IfPetrol+Automatic+CC+Weight,data = train_data_3)

summary(m4_2)
#R-squared raised 
plot(m4_2)
#bad data 544,114,491
train_data_3 = train_data[-which(rownames(train_data)%in%
                                    c(82,83,112,491,284,293,544,948,114)),]
m4_3 = lm(Price~km_scaled+I(km_scaled^2)+Age+
             IfPetrol+Automatic+CC+Weight,data = train_data_3)

summary(m4_3)
#R-squared raised 
plot(m4_3)
#residual vs fitted is between -4000 and +4000 so Ok
#qq-plot is very well fitted
#and have no problem in cook's distance

#now we can reference to t-test very well and firm
#lest take look again at summary
summary(m4_3)
#two feature have not good p-value CC and Automatic
#lets take look at scatter plot cc on price
plot(train_data$CC,train_data$Price)
#as we can see we have no relationship between cc and price

#lets run another lm without these two features

#----Model without CC and Automatic----

m5 = lm(Price~km_scaled+I(km_scaled^2)+Age+
           IfPetrol+Weight,data = train_data_3)

summary(m5)

#check normality of residual

hist(m5$residuals,probability = T)

qqnorm(m5$residuals)
qqline(m5$residuals,col='red')

anscombe.test(m5$residuals)
jarque.test(m5$residuals)

#the residual are following normal distribution

plot(m5)

# check multicollinearity 
car::vif(m5)
#all under 10 so no problem

coef(m5)

#get confident interval

confint(m5)

#check how many data we remove

dim(train_data)
dim(train_data_3)
(927-918)/927*100
#its under 1% so Ok

#----TEST OUR FINAL MODEL----

#step 1: make all features that we have in train

test_data$km_scaled =  scale(test_data$KM)
test_data$IfPetrol = ifelse(test_data$FuelType=="Petrol",T,F)

head(test_data)

#now predict

test_data$pred = predict(m5,test_data)

#draw actual vs prediction

plot(test_data$Price,test_data$pred)
abline(0,1,col='red',lwd=3)

#use absolute error in final step (MAE=MeanAbsoluteError)

abs_err=abs(test_data$Price-test_data$pred)

#MAE = 792.28
mean(abs_err)
#Median = 606.14
median(abs_err)
#sd=747.39
sd(abs_err)
#max=8482.02
max(abs_err)
#min=3.47
min(abs_err)

hist(abs_err)
boxplot(abs_err)

#Error percentage median,sd,mean,max,min----

e_percen = abs(test_data$Price-test_data$pred)/test_data$Price*100
#MAPE=Mean absolute percentage error 
mean(e_percen)
#=8.33%
median(e_percen)
#=6.37%

max(e_percen)
#=90.33%
min(e_percen)
#0.042%

#marketing required 

sum(e_percen<15)/length(e_percen)*100
#=84.42%
#pass marketing requirement
#at the end take deep look at data that we remove and know where is our
#model weakness

train_data[which(rownames(train_data)%in%
                    c(82,83,112,491,284,293,544,948,114)),]

#and we can also see test data which have highest error and 
#find out is there any pattern in this comparison or not
#----End of Code----

