require(dplyr)

health.data = read.csv("Big_Cities_Health_Data_Inventory.csv")
summary(health.data)
attach(health.data)

lm.fit=lm(Value~Place,data=health.data) 
summary(lm.fit)

pairs(health.data)

health_atlanta <- health.data %>%
  filter(Place == "Atlanta (Fulton County), GA")

health_atlanta_hiv <- health_atlanta %>%
  filter(Indicator.Category == "HIV/AIDS")

lm.fit_at_hiv <- lm(Value~Year, data=health_atlanta_hiv)
summary(lm.fit_at_hiv)

pairs(health_atlanta_hiv)


dataset<-health.data %>%
  filter(Year == 2013)%>%
  filter(Gender=="Both")%>%
  filter(Place == "Atlanta (Fulton County), GA")

lm.fit=lm(Value~Race..Ethnicity,data=dataset) 
summary(lm.fit)

AIDS_mort_rate <- health.data %>%
  filter(Indicator == "HIV-Related Mortality Rate (Age-Adjusted; Per 100,000 people)")

pairs(AIDS_mort_rate)

lm.fit=lm(Value~Year,data=AIDS_mort_rate) 
summary(lm.fit)

lm.fit=lm(Value~Gender,data=AIDS_mort_rate) 
summary(lm.fit)

lm.fit=lm(Value~Race..Ethnicity,data=AIDS_mort_rate) 
summary(lm.fit)

lm.fit=lm(Value~Place,data=AIDS_mort_rate) 
summary(lm.fit)

library(MASS)
Boston

library(ISLR)
Auto

attach(AIDS_mort_rate)
#z <- factor(format(AIDS_mort_rate$Year,'%Y'))

#Year = as.factor(Year)
#AIDS_mort_rate
#AIDS_mort_rate$Year
#AIDS_mort_rate$Year = as.factor(AIDS_mort_rate$Year)
#AIDS_mort_rate$Year

#AIDS_mort_rate$Year = as.Date(AIDS_mort_rate$Year,)

AIDS_mort_rate$Year = as.Date(AIDS_mort_rate$Year, format = '%Y')

AIDS_mort_rate$Year = factor(format(AIDS_mort_rate$Year,'%Y'))
AIDS_mort_rate$Year = as.numeric(AIDS_mort_rate$Year)
lm.fit=lm(Value~Year,data=AIDS_mort_rate) 
summary(lm.fit)

AIDS_mort_rate$Place = as.numeric(AIDS_mort_rate$Place)
lm.fit=lm(Value~Place,data=AIDS_mort_rate) 
summary(lm.fit)

plot(lm.fit)

AIDS_mort_rate$Race..Ethnicity = as.numeric(AIDS_mort_rate$Race..Ethnicity)
lm.fit=lm(Value~Race..Ethnicity,data=AIDS_mort_rate) 
summary(lm.fit)

plot(lm.fit)

AIDS_mort_rate$Gender = as.numeric(AIDS_mort_rate$Gender)
lm.fit=lm(Value~Gender,data=AIDS_mort_rate) 
summary(lm.fit)

plot(lm.fit)

AIDS_mort_rate <- AIDS_mort_rate[c(1:7)]
AIDS_mort_rate <- AIDS_mort_rate[c(3:7)]
pairs(AIDS_mort_rate)

lm.fitall=lm(Value~.,data=AIDS_mort_rate) 
summary(lm.fitall)

by_ind <- health.data %>% 
  group_by(Indicator) %>% 
  summarize(count = n())
by_ind


by_place <- health.data %>% 
  group_by(Place) %>% 
  summarize(count = n())
by_place

# knn

dataset1 <- health.data%>%
  filter(Indicator == "All Types of Cancer Mortality Rate (Age-Adjusted; Per 100,000 people)")

ran <- sample(1:nrow(dataset1), 0.9 * nrow(dataset1))

nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

#ds1_norm <- as.data.frame(lapply(dataset1[,c(3,4,5,7)], nor))
dataset1 <- dataset1[c(3,4,5,6,7)]
ds1_train <- dataset1[ran,] 

##extract testing set
ds1_test <- dataset1[-ran,] 

target_category <- dataset1[ran,4]

test_category <- dataset1[-ran,4]

library(class)

pr <- knn(ds1_train,ds1_test,cl=target_category,k=11)

summary(dataset1$Value)
hist(dataset1$Value)

mean(dataset1$Value)
var(dataset1$Value)

#dataset1$Value <- as.factor(dataset1$Value)
#dataset1$Value <- as.numeric(dataset1$Value)

obese.st <- health.data %>%
  filter(Indicator == "Percent of High School Students Who Are Obese")

obese.st <- obese.st[c(3:7)]
obese.st$Year = as.Date(obese.st$Year, format = '%Y')

obese.st$Year = factor(format(obese.st$Year,'%Y'))
#obese.st$Year = as.Date(obese.st$Year)

obese.st$Year = as.numeric(obese.st$Year)
lm.fit=lm(Value~Year,data=obese.st) 
summary(lm.fit)

obese.st$Place = as.numeric(obese.st$Place)
lm.fit=lm(Value~Place,data=obese.st) 
summary(lm.fit)

plot(lm.fit)

obese.st$Race..Ethnicity = as.numeric(obese.st$Race..Ethnicity)
lm.fit=lm(Value~Race..Ethnicity,data=obese.st) 
summary(lm.fit)

plot(lm.fit)


obese.st$Gender = as.numeric(obese.st$Gender)
lm.fit=lm(Value~Gender,data=obese.st) 
summary(lm.fit)

plot(lm.fit)

pairs(obese.st)

lm.fitall=lm(Value~.,data=obese.st) 
summary(lm.fitall)

drink.st <- health.data %>%
  filter(Indicator == "Percent of High School Students Who Binge Drank")

drink.st <- drink.st[c(3:7)]
pairs(drink.st)
#pairs(drink.st, col = drink.st$Value)

lm.fitg=lm(Value~Gender,data=drink.st) 
summary(lm.fitg)

par(mfrow = c(2, 2))
plot(lm.fit)

summary(drink.st$Value)
hist(drink.st$Value)



lm.fitall=lm(Value~.,data=drink.st) 
summary(lm.fitall)

is.na(drink.st)
which(is.na(drink.st))
nrow(!is.na(drink.st))

na.omit(drink.st)

sum(is.na(drink.st[5]))

drink.ad <- health.data %>%
  filter(Indicator == "Percent of Adults Who Binge Drank")

is.na(drink.ad)
which(is.na(drink.ad))
nrow(!is.na(drink.ad))

na.omit(drink.ad)

sum(is.na(drink.ad[1:5]))
