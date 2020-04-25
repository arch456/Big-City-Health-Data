# Load the dataset and run summary()
health.data = read.csv("Big_Cities_Health_Data_Inventory.csv")
summary (health.data)

# ii)	Data Pre-processing
# Extract the data for High School Students
require(dplyr)
drink.st <- health.data %>%
  filter(Indicator == "Percent of High School Students Who Binge Drank")

# Extract the data for Adults
drink.ad <- health.data %>%
  filter(Indicator == "Percent of Adults Who Binge Drank")

# Remove unwanted variables
drink.st <- drink.st[c(3:7)]
drink.ad <- drink.ad[c(3:7)]

#library(MASS)
require(tree)

set.seed(1) 
train = sample(1:nrow(drink.st), nrow(drink.st)/2)
tree.drink.st=tree(Value~.,drink.st,subset=train)
summary(tree.drink.st)

plot(tree.drink.st)
text(tree.drink.st,pretty=0, cex = 0.6)
tree.drink.st

cv.drink.st=cv.tree(tree.drink.st)
plot(cv.drink.st$size,cv.drink.st$dev,type='b') 

# Pruning
prune.drink.st=prune.tree(tree.drink.st,best=5) 
plot(prune.drink.st) 
text(prune.drink.st,pretty=0) 

#Making predictions
yhat=predict(tree.drink.st,newdata=drink.st[-train,]) 
drink.st.test = drink.st[-train,"Value"] 
plot(yhat,drink.st.test)
abline(0,1)

#MSE
mean((yhat-drink.st.test)^2) 

# Bagging
require(randomForest)
set.seed(1) 
bag.drink.st=randomForest(Value~.,data=drink.st,subset=train,mtry=4,ntree = 500, importance=TRUE) 
bag.drink.st

yhat.bag = predict(bag.drink.st,newdata=drink.st[-train,]) 
plot(yhat.bag, drink.st.test) 
abline(0,1) 

mean((yhat.bag-drink.st.test)^2) 

# Random Forest
set.seed(1) 
rf.drink.st=randomForest(Value~.,data=drink.st,subset=train,mtry=4,importance=TRUE ) 
yhat.rf = predict(rf.drink.st,newdata=drink.st[-train,]) 
mean((yhat.rf-drink.st.test)^2) 

# Same as Bagging because we use the same number of variables

importance(rf.drink.st)
varImpPlot(rf.drink.st) 

# This shows that Race..Ethnicity and Place are the most important variables

# Boosting
require(gbm)

set.seed(1)
boost.drink.st=gbm(Value~.,data=drink.st[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)

summary(boost.drink.st)
boost.drink.st

plot(boost.drink.st)
#Place and Race..Ethnicity are the most important variables as seen above. We can also produce partial dependence plots for these two variables. The plots below show marginal effect of selected variables on the response. 

par(mfrow=c(1,2)) 
plot(boost.drink.st, x = drink.st$Race..Ethnicity, ylim = (0:15),type = "b")
plot(boost.drink.st,i="Place")
plot(boost.drink.st,i="Race..Ethnicity")
text(boost.drink.st, pretty = 0)


yhat.boost=predict(boost.drink.st,newdata=drink.st[-train,],n.trees=5000) 
mean((yhat.boost-drink.st.test)^2) 

boost.drink.st=gbm(Value~.,data=drink.st[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F) 
yhat.boost=predict(boost.drink.st,newdata=drink.st[-train,],n.trees=5000) 
mean((yhat.boost-drink.st.test)^2) 

# Best mean is obtained with Random Forest

#install.packages("ggplot2")
#library(ggplot2)

#ggplot(boost.drink.st, aes(Place, y)) +
#  geom_point() +
 # coord_flip()
---
  title: "Tree_Analysis"
author: "Archana Chittoor"
output: word_document
---
  
  The second type of analysis we perform on our data is Decision Tree analysis. 
We have chosen the Regression tree as our method of data analysis because our response Value is numerical (quantitative). 

The Regression tree has advantages over other Regression models. It is easier to interpret and has a good graphical representation. Our intention is to investigate the relationship between the response Value and the predictors Gender, Ethnicity, Value and Place by using Decision Trees. We will also implement Bagging, Boosting and Random Forests, selecting the best method that produces the minimum Mean Squared Error (MSE). In addition, we will compare and analyse the importance of each of our predictor variables in relationship to the Value indicator.


```{r}
# Load the dataset and run summary()

health.data = read.csv("Big_Cities_Health_Data_Inventory.csv")
summary (health.data)
```

### Extracting and Refining High School Students' data

We first extract the Smoking and Drinking data for high school students by applying the filters with Indicator as "Percent of High School Students Who Binge Drank" and "Percent of High School Students Who Currently Smoke" respectively. This gives us two datasets drink.st and smoke.st. Once the data is extracted, we will retain only the variables of interest and eliminate the others, for further analysis. Additionally, we also rename the column Race..Ethnicity to Ethnicity because it is not recommended to have special symbols in column names. Also, we would need to  remove rows with cumulative data, such as "All" in Ethnicity and "U.S. Total" in Place as these do not help us in the interpretion of results.

```{r}
require(dplyr)

# Drinking data for High School Students
drink.st <- health.data %>%
  filter(Indicator == "Percent of High School Students Who Binge Drank")

# Smoking data for Students
smoke.st <- health.data %>%
  filter(Indicator == "Percent of High School Students Who Currently Smoke")

# Remove unwanted variables and rename some columns

drink.st <- drink.st[c(3:7)]
drink.st$Ethnicity <- drink.st$Race..Ethnicity
drink.st <- drink.st[-c(3)]

smoke.st <- smoke.st[c(3:7)]
smoke.st$Ethnicity <- smoke.st$Race..Ethnicity
smoke.st <- smoke.st[-c(3)]


```


### Extracting and Refining Adults' data  

```{r}

# Drinking data for Adults
drink.ad <- health.data %>%
  filter(Indicator == "Percent of Adults Who Binge Drank")

# Smoking data for Adults
smoke.ad <- health.data %>%
  filter(Indicator == "Percent of Adults Who Currently Smoke")

# Remove unwanted variables and rename some columns
drink.ad <- drink.ad[c(3:7)]
drink.ad$Ethnicity <- drink.ad$Race..Ethnicity
drink.ad <- drink.ad[-c(3)]

smoke.ad <- smoke.ad[c(3:7)]
smoke.ad$Ethnicity <- smoke.ad$Race..Ethnicity
smoke.ad <- smoke.ad[-c(3)]

# Remove missing data which has been found only in smoke.ad dataset
smoke.ad <- smoke.ad %>%
  filter(Value != "NA")
```

We will be performing our entire analysis on these four datasets which have been generated based on our Questons of Interest.

### a) Response (variable of interest) with the type and units:

For all four data sets that we work on, **Value** is the response or variable of interest. It is a Numerical (or quantitative) variable. It has no units but rather a numerical indicator about a particular health condition that we are interested in. 

### b) Explanatory/grouping variable(s) with the type and units:

The explanatory variables for the four datasets (drink.st, smoke.st, drink.ad and smoke.ad) are:
  * Year (type: Date)
* Gender (Factor with 3 levels - Male, Female, Both)
* Ethnicity (Factor with 9 levels such as Native American, Asian/PI and so on )
* Place (Factor with 29 levels)

We will not use the other columns like Indicator. Category, Indicator, BCBH.Requested.Methodology, Source , Methods and Notes in the data as they do not add any value and we obtain no new relationships or dependencies when these are taken into account.

### c) How Regression Trees apply to our analysis:

We use Regression trees to investigate the relationships in our data as per the below questions of interest:
  
  * i) What are the major factors causing smoking and drinking problems among High School students in the most urban cities of the United States? How much are these conditions influenced by the place, ethnicity, and gender of the students? 
  
  * ii) Similarly, how much effect do predictors like place, gender and ethnicity have on smoking and drinking problems among adults in US’s biggest cities?
  
  To address the above questions, we need to find relationships/ dependencies between our response **Value** and the predictors given by **Place, Gender, Year and Ethnicity**. As the response is numerical, it makes sense to use Regression trees to generate trees that examine our data. We will implement Bagging, Random Forest and Boosting to reduce the Mean Squared Error. Furthermore, we can determine which of the variables are the most important, and list them down according to their significance. 

**Limitations of using Regression Trees on our data:**
  We are limited by the number of variables which is four. Due to this, Random Forest would be same as Bagging because we need to use all four variables in both, and anything less than that will not give us relevant outputs. 

However, Regression trees prove useful for analyzing the data when the response is numerical as mentioned above. They provide an easy interpretation and a good graphical representation as well.


Describe (shortly) the methods you choose to apply and how they are related to your problem
statement. Discuss possible limitations related to the data or selected methods.

d) State hypotheses in words and using appropriate notation (if appropriate).
e) List appropriate data conditions (if applicable). Test any (if possible).


## Analysis

### Basic Regression Trees

We first create basic Regression trees for each of our four datasets **drink.st, smoke.st, drink.ad and smoke.ad**.

#### i) Drinking data for Students

```{r}
require(tree)

set.seed(1) 

## Create the Training dataset

train = sample(1:nrow(drink.st), nrow(drink.st)/2)
tree.drink.st=tree(Value~.,drink.st,subset=train)
summary(tree.drink.st)

```

```{r}
plot(tree.drink.st)
text(tree.drink.st,pretty=0, cex = 0.6)
tree.drink.st
```

We will prune the tree now.  

##### Pruning:

```{r}
cv.drink.st=cv.tree(tree.drink.st)
plot(cv.drink.st$size,cv.drink.st$dev,type='b') 

```

```{r}
prune.drink.st=prune.tree(tree.drink.st,best=5) 
plot(prune.drink.st) 
text(prune.drink.st,pretty=0, cex = 0.6) 
prune.drink.st
```

**Interpretation:**
  
  The tree after pruning to 5 terminal nodes seems to be easier to interpret and has a better graphical representation. The best predictor seems to be Place because it is used for the initial split, where the Place is Baltimore, Los Angeles, San Diego on one side and all other cities (25 cities) are on the other side. Ethnicity is the next best predictor used and the tree is split depending on it being Asian/PI, Black, Hispanic, Multiracial on one side and all other ethnicities on the other side. The tree() function has used only Place and Ethnicity for building the Regression tree. In addition, we can see that the predictions have higher values on the left sub-tree as compared to the other side, which is as expected.


##### Making Predictions on test data:

```{r}
yhat=predict(tree.drink.st,newdata=drink.st[-train,]) 
drink.st.test = drink.st[-train,"Value"] 
plot(yhat,drink.st.test)
abline(0,1)
```

##### Mean Squared Error

```{r}

mean((yhat-drink.st.test)^2) 

```

The error rate is quite high and we need to implement Bagging, Random Forest or Boosting to reduce the error and see if we can obtain a better fit.


##### Bagging
```{r}

require(randomForest)
set.seed(1) 
bag.drink.st=randomForest(Value~.,data=drink.st,subset=train,mtry=4,ntree = 500, importance=TRUE) 
bag.drink.st

yhat.bag = predict(bag.drink.st,newdata=drink.st[-train,]) 
plot(yhat.bag, drink.st.test) 
abline(0,1) 

mean((yhat.bag-drink.st.test)^2) 

```

Bagging reduces the error rate significantly and it is computed as 14.59.

##### Random Forest:

```{r}
set.seed(1) 
rf.drink.st=randomForest(Value~.,data=drink.st,subset=train,mtry=4,importance=TRUE ) 
yhat.rf = predict(rf.drink.st,newdata=drink.st[-train,]) 
mean((yhat.rf-drink.st.test)^2) 

```

Random Forest gives the same MSE as Bagging because both are equivalent in this case ( due to same value of mtry).


##### Boosting

```{r}
require(gbm)
set.seed(1)
boost.drink.st=gbm(Value~.,data=drink.st[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)

summary(boost.drink.st)
```


Place and Ethnicity are the most important variables as seen above. We can also produce partial dependence plots for these two variables. The plots below show marginal effect of selected variables on the response. 

```{r}
par(mfrow=c(1,2)) 
plot(boost.drink.st,i="Place", type = "l") 
plot(boost.drink.st,i="Ethnicity", type = "l")

yhat.boost=predict(boost.drink.st,newdata=drink.st[-train,],n.trees=5000) 
mean((yhat.boost-drink.st.test)^2) 

boost.drink.st=gbm(Value~.,data=drink.st[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F) 
yhat.boost=predict(boost.drink.st,newdata=drink.st[-train,],n.trees=5000) 
mean((yhat.boost-drink.st.test)^2) 

```

The MSE when we perform Boosting is more than that of Bagging, 23.07 when we use default Shrinkage Parameter and 40.94 when the Shrinkage Parameter is increased to 0.2 .

Therefore, we choose Regression Tree with Bagging as the best model as it generates the least MSE.

##### Importance of Variables:

```{r}
importance(bag.drink.st)
varImpPlot(bag.drink.st) 
```

**Conclusion:**
  As seen above the most important predictor is **Place** and the next best predictor is **Ethnicity**.  
On average, when we examine the plots generated after Boosting, we find that the cities **Miami, Florida and San Antonio, TX** have the highest problem of Binge Drinking among High School students. Cities such as **Los Angeles, CA and	San Diego County, CA** have the least indicator values leading to the inference that these cities seem to have least binge drinking problems among students.  
When it comes to ethnicities, we find that White community has the highest drinking rate among students and Black, Asian/PI have the lowest rates.


#### ii) Smoking data for Students   

```{r}
set.seed(1) 

## Create the Training dataset

train = sample(1:nrow(smoke.st), nrow(smoke.st)/2)
tree.smoke.st=tree(Value~.,smoke.st,subset=train)
summary(tree.smoke.st)

```

```{r}
plot(tree.smoke.st)
text(tree.smoke.st,pretty=0, cex = 0.6)
tree.smoke.st

```


We will perform Pruning on the tree now.  

##### Pruning:

```{r}
cv.smoke.st=cv.tree(tree.smoke.st)
plot(cv.smoke.st$size,cv.smoke.st$dev,type='b') 

```

```{r}
prune.smoke.st=prune.tree(tree.smoke.st,best=7) 
plot(prune.smoke.st) 
text(prune.smoke.st,pretty=0, cex = 0.6) 
prune.smoke.st
```

**Interpretation:**
  
  The tree after pruning to 7 terminal nodes seems to be easier to interpret and has a better graphical reperesentation. The best predictor seems to be Ethnicity in this case because it is used for the initial split, where the Place is Asian/PI, lack on one side on one side and all other ethnicities are on the other side. Place seems to be the next best predictor used and the tree is split with Miami, New York, San Francisco on the higher side and all others on the other lower side. The Year is also used to split the data on the left sub-tree. 


##### Making Predictions on test data:

```{r}
yhat=predict(tree.smoke.st,newdata=smoke.st[-train,]) 
smoke.st.test = smoke.st[-train,"Value"] 
plot(yhat,smoke.st.test)
abline(0,1)
```

##### Mean Squared Error

```{r}

mean((yhat-smoke.st.test)^2) 

```

The error rate is not bad and we can implement Bagging, Random Forest or Boosting to reduce the error and see if we can obtain a better fit.


##### Bagging
```{r}

require(randomForest)
set.seed(1) 
bag.smoke.st=randomForest(Value~.,data=smoke.st,subset=train,mtry=4,ntree = 500, importance=TRUE) 
bag.smoke.st

yhat.bag = predict(bag.smoke.st,newdata=smoke.st[-train,]) 
plot(yhat.bag, smoke.st.test) 
abline(0,1) 

mean((yhat.bag-smoke.st.test)^2) 

```

Bagging reduces the error rate significantly and it is computed as 12.81.

##### Random Forest:

```{r}
set.seed(1) 
rf.smoke.st=randomForest(Value~.,data=smoke.st,subset=train,mtry=4,importance=TRUE ) 
yhat.rf = predict(rf.smoke.st,newdata=smoke.st[-train,]) 
mean((yhat.rf-smoke.st.test)^2) 

```

Random Forest gives the same MSE as Bagging because both are equivalent in this case ( due to same value of mtry).

##### Boosting

```{r}
require(gbm)
set.seed(1)
boost.smoke.st=gbm(Value~.,data=smoke.st[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)

summary(boost.smoke.st)
```


Place and Ethnicity are the most important variables as seen above. We can also produce partial dependence plots for these two variables. The plots below show marginal effect of selected variables on the response. 

```{r}
par(mfrow=c(1,2)) 
plot(boost.smoke.st,i="Place", type = "l") 
plot(boost.smoke.st,i="Ethnicity", type = "l")

yhat.boost=predict(boost.smoke.st,newdata=smoke.st[-train,],n.trees=5000) 
mean((yhat.boost-smoke.st.test)^2) 

boost.smoke.st=gbm(Value~.,data=smoke.st[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F) 
yhat.boost=predict(boost.smoke.st,newdata=smoke.st[-train,],n.trees=5000) 
mean((yhat.boost-smoke.st.test)^2) 

```

The MSE when we perform Boosting is more than that of Bagging, 16.09 when we use default Shrinkage Parameter and 34.99 when the Shrinkage Parameter is increased to 0.2 .

Therefore, we choose Regression Tree with Bagging as the best model as it generates the least MSE.

##### Importance of Variables:

```{r}
importance(bag.smoke.st)
varImpPlot(bag.smoke.st) 
```

As seen above the most important predictor is **Place** and the next best predictor is **Ethnicity**. 

Also, as seen in the plots generated after Boosting, we find that **Miami and Seattle** have higher smoking rates among high school students whereas the rates are lowest in **Detroit**. Similarly, when it comes to ethnicities, smoking rates are highest in Multiracial section of society and lowest in Black, Asian/PI communities.


#### iii) Drinking data for Adults

```{r}
require(tree)

set.seed(1) 

## Create the Training dataset

train = sample(1:nrow(drink.ad), nrow(drink.ad)/2)
tree.drink.ad=tree(Value~.,drink.ad,subset=train)
summary(tree.drink.ad)

```


```{r}
plot(tree.drink.ad)
text(tree.drink.ad,pretty=0, cex = 0.6)
tree.drink.ad
```


We will prune the tree now.  

##### Pruning:


```{r}
cv.drink.ad=cv.tree(tree.drink.ad)
plot(cv.drink.ad$size,cv.drink.ad$dev,type='b') 

```

```{r}
prune.drink.ad=prune.tree(tree.drink.ad,best=8) 
plot(prune.drink.ad) 
text(prune.drink.ad,pretty=0, cex = 0.6) 
prune.drink.ad
```

**Interpretation:**
  
  The tree after pruning to 8 terminal nodes seems to be easier to interpret and has a better graphical reperesentation. The best predictor seems to be Place followed by ethnicity where Asian/PI, Black are on the lower side. Year also seems to be important as the year 2013 seems to have higher rates of drinking issues among adults.


##### Making Predictions on test data:

```{r}
yhat=predict(tree.drink.ad,newdata=drink.ad[-train,]) 
drink.ad.test = drink.ad[-train,"Value"] 
plot(yhat,drink.ad.test)
abline(0,1)
```

##### Mean Squared Error

```{r}

mean((yhat-drink.ad.test)^2) 

```

The error rate is quite high and we need to implement Bagging, Random Forest or Boosting to reduce the error and see if we can obtain a better fit.


##### Bagging
```{r}

require(randomForest)
set.seed(1) 
bag.drink.ad=randomForest(Value~.,data=drink.ad,subset=train,mtry=4,ntree = 500, importance=TRUE) 
bag.drink.ad

yhat.bag = predict(bag.drink.ad,newdata=drink.ad[-train,]) 
plot(yhat.bag, drink.ad.test) 
abline(0,1) 

mean((yhat.bag-drink.ad.test)^2) 

```

Bagging reduces the error rate significantly and it is computed as 67.55 which is still high.

##### Random Forest:

```{r}
set.seed(1) 
rf.drink.ad=randomForest(Value~.,data=drink.ad,subset=train,mtry=4,importance=TRUE ) 
yhat.rf = predict(rf.drink.ad,newdata=drink.ad[-train,]) 
mean((yhat.rf-drink.ad.test)^2) 

```

Random Forest gives the same MSE as Bagging because both are equivalent in this case ( due to same value of mtry).


##### Boosting

```{r}
require(gbm)
set.seed(1)
boost.drink.ad=gbm(Value~.,data=drink.ad[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)

summary(boost.drink.ad)
```


Again, we see that **Place and Ethnicity** are the most important variables as seen above. We can also produce partial dependence plots for these two variables. The plots below show marginal effect of selected variables on the response. 

```{r}
par(mfrow=c(1,2)) 
plot(boost.drink.ad,i="Place") 
plot(boost.drink.ad,i="Ethnicity")

yhat.boost=predict(boost.drink.ad,newdata=drink.ad[-train,],n.trees=5000) 
mean((yhat.boost-drink.ad.test)^2) 

boost.drink.ad=gbm(Value~.,data=drink.ad[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F) 
yhat.boost=predict(boost.drink.ad,newdata=drink.ad[-train,],n.trees=5000) 
mean((yhat.boost-drink.ad.test)^2) 

```

There is no improvement in MSE when we perform Boosting. It is more than that of Bagging, 79.02 when we use default Shrinkage Parameter and 83.89 when the Shrinkage Parameter is increased to 0.2 .

Therefore, we choose Regression Tree with Bagging as the best model as it generates the least MSE.

##### Importance of Variables:

```{r}
importance(bag.drink.ad)
varImpPlot(bag.drink.ad) 
```

Again, it is observed that the most important predictor is **Place** and the next best predictor is **Ethnicity**.


#### iv) Smoking data for Adults

```{r}
require(tree)

set.seed(1) 

## Create the Training dataset

train = sample(1:nrow(smoke.ad), nrow(smoke.ad)/2)
tree.smoke.ad=tree(Value~.,smoke.ad,subset=train)
summary(tree.smoke.ad)

```

```{r}
plot(tree.smoke.ad)
text(tree.smoke.ad,pretty=0, cex = 0.6)
tree.smoke.ad
```


We will perform Pruning on the tree now.  

##### Pruning:

```{r}
cv.smoke.ad=cv.tree(tree.smoke.ad)
plot(cv.smoke.ad$size,cv.smoke.ad$dev,type='b') 

```

```{r}
prune.smoke.ad=prune.tree(tree.smoke.ad,best=7) 
plot(prune.smoke.ad) 
text(prune.smoke.ad,pretty=0, cex = 0.6) 
prune.smoke.ad
```

**Interpretation:**
  
  The tree after pruning to 5 terminal nodes seems to be easier to interpret and has a better graphical reperesentation. The best predictor seems to be Place because it is used for the initial split, where the Place is Baltimore, Los Angeles, San Diego on one side and all other cities (25 cities) are on the other side. Ethnicity is the next best predictor used and the tree is split depending on it being Asian/PI, Black, Hispanic, Multiracial on one side and all other ethnicities on the other side. The tree() function has used only Place and Ethnicity for building the Regression tree. In addition, we can see that the predictions have higher values on the left sub-tree as compared to the other side, which is as expected.


##### Making Predictions on test data:

```{r}
yhat=predict(tree.smoke.ad,newdata=smoke.ad[-train,]) 
smoke.ad.test = smoke.ad[-train,"Value"] 
plot(yhat,smoke.ad.test)
abline(0,1)
```

##### Mean Squared Error

```{r}

mean((yhat-smoke.ad.test)^2) 

```

The error rate is quite high and we need to implement Bagging, Random Forest or Boosting to reduce the error and see if we can obtain a better fit.


##### Bagging
```{r}

require(randomForest)
set.seed(1) 
bag.smoke.ad=randomForest(Value~.,data=smoke.ad,subset=train,mtry=4,ntree = 500, importance=TRUE) 
bag.smoke.ad

yhat.bag = predict(bag.smoke.ad,newdata=smoke.ad[-train,]) 
plot(yhat.bag, smoke.ad.test) 
abline(0,1) 

mean((yhat.bag-smoke.ad.test)^2) 

```

Bagging reduces the error rate significantly and it is computed as 14.59.

##### Random Forest:

```{r}
set.seed(1) 
rf.smoke.ad=randomForest(Value~.,data=smoke.ad,subset=train,mtry=4,importance=TRUE ) 
yhat.rf = predict(rf.smoke.ad,newdata=smoke.ad[-train,]) 
mean((yhat.rf-smoke.ad.test)^2) 

```

Random Forest gives the same MSE as Bagging because both are equivalent in this case ( due to same value of mtry).


##### Boosting

```{r}
require(gbm)
set.seed(1)
boost.smoke.ad=gbm(Value~.,data=smoke.ad[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)

summary(boost.smoke.ad)
```


Place and Ethnicity are the most important variables as seen above. We can also produce partial dependence plots for these two variables. The plots below show marginal effect of selected variables on the response. 

```{r}
par(mfrow=c(1,2)) 
plot(boost.smoke.ad,i="Place", type = "l") 
plot(boost.smoke.ad,i="Ethnicity", type = "l")

yhat.boost=predict(boost.smoke.ad,newdata=smoke.ad[-train,],n.trees=5000) 
mean((yhat.boost-smoke.ad.test)^2) 

boost.smoke.ad=gbm(Value~.,data=smoke.ad[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F) 
yhat.boost=predict(boost.smoke.ad,newdata=smoke.ad[-train,],n.trees=5000) 
mean((yhat.boost-smoke.ad.test)^2) 

```

The MSE when we perform Boosting is more than that of Bagging, 23.07 when we use default Shrinkage Parameter and 40.94 when the Shrinkage Parameter is increased to 0.2 .

Therefore, we choose Regression Tree with Bagging as the best model as it generates the least MSE.

##### Importance of Variables:

```{r}
importance(bag.smoke.ad)
varImpPlot(bag.smoke.ad) 
```

As seen above the most important predictor is Place and the next best predictor is Ethnicity.

