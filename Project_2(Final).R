rm(list=ls())
library(rpart)
library(MASS)
library(ggplot2)
library("scales")
library("psych")
library("gplots")
library(corrgram)
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees',"dplyr","plyr","reshape","data.table")
getwd()
#devtools::install_github("hadley/dplyr")
.libPaths()
library(githubinstall)
#githubinstall("dplyr")
#install.packages(x)
#rm(x)



#Extracting Data
marketing_train= read.csv("C:\\Users\\Deepanshu\\Desktop\\Edwisor\\Projects\\Project 2\\Absenteeism_at_work_Project.csv", header = T)


#Missing Values Analysis
missing_val = data.frame(apply(marketing_train,2,function(x){sum(is.na(x))}))
sum(missing_val)

new_DF <- marketing_train[rowSums(is.na(marketing_train)) > 0,]

#Changing variable types
cnames <- c('Distance.from.Residence.to.Work', 'Service.time', 'Age', 'Work.load.Average.day', 'Transportation.expense',
          'Hit.target', 'Son', 'Pet', 'Weight', 'Height','Absenteeism.time.in.hours','Body.mass.index')                          
cat_names <- c('Social.smoker',
           'Month.of.absence',
           'Social.drinker',
           'Reason.for.absence',
           'Disciplinary.failure',
           'ID',
           'Education',
           'Seasons',
           'Day.of.the.week')
#Checking the summary
summary(marketing_train[,cnames])
lapply(marketing_train[,cnames], function(feat) length(unique(feat)))
str(marketing_train)


for(i in cat_names){                  
  marketing_train[,i] = as.factor(marketing_train[,i])      
  } 
str(marketing_train)
library(DMwR)
marketing_train=marketing_train[!is.na(marketing_train$Absenteeism.time.in.hours), ]

#Imputing all the missing values using MICE
library(mice)
miceMod <- mice(marketing_train, method="rf", seed=1)  # perform mice imputation, based on random forests.
miceOutput <- complete(miceMod)  # generate the completed data.
anyNA(miceOutput)

write.csv(miceOutput, "C:\\Users\\Deepanshu\\Desktop\\Edwisor\\Projects\\Project 2\\mice_output_pro_2.csv", row.names = F)
miceOutput=read.csv ("C:\\Users\\Deepanshu\\Desktop\\Edwisor\\Projects\\Project 2\\mice_output_pro_2.csv")

da=miceOutput

#######Data exploration and visualizing data########

library(ggthemes)
library(grid)
library(gridExtra)
  p <- ggplot(da, aes(x = Pet, fill = Pet)) + geom_bar() 
s <- ggplot(da, aes(x = Son, fill = Son)) + geom_bar()

SS <- ggplot(da, aes(x =  Social.smoker, fill =  Social.drinker)) + geom_bar() 

S <- ggplot(da, aes(x =   Seasons,fill = Seasons)) + geom_bar()

grid.arrange(p,s, nrow = 1)
grid.arrange(SS,S, nrow = 1)

library(dplyr)

absent <- as.data.frame( da %>% select(everything()) %>% filter(Absenteeism.time.in.hours > 0))
Reason <-  as.data.frame(absent %>% group_by(Reason.for.absence) %>% summarise(count= n(), percent = round(count*100/nrow(absent),1))%>% arrange(desc(count)))
ggplot(Reason,aes(x = reorder(Reason.for.absence,percent), y= percent, pos=3, xpd=NA, fill= Reason.for.absence)) + geom_bar(stat = 'identity') + coord_flip() + theme(legend.position='none') +  
  geom_text(aes(label = percent), vjust = 0.5, hjust = 1.1) + xlab('Reason for absence')


a <- ggplot(da, aes(x = Age, fill = Son)) + geom_bar()            
grid.arrange(a, nrow = 1)

for (i in cnames)
{
  print(i)
  value=da[,i][da[,i] %in% boxplot.stats(da[,i],coef=1.5)$out]
  print(value)
}


for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i])), data = subset(marketing_train))+ 
           stat_boxplot(geom = "errorbar", width = 0.1) +
           geom_boxplot(outlier.colour="red", fill = "white" ,outlier.shape=18,
                        outlier.size=2, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i]))
}

# Plotting plots together
gridExtra::grid.arrange(gn1,gn5,gn2,gn6,gn3,gn7,gn8,gn9,gn10,gn11,ncol=3, nrow=4)

marketing_train=miceOutput

#Replacing outliers with NA using boxplot 
for(i in cnames){
     val = marketing_train[,i][marketing_train[,i] %in% boxplot.stats(marketing_train[,i])$out]
     print(length(val))
     marketing_train[,i][marketing_train[,i] %in% val] = NA
   }

#Imputing outliers using MICE method   
miceMod_2 <- mice(marketing_train, method="rf", seed=1)  # perform mice imputation, based on random forests.
miceOutput_2 <- complete(miceMod_2)  # generate the completed data.
anyNA(miceOutput_2)

write.csv(miceOutput, "C:\\Users\\Deepanshu\\Desktop\\Edwisor\\Projects\\Project 2\\mice_output_2_pro_2.csv", row.names = F)

miceOutput_2=read.csv("C:\\Users\\Deepanshu\\Desktop\\Edwisor\\Projects\\Project 2\\mice_output_2_pro_2.csv")
marketing_train=miceOutput_2


#Feature Selection####


# Correlation Plot 

corrgram(marketing_train[,cnames], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


#ANOVA Analysis
anova_multi_way <- aov(Absenteeism.time.in.hours~(Social.smoker)+
                                                  (Month.of.absence)+
                                                  (Social.drinker)+
                                                  (Reason.for.absence)+
                                                  (Disciplinary.failure)+
                                                  (ID)+
                                                  (Education)+
                                                  (Seasons)+
                                                  (Day.of.the.week), data = marketing_train)
summary(anova_multi_way)

## Dimension Reduction
marketing_train = subset(marketing_train, 
                         select = -c(Body.mass.index,ID))

#####Feature Scaling####
cnames <- c('Distance.from.Residence.to.Work', 'Service.time', 'Age', 'Work.load.Average.day', 'Transportation.expense',
            'Hit.target', 'Son', 'Pet', 'Weight', 'Height')
cat_names <- c('Social.smoker',
               'Month.of.absence',
               'Social.drinker',
               'Reason.for.absence',
               'Disciplinary.failure',
               'Education',
               'Seasons',
               'Day.of.the.week')
for (i in cat_names){

  marketing_train[,i] = as.factor(marketing_train[,i]) 
}

#Scaling all numeric variables leaving our target variable untouched
for(i in cnames){
  print(i)
  marketing_train[,i] = as.numeric(marketing_train[,i]) 
  marketing_train[,i] = (marketing_train[,i] - min(marketing_train[,i]))/
    (max(marketing_train[,i] - min(marketing_train[,i])))
}
str(marketing_train)

#Removing the rows containing absurd information
marketing_train= marketing_train[!marketing_train$Month.of.absence==0 & !marketing_train$Reason.for.absence==0, ]

#Creating dummies for categorical variables
DFdummies <- as.data.frame(model.matrix(~. -1, marketing_train))
dim(DFdummies)

library(DataCombine)
rmExcept(c("marketing_train","DFdummies","miceoutput_2")) 

wdf=marketing_train
marketing_train=DFdummies

#Creating train and test data
library(caret)
set.seed(1)
train.index = createDataPartition(marketing_train$Absenteeism.time.in.hours, p = .80, list = FALSE)
X_train = marketing_train[ train.index,]
y_train  = marketing_train[-train.index,]
y_test=y_train$Absenteeism.time.in.hours
X_test=X_train$Absenteeism.time.in.hours

library(usdm)


#Creating a function to run all types of regression and comparing the values
Show.RMSE <- function(method, train_data, test_data){
  regressor_fit <- caret::train(Absenteeism.time.in.hours~., data = X_train, method = method)
  
  y_pred <- predict(regressor_fit, y_train)
  print("RMSE value of test data")
  print(caret::RMSE(y_test, y_pred)) 
}
require(gbm)
library (ridge)
library(enet)
library(elasticnet)
library(h20)
regressors=c('lm','knn','svmLinear3', 'rpart2','rf','xgbTree','ridge','lasso','gbm_h2o')

#Running all the regressions and checking the performance on test data
for(i in regressors){
  print(i)
Show.RMSE(i, X_train, y_test) 
print(strrep('-',50))
}

#Finding the optimum parameters for XG Boost tree using repeatedCV
control <- trainControl(method="repeatedcv", number=5, repeats=2)
reg_XG <- caret::train(Absenteeism.time.in.hours~., data = X_train, method = "xgbTree",trControl = control)
reg_XG$bestTune
