##############  PACKAGES  #############

library(tidyverse)
library(dplyr)
library(grid)
library(gridExtra)
library(caTools)
library(e1071)
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)


###########  DATA EXPLORATION  ##########

skin<- read.csv("c:/data/Skin_Nonskin.csv")
View(skin)
str(skin)     #displays internal structure
summary(skin) #displays descriptive statistics of every variable in the dataset
head(skin)    #displays top 6 values

###########  DATA VISUALIZATION  ##########

boxplot(skin[,1:3])    #represent descriptive statistics of each variable

#Histogram representation
# Blue component
B <- ggplot(data=skin, aes(x=B))+
  geom_histogram(binwidth=5, color="blue", aes(fill=as.factor(skin_color))) + 
  xlab("Blue Pixel") +  
  ylab("Frequency") + 
  theme(legend.position="none")+
  ggtitle("Histogram of Blue")+
  geom_vline(data=skin, aes(xintercept = mean(B)),linetype="dashed",color="black")

# Green
G <- ggplot(data=skin, aes(x=G))+
  geom_histogram(binwidth=1, color="green", aes(fill=skin_color)) + 
  xlab("Green pixel") +  
  ylab("Frequency") + 
  theme(legend.position="none")+
  ggtitle("Histogram of Green")+
  geom_vline(data=skin, aes(xintercept = mean(G)),linetype="dashed",color="black")

# Red
R <- ggplot(data=skin, aes(x=R))+
  geom_histogram(binwidth=1, color="red", aes(fill=skin_color)) + 
  xlab("Red Pixel") +  
  ylab("Frequency") + 
  theme(legend.position="none")+
  ggtitle("Histogram of Red")+
  geom_vline(data=skin, aes(xintercept = mean(R)),linetype="dashed",color="black")


# Plot all visualizations
grid.arrange(B + ggtitle(""),
             G + ggtitle(""),
             R + ggtitle("Histogram of Red"),
             nrow = 3,
             top = textGrob("Skin Non-skin Frequency Histogram", 
                            gp=gpar(fontsize=15))
)

############# Splitting the rows ##########

sample_data = sample.split(skin, SplitRatio = 0.75)
train <- subset(skin, sample_data == TRUE)
test <- subset(skin, sample_data == FALSE)

#############  NAIVE BAYES     ############

# set seed function get same result each time
set.seed(120)  
skin_bayes <- naiveBayes(skin_color ~ . , data = train)
skin_bayes

# Predicting on test data'
pred <- predict(skin_bayes, newdata = test)

# Confusion Matrix
skin_matrix <- table(test$skin_color, pred)
skin_matrix

# Model Evaluation
confusionMatrix(skin_matrix)


#############  DECISION TREE   ############

skin_decision <- rpart(formula = skin_color ~.,
                       data = train,
                       method = "class",
                       control = rpart.control(cp = 0),
                       parms = list(split = "information"))
print(skin_decision)

# Plot
rpart.plot(skin_decision,type= 4 , extra=1)

# Predicting on test data
skin_pred <- predict(object = skin_decision,
                     newdata = test,
                     type = "class")
# Model Evaluation
skin_dec_matrix <- table(test$skin_color, skin_pred)

# Confusion Matrix
confusionMatrix(skin_dec_matrix)


