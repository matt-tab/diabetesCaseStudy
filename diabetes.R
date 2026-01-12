library(caret)
diabetes = read.csv("diabetes.csv")
head(diabetes)
## Logistic regression model

diabetesSample = nrow(diabetes)
set.seed(3338)
train.index=sample(diabetesSample, 0.7*diabetesSample)
train=diabetes[train.index,]
test=diabetes[-train.index,]

lmDiabetes = glm(Outcome~., data=train, family = "binomial")
summary(lmDiabetes)

prob <- predict(lmDiabetes, test, type = "response")
pred_class <- ifelse(prob > 0.5, 1, 0)

roc_obj <- roc(test$Outcome, pred_class)
aucLM <- auc(roc_obj)

plot(roc_obj, col = "green", main = paste("ROC Curve for Diabetes Logistic Model (AUC =", round(aucLM, 3), ")"))
# Add AUC text to the plot

rocLM <- roc_obj
aucLM <- auc(roc_obj)
aucLM

# Confusion matrix
confusionMatrix(as.factor(pred_class), as.factor(test$Outcome))
AIC(lmDiabetes)
BIC(lmDiabetes)

barplot(
    height   = table(diabetes$Outcome),
    names.arg = c("No diabetes", "Diabetes"),
    main     = "Count of Outcomes",
    xlab     = "Outcome",
    ylab     = "Count"
 )
  

## Decision tree
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)


diabetesSample = nrow(diabetes)
set.seed(3338)
train.index=sample(diabetesSample, 0.7*diabetesSample)
train=diabetes[train.index,]
test=diabetes[-train.index,]

set.seed(100)
cv.ct=rpart(Outcome~., data=train, method="class")


### plot the tree
rpart.plot(cv.ct, extra=1, fallen.leaves=F)
pred.class=predict(cv.ct, test, type="class")
pred.prob=predict(cv.ct, test,type = "prob")[, 2]

confusionMatrix(as.factor(pred.class), as.factor(test$Outcome))

rocDT <- roc(test$Outcome, pred.prob)

aucDT <- auc(rocDT)
aucDT
plot(rocDT,
     col = "blue",
     lwd = 3,
     main = paste("Decision Tree ROC curve (AUC =", round(auc_value, 3), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

## KNN next

train$Outcome = as.factor(train$Outcome)

################################################
### Cross validation to choose k
trControl= trainControl(method="cv", number=5, allowParallel=TRUE)

model= train(Outcome~., data=train, method="knn", 
             preProcess=c("center", "scale"),
             tuneGrid=expand.grid(k=seq(1,19,1)), ##checks k by 1 from 1 - 19 
             trControl=trControl)

model
## 15 optimal
model= train(Outcome~., data=train, method="knn", 
             preProcess=c("center", "scale"),
             tuneGrid=expand.grid(k=15), 
             trControl=trControl)

model

pred.knn=predict(model, test)
prob.knn = predict(model, test, type = "prob")
prob.knn
colnames(prob.knn)
prob.pos <-  prob.knn[["1"]]
test$Outcome

confusionMatrix(as.factor(pred.knn), as.factor(test$Outcome))

rocObjKNN <- roc(as.factor(test$Outcome), prob.pos)
aucKNN <- auc(rocObjKNN)
aucKNN

##ROC curve (K-NN)
plot(rocObjKNN,
     col = "red",
     lwd = 3,
     main = paste("K-NN ROC curve (AUC =", round(aucKNN, 3), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

plot(rocObjKNN,
     col = "red",
     lwd = 3,
     main = paste("Comparion of ROC curves (K-NN, Decision Tree, Logistic Regression)"))

lines(rocDT, col = "blue", lwd = 3)
lines(rocLM, col = "green", lwd = 3)

legend("bottomright",
       legend = c(
         paste0("K-NN AUC = ", round(auc(rocObjKNN), 3)),
         paste0("Decision Tree AUC = ", round(auc(rocDT), 3)),
         paste0("Logistic Regression Model AUC = ", round(aucLM, 3))
       ),
       col = c("red", "blue", "green"),
       lwd = 2)

##histograms to find out distributions
library(ggplot2)

summary(diabetes$Age)
hist(diabetes$Age,
     main = "Distribution of Variable",
     xlab = "Variable",
     col = "lightblue",
     border = "white")








##correlation matrix to understand the data
corMatrix <- cor(diabetes, use = "complete.obs")
print(corMatrix)
library(corrplot)
corrplot(corMatrix, method = "color", tl.cex = 0.8, number.cex = 0.7, addCoef.col = "black" )

##other regression models to understand relationships
model <- lm(SkinThickness~ Insulin, data = diabetes)
summary(model)
