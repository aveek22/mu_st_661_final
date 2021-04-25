#setwd("C:/Users/sujit/Desktop/Maynooth Studies/Semester 2/ST662 - Topics in Data Analytics/Project/bank-additional/bank-additional")

library(caret)
library(dplyr)
library(tidyverse)
library(MASS)
library(pROC)


banks <- read.table('bank-additional-full.csv',sep=',',header = T)

str(banks)
summary(banks)

banks$y1 = ifelse(banks$y=='yes',1,0)
str(banks)


prop.table(table(banks$y))


banks$job  <- as.factor(banks$job)
banks$marital <- as.factor(banks$marital)
banks$education <- as.factor(banks$education)
banks$default <- as.factor(banks$default)
banks$housing <- as.factor(banks$housing)
banks$loan <- as.factor(banks$loan)
banks$contact <- as.factor(banks$contact)
banks$month <- as.factor(banks$month)
banks$day_of_week <- as.factor(banks$day_of_week)
banks$poutcome <- as.factor(banks$poutcome)

banks$y <- as.factor(banks$y)
banks$y1 <- as.factor(banks$y1)

banks1 <- select(banks,-22)
banks <- select(banks,-21)





plot1 <- ggplot(banks, aes(x=age, fill=marital)) + 
  geom_histogram(binwidth = 2, alpha=0.7) +
  facet_grid(cols = vars(y1)) +
  expand_limits(x=c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  ggtitle("Age Distribution by Marital Status")

plot1

mu <- banks %>% group_by(y) %>% summarize(grp.mean=mean(age))

ggplot (banks, aes(x=age)) + 
  geom_histogram(color = "orange", fill = "orange", binwidth = 5) +
  facet_grid(cols=vars(y1)) + 
  ggtitle('Age Distribution by Subscription') + ylab('Count') + xlab('Age') +
  scale_x_continuous(breaks = seq(0,100,5)) +
  geom_vline(data=mu, aes(xintercept=grp.mean), color="red", linetype="dashed")



bank_new <- downSample(x = banks[, -21], y = banks[, 21], yname = "y")

bank_new1 <- downSample(x = banks1[, -21], y = banks1[, 21], yname = "y")





set.seed(11)
intrain <- sample(nrow(bank_new1), nrow(bank_new1)*0.7)
bank_train1 <- bank_new1[intrain,]
bank_test1 <- bank_new1[-intrain,]

str(bank_train)




model_camp <- glm(y~.,data = bank_train1, family = "binomial")

summary(model_camp)


model2 <- stepAIC(model_camp, direction = "backward")


summary(model2)


intrain1 <- sample(nrow(bank_new), nrow(bank_new)*0.7)
bank_train3 <- bank_new[intrain1,]
bank_test3 <- bank_new[-intrain1,]

model_camp_pr <- glm(y~.,data = bank_train3, family = "binomial")
model21 <- stepAIC(model_camp_pr, direction = "backward")





plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pred >= threshold & df$y == 1, "TP", v)
  v <- ifelse(df$pred >= threshold & df$y == 0, "FP", v)
  v <- ifelse(df$pred < threshold & df$y == 1, "FN", v)
  v <- ifelse(df$pred < threshold & df$y == 0, "TN", v)
  
  df$pred_type <- v
  
  ggplot(data=df, aes(x=y, y=pred)) + 
    geom_violin(fill='black', color=NA) + 
    geom_jitter(aes(color=pred_type), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold))
}


pred_lm = predict(model21, type='response', newdata=bank_test3)

# plot the prediction distribution
predictions_LR <- data.frame(y = bank_test$y, pred = NA)
predictions_LR$pred <- pred_lm
plot_pred_type_distribution(predictions_LR,0.30)



bank_test1$pred_camp <-predict(model2, type = "response", newdata = bank_test1)



bank_test1$pred_camph <- factor(ifelse(bank_test1$pred_camp > 0.5, "yes","no"))
log_conf <- confusionMatrix(bank_test$pred_camph, bank_test1$y, positive = "yes")
log_conf$byClass


test_prob = predict(model2, newdata = bank_test1, type = "response")
test_roc = roc(bank_test1$y ~ test_prob, plot = TRUE, print.auc = TRUE)




crossValSettings <- trainControl(method = "repeatedcv", number = 10,
                                 savePredictions = TRUE)
crossVal <- train(y ~ ., data = bank_new, family = "binomial",
                  method = "glm", trControl = crossValSettings, tuneLength = 2)
pred <- predict(crossVal, newdata = bank_new)
confusionMatrix(data = pred, bank_new$y)

