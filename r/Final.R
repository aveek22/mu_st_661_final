banks <- read.table('bank-additional-full.csv',sep=',',header = T)

str(banks)
summary(banks)

banks$y = ifelse(banks$y=='yes',1,0)


prop.table(table(banks$y))

library(caret)

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
bank_new <- downSample(x = banks[, -21], y = banks[, 21], yname = "y")

str(bank_new)

prop.table(table(bank_new$y))



p3 <- ggplot(banks, aes(x=age, fill=marital)) + 
  geom_histogram(binwidth = 2, alpha=0.7) +
  facet_grid(cols = vars(y)) +
  expand_limits(x=c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  ggtitle("Age Distribution by Marital Status")

p3
library(tidyverse)
mu <- banks %>% group_by(y) %>% summarize(grp.mean=mean(age))

ggplot (banks, aes(x=age)) + 
  geom_histogram(color = "orange", fill = "orange", binwidth = 5) +
  facet_grid(cols=vars(y)) + 
  ggtitle('Age Distribution by Subscription') + ylab('Count') + xlab('Age') +
  scale_x_continuous(breaks = seq(0,100,5)) +
  geom_vline(data=mu, aes(xintercept=grp.mean), color="red", linetype="dashed")

set.seed(11)
intrain <- sample(nrow(bank_new), nrow(bank_new)*0.7)
bank_train <- bank_new[intrain,]
bank_test <- bank_new[-intrain,]

str(bank_train)




model_camp <- glm(y~.,data = bank_train, family = "binomial")
summary(model_camp)

library(MASS)
model2 <- stepAIC(model_camp, direction = "backward")


summary(model2)


# 
# 
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


pred_lm = predict(model2, type='response', newdata=bank_test)

# plot the prediction distribution
predictions_LR <- data.frame(y = bank_test$y, pred = NA)
predictions_LR$pred <- pred_lm
plot_pred_type_distribution(predictions_LR,0.30)





bank_test$pred_camp <-predict(model2, type = "response", newdata = bank_test)


bank_test$pred_camph <- factor(ifelse(bank_test$pred_camp > 0.5, "yes","no"))
log_conf <- confusionMatrix(bank_test$pred_camph, bank_test$y, positive = "yes")


library(pROC)
test_prob = predict(model2, newdata = bank_test, type = "response")
test_roc = roc(bank_test$y ~ test_prob, plot = TRUE, print.auc = TRUE)



library(caret)
crossValSettings <- trainControl(method = "repeatedcv", number = 10,
                                 savePredictions = TRUE)
crossVal <- train(y ~ ., data = bank_new, family = "binomial",
                  method = "glm", trControl = crossValSettings, tuneLength = 2)
pred <- predict(crossVal, newdata = bank_new)
confusionMatrix(data = pred, bank_new$y)
