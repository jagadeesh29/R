library(randomForest)
library(tree)
library(caret)
library(Metrics)
library(e1071)
library(dplyr)
dplyr_eg <- churn %>% filter(Day.Calls <= 50) %>%  select(Account.Length, Day.Calls)
churn_data <- read.csv("CHURN.txt", header = TRUE)
churn <- churn_data[, -4 ]
str(churn)

sapply(churn,class)
colSums(is.na(churn))
summary(churn)
# churn$Churn. <- as.character(churn$Churn.)

churn_var <- summary(churn$Churn.)
# Bar chart of variable Churn
#par(mfrow = c(1,2))
barplot(churn_var,ylim = c(0, 3333),main = "Bar Graph of Churners and Non-Churners",col = "red")
box(which = "plot",lty = "solid",col = "black")

# Make a table for counts of Churn and International Plan
counts <- table(churn$Churn,
                churn$Int.l.Plan,
                dnn=c("Churn", "International Plan"))
counts

#Overlayed bar chart
barplot(counts,
        legend = rownames(counts),
        col = c("blue", "red"),
        ylim = c(0, 3300),
        ylab = "Count",
        xlab = "International Plan",
        main = "Comparison Bar Chart:
Churn Proportions by
International Plan")
box(which = "plot",
    lty = "solid",
    col="black")

# Create a table with sums for both variables
sumtable <- addmargins(counts,
                       FUN = sum)
sumtable
# Create a table of proportions over rows
row.margin <- round(prop.table(counts,margin = 1),4)*100
row.margin
# Create a table of proportions over columns
col.margin <- round(prop.table(counts,margin = 2),4)*100
col.margin

# Clustered Bar Chart, with legend
barplot(counts,
        col = c("blue", "red"),
        ylim = c(0, 3300),
        ylab = "Count",
        xlab = "International Plan",
        main = "Churn Count by
International Plan",
        beside = TRUE)
legend("topright",
       c(rownames(counts)),
       col = c("blue", "red"),
       pch = 15,
       title = "Churn")
box(which = "plot",
    lty = "solid",
    col="black")

# Clustered Bar Chart of Churn and International Plan with legend
barplot(t(counts),
        col = c("blue", "green"),
        ylim = c(0, 3300),
        ylab = "Counts",
        xlab = "Churn",
        main = "International Plan Count by
Churn",
        beside = TRUE)
legend("topright",
       c(rownames(counts)),
       col = c("blue", "green"),
       pch = 15,
       title = "Int'l Plan")
box(which = "plot",
    lty = "solid",
    col="black")
# Histogram of non-overlayed Customer Service Calls
hist(churn$CustServ.Calls,
     xlim = c(0,10),
     col = "orange",
     ylab = "Count",
     xlab = "Customer Service Calls",
     main = "Histogram of Customer Service
Calls")

# Overlayed bar charts
library(ggplot2)
ggplot() +
  geom_bar(data = churn,
           aes(x = factor(churn$CustServ.Calls),
               fill = factor(churn$Churn)),
           position = "stack") +
  scale_x_discrete("Customer Service
                   Calls") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="Churn"))
+
  scale_fill_manual(values=c("blue",
                             "red"))
ggplot() +
  geom_bar(data=churn,
           aes(x = factor(churn$CustServ.Calls),
               fill = factor(churn$Churn)),
           position = "fill") +
  scale_x_discrete("Customer Service Calls")
+
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="Churn")) +
  scale_fill_manual(values=c("blue", "red"))

# Scatterplot of Evening Minutes and Day Minutes, colored by Churn
plot(churn$Eve.Mins,
     churn$Day.Mins,
     xlim = c(0, 400),
     ylim = c(0, 400),
     xlab = "Evening Minutes",
     ylab = "Day Minutes",
     main = "Scatterplot of Day
     and Evening Minutes by
     Churn",
     col = ifelse(churn$Churn==
                    "True",
                  "red",
                  "blue"))
legend("topright",
       c("True",
         "False"),
       col = c("red",
               "blue"),
       pch = 1,
       title = "Churn")
# Scatterplot of Day Minutes and Customer Service Calls, colored by
Churn
plot(churn$Day.Mins,
     churn$CustServ.Calls,
     xlim = c(0, 400),
     xlab = "Day Minutes",
     ylab = "Customer Service Calls",
     main = "Scatterplot of Day Minutes and
Customer Service Calls by Churn",
     col = ifelse(churn$Churn=="True",
                  "red",
                  "blue"),
     pch = ifelse(churn$Churn=="True",
                  16, 20))
legend("topright",
       c("True",
         "False"),
       col = c("red",
               "blue"),
       pch = c(16, 20),
       title = "Churn")
# Scatterplot matrix
pairs( ~churn$Day.Mins+
         churn$Day.Calls+
         churn$Day.Charge)

# churn$Int.l.Plan <- as.factor(churn$Int.l.Plan)
# churn$VMail.Plan <- as.factor(churn$VMail.Plan)
# churn$Churn. <- as.factor(churn$Churn.)
# churn$State <-  as.factor(churn$State)
# churn$CustServ.Calls <- as.factor(churn$CustServ.Calls)
str(churn)
attach(churn)

table(churn$Churn.)
plot(Churn.~., churn)
# 
# par(mfrow=c(1, 2))  # divide graph area in 2 columns
# boxplot(churn$VMail.Message, main="vmail message", sub=paste("Outlier rows: ", boxplot.stats(churn$VMail.Message)$out))
# boxplot(churn$Day.Mins, main="DAY mins", sub=paste("Outlier: ", boxplot.stats(churn$Day.Mins)$out))

set.seed(30)
intrain <- sample(1:nrow(churn), .7*nrow(churn))
training <- churn[intrain,]
testing <- churn[-intrain,]
colnames(churn)
model_logi<- glm(Churn.~. -State, training,family = "binomial")
summary(model_logi)

 pred_log <- predict(model_logi, testing, type = "response")
pred_log

pred_cond <- ifelse(pred_log <= .5, 0,1)
length(pred_log)
length(testing$Churn.)
table(pred_cond,testing$Churn.)
confusionMatrix(pred_cond, testing$Churn.)

library(caret)
accuracy(pred_cond,testing$Churn.)
#decision tree

model_tree <- tree(Churn. ~.-State, data = training)
summary(model_tree)

plot(model_tree)
text(model_tree,pretty = 0)

tree_cv <- cv.tree(model_tree)
summary(tree_cv)
plot(tree_cv$size,tree_cv$dev,type = "b")
plot(tree_cv$dev,tree_cv$k,type = "b")

prune_tree <- prune.tree(model_tree, best = 11)
plot(prune_tree)
text(prune_tree,pretty = 0)

pred_tree <- predict(prune_tree, testing, type = "class")
pred_tree

table(pred_tree, testing$Churn.)
confusionMatrix(pred_tree, testing$Churn.)



