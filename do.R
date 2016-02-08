source("load.R")
source("clean.R")
source("func.R")

# ---- Feature generation ----


sa <- simple_features(sa)
sa <- get_sentiments(sa, pos, neg)

# ---- Data vis ----

gg_bar(sa, rating)
gg_bar(sa, word_count)
gg_bar_pretty(sa, word_count, rating)

# ---- DTM modeling ----

corpus <- Corpus(VectorSource(sa$description))
dtm <- DocumentTermMatrix(corpus, control = list(weighting = function(x) { 
  weightTfIdf(x, normalize = FALSE)}))
sa.pre.dtm <- sa

detach(package:ggplot2)
dictionary <- get_dictionary(dtm)
a3w <- get_a3w(dtm, dictionary)
tags <- get_tags(a3w)
ref <- get_ref(dictionary, tags, a3w)
library(ggplot2)

posf <- sapply(levels(tags), function(x) pos_feature(sa, x, ref))
sa <- cbind(sa, posf)

sa.post.posf <- sa
sa <- add_common(dtm, sa, 0.99)
sa <- common_sent(dtm, sa, pos, neg, 0.995)
sa <- cbind(sa, lda)

# ---- Prep for modeling ----

sa <- name_clean(sa)
# 
# indices <- sample(1:nrow(sa), .8*nrow(sa), replace = FALSE)
# 
# train <- sa[indices,-2]
# test <- sa[-indices,-2]

train <- sa[1:(nrow(sa)*.8), -2]
test <- sa[(nrow(sa)*.8+1):nrow(sa), -2]

# ---- Stacking ----

is.bad <- as.numeric(train$rating == 0 | train$rating == 1)
is.good <- as.numeric(train$rating == 3 | train$rating == 4)

g1 <- glm(is.bad~., train[,-1], family = binomial(link = "logit"))
g2 <- glm(is.good~., train[,-1], family = binomial(link = "logit"))

train$bp <- predict(g1, type = "response")
train$gp <- predict(g2, type = "response")
test$bp <- predict(g1, test, type = "response")
test$gp <- predict(g2, test, type = "response")

# ---- Stupid Models ----

# linear regression
train.reg <- lm(as.numeric(as.character(rating))~., train)
pred.reg <- predict(train.reg, test)
pred.reg <- round(pred.reg)
pred.reg[pred.reg < 0] = 0
pred.reg[pred.reg > 4] = 4
sum(pred.reg == test$rating)/nrow(test)

ggplot(test, aes(x = as.factor(rating), 
               y = (..count..)/sum(..count..)*100, 
               fill = factor(pred.reg))) + 
  geom_histogram() + labs(y = "percent")

# naive bayes
train.nb <- naiveBayes(as.factor(rating)~.,train)
pred.nb <- predict(train.nb, test, "class")
sum(pred.nb == test$rating)/nrow(test)

ggplot(test, aes(x = as.factor(rating), 
                 y = (..count..)/sum(..count..)*100, 
                 fill = factor(pred.nb))) + 
  geom_histogram() + labs(y = "percent")

# decision tree
train.tree <- rpart(as.factor(rating)~.,train)
pred.tree <- predict(train.tree, test, "class")
sum(pred.tree == test$rating)/nrow(test)

ggplot(test, aes(x = as.factor(rating), 
                 y = (..count..)/sum(..count..)*100, 
                 fill = factor(pred.tree))) + 
  geom_histogram() + labs(y = "percent")

# ---- Better models ----

# svm
train.svm <- svm(as.factor(rating)~.,train)
pred.svm <- predict(train.svm, test)
sum(pred.svm == test$rating)/nrow(test)

# gbm
train.gbm <- gbm(rating~., data = train,
                 var.monotone=rep(0,ncol(train)-1), n.trees=1000,
                 shrinkage=.1, interaction.depth=3, bag.fraction = .5,
                 train.fraction = 1, n.minobsinnode = 10, cv.folds= 10, 
                 keep.data=TRUE, verbose=FALSE)
best.iter <- gbm.perf(train.gm,method="cv");best.iter
summary(train.gbm,n.trees=best.iter)
pred.gbm <- predict(train.gbm, test, best.iter, type = "response")
sum(pred.gbm == test$rating)/nrow(test)

# random forest
train.rf <- randomForest(rating~.,train)
pred.rf <- predict(train.rf, test)
sum(pred.rf == test$rating)/nrow(test)

ggplot(test, aes(x = as.factor(rating), 
                 y = (..count..)/sum(..count..)*100, 
                 fill = factor(pred.rf))) + 
  geom_histogram() + labs(y = "percent")

# multinomial logistic regresion
train.mlr <- multinom(rating~., train)
pred.mlr <- predict(train.mlr, test)
sum(pred.mlr == test$rating)/nrow(test)
 