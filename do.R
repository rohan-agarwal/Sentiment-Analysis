# ---- Initial Features ----

sa$word_count <- sapply(gregexpr("\\W+", sa$description), length) + 1
sa$contains_comma <- as.numeric(
  sapply(sa$description, function(x) grepl("\\,", x)))
sa$contains_apos <- as.numeric(
  sapply(sa$description, function(x) grepl("\\'", x)))

sent_match <- function(sentence, list) {
  
  sentence = gsub('[[:punct:]]', '', sentence)
  sentence = gsub('[[:cntrl:]]', '', sentence)
  sentence = gsub('\\d+', '', sentence)
  sentence = tolower(sentence)
  
  word.list = str_split(sentence, '\\s+')
  words = unlist(word.list)
  
  matches = match(words, list)
  matches <- !is.na(matches)
  
  return(sum(matches))
  
}

sa$pos_score <- sapply(sa$description, function(x) sent_match(x, pos))
sa$neg_score <- sapply(sa$description, function(x) sent_match(x, neg))

# ---- Data exploration ----

ggplot(sa, aes(x = as.factor(rating), 
               y = (..count..)/sum(..count..)*100)) + 
  geom_histogram(fill = "#0072B2") + 
  labs(y = "percent")

ggplot(sa, aes(x = as.factor(word_count), 
               y = (..count..)/sum(..count..)*100)) + 
  geom_histogram(fill = "#0072B2") + labs(y = "percent")

ggplot(sa, aes(x = as.factor(neg_score), 
               y = (..count..)/sum(..count..)*100, 
               fill = rating)) + geom_histogram() + labs(y = "percent")

# ---- DTM modeling ----

corpus <- Corpus(VectorSource(sa$description))
dtm <- DocumentTermMatrix(corpus)
dtm.common <- removeSparseTerms(dtm, 0.999)
common_words <- as.data.frame(as.matrix(dtm.common))
sa <- cbind(sa, common_words)

dictionary <- dtm$dimnames$Terms
dictionary <- paste(dictionary, collapse = " ")
dictionary <- as.String(dictionary)
detach(package:ggplot2)
a2 <- annotate(dictionary, list(sent_tag, word_tag))
a3 <- annotate(dictionary, pos_tag, a2)
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features,`[[`, "POS")
sprintf("%s/%s", dictionary[a3w], tags)
ref <- data.frame(cbind(dictionary[a3w], tags))
names(ref) <- c("word", "tag")

tag_count <- function(tag, sentence, ref) {
  
  sentence = gsub('[[:punct:]]', '', sentence)
  sentence = gsub('[[:cntrl:]]', '', sentence)
  sentence = gsub('\\d+', '', sentence)
  sentence = tolower(sentence)
  
  word.list = str_split(sentence, '\\s+')
  words = unlist(word.list)
  
  matches = match(words, ref$word)
  tags = ref$tag[matches]
  count = length(grep(tag, tags))
  
  return(count)
  
}

for (i in 1:length(levels(tags))) {
  
  print(i)
  
  tag = levels(tags)[i]
  tmp[[i]] <- sapply(sa$description, 
                   function(x) tag_count(tag, x, ref))
  
}

for (i in 1:length(levels(tags))) {
  
  sa <- cbind(sa, tmp[[i]])
  
}

names(sa)[(ncol(sa)-37):ncol(sa)] <- levels(tags)

# ---- Prep for modeling ----

names(sa)[names(sa)=="..."] = "dotdotdot"
names(sa)[names(sa)=="."] = "dot"
names(sa)[names(sa)=="else"] = "elsee"
names(sa) <- sub("\\'","apos_",names(sa))
names(sa) <- sub("\\-","dash_",names(sa))
train <- sa[1:(nrow(sa)*.7),-2]
test <- sa[(nrow(sa)*.7+1):nrow(sa),-2]

# ---- Stupid Models ----

# linear regression
train.reg <- lm(rating~., train)
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

# random forest
train.rf <- randomForest(rating~.,train[,1:170])
pred.rf <- predict(train.rf, test)
sum(pred.rf == test$rating)/nrow(test)
