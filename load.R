# ---- Loading all files and packages ----

library(rpart)
library(nnet)
library(gbm)
library(e1071)
library(randomForest)
library(tm)
library(ggplot2)
library(stringr)
library(openNLP)
library(bigmemory)

sa <- read.table("SA_dataset.tsv", sep = "\t", quote = "")
sa.backup <- sa

pos <- scan('positive-words.txt', what='character', comment.char=';')
neg <- scan('negative-words.txt', what='character', comment.char=';')

sent_tag <- Maxent_Sent_Token_Annotator()
word_tag <- Maxent_Word_Token_Annotator()
pos_tag <- Maxent_POS_Tag_Annotator()

lda <- read.csv('lda.csv')
