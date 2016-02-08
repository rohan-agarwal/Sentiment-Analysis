simple_features <- function(sa) {
  
  word_count <- sapply(gregexpr("\\W+", sa$description), length) + 1
  contains_comma <- as.numeric(
    sapply(sa$description, function(x) grepl("\\,", x)))/word_count
  contains_apos <- as.numeric(
    sapply(sa$description, function(x) grepl("\\'", x)))/word_count
  
  sa <- cbind(sa, word_count, contains_comma, contains_apos)
  
  return(sa)
  
}

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

get_sentiments <- function(sa, pos, neg) {
  
  pos_score <- sapply(sa$description, function(x) sent_match(x, pos))/sa$word_count
  neg_score <- sapply(sa$description, function(x) sent_match(x, neg))/sa$word_count
  
  sa <- cbind(sa, pos_score, neg_score)
  
  return(sa)
  
}

gg_bar <- function(sa, att) {
  
  localenv <- environment()
  
  p <- ggplot(data = sa, aes_string(x = deparse(substitute(att))),
              aes(y = (..count..)/sum(..count..)*100),
              environment = localenv) + 
    geom_histogram(fill = "#0072B2") + 
    labs(y = "percent")
  
  print(p)
  
}

gg_bar_pretty <- function(sa, att, color) {
  
  localenv <- environment()
  
  p <- ggplot(data = sa, aes_string(x = deparse(substitute(att)),
                                    fill = deparse(substitute(color))),
              aes(y = (..count..)/sum(..count..)*100),
              environment = localenv) + 
    geom_histogram() + 
    labs(y = "percent")
  
  print(p)
  
}

add_common <- function(dtm, sa, sparsity) {
  
  dtm.common <- removeSparseTerms(dtm, sparsity)
  common_words <- as.data.frame(as.matrix(dtm.common))
  
  sa <- cbind(sa, common_words)
  return(sa)
  
}

common_sent <- function(dtm, sa, pos, neg, sparsity) {
  
  dtm.pos <- dtm[, dtm$dimnames$Terms %in% pos]
  dtm.pos <- removeSparseTerms(dtm.pos, sparsity)
  df.pos <- as.data.frame(as.matrix(dtm.pos))
  
  dtm.neg <- dtm[, dtm$dimnames$Terms %in% neg]
  dtm.neg <- removeSparseTerms(dtm.neg, sparsity)
  df.neg <- as.data.frame(as.matrix(dtm.neg))
  
  dtm.common <- removeSparseTerms(dtm, sparsity)
  
  
  sa <- cbind(sa, df.pos, df.neg)
  return(sa)
  
}

get_dictionary <- function(dtm) {
  
  dictionary <- dtm$dimnames$Terms
  dictionary <- paste(dictionary, collapse = " ")
  dictionary <- as.String(dictionary)
  
  return(dictionary)
  
}

get_a3w <- function(dtm, dictionary) {
  
  a2 <- annotate(dictionary, list(sent_tag, word_tag))
  a3 <- annotate(dictionary, pos_tag, a2)
  a3w <- subset(a3, type == "word")
  return(a3w)

  
}

get_tags <- function(a3w) {
  
  tags <- sapply(a3w$features,`[[`, "POS")
  tags <- as.factor(tags)
  return(tags)
  
}



get_ref <- function(dictionary, tags, a3w) {
 
  ref <- data.frame(cbind(dictionary[a3w], tags))
  names(ref) <- c("word", "tag")
  return(ref) 
  
}

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

pos_feature <- function(sa, tag, ref) {
  
  print("starting new tag...")
  flush.console()
  
  feature <- sapply(sa$description, 
                    function(x) tag_count(tag, x, ref))/sa$word_count
  
  return(as.numeric(feature))
  
}


name_clean <- function(sa) {
  
  names(sa)[names(sa)=="..."] = "dotdotdot"
  names(sa)[names(sa)=="."] = "dot"
  names(sa)[names(sa)=="for"] = "forr"
  names(sa)[names(sa)=="else"] = "elsee"
  names(sa) <- sub("\\'","apos_",names(sa))
  names(sa) <- sub("\\'","apos_",names(sa))
  names(sa) <- sub("\\-","dash_",names(sa))
  names(sa) <- sub("\\-","dash_",names(sa))
  names(sa) <- sub("\\,","comma",names(sa))
  names(sa) <- sub("\\:","colon",names(sa))
  names(sa) <- sub("\\$","",names(sa))
  
  return(sa)
  
}

# set.seed(3932)
