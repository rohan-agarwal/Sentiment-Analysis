# ---- Data cleanup file ----

names(sa) <- c("rating", "description")
sa$description <- as.character(sa$description)
sa$rating <- as.factor(sa$rating)

lda[is.na(lda)] <- 0
