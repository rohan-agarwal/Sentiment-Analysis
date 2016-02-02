# ---- Data cleanup file ----

names(sa) <- c("rating", "description")
sa$description <- as.character(sa$description)
