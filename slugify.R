slugify <- function(x) {
  x <- gsub("[^[:alnum:] ]", "", x)
  x <- gsub(" ", "_", x)
  x <- tolower(x)
  
  return(x)
}
