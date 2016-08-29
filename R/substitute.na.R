#this function takes a object and substitutes the NA's with a given level
substitute.na <- function(what,level){
  what[which(is.na(what))] <- level
}