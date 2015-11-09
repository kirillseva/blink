recombinator <- function(lst) {
  cls <- structure(NULL, class = class(lst[[1]]))
  UseMethod("recombinator", object = cls)
}

recombinator.default <- function(lst) {
  stop('No default recombinator exists for objects of class ',
  class(lst[[1]]), '.\n',
  'Please supply your own recombination method using `strategy` argument ',
  'in `decorate` function.')
}

recombinator.data.frame <- function(lst) {
  as.data.frame(dplyr::bind_rows(lst))
}

recombinator.list <- recombinator.data.frame
recombinator.numeric   <- unlist
recombinator.integer   <- unlist
recombinator.character <- unlist
recombinator.logical   <- unlist
