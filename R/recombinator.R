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
  if (isTRUE(require(dplyr, quietly = TRUE))) {
    as.data.frame(dplyr::bind_rows(lst))
  } else if (isTRUE(require(plyr, quietly = TRUE))) {
    plyr::rbind.fill
  } else rbind
}

recombinator.list <- recombinator.data.frame
recombinator.numeric   <- unlist
recombinator.integer   <- unlist
recombinator.character <- unlist
recombinator.logical   <- unlist
