verify_args <- function(fun, salt, type, id_col) {
  stopifnot(is.function(fun))
  stopifnot((is.character(salt) && Negate(any)(is.na(salt))) || is.null(salt))
  stopifnot(is.simple_string(type))
  stopifnot(is.simple_string(id_col))
}

is.simple_string <- function(st) {
  is.character(st) &&
  length(st) == 1 &&
  nzchar(st) &&
  !is.na(st)
}

BANNED_NAMES <- c(
  'overwrite.'
)

verify_formals <- function(fun) {
  ## For any injected arguments by blink we should make sure
  ## there are no argument collisions
  vapply(names(formals(fun)), function(nm) {
    if (nm %in% BANNED_NAMES) {
      stop("Please rename your function's argument `", crayon::red(nm),
      "` as this argument is reserved by blink package.")
    }
    TRUE
  }, logical(1))
}

make_key <- function(id, type) {
  stopifnot(length(id) == 1)
  stopifnot(is.simple_string(type))
  paste0(type, ":", id)
}
