#' @export
decorate <- function(fun, salt, type, id_col = 'id') {
  verify_args(fun, salt, type, id_col)
  verify_formals(fun)
  make_cached_fn(fun, salt, type, id_col)
}

make_cached_fn <- function(fun, salt, type, id_col) {
  ## Let's create a new function that will do our bidding
  cached_fn <- new("function")
  ## We will also inject ellipsis (`...`) for additional
  ## arguments in case we want to add them, like
  ## for cache invalidation or what not
  ## However if ellipsis is already present we don't need to do
  ## any dark magic
  if(!any(grepl("...", names(formals(fun))))) {
    formals(cached_fn) <- c(formals(fun), unlist(alist(... = )))
  } else { formals(cached_fn) <- formals(fun) }
  body(cached_fn) <- make_body_fn()
  env(cached_fn) <- list2env(list(
    `__fun` = fun, `__salt` = salt, `__type` = type, `__id_col` = id_col
  ), parent = environment(fun))
  class(cached_fn) <- c(class(cached_fn), 'blink_cached_fn')
  cached_fn
}

make_body_fn <- function() {
  quote({
    raw_call <- match.call()
    call     <- as.list(raw_call[-1])

    ids <- call[[`__id_col`]]
  })
}
