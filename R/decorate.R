#' Decorate the slow function to enable caching
#'
#' @param fun function. Function to be wrapped around a caching layer.
#' @param id_col character. The name of the argument to the original function
#'   that acts like id.
#' @param type character. Type of id, like customer_id, author_id, song_id etc.
#' @param salt character. The names of the other parameters to be supplied to the
#'   original function that modify the results. For example, in
#'   get_movie_details(id = 100, type = 'film', extended_details = TRUE)
#'   extended_details set to true might be returning a larger dataframe.
#' @param strategy function. Blink will turn your function into a vectorized
#'   version, and will use this function to recombine the results.
#'   A sensible default is provided for common classes.
#'
#' @export
decorate <- function(fun, salt = NULL, type = 'type', id_col = 'id',
    strategy = recombinator) {
  verify_args(fun, salt, type, id_col, strategy)
  verify_formals(fun)
  make_cached_fn(fun, salt, type, id_col, strategy)
}

make_cached_fn <- function(fun, salt, type, id_col, strategy) {
  ## Let's create a new function that will do our bidding
  ## A function is fully defined by
  ## - formals
  ## - body
  ## - environment
  ##
  ## Let's perform some surgery and manually construct a function
  ## from those building blocks
  cached_fn <- new("function")
  ## We will also inject ellipsis (`...`) for additional
  ## arguments in case we want to add them, like
  ## for cache invalidation or what not
  ## However if ellipsis is already present we don't need to do
  ## any dark magic
  if(!any(grepl("^...$", names(formals(fun))))) {
    formals(cached_fn) <- c(formals(fun), unlist(alist(... = )))
  } else { formals(cached_fn) <- formals(fun) }
  environment(cached_fn) <- list2env(list(
    `__fun` = fun, `__salt` = salt, `__type` = type,
    `__id_col` = id_col, `__strategy` = strategy
  ), parent = environment(fun))
  body(cached_fn) <- make_body_fn()
  class(cached_fn) <- c(class(cached_fn), 'blink_cached_fn')
  cached_fn
}

make_body_fn <- function() {
  quote({
    raw_call <- match.call()
    call     <- as.list(raw_call[-1])
    for (name in names(call)) {
      call[[name]] <- eval.parent(call[[name]])
    }

    ## extract metadata from environment for convenience
    ids       <- call[[`__id_col`]]
    type      <- call[[`__type`]]
    fun       <- `__fun`
    strategy  <- `__strategy`
    overwrite <- isTRUE(call[['overwrite.']])
    ## for salt it's a little bit interesting
    ## we want to take a hash of all params that are part of the salt
    ## and it should be deterministic with respect to sorting
    ## Here we rely on the fact that when we subset the list we will get
    ## the results in the same order as `__salt`
    salt <- digest::digest(call[`__salt`])
    ## make sure ids are not NA
    stopifnot(all(!is.na(ids)) && length(ids) > 0)

    ## lapply over all ids and retrieve data
    result <- as.data.frame(purrr::map_df(ids, function(i) {
      key <- make_key(i, type)
      if (!overwrite && blink:::`exists_in_cache?`(key, salt)) {
        blink:::get_from_cache(key, salt)
      } else {
        args <- call
        args[[`__id_col`]] <- i
        # strip banned names out of the function call
        vapply(BANNED_NAMES, function(nm) { args[[nm]] <<- NULL; TRUE }, logical(1))
        content <- do.call(fun, args)
        blink:::set_cache(key, salt, content)
        content
      }
    }))
    # strategy(result)
  })
}
