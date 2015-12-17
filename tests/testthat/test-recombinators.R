context('caching recombinators')

describe('Can cache functions with various outputs using default recombinator', {
  test_that('Dataframes', {
    test_fn <- function(id, type) {
      data.frame(id = id, type = type)
    }
    with_redis({
      cached_fn <- decorate(test_fn)
      tmp <- cached_fn(1:10, type = 'hello')
      expect_is(tmp, 'data.frame')
      expect_equal(dim(tmp), c(10, 2))
    })
  })

  test_that('Lists get recombined in dataframes', {
    test_fn <- function(id, type) {
      list(id = id, type = type)
    }
    with_redis({
      cached_fn <- decorate(test_fn)
      tmp <- cached_fn(1:10, type = 'hello')
      expect_is(tmp, 'data.frame')
      expect_equal(dim(tmp), c(10, 2))
    })
  })

  test_that('integer vectors', {
    test_fn <- function(id, type) {
      setNames(id, type)
    }
    with_redis({
      cached_fn <- decorate(test_fn, strategy = purrr::map_int)
      tmp <- cached_fn(1:10, type = 'hello')
      expect_is(tmp, 'integer')
      expect_equal(length(tmp), 10)
    })
  })

  test_that('numeric vectors', {
    test_fn <- function(id, type) {
      setNames(id, type)
    }
    with_redis({
      cached_fn <- decorate(test_fn, strategy = purrr::map_dbl)
      tmp <- cached_fn(c(1, 2, 3), type = 'hello')
      expect_is(tmp, 'numeric')
      expect_equal(length(tmp), 3)
    })
  })

  test_that('character vectors', {
    test_fn <- function(id, type) {
      setNames(id, type)
    }
    with_redis({
      cached_fn <- decorate(test_fn, strategy = purrr::map_chr)
      tmp <- cached_fn(c('hello', 'world'), type = 'hello')
      expect_is(tmp, 'character')
      expect_equal(length(tmp), 2)
    })
  })

  test_that('logical vectors', {
    test_fn <- function(id, type) {
      setNames(id, type)
    }
    with_redis({
      cached_fn <- decorate(test_fn, strategy = purrr::map_lgl)
      tmp <- cached_fn(c(T, F, T), type = 'hello')
      expect_is(tmp, 'logical')
      expect_equal(length(tmp), 3)
    })
  })
})
