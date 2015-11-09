context('caching recombinators')

describe('Must supply recombination strategy for weird inputs', {
  test_that('Errors without explicit strategy', {
    with_redis({
      cached_fn <- decorate(weird_fn)
      expect_error(cached_fn(1:10, 'weirdness'),
      'No default recombinator exists for objects of class weird')
    })
  })

  test_that('Can recombine with a strategy', {
    with_redis({
      cached_fn <- decorate(weird_fn, strategy = function(x) unlist(lapply(x, as.numeric)))
      expect_equal(cached_fn(1:10, 'weirdness'), 1:10)
    })
  })
})

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
      cached_fn <- decorate(test_fn)
      tmp <- cached_fn(1:10, type = 'hello')
      expect_is(tmp, 'integer')
      expect_equal(length(tmp), 10)
      expect_equal(names(tmp), rep('hello', 10))
    })
  })

  test_that('numeric vectors', {
    test_fn <- function(id, type) {
      setNames(id, type)
    }
    with_redis({
      cached_fn <- decorate(test_fn)
      tmp <- cached_fn(c(1, 2, 3), type = 'hello')
      expect_is(tmp, 'numeric')
      expect_equal(length(tmp), 3)
      expect_equal(names(tmp), rep('hello', 3))
    })
  })

  test_that('character vectors', {
    test_fn <- function(id, type) {
      setNames(id, type)
    }
    with_redis({
      cached_fn <- decorate(test_fn)
      tmp <- cached_fn(c('hello', 'world'), type = 'hello')
      expect_is(tmp, 'character')
      expect_equal(length(tmp), 2)
      expect_equal(names(tmp), rep('hello', 2))
    })
  })

  test_that('logical vectors', {
    test_fn <- function(id, type) {
      setNames(id, type)
    }
    with_redis({
      cached_fn <- decorate(test_fn)
      tmp <- cached_fn(c(T, F, T), type = 'hello')
      expect_is(tmp, 'logical')
      expect_equal(length(tmp), 3)
      expect_equal(names(tmp), rep('hello', 3))
    })
  })
})
