context('Cached function')

describe('Cached function is fast and has same results', {
  test_that('We can cache a function', {
    with_redis({
      # id and type get imputed by default values
      cached_fn <- decorate(get_song_count_for, salt = 'album')
      # Cache one value at a time - notice that initial function is not vectorized
      expect_equal(
        cached_fn('author', 1, 'other_side_of_the_moon'),
        tmp1 <- get_song_count_for('author', 1, 'other_side_of_the_moon')
      )
      expect_equal(
        cached_fn('author', 2, 'other_side_of_the_moon'),
        tmp2 <- get_song_count_for('author', 2, 'other_side_of_the_moon')
      )
      # The cached function is vectorised however, and the results should match
      # Furthermore, results should be computed very fast!
      expect_true(takes_less_than(0.5)(
        tmp3 <- cached_fn('author', 1:2, 'other_side_of_the_moon')
      )$passed)
      # vectorization works
      expect_equal(tmp3, rbind(tmp1, tmp2))
    })
  })

  test_that('We can store big datasets', {
    with_redis({
      cached_fn <- decorate(get_bigdata_for)
      tmp <- cached_fn(type = 'author', id = 1:10)
      expect_equal(dim(tmp), c(10, 10002))
      expect_equal(tmp, cached_fn(type = 'author', id = 1:10))
    })
  })

  test_that('We can store a bunch of ids', {
    with_redis({
      cached_fn <- decorate(get_total_song_length_for, salt = 'decade')
      ROWS <- 1e4
      tmp <- cached_fn('author', 1:ROWS, '2000')
      expect_equal(dim(tmp), c(ROWS, 2))
      expect_equal(tmp, cached_fn('author', 1:ROWS, '2000'))
    })
  })
})
