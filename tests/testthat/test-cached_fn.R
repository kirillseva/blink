context('Cached function')

describe('Cached function is fast and has same results', {
  test_that('We can cache a function', {
    with_redis({
      # id and type get imputed by default values
      cached_fn <- decorate(get_album_length_for, salt = 'album')
      # Cache one value at a time - notice that initial function is not vectorized
      expect_equal(
        cached_fn('author', 1, 'other_side_of_the_moon'),
        tmp1 <- get_album_length_for('author', 1, 'other_side_of_the_moon')
      )
      expect_equal(
        cached_fn('author', 2, 'other_side_of_the_moon'),
        tmp2 <- get_album_length_for('author', 2, 'other_side_of_the_moon')
      )
      # The cached function is vectorised however, and the results should match
      # Furthermore, results should be computed very fast!
      takes_less_than(0.5)(
        tmp3 <- cached_fn('author', 1:2, 'other_side_of_the_moon')
      )
      # vectorization works
      expect_equal(tmp3, rbind(tmp1, tmp2))
    })
  })
})
