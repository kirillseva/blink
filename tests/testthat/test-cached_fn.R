context('Cached function')

describe('Cached function is fast and has same results', {
  test_that('Some arguments are reserved', {
    expect_error(decorate(function(id, type, overwrite.) print('hello')),
    "Please rename your function")
  })

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
      is_faster_than(0.5,
        tmp3 <- cached_fn('author', 1:2, 'other_side_of_the_moon')
      )
      # vectorization works
      expect_equal(tmp3, rbind(tmp1, tmp2))
    })
  })

  test_that('Can use a different recombination strategy', {
    with_redis({
      cached_fn <- decorate(get_song_count_for, salt = 'album', strategy = plyr::rbind.fill)
      expect_equal(
        cached_fn('author', 1, 'other_side_of_the_moon'),
        tmp1 <- get_song_count_for('author', 1, 'other_side_of_the_moon')
      )
      expect_equal(
        cached_fn('author', 2, 'other_side_of_the_moon'),
        tmp2 <- get_song_count_for('author', 2, 'other_side_of_the_moon')
      )
      is_faster_than(0.5,
        tmp3 <- cached_fn('author', 1:2, 'other_side_of_the_moon')
      )
      expect_equal(tmp3, rbind(tmp1, tmp2))
    })
  })

  test_that('Can overwrite cache when needed', {
    with_redis({
      cached_fn <- decorate(get_song_count_for, salt = 'album')
      is_slower_than(2,
        tmp1 <- cached_fn('author', 1:5, 'other_side_of_the_moon')
      )
      is_faster_than(0.5,
        tmp2 <- cached_fn('author', 1:5, 'other_side_of_the_moon')
      )
      expect_equal(tmp1, tmp2)
      is_slower_than(2,
        tmp2 <- cached_fn('author', 1:5, 'other_side_of_the_moon', overwrite. = TRUE)
      )
      expect_equal(tmp2, tmp1)
    })
  })

  test_that('We can store big datasets', {
    # Performance/regression tests on Travis are not a good idea
    skip_on_travis()
    with_redis({
      cached_fn <- decorate(get_bigdata_for)
      tmp <- cached_fn(type = 'author', id = 1:10)
      expect_equal(dim(tmp), c(10, 10002))
      is_faster_than(3,
        expect_equal(tmp, cached_fn(type = 'author', id = 1:10))
      )
    })
  })

  test_that('We can store a bunch of ids', {
    # Performance/regression tests on Travis are not a good idea
    skip_on_travis()
    with_redis({
      ROWS <- 1e4
      cached_fn <- decorate(get_total_song_length_for, salt = 'decade')
      tmp <- cached_fn('author', 1:ROWS, '2000')
      expect_equal(dim(tmp), c(ROWS, 2))
      is_faster_than(30,
        expect_equal(tmp, cached_fn('author', 1:ROWS, '2000'))
      )
    })
  })
})
