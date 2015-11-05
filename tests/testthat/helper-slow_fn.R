get_song_count_for <- function(type, id, album) {
  # Simulate thought process
  Sys.sleep(1)
  setNames(data.frame(id, 12), c(paste0(type, '_id'), 'songs'))
}

get_bigdata_for <- function(type, id, ...) {
  name <- if (id %% 2 == 0) 'John' else 'Jane'
  # some really big data
  ret <- data.frame(matrix(0.4242, nrow = 1, ncol = 1e4))
  ret[[paste0(type, '_id')]] <- id
  ret$author_name <- name
  ret
}

get_total_song_length_for <- function(type, id, decade) {
  setNames(data.frame(id, 228), c(paste0(type, '_id'), 'minutes'))
}
