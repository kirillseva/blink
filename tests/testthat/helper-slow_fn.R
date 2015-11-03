get_album_length_for <- function(type, id, album) {
  # Simulate thought process
  Sys.sleep(1)
  setNames(data.frame(id, 42), c(paste0(type, '_id'), paste0(album, '_length')))
}
