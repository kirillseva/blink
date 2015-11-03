set_cache <- function(key, salt, content) {
  redis_connect()
  rredis::redisHMSet(key, setNames(list(content), salt))
}

get_from_cache <- function(key, salt) {
  redis_connect()
  rredis::redisHMGet(key, salt)[[1]]
}

`exists_in_cache?` <- function(key, salt) {
  redis_connect()
  facts <- rredis::redisHKeys(key)
  if (is.null(facts)) {
    FALSE
  } else {
    facts <- vapply(facts, as.character, character(1))
    salt %in% facts
  }
}
