library(devtools)

with_redis <- function(expr) {
  devtools::with_options(
    c(blink.redis.yml = system.file("redis.yml", package = "blink")),
    {
      redis_connect();
      on.exit(rredis::redisFlushDB())
      eval(expr)
    }
  )
}
