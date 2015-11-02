context("connection")

describe("redis_connect", {
  test_that("Redis connect does it's job", {
    pong <- with_redis(rredis::redisCmd("PING"))
    expect_equal("PONG", pong)
  })

  test_that("Redis connect throws a sad error", {
    try(rredis::redisClose())
    old <- options('blink.redis.yml' = NULL);
    on.exit(options(old))
    expect_error(redis_connect(), 'Please set option `blink.redis.yml` to be able to use blink')
  })
})
