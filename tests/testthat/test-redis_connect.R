context("connection")

describe("redis_connect", {
  test_that("Redis connect does it's job", {
    pong <- with_redis(rredis::redisCmd("PING"))
    expect_equal("PONG", pong)
  })
})
