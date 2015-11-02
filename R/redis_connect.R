##############################################
# Set proper options for connecting to redis #
##############################################
redis_connect <- function() {
  tryCatch(nzchar(rredis::redisCMD('PING')), error = function(...) {
    redis.yml <- getOption('blink.redis.yml')
    if (is.null(redis.yml)) {
      stop(
        'Please set option `blink.redis.yml` to be able to use blink. ',
        'An example would look like:\n',
        '  host: "0.0.0.0"\n',
        '  port: 6379\n',
        '  password: "blink"\n',
        '  nodelay: FALSE')
    }
    config <- yaml::yaml.load_file(redis.yml)
    ## rredis requires that port, for example, is numeric. So let's
    ## So we convert the inputs into proper types
    config$port    <- as.numeric(config$port)
    config$nodelay <- as.logical(config$nodelay)
    do.call(rredis::redisConnect, config)
    TRUE
  })
}
