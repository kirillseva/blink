# Design notes

Let's say you work for an internet radio and you have a function that fetches data
from network to further analyze in R.

```r
my_expensive_function <- function(ids, type, params, ...) {
  match.arg(type, c('user', 'playlist', 'artist', 'song'))
  tmp <- pull_info_from_network(ids, type, params)
  tmp <- complex_transformation(tmp, ...)
  tmp
}
```

This is not an uncommon example and kind of mimics the structure of a typical
RDBMS.

Calling this function on one particular `id` is usually not too bad. However,
if you need to pull thousands of records into your R session it will take a while.

One possible solution to tackle this problem is to cache the results somewhere.
[cachemeifyoucan](http://github.com/robertzk/cachemeifyoucan)
uses PostgreSQL database as the backend for caching. There is an easy mapping
between data.frames and SQL tables so it's a pretty fast and robust solution.

Blink solves the same problem by using [Redis](http://redis.io) as the persistent
backend. Why Redis? It's fast, and it has some cool built-in data structures
that could be useful to implement this type of caching layer with.

This document is being written before the package development has started so at
this time I cannot share the benchmarks with you. However I can speculate :)

Quite a large number of people already use Redis as their caching layer for
web-apps. As they report, most of the read time is unfortunately being spent in
deserialization. I will rely on rredis deserialization, however it may be
beneficial to use `jsonlite` package and store results as a JSON.

One benefit of doing that is that you can actually build a website on top of the
Redis database and see the status of your caching layer.

Alternatively you can just build the website using
[microserver](http://github.com/robertzk/microserver) package.

Stay tuned.
