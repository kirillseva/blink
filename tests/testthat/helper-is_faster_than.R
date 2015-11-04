is_faster_than <- function(seconds, expr) {
  expect_less_than(system.time(eval(expr))['elapsed'], seconds)
}
