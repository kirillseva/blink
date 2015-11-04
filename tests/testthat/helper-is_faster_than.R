is_faster_than <- function(seconds, expr) {
  expect_true(takes_less_than(seconds)(eval(expr)))
}
