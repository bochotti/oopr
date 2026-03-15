## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("benchmark", {
skip();
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
R6q <- quote({
R6::R6Class("R6",
public = list(
  initialize = function(x = 1) self$x <- x
 ,getx       = function()      self$x
 ,inc        = function(n = 1) self$x <- self$x + n
 ,x          = NULL
))
})
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OPq <- quote({
oopr("OP",,
{
OP <- \(x = 1) { this$x <- x; }
public:
  getx <- \()      this$x;
  inc  <- \(n = 1) this$x <- this$x + n;
  x    <- NULL;
})
})
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
microbenchmark::microbenchmark(R6 = R6 <- eval(R6q), OP = eval(OPq))

lobstr::obj_sizes(R6, OP)

lobstr::obj_sizes(R6$new(), OP())

microbenchmark::microbenchmark(r6 <- R6$new(), op <- OP())

lobstr::obj_sizes(r6, op)

lobstr::obj_sizes(R6, r6, OP, op)

microbenchmark::microbenchmark(
  r6$x,      op$x
 ,r6$getx(), op$getx()
 ,r6$inc(),  op$inc()
)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
microbenchmark::microbenchmark(
  R6 = R6I <- R6::R6Class("R6I", inherit = R6)
 ,OP = oopr("OPI", public:OP, {})
)

lobstr::obj_sizes(R6I, OPI)

microbenchmark::microbenchmark(r6i <- R6I$new(), opi <- OPI())

lobstr::obj_sizes(r6i, opi)

lobstr::obj_sizes(R6I, r6i, OPI, opi)

microbenchmark::microbenchmark(
  r6i$x,      opi$x
 ,r6i$getx(), opi$getx()
 ,r6i$inc(),  opi$inc()
)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
})
