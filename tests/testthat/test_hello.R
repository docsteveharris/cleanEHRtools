context("Testing package setup")

# First arg = description of function
# Subsequent args = function for testing e.g. expect_equal

test_that("Check example reference class", {
    df <- data.table(x=seq(5), y=seq(5))
    suppressMessages(obj <- hello(a="x", b=df))
    # print(obj$a)})
    expect_equivalent(obj$a, "Hello x")
    expect_equivalent(obj$c, 3)
})
