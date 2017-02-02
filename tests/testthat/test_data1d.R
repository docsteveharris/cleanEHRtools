context("Testing data1d class")

# First arg = description of function
# Subsequent args = function for testing e.g. expect_equal


test_that("Check data1d reference class", {
    library(data.table)
    # library(testthat)
    set.seed(2017)
    d <- data.table(
            site_id=c(rep("A", 50),rep("B", 50)),
            episode_id=rep(1:50,times=2),
            val=sample(letters[1:10], 100, replace=T))

    x <- data1d(d,  name= "val")

    str(x)
    x$tab()
    expect_equivalent(x$tab()[['a']], 8)
    expect_equivalent(x$tab()[['b']], 14)
    expect_equivalent(length(x$tab()), 11)


})
