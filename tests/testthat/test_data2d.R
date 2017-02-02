context("Testing data2d class")

# First arg = description of function
# Subsequent args = function for testing e.g. expect_equal


test_that("Check data2d reference class", {
    library(data.table)
    set.seed(2017)
    d <- data.table(
      site_id=c(rep("A", 50),rep("B", 50)),
      episode_id=rep(1:10,each=10),
      time=rep(1:10,len=10),
      val=round(rnorm(100,100,10)))
    # print(head(d))

    suppressMessages(hrate <- data2d(d,  name= "val"))

    x <- hrate$gen_summ()
    expect_equivalent(x$min, 74)
    expect_equivalent(x$max, 131)
    hrate_t <- hrate$summ_t(t0=0, t1=6)
    # print(hrate_t)
    expect_equivalent(hrate_t$length, 50)

    # Test the periods function
    x <- unlist(replicate(1e3, c(1,rep(NA,sample(10,1)))))
    y <- hrate$gen_summ(hrate$gen_periods(x))[c(1,2,4,5,6)]
    expect_equivalent(y,c(0,4,6,9,11))

    # print(hrate$summ_t)
    # print(hrate$summ_t(t0=5, t1=10))
    # expect_equivalent(hrate$summ_t(t0=5, t1=10)$length, 50)
})
