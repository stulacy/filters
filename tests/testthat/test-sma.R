test_that("sma works for window_size=3 and no recentre", {
    x <- seq(10)
    out <- sma(x, window_size=3, recentre=FALSE)
    exp <- c(NA, NA, 2:9)
    expect_equal(out, exp)
})

test_that("sma works for window_size=3 with recentre", {
    x <- seq(10)
    out <- sma(x, window_size=3, recentre=TRUE)
    exp <- c(NA, 2:9, NA)
    expect_equal(out, exp)
})

test_that("sma works for window_size=4 and no recentre", {
    x <- seq(10)
    out <- sma(x, window_size=4, recentre=FALSE)
    exp <- c(NA, NA, NA, seq(2.5, 8.5, by=1))
    expect_equal(out, exp)
})

# With an even window size the recentering 
# will leave a lag of half a sample as it isn't 
# symmetrical. The way it is implemented here
# is that a window on point i ranges from
# i-1, i, i+1, i+2
test_that("sma works for window_size=4 with recentre", {
    x <- seq(10)
    out <- sma(x, window_size=4, recentre=TRUE)
    exp <- c(NA, seq(2.5, 8.5, by=1), NA, NA)
    expect_equal(out, exp)
})

# Single method only returns NA when the corresponding time-point was NA
test_that("sma handles missing data with method 'single'", {
    x <- seq(10)
    x[4] <- NA
    x[7] <- NA
    out <- sma(x, window_size=3, recentre=FALSE, miss='single')
    exp <- c(NA, NA, 2, NA, 4, 5.5, NA, 7, 8.5, 9)
    expect_equal(out, exp)
})

# All miss method returns NA for all values in a window if any of them are NA
test_that("sma handles missing data with method 'all'", {
    x <- seq(10)
    x[2] <- NA
    x[8] <- NA
    out <- sma(x, window_size=3, recentre=FALSE, miss='all')
    exp <- c(NA, NA, NA, NA, 4, 5, 6, NA, NA, NA)
    expect_equal(out, exp)
})

# None miss method omits missing values, so will only return NA if all window values are NA
test_that("sma handles missing data with method 'none'", {
    x <- seq(10)
    x[2] <- NA
    x[8] <- NA
    out <- sma(x, window_size=3, recentre=FALSE, miss='none')
    exp <- c(NA, NA, 2, 3.5, 4, 5, 6, 6.5, 8, 9.5)
    expect_equal(out, exp)
})

# Now have an NA caused by missing a full window
test_that("sma handles completely missing windows with method 'none'", {
    x <- seq(10)
    x[2:4] <- NA
    x[8] <- NA
    out <- sma(x, window_size=3, recentre=FALSE, miss='none')
    exp <- c(NA, NA, 1, NA, 5, 5.5, 6, 6.5, 8, 9.5)
    expect_equal(out, exp)
})