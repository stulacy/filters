test_that("hampel works for window_size=3 and no recentre", {
    x <- seq(10)
    # Add outlier
    x[8] <- 1000
    out <- hampel(x, window_size=3, recentre=FALSE)
    exp <- c(NA, NA, 3:10)
    exp[8] <- 7
    expect_equal(out, exp)
})

test_that("hampel works for window_size=3 with recentre", {
    x <- seq(10)
    # Add outlier
    x[8] <- 1000
    out <- hampel(x, window_size=3, recentre=TRUE)
    exp <- c(NA, 2:9, NA)
    exp[8] <- 9
    expect_equal(out, exp)
})

test_that("hampel works for window_size=4 and no recentre", {
    x <- seq(10)
    # Add outlier
    x[6] <- 1000
    out <- hampel(x, window_size=4, recentre=FALSE)
    exp <- c(NA, NA, NA, 4:10)
    exp[6] <- 4.5
    expect_equal(out, exp)
})

# With an even window size the recentering 
# will leave a lag of half a sample as it isn't 
# symmetrical. The way it is implemented here
# is that a window on point i ranges from
# i-1, i, i+1, i+2
test_that("hampel works for window_size=4 with recentre", {
    x <- seq(10)
    # Add outlier
    x[6] <- 1000
    out <- hampel(x, window_size=4, recentre=TRUE)
    exp <- c(NA, 2:8, NA, NA)
    exp[6] <- 7.5
    expect_equal(out, exp)
})

# Single method only returns NA when the corresponding time-point was NA
test_that("hampel handles missing data with method 'single'", {
    x <- seq(10)
    x[4] <- NA
    # Add outlier
    x[3] <- 1000
    x[7] <- NA
    out <- hampel(x, window_size=3, recentre=FALSE, miss='single')
    exp <- c(NA, NA, 2, NA, 5, 6, NA, 8, 9, 10)
    expect_equal(out, exp)
})

# All miss method returns NA for all values in a window if any of them are NA
test_that("hampel handles missing data with method 'all'", {
    x <- seq(10)
    x[2] <- NA
    # Add outlier
    x[6] <- 1000
    x[8] <- NA
    out <- hampel(x, window_size=3, recentre=FALSE, miss='all')
    exp <- c(NA, NA, NA, NA, 5, 5, 7, NA, NA, NA)
    expect_equal(out, exp)
})

# None miss method omits missing values, so will only return NA if all window values are NA
test_that("hampel handles missing data with method 'none'", {
    x <- seq(10)
    x[2] <- NA
    # Add outlier
    x[6] <- 1000
    x[8] <- NA
    out <- hampel(x, window_size=3, recentre=FALSE, miss='none')
    exp <- c(NA, NA, 3, 4, 5, 5, 7, NA, 9, 10)
    expect_equal(out, exp)
})

# Now have an NA caused by missing a full window
test_that("hampel handles completely missing windows with method 'none'", {
    x <- seq(10)
    x[2:4] <- NA
    x[8] <- NA
    out <- hampel(x, window_size=3, recentre=FALSE, miss='none')
    exp <- c(NA, NA, NA, NA, 5, 6, 7, NA, 9, 10)
    expect_equal(out, exp)
})