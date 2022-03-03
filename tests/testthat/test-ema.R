test_that("ema works for alpha=0.5", {
    alpha <- 0.5
    x <- seq(10)
    out <- ema(x, alpha=alpha)
    
    exp <- numeric(10)
    exp[1] <- x[1]
    prev_state <- exp[1]
    for (i in 2:10) {
        exp[i] <- alpha * x[i] + (1-alpha) * prev_state;
        prev_state <- exp[i];
    }
    expect_equal(out, exp)
})

test_that("ema works for alpha=0.1", {
    alpha <- 0.1
    x <- seq(10)
    out <- ema(x, alpha=alpha)
    
    exp <- numeric(10)
    exp[1] <- x[1]
    prev_state <- exp[1]
    for (i in 2:10) {
        exp[i] <- alpha * x[i] + (1-alpha) * prev_state;
        prev_state <- exp[i]
    }
    expect_equal(out, exp)
})

# Reset miss option results in NA for any missing in the raw time-series
# And then resets the filter to the current value when data is next available
# Just like how the filter works at the start of a series
test_that("ema works with reset miss method", {
    alpha <- 0.5
    x <- seq(10)
    x[4] <- NA
    out <- ema(x, alpha=alpha, miss='reset')
    
    exp <- numeric(10)
    exp[1] <- x[1]
    prev_state <- exp[1]
    for (i in 2:10) {
        if (is.na(prev_state) & !is.na(x[i])) {
            prev_state <- x[i]
        }
        exp[i] <- alpha * x[i] + (1-alpha) * prev_state;
        prev_state <- exp[i]
    }
    expect_equal(out, exp)
})

# carry miss option returns NA when the current time-point is missing but retains the
# previous filtered value for when data is next available
test_that("ema works with carry miss method", {
    alpha <- 0.5
    x <- seq(10)
    x[4] <- NA
    out <- ema(x, alpha=alpha, miss='carry')
    
    exp <- numeric(10)
    exp[1] <- x[1]
    prev_state <- exp[1]
    for (i in 2:10) {
        if (is.na(x[i])) {
            # Missing input leads to missing filtered
            exp[i] <- NA
            # Don't update previous state, carry it on
        } else {
            exp[i] <- alpha * x[i] + (1-alpha) * prev_state;
            prev_state <- exp[i]
        }
    }
    expect_equal(out, exp)
})

# carry_interpolate miss option returns the previous filtered value when the current time-point 
# is missing and also retains the previous filtered value for when data is next available
test_that("ema works with carry_interpolate method", {
    alpha <- 0.5
    x <- seq(10)
    x[4] <- NA
    out <- ema(x, alpha=alpha, miss='carry_interpolate')
    
    exp <- numeric(10)
    exp[1] <- x[1]
    prev_state <- exp[1]
    for (i in 2:10) {
        if (is.na(x[i])) {
            # Missing input returns the previous state
            exp[i] <- prev_state
            # Don't update previous state, carry it on
        } else {
            exp[i] <- alpha * x[i] + (1-alpha) * prev_state;
            prev_state <- exp[i]
        }
    }
    expect_equal(out, exp)
})

test_that("ema works with reset miss method with 2 consecutive missing values", {
    alpha <- 0.5
    x <- seq(10)
    x[4:5] <- NA
    out <- ema(x, alpha=alpha, miss='reset')
    
    exp <- numeric(10)
    exp[1] <- x[1]
    prev_state <- exp[1]
    for (i in 2:10) {
        if (is.na(prev_state) & !is.na(x[i])) {
            prev_state <- x[i]
        }
        exp[i] <- alpha * x[i] + (1-alpha) * prev_state;
        prev_state <- exp[i]
    }
    expect_equal(out, exp)
})

test_that("ema works with carry miss method with 2 consecutive missing values", {
    alpha <- 0.5
    x <- seq(10)
    x[4:5] <- NA
    out <- ema(x, alpha=alpha, miss='carry')
    
    exp <- numeric(10)
    exp[1] <- x[1]
    prev_state <- exp[1]
    for (i in 2:10) {
        if (is.na(x[i])) {
            # Missing input leads to missing filtered
            exp[i] <- NA
            # Don't update previous state, carry it on
        } else {
            exp[i] <- alpha * x[i] + (1-alpha) * prev_state;
            prev_state <- exp[i]
        }
    }
    expect_equal(out, exp)
})

test_that("ema works with carry_interpolate miss method with 2 consecutive missing values", {
    alpha <- 0.5
    x <- seq(10)
    x[4:5] <- NA
    out <- ema(x, alpha=alpha, miss='carry_interpolate')
    
    exp <- numeric(10)
    exp[1] <- x[1]
    prev_state <- exp[1]
    for (i in 2:10) {
        if (is.na(x[i])) {
            # Missing input returns the previous state
            exp[i] <- prev_state
            # Don't update previous state, carry it on
        } else {
            exp[i] <- alpha * x[i] + (1-alpha) * prev_state;
            prev_state <- exp[i]
        }
    }
    expect_equal(out, exp)
})

# TODO test handles first missing value