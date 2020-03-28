
# Create example data
set.seed(1)
n_row <- 100
cell_vals <- 1:2
x <- sample(cell_vals, n_row, replace = TRUE)
y <- sample(cell_vals, n_row, replace = TRUE)
z <- sample(cell_vals, n_row, replace = TRUE)
n_missing_x <- 20
n_missing_y <- 10
x[sample(1:n_row, n_missing_x, replace = FALSE)] <- NA
y[sample(1:n_row, n_missing_y, replace = FALSE)] <- NA
test_dat <- data.frame(x = x, y = y, z = z, stringsAsFactors = FALSE)
test_res <- qc_missingness(c("x", "y"),
                           data = test_dat,
                           strata_col = "z",
                           format = FALSE)
n_row <- as.character(n_row)
n_missing_x <- as.character(n_missing_x)
n_missing_y <- as.character(n_missing_y)
test_dat_2 <- test_dat[test_dat$z == 1, ]
test_dat_3 <- test_dat[test_dat$z == 2, ]
x_row_idx <- test_res$Variable == "x"
y_row_idx <- test_res$Variable == "y"

test_that("counts total values appropriately", {
    expect_equal(test_res[x_row_idx, 2], n_row)
    expect_equal(test_res[y_row_idx, 2], n_row)
})

test_that("counts total missing appropriately", {
    expect_equal(test_res[x_row_idx, 3], n_missing_x)
    expect_equal(test_res[y_row_idx, 3], n_missing_y)
})

test_that("counts percentages appropriately", {
    expect_equal(as.numeric(test_res[x_row_idx, 4]),
                 sum(is.na(test_dat$x))/as.numeric(n_row)*100)
    expect_equal(as.numeric(test_res[y_row_idx, 4]),
                 sum(is.na(test_dat$y))/as.numeric(n_row)*100)

    test_dat_2 <- test_dat[test_dat$z == 1, ]
    expect_equal(as.numeric(test_res[x_row_idx, 7]),
                 sum(is.na(test_dat_2$x))/nrow(test_dat_2)*100)
    expect_equal(as.numeric(test_res[y_row_idx, 7]),
                 sum(is.na(test_dat_2$y))/nrow(test_dat_2)*100)

    test_dat_3 <- test_dat[test_dat$z == 2, ]
    expect_equal(as.numeric(test_res[x_row_idx, 10]),
                 sum(is.na(test_dat_3$x))/nrow(test_dat_3)*100)
    expect_equal(as.numeric(test_res[y_row_idx, 10]),
                 sum(is.na(test_dat_3$y))/nrow(test_dat_3)*100)
})

test_that("records p-values appropriately", {
    expect_equal(as.numeric(test_res[x_row_idx, 11]),
                 chisq.test(test_dat$x, test_dat$z)$p.value)
    expect_equal(as.numeric(test_res[y_row_idx, 11]),
                 chisq.test(test_dat$y, test_dat$z)$p.value)

    expect_equal(as.numeric(test_res[x_row_idx, 14]),
                 fisher.test(test_dat$x, test_dat$z)$p.value)
    expect_equal(as.numeric(test_res[y_row_idx, 14]),
                 fisher.test(test_dat$y, test_dat$z)$p.value)
})


