# Create example data
set.seed(1)
n_row <- 1000
cell_vals <- 1:2
x <- sample(cell_vals, n_row, replace = TRUE)
y <- sample(cell_vals, n_row, replace = TRUE)
z <- sample(cell_vals, n_row, replace = TRUE)
n_missing_x <- 50
n_missing_y <- 20
x[sample(1:n_row, n_missing_x, replace = FALSE)] <- NA
y[sample(1:n_row, n_missing_y, replace = FALSE)] <- NA
test_dat <- data.frame(x = x, y = y, z = z, stringsAsFactors = FALSE)
test_res <- qc_missingness(cols = c("x", "y"),
                           data = test_dat,
                           strata_col = "z",
                           format = FALSE)
formatted_res <- qc_missingness(cols = c("x", "y"),
                           data = test_dat,
                           strata_col = "z",
                           format = TRUE)
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
                 chisq.test(is.na(test_dat$x), test_dat$z)$p.value)
    expect_equal(as.numeric(test_res[y_row_idx, 11]),
                 chisq.test(is.na(test_dat$y), test_dat$z)$p.value)

    expect_equal(as.numeric(test_res[x_row_idx, 14]),
                 fisher.test(is.na(test_dat$x), test_dat$z)$p.value)
    expect_equal(as.numeric(test_res[y_row_idx, 14]),
                 fisher.test(is.na(test_dat$y), test_dat$z)$p.value)
})

test_that("produces expected formatted columns", {
    formatted_output_cols <-
        c("Missing_N(%)", "total", "z=1", "z=2", "chisq.test.p",
          "fisher.test.p")
    expect_equal(colnames(formatted_res),
                 formatted_output_cols)
})
