
# Make logical index of missing values
mk_missing_idx <- function(x, missing_vals = NULL) {
    x <- as.character(x)
    na_idx <- is.na(x)
    if (is.null(missing_vals)) {
        missing_idx <- na_idx
    } else {
        missing_vals <- as.character(missing_vals)
        missing_idx <- na_idx | (x %in% missing_vals)
    }
    return(missing_idx)
}

# Calc total, number missing, and percent missing
n_perc_missing <- function(missing_idx) {
    # Calc total, number missing, and percent missing
    n_total <- length(missing_idx)
    n_missing <- sum(missing_idx)
    perc_missing <- n_missing / n_total * 100
    out <- list(n_total = n_total,
                n_missing = n_missing,
                perc_missing = perc_missing)
    return(out)
}

# Get pvalue and any warning or error messages for pearson's chi-squared test
pearson_p <- function(tab) {
    out <- catch_conditions(chisq.test, tab)
    out$result <- out$result$p.value
    names(out) <- c("p", "warn", "err")
    return(out)
}

# Get p-value and any warning or error messages for fisher's exact test
fisher_p <- function(tab) {
    out <- catch_conditions(fisher.test, tab)
    out$result <- out$result$p.value
    names(out) <- c("p", "warn", "err")
    return(out)
}

#' @export
qc_missingness <-
    function(cols,
             data,
             strata_col = NULL,
             missing_vals = NULL,
             format = TRUE) {

        multiple_strata_exist <-
            !is.null(strata_col) && length(unique(data[, strata_col])) > 1

        # Initialize empty list to hold all results
        complete_res_list <- vector(mode = "list", length = length(cols))
        names(complete_res_list) <- cols

        for (col in cols) {
            # Create/reset placeholders for subgroups and hypothesis testing lists
            subgr_res_list <- NULL
            hyp_test_list <- NULL

            # Calc perc missing for entire column
            total_res_list <-
                list(total =
                         n_perc_missing(
                             mk_missing_idx(data[, col], missing_vals)
                         ))

            # Calc perc missing for each subset of x indexed by
            # each unique value of y
            if (multiple_strata_exist){
                strata <- sort(unique(data[, strata_col]))
                subgr_res_list <-
                    vector(mode = "list", length = length(strata))
                names(subgr_res_list) <- paste0(strata_col, "=", strata)

                for (i in 1:length(strata)) {
                    stratum_idx <- which(data[, strata_col] == strata[i])

                    subgr_res_list[[i]] <-
                        n_perc_missing(
                            mk_missing_idx(data[, col][stratum_idx],
                                           missing_vals)
                        )
                }

                tab <- table(data[, col], data[, strata_col])

                hyp_test_list <- list(pearson = pearson_p(tab),
                                      fisher = fisher_p(tab))

            }
            # Combine lists
            complete_res_list[[col]] <-
                unlist(c(total_res_list, subgr_res_list, hyp_test_list))
        }
        col_names <- names(complete_res_list[[1]])
        out <- data.frame(matrix(unlist(complete_res_list),
                                 nrow = length(complete_res_list),
                                 byrow = TRUE),
                          stringsAsFactors = FALSE)
        colnames(out) <- col_names
        out <- cbind(data.frame(Variable = cols, stringsAsFactors = FALSE),
                     out)
        if (format) {
            # Format percentages
            perc_idx <- grep("perc_missing$", colnames(out))
            for (i in perc_idx) {
                out[, i] <- sprintf('%.1f', as.numeric(out[, i]))
            }
            p_idx <- grep("\\.p$", colnames(out))
            for (i in p_idx) {
                out[, i] <- sprintf('%.3f', as.numeric(out[, i]))
            }
            suffixes <- gsub("\\.n_total$",
                             "",
                             grep("\\.n_total$", colnames(out), value = TRUE))
            for (suffix in suffixes) {
                n_index <-
                    which(colnames(out) == paste0(suffix, ".n_missing"))
                perc_index <-
                    which(colnames(out) == paste0(suffix, ".perc_missing"))
                out[, n_index] <- paste0(out[, n_index], "(",
                                         out[, perc_index], ")")
                colnames(out)[n_index] <- paste0(suffix, ", Missing N(%)")
            }
            to_remove <- c("n_total", "perc_missing", "warn", "err")
            pattern <- paste(paste0("\\.", to_remove, "$"), collapse = "|")
            omit_idx <- grep(pattern, colnames(out))
            out <- out[, -omit_idx]
        }
        return(out)
    }
