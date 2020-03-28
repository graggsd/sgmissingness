
# Calc total, number missing, and percent missing
n_perc_missing <- function(x) {
    missing_idx <- is.na(x)
    # Calc total, number missing, and percent missing
    n_total <- length(missing_idx)
    n_missing <- sum(missing_idx)
    perc_missing <- n_missing / n_total * 100
    out <- c(n_total = n_total,
             n_missing = n_missing,
             perc_missing = perc_missing)
    return(out)
}

get_p <- function(x, y, fun, ...) {
    out <- catch_conditions(fun, x, y, ...)
    if (!all(is.na(out[[1]]))) {
        out$result <- out$result$p.value
    }
    names(out) <- c("p", "warn", "err")
    out <- list(nm = out)
    names(out) <- deparse(substitute(fun))
    return(out)
}


#' @export
qc_missingness <-
    function(data,
             cols,
             strata_col = NULL,
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
                list(total = n_perc_missing(data[, col]))

            # Calc perc missing for each subset of x indexed by
            # each unique value of y
            if (multiple_strata_exist){
                strata <- sort(unique(data[, strata_col]))
                subgr_res_list <-
                    vector(mode = "list", length = length(strata))
                names(subgr_res_list) <- paste0(strata_col, "=", strata)

                for (i in 1:length(strata)) {
                    stratum_idx <- which(data[, strata_col] == strata[i])

                    # Calculate n and percent for subgroups
                    subgr_res_list[[i]] <-
                        n_perc_missing(data[stratum_idx, col])
                }

                # Calculate results of hypothesis tests
                hyp_test_list <- c(get_p(is.na(data[, col]),
                                         data[, strata_col],
                                         chisq.test),
                                   get_p(is.na(data[, col]),
                                         data[, strata_col],
                                         fisher.test))
            }
            # Combine lists
            complete_res_list[[col]] <-
                unlist(c(total_res_list, subgr_res_list, hyp_test_list))

        }

        # Format into data.frame with appropriate column names ----------------
        col_names <- names(complete_res_list[[1]])
        out <- data.frame(matrix(unlist(complete_res_list),
                                 nrow = length(complete_res_list),
                                 byrow = TRUE),
                          stringsAsFactors = FALSE)
        colnames(out) <- col_names
        out <- cbind(data.frame(Variable = cols,
                                stringsAsFactors = FALSE),
                     out)

        # Formatting for a more readable report ------------------------------
        if (format) {
            # Format percentages
            perc_idx <- grep("perc_missing$", colnames(out))
            for (i in perc_idx) {
                out[, i] <- sprintf('%.1f', as.numeric(out[, i]))
            }
            # Format p-values
            p_idx <- grep("\\.p$", colnames(out))
            for (i in p_idx) {
                out[, i] <- sprintf('%.3f', as.numeric(out[, i]))
            }
            # Rename columns to something more compact and readable

            # Get common prefixes for column names
            prefixes <- gsub("\\.n_total$",
                             "",
                             grep("\\.n_total$", colnames(out), value = TRUE))
            for (prefix in prefixes) {
                # Get location for appropriate "percent" and "n" columns
                # for a given prefix
                n_index <-
                    which(colnames(out) == paste0(prefix, ".n_missing"))
                perc_index <-
                    which(colnames(out) == paste0(prefix, ".perc_missing"))
                # At the position of the appropriate "n" column, replace
                # with a combined version of N and percent
                out[, n_index] <- paste0(out[, n_index], "(",
                                         out[, perc_index], ")")
                # Rename this column appropriately
                colnames(out)[n_index] <- prefix
            }
            to_remove <- c("n_total", "perc_missing", "warn", "err")
            pattern <- paste(paste0("\\.", to_remove, "$"), collapse = "|")
            omit_idx <- grep(pattern, colnames(out))
            out <- out[, -omit_idx]
            colnames(out)[1] <- "Missing_N(%)"
        } # FINISH FORMATTING  -------------------------
        return(out)
    }

