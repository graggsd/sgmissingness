catch_cond_factory <- function(fun) {
    # Create new function that outputs a list, including the results of
    # the input function as well as the text from any error messages
    # or warnings
    out <- function(...) {

        # Set up placeholder messages for errors and warnings
        err <- ""
        warn <- NULL

        # Capture results from successfully evaluated function or any
        # resulting alternative in the case of an error
        res <-
            withCallingHandlers(
                # Error handling
                tryCatch(
                    # Evaluated expression - if no error, the output of
                    # argument 'fun' will be assigned to 'res'
                    fun(...),
                    # Error handler
                    error = function(e) {
                        # Assign results of error message to err in parent
                        # environment
                        err <<- conditionMessage(e)
                        # Return NA as inner function output if an error occurs
                        return(NA)
                    }
                ),
                # Warning handling
                warning = function(w) {
                    # Assign results of warning message to warn in parent
                    # environment
                    warn <<- c(warn, conditionMessage(w))
                    invokeRestart("muffleWarning")
                }
            )

        # Combine values in warn
        if (!is.null(warn)) {
            warn <- paste(paste0("Warning ", 1:length(warn), ": "),
                          warn,
                          collapse = "; ")
        } else {
            warn <- ""
        }

        # Create output for newly constructed function
        # argument
        out_inner <- list(result = res,  warning = warn, error = err)
        return(out_inner)
    }
    return(out)
}

catch_conditions <- function(fun, ...) {
    # Set up placeholder messages for errors and warnings
    err <- ""
    warn <- NULL

    # Capture results from successfully evaluated function or any
    # resulting alternative in the case of an error
    res <-
        withCallingHandlers(
            # Error handling
            tryCatch(
                # Evaluated expression - if no error, the output of
                # argument 'fun' will be assigned to 'res'
                fun(...),
                # Error handler
                error = function(e) {
                    # Assign results of error message to err in parent
                    # environment
                    err <<- conditionMessage(e)
                    # Return NA as inner function output if an error occurs
                    return(NA)
                }
            ),
            # Warning handling
            warning = function(w) {
                # Assign results of warning message to warn in parent
                # environment
                warn <<- c(warn, conditionMessage(w))
                invokeRestart("muffleWarning")
            }
        )

    # Combine values in warn
    if (!is.null(warn)) {
        warn <- paste(paste0("Warning ", 1:length(warn), ": "),
                      warn,
                      collapse = "; ")
    } else {
        warn <- ""
    }

    # Create output for newly constructed function
    # argument
    out <- list(result = res,  warning = warn, error = err)
    return(out)
}
