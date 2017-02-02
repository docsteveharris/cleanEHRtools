#' @title hello I am testing this package
#'
#' @description
#' tests the package build

# Now let's try creating a class for 2d data items
# Use reference class objects
hello <- setRefClass("hello",
                      fields = list(
                                    a="character",
                                    b="data.table",
                                    c="numeric"
                                    ),
                      methods =list(
                                    initialize = function(a, b) { # execute when initialised.
                                        message("Initializing your ass")
                                        .self$a <- paste("Hello", a) # assign values here
                                        .self$b <- b
                                        .self$c <- mean(b$x)
                                    }))
