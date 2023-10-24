#' @title List Object Aliases
#'
#' @description Aliases for \code{\link[base]{ls}} providing additional details.
#'
#' @references The basis for this work came from a Stack Overflow posting:
#' \url{https://stackoverflow.com/q/1358003/1104685}
#'
#' @seealso \code{\link[base]{ls}}
#'
#' @param pos specifies the environment as a position in the search list
#' @param pattern an optional regular expression.  Only names matching
#' \code{pattern} are returned.  \code{\link[utils]{glob2rx}} can be used to
#' convert wildcard patterns to regular expressions.
#' @param order_by a character, order the results by \dQuote{object},
#' \dQuote{size} (default), \dQuote{class}, \dQuote{rows}, or \dQuote{columns}.
#' @param decreasing logical, defaults to \code{TRUE}, decreasing order? passed
#' to \code{\link[base]{order}}.
#'
#' @return a data.frame with columns
#' \itemize{
#'  \item object: name of the object
#'  \item class: class, or mode if class is not present, of the object
#'  \item size: approximate size, in bytes, of the object in memory
#'  \item rows: number of rows for data.frames or matrices, or the number of
#'  elements for a list like structure
#'  \item columns: number of columns for data.frames or matrices
#' }
#'
#' @examples
#' # View your current workspace
#' \dontrun{
#' ls()
#' ll()
#' }
#'
#' # View another environment
#' e <- new.env()
#' ll(e)
#'
#' e$fit <- lm(mpg ~ wt, mtcars)
#' e$fit2 <- lm(mpg ~ wt + am + vs, data = mtcars)
#' e$x <- rnorm(1e5)
#' e$y <- runif(1e4)
#' e$z <- with(e, x * y)
#' e$w <- sum(e$z)
#'
#' ls(e)
#' ll(e)
#'
#' @export
ll <- function (pos = 1, pattern, order_by = "size", decreasing = order_by %in% c("size", "rows", "columns")) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    if (length(names) == 0) {
      return(names)
    }
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, utils::object.size)
    obj.dim <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame("object" = names,
                      "class" = obj.type,
                      "size"  = obj.size,
                      "rows"  = obj.dim[, 1],
                      "columns" = obj.dim[, 2])

    out <- out[order(out[[order_by]], decreasing = decreasing), ]

    rownames(out) <- NULL

    out
}

