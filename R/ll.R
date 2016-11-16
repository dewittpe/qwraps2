#' @title List Object Aliases
#'
#' @description Aliases for \code{\link[base]{ls}} providing additional details.
#'
#' @references The basis for this work came from a Stack Overflow posting:
#' \url{http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session}
#'
#' @seealso \code{\link[base]{ls}}
#'
#' @param pos specifies the environment as a position in the search list
#' @param pattern an optional regular expression.  Only names matching
#' \code{pattern} are returned.  \code{\link[utils]{glob2rx}} can be used to
#' convert wildcard patterns to regular expressions.
#' @param order_by a character, order the results by \dQuote{Size} (default),
#' \dQuote{Type}, \dQuote{Rows}, or \dQuote{Columns}.
#' @param decreasing logical, defaults to \code{TRUE}, decreasing order? passed
#' to \code{\link[base]{order}}.
#' @param head logical, if \code{TRUE} then only return the first \code{n}
#' objects per \code{order_by} and \code{decreasing}.
#' @param n number of rows to return, ignored if \code{head = FALSE}.
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
#' e$fit <- lm(mpg ~ wt, mtcars)
#' e$fit2 <- lm(mpg ~ wt + am + vs, data = mtcars)
#' e$x <- rnorm(1e5)
#' e$y <- runif(1e4)
#' e$z <- with(e, x * y)
#' e$w <- sum(e$z)
#' ls(e)
#' ll(e)
#' ll(e, head = TRUE)
#'
#' @export
ll <- function (pos = 1, pattern, order_by = "Size", decreasing = TRUE, head = FALSE, n = 5) {
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
    out <- data.frame(obj.type, obj.size, obj.dim)
    names(out) <- c("Type", "Size", "Rows", "Columns")

    out <- out[order(out[[order_by]], decreasing = decreasing), ]

    if (head)
        out <- head(out, n)
    out
}

