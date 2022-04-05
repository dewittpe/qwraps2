#' Operators
#'
#' A set of helpful operators to make writing and basic data analysis easier.
#'
#' @param e1 a character string
#' @param e2 a character string
#'
#' @export
#' @rdname qwraps2-utilities
#' @examples
#'
#' # base R
#' paste0("A longer string ", "can be ", "built")
#'
#' # with the %s% operator
#' "A longer string " %s% "can be " %s% "built"
#'
"%s%" <- function(e1, e2) {
  if(!inherits(e1, "character") | !inherits(e2, "character")) {
    stop("%s% will not coerce to strings.  That is, 'a' %s% 1 will fail, 'a' %s% '1' will work")
  }
  paste0(e1, e2)
}

# inspired by overloadding the + operator
# `+` <- function(e1, e2) {
#   if(inherits(e1, "character") & inherits(e2, "character")) {
#     paste0(e1, e2)
#   } else {
#     .Primitive("+")(e1, e2)
#   }
# }
