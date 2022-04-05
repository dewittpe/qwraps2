library(qwraps2)

# expect %s% to work like a inline paste0
astr <- "test string 1"
bstr <- "abcd e"
cstr <- "  zyxw."
dstrs <- c("ONE", "TWO", "THREE")

stopifnot(astr %s% bstr %s% cstr == paste0(astr, bstr, cstr))
stopifnot(astr %s% dstrs %s% bstr == paste0(astr, dstrs, bstr))

# expect error if using non-character
stopifnot(class(try(astr %s% "1", silent = TRUE)) == "character")
stopifnot(class(try(astr %s%  1,  silent = TRUE)) == "try-error")

