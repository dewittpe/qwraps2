#' GNU Make like check if traget(s) need to be updated
#'
#' Use modification time and/or checksums of prerequisites to determine if the
#' target(s) need to be updated.  Similar logic to that used by GNU Make.
#'
#' @param target a character vector of paths to targets
#' @param prerequisite a character vector of dependencies
#'
#'
#' @name check_target
NULL

#' @rdname check_target
#' @export
check_target_mtime <- function(target, prerequisite, verbose) {
  DF <- data.frame(file = c(target, prerequisite),
                   role = c(rep('target', length(target)), rep('prerequisite', length(prerequisite))))
  DF[["exists"]] <- file.exists(c(target, prerequisite))
  DF[["mtime"]] <- file.info(c(target, prerequisite))[["mtime"]]

  # find which prerequisite(s) are younger than the target(s)
  DF <- split(DF, DF$role)
  targets <- DF[["target"]]
  prereqs <- DF[["prerequisite"]]

  younger_prereqs <-
    lapply(targets$mtime,
           function(mt) {
             rtn <- prereqs$file[ prereqs$mtime > mt ]
             rtn[!is.na(rtn)]
           })

  targets[["needs_rebuild"]] <- sapply(younger_prereqs, function(x) length(x) > 0)
  targets[["rebuild_because"]] <- sapply(younger_prereqs, function(x) if(length(x) > 0) {paste("older than:", paste(x, collapse = ", "))})

  idx <- !targets[["exists"]]
  targets[["needs_rebuild"]][idx] <- TRUE
  targets[["rebuild_because"]][idx] <- "target does not exist"

  targets
}
