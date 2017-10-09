pkgs <- read.dcf("DESCRIPTION", fields = c("Imports", "Suggests", "LinkingTo", "VignetteBuilder"))
pkgs <- as.character(pkgs)
pkgs <- unlist(strsplit(pkgs, ",\n"))
pkgs <- gsub("\\s+\\(.+\\)", "", pkgs)
pkgs <- na.omit(pkgs)
attributes(pkgs) <- NULL

unavailable_pkgs <- c()
pkgs <- pkgs[!(pkgs %in% rownames(installed.packages()))]

while(length(pkgs) > 0) {
  x <- try(install.packages(pkgs[1], repos = "http://cran.rstudio.com"))

  if (class(x) == "try-error") {
    unavailable_pkgs <- c(unavailable_pkgs, pkgs[1])
    pkgs <- pkgs[-1]
  } else {
    pkgs <- pkgs[!(pkgs %in% rownames(installed.packages()))]
  }
}

if (length(unavailable_pkgs)) {
  cat("Packages that were not installed:", unavailable_pkgs, sep = "\n  ")
}

