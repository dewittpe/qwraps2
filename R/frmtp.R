#' @title Format P-values
#'
#' @description A function for formating P-values in general and in journal
#' specific formats.
#'
#' @details
#' The single function call to \code{frmtp} formats P-values per journal
#' requirements.  As I work on papers aimed at different journals, the
#' formatting functions will be extended to match.
#'
#' Default settings are controlled through \code{options()}.
#' 
#' Default settings report the P-value exactly if P >
#' \code{getOptions("qwraps2_frmtp_digits, 2)} and reports ''P <
#' \code{10^-(getOptions("qwraps2_frmtp_digits, 2))} otherwise.  By the leading
#' zero is controlled via \code{getOptions("qwraps2_frmpt_leading0", TRUE)} and
#' a captial or lower case P is controled by
#' \code{getOptions("qwraps2_frmpt_case", "upper")}.
#' These options are ignored if \code{style != "Default"}.
#'
#' Journals with predefined P-value formatting
#' 
#' \item{Pediatric Dentistry:}  \url{http://www.aapd.org/publications/} has the
#' following requirements as of March 2015.  ``If P > .01, the actual value for
#' P should be expressed to 2 digits.  Non-significant values should not be
#' expressed as "NS" whether or note P is significant, unless rounding a
#' significant P-value expressed to 3 digits would make it non significant (ie
#' P=.049, not P=.05).  If P<.01, it should be express to 3 digits (eg, P=.003,
#' not P<.05).  Actual P-values should be expressed unless P<.001, in which case
#' they should be so designated.''
#'
#' @seealso
#' \code{\link{frmt}}
#'
#' @param x a vector of P-values to format
frmtp <- function(x) {  
  # sytle <- getOption("qwraps2_journal", "Default")
  # markup <-  getOption("qwraps2_markup", "latex")
  f <- match.fun(paste0("frmtp_"), sytle)
  f(x) 
}

frmtp_defualt <- function(x) { 
  p_cutoff <- 10^-getOptions("qwraps2_frmtp_digits", 2)
  
  if (x < p_cutoff) { 
    p_val <- "< .01"
  } else {
    p_val <- paste0("= ", formatC(x, digits = getOptions("qwraps2_frmtp_digits", 2), format = "f")) 
  }
  p_val
}


frmtp_pediatric_dentistry <- function(p) { 
  sapply(p, function(x) {

         if (x < 0.001) { 
           p_val <- "< .001"
         } else if (x < 0.01) { 
           p_val <- paste0("= ", formatC(x, digits = 3, format = "f")) %>% 
                    gsub("0\\.", "\\.", .)
         } else if (x < 0.05 & round(x, 2) == 0.05) { 
           p_val <- paste0("= ", formatC(x, digits = 3, format = "f")) %>% 
                    gsub("0\\.", "\\.", .)
         } else { 
           p_val <- paste0("= ", formatC(x, digits = 2, format = "f")) %>% 
                    gsub("0\\.", "\\.", .)
         }
         paste0("$P ", p_val, "$")
      })
}

# Test for frmtp
# ps <- c(0.1, 0.2, 0.001, 0.00092, 0.047, 0.034781, 0.000000872, 0.787887, 0.05,
#         0.043)
# cbind(ps, frmtp(ps))

