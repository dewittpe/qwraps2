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
#' Default settings are controlled through the function arguments but should be
#' set via \code{options()}.
#' 
#' Default settings report the P-value exactly if P >
#' \code{getOptions("qwraps2_frmtp_digits", 2)} and reports 
#' P < \code{10^-(getOptions("qwraps2_frmtp_digits", 2))} otherwise.  By the
#' leading zero is controlled via 
#' \code{getOptions("qwraps2_frmtp_leading0", TRUE)} 
#' and a upper or lower case P is controlled by
#' \code{getOptions("qwraps2_frmtp_case", "upper")}.  These options are ignored 
#' if \code{style != "default"}.
#'
#' Journals with predefined P-value formatting
#' 
#' \itemize{
#'   \item Pediatric Dentistry:  \url{http://www.aapd.org/publications/} has the
#' following requirements as of March 2015.  
#'
#' If P > .01, the actual value for
#' P should be expressed to 2 digits.  Non-significant values should not be
#' expressed as "NS" whether or note P is significant, unless rounding a
#' significant P-value expressed to 3 digits would make it non significant (ie
#' P=.049, not P=.05).  If P<.01, it should be express to 3 digits (eg, P=.003,
#' not P<.05).  Actual P-values should be expressed unless P<.001, in which case
#' they should be so designated.
#'
#'   \item 
#' }
#'
#' @seealso
#' \code{\link{frmt}}
#'
#' @param x a vector of P-values to format
#' @param style a character string indicating a specific journal requirements
#' for p-value formatting.  
#' @param digits number of digits to the right of the decimal point.
#' @param markup a character string indicating if the output should be latex or
#' markup.
#' @param case a character string indicating if the output should be upper case
#' or lower case.
#' @param leading0 boolean, whether or not the p-value should be reported as
#' 0.0123 (TRUE, default), or .0123 (FALSE).
#' @param ... not currently implemented.
#'
#' @return a character vector of the formatted p-values
#'
#' @examples
#' 
#' ps <- c(0.2, 0.001, 0.00092, 0.047, 0.034781, 0.0000872, 0.787, 0.05, 0.043)
#' # LaTeX is the default markup language
#' cbind("raw"      = ps, 
#'       "default"  = frmtp(ps), 
#'       "3lower"   = frmtp(ps, digits = 3, case = "lower"),
#'       "PediDent" = frmtp(ps, style = "pediatric_dentistry"))
#'
#' # Using markdown
#' cbind("raw"      = ps, 
#'       "default"  = frmtp(ps, markup = "markdown"), 
#'       "3lower"   = frmtp(ps, digits = 3, case = "lower", markup = "markdown"),
#'       "PediDent" = frmtp(ps, style = "pediatric_dentistry", markup = "markdown"))
#'
#' @export   
frmtp <- function(x,
                  style    = getOption("qwraps2_journal", "default"), 
                  digits   = getOption("qwraps2_frmtp_digits", 2), 
                  markup   = getOption("qwraps2_markup", "latex"),
                  case     = getOption("qwraps2_frmtp_case", "upper"), 
                  leading0 = getOption("qwraps2_frmtp_leading0", TRUE), 
                  ...) {  

  rtn <- 
    switch(style,
           default             = frmtp_default(x, digits, case, leading0), 
           pediatric_dentistry = frmtp_pediatric_dentistry(x))

  if (markup == "latex") { 
    rtn <- paste0("$", rtn, "$")
  } else if (markup == "markdown") {
    rtn <- gsub("(P|p)", "\\*\\1\\*", rtn)
  }
  return(rtn)
}

frmtp_default <- function(x, digits, case, leading0) { 
  p_cutoff <- 10^-digits
  
  sapply(x, 
         function(xx) { 
           if (xx < p_cutoff) { 
             p_val <- paste("P <", formatC(p_cutoff, format = "g"))
           } else {
             p_val <- paste0("P = ", formatC(xx, digits = digits, format = "f")) 
           }

           if (case == "lower") { 
             p_val <- tolower(p_val)
           }

           if (!leading0) { 
             p_val <- gsub("0\\.", "\\.", p_val)
           }

           p_val
         })
}

frmtp_pediatric_dentistry <- function(p) { 
  sapply(p, function(x) {

         if (x < 0.001) { 
           p_val <- "< .001"
         } else if (x < 0.01) { 
           p_val <- paste0("= ", formatC(x, digits = 3, format = "f"))
         } else if (x < 0.05 & round(x, 2) == 0.05) { 
           p_val <- paste0("= ", formatC(x, digits = 3, format = "f"))
         } else { 
           p_val <- paste0("= ", formatC(x, digits = 2, format = "f"))
                             }
         paste0("P ", gsub("0\\.", "\\.", p_val))
      })
}

