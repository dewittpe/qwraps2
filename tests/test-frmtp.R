library(qwraps2)

# Tests for frmtp
# LaTeX is the default markup language
ps <- c(0.2, 0.001, 0.00092, 0.047, 0.034781, 0.0000872, 0.787, 0.05, 0.043)

out_latex <-
  cbind("raw"       = ps,
        "default"   = frmtp(ps),
        "!leading0" = frmtp(ps, leading0 = FALSE),
        "3lower"    = frmtp(ps, digits = 3, case = "lower"),
        "PediDent"  = frmtp(ps, style = "pediatric_dentistry"),
        "obgyn"     = frmtp(ps, style = "obstetrics_gynecology")
  )

out_markdown <-
  cbind("raw"       = ps,
        "default"   = frmtp(ps, markup = "markdown"),
        "!leading0" = frmtp(ps, markup = "markdown", leading0 = FALSE),
        "3lower"    = frmtp(ps, markup = "markdown", digits = 3, case = "lower"),
        "PediDent"  = frmtp(ps, markup = "markdown", style = "pediatric_dentistry"),
        "obgyn"     = frmtp(ps, markup = "markdown", style = "obstetrics_gynecology")
  )

expected_latex <-
  structure(c("0.2", "0.001", "0.00092", "0.047", "0.034781", "8.72e-05",
              "0.787", "0.05", "0.043", "$P = 0.2000$", "$P = 0.0010$", "$P = 0.0009$",
              "$P = 0.0470$", "$P = 0.0348$", "$P < 0.0001$", "$P = 0.7870$",
              "$P = 0.0500$", "$P = 0.0430$", "$P = .2000$", "$P = .0010$",
              "$P = .0009$", "$P = .0470$", "$P = .0348$", "$P < .0001$", "$P = .7870$",
              "$P = .0500$", "$P = .0430$", "$p = 0.200$", "$p = 0.001$", "$p < 0.001$",
              "$p = 0.047$", "$p = 0.035$", "$p < 0.001$", "$p = 0.787$", "$p = 0.050$",
              "$p = 0.043$", "$P = .20$", "$P = .001$", "$P < .001$", "$P = .047$",
              "$P = .03$", "$P < .001$", "$P = .79$", "$P = .05$", "$P = .04$",
              "$P = .200$", "$P = .001$", "$P < .001$", "$P = .047$", "$P = .035$",
              "$P < .001$", "$P = .787$", "$P = .050$", "$P = .043$"), .Dim = c(9L,
              6L), .Dimnames = list(NULL, c("raw", "default", "!leading0",
                                            "3lower", "PediDent", "obgyn")))

expected_markdown <-
  structure(c("0.2", "0.001", "0.00092", "0.047", "0.034781", "8.72e-05",
              "0.787", "0.05", "0.043", "*P* = 0.2000", "*P* = 0.0010", "*P* = 0.0009",
              "*P* = 0.0470", "*P* = 0.0348", "*P* < 0.0001", "*P* = 0.7870",
              "*P* = 0.0500", "*P* = 0.0430", "*P* = .2000", "*P* = .0010",
              "*P* = .0009", "*P* = .0470", "*P* = .0348", "*P* < .0001", "*P* = .7870",
              "*P* = .0500", "*P* = .0430", "*p* = 0.200", "*p* = 0.001", "*p* < 0.001",
              "*p* = 0.047", "*p* = 0.035", "*p* < 0.001", "*p* = 0.787", "*p* = 0.050",
              "*p* = 0.043", "*P* = .20", "*P* = .001", "*P* < .001", "*P* = .047",
              "*P* = .03", "*P* < .001", "*P* = .79", "*P* = .05", "*P* = .04",
              "*P* = .200", "*P* = .001", "*P* < .001", "*P* = .047", "*P* = .035",
              "*P* < .001", "*P* = .787", "*P* = .050", "*P* = .043"), .Dim = c(9L,
              6L), .Dimnames = list(NULL, c("raw", "default", "!leading0",
                                            "3lower", "PediDent", "obgyn")))

stopifnot(identical(out_latex, expected_latex))
stopifnot(identical(out_markdown, expected_markdown))

