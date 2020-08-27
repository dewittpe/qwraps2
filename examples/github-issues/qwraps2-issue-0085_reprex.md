    # update qwraps2 to the development version
    # devtools::install_github("dewittpe/qwraps2")
    library(qwraps2)

    first_func <- function() {
      myresults <- list(
        preds = as.factor(c(0, 1, 0, 1, 0, 1, 0, 0, 0)),
        refs  = as.factor(c(0, 1, 0, 1, 0, 1, 0, 1, 1))
      )

      # This would error in verion 0.4.2
      tryCatch({ second_func(func_input = myresults) }, error=function(e) { print('second_func failed') })

      # This would error in version 0.4.2
      tryCatch({ third_func(func_input = myresults) }, error=function(e) { print('third_func failed') })

      # This would not error in version 0.4.2
      tryCatch({ fourth_func(func_input = myresults) }, error=function(e) { print('fourth_func failed')
      })

      # This would not error in version 0.4.2
      tryCatch({ fifth_func(func_input = myresults) }, error=function(e) { print('fifth_func failed') })
    }

    second_func <- function(func_input) {
      print('called second_func')
      print(confusion_matrix(func_input$preds, func_input$refs))
    }

    third_func <- function(func_input) {
      print('called third_func')
      third_func_var <- func_input
      print(confusion_matrix(third_func_var$preds, third_func_var$refs))
    }

    fourth_func <- function(func_input) {
      print('called fourth_func')
      fourth_preds <- func_input$preds
      fourth_refs <- func_input$refs
      print(confusion_matrix(fourth_preds, fourth_refs))
    }

    fifth_func <- function(func_input) {
      print('called fifth_func')
      print(confusion_matrix(refs ~ preds, data=func_input))
    }

    first_func()
    #> [1] "called second_func"
    #> 
    #> Truth:           func_input$refs 
    #> Prediction:      func_input$preds 
    #> Positive Value:  0 
    #> 
    #>                     True Condition
    #> Prediction condition pos. (0) neg. (1)
    #>             pos. (0)        4        2
    #>             neg. (1)        0        3
    #> 
    #>                   Est       LCL       UCL
    #> Accuracy    0.7777778 0.4525890 0.9367749
    #> Sensitivity 1.0000000 0.7008550 1.0000000
    #> Specificity 0.6000000 0.3004752 0.8396957
    #> PPV         0.6666667 0.3542021 0.8794162
    #> NPV         1.0000000 0.7008550 1.0000000
    #> FNR         0.0000000 0.0000000 0.2991450
    #> FPR         0.4000000 0.1603043 0.6995248
    #> FDR         0.3333333 0.1205838 0.6457979
    #> FOR         0.0000000 0.0000000 0.2991450
    #> F1          0.8000000        NA        NA
    #> MCC         0.6324555        NA        NA
    #> [1] "called third_func"
    #> 
    #> Truth:           third_func_var$refs 
    #> Prediction:      third_func_var$preds 
    #> Positive Value:  0 
    #> 
    #>                     True Condition
    #> Prediction condition pos. (0) neg. (1)
    #>             pos. (0)        4        2
    #>             neg. (1)        0        3
    #> 
    #>                   Est       LCL       UCL
    #> Accuracy    0.7777778 0.4525890 0.9367749
    #> Sensitivity 1.0000000 0.7008550 1.0000000
    #> Specificity 0.6000000 0.3004752 0.8396957
    #> PPV         0.6666667 0.3542021 0.8794162
    #> NPV         1.0000000 0.7008550 1.0000000
    #> FNR         0.0000000 0.0000000 0.2991450
    #> FPR         0.4000000 0.1603043 0.6995248
    #> FDR         0.3333333 0.1205838 0.6457979
    #> FOR         0.0000000 0.0000000 0.2991450
    #> F1          0.8000000        NA        NA
    #> MCC         0.6324555        NA        NA
    #> [1] "called fourth_func"
    #> 
    #> Truth:           fourth_refs 
    #> Prediction:      fourth_preds 
    #> Positive Value:  0 
    #> 
    #>                     True Condition
    #> Prediction condition pos. (0) neg. (1)
    #>             pos. (0)        4        2
    #>             neg. (1)        0        3
    #> 
    #>                   Est       LCL       UCL
    #> Accuracy    0.7777778 0.4525890 0.9367749
    #> Sensitivity 1.0000000 0.7008550 1.0000000
    #> Specificity 0.6000000 0.3004752 0.8396957
    #> PPV         0.6666667 0.3542021 0.8794162
    #> NPV         1.0000000 0.7008550 1.0000000
    #> FNR         0.0000000 0.0000000 0.2991450
    #> FPR         0.4000000 0.1603043 0.6995248
    #> FDR         0.3333333 0.1205838 0.6457979
    #> FOR         0.0000000 0.0000000 0.2991450
    #> F1          0.8000000        NA        NA
    #> MCC         0.6324555        NA        NA
    #> [1] "called fifth_func"
    #> 
    #> Truth:           refs 
    #> Prediction:      preds 
    #> Positive Value:  0 
    #> 
    #>                     True Condition
    #> Prediction condition pos. (0) neg. (1)
    #>             pos. (0)        4        2
    #>             neg. (1)        0        3
    #> 
    #>                   Est       LCL       UCL
    #> Accuracy    0.7777778 0.4525890 0.9367749
    #> Sensitivity 1.0000000 0.7008550 1.0000000
    #> Specificity 0.6000000 0.3004752 0.8396957
    #> PPV         0.6666667 0.3542021 0.8794162
    #> NPV         1.0000000 0.7008550 1.0000000
    #> FNR         0.0000000 0.0000000 0.2991450
    #> FPR         0.4000000 0.1603043 0.6995248
    #> FDR         0.3333333 0.1205838 0.6457979
    #> FOR         0.0000000 0.0000000 0.2991450
    #> F1          0.8000000        NA        NA
    #> MCC         0.6324555        NA        NA

<sup>Created on 2020-08-27 by the [reprex package](https://reprex.tidyverse.org) (v0.3.0)</sup>
<details>
<summary>
Session info
</summary>

    devtools::session_info()
    #> ─ Session info ───────────────────────────────────────────────────────────────
    #>  setting  value                       
    #>  version  R version 4.0.2 (2020-06-22)
    #>  os       macOS Catalina 10.15.6      
    #>  system   x86_64, darwin17.0          
    #>  ui       X11                         
    #>  language (EN)                        
    #>  collate  en_US.UTF-8                 
    #>  ctype    en_US.UTF-8                 
    #>  tz       America/Denver              
    #>  date     2020-08-27                  
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  package     * version    date       lib source        
    #>  assertthat    0.2.1      2019-03-21 [1] CRAN (R 4.0.0)
    #>  backports     1.1.9      2020-08-24 [1] CRAN (R 4.0.2)
    #>  callr         3.4.3      2020-03-28 [1] CRAN (R 4.0.0)
    #>  cli           2.0.2      2020-02-28 [1] CRAN (R 4.0.0)
    #>  crayon        1.3.4      2017-09-16 [1] CRAN (R 4.0.0)
    #>  desc          1.2.0      2018-05-01 [1] CRAN (R 4.0.0)
    #>  devtools      2.3.1      2020-07-21 [1] CRAN (R 4.0.2)
    #>  digest        0.6.25     2020-02-23 [1] CRAN (R 4.0.0)
    #>  ellipsis      0.3.1      2020-05-15 [1] CRAN (R 4.0.0)
    #>  evaluate      0.14       2019-05-28 [1] CRAN (R 4.0.0)
    #>  fansi         0.4.1      2020-01-08 [1] CRAN (R 4.0.0)
    #>  fs            1.5.0      2020-07-31 [1] CRAN (R 4.0.2)
    #>  glue          1.4.1      2020-05-13 [1] CRAN (R 4.0.0)
    #>  highr         0.8        2019-03-20 [1] CRAN (R 4.0.0)
    #>  htmltools     0.5.0      2020-06-16 [1] CRAN (R 4.0.0)
    #>  knitr         1.29       2020-06-23 [1] CRAN (R 4.0.0)
    #>  magrittr      1.5        2014-11-22 [1] CRAN (R 4.0.0)
    #>  memoise       1.1.0      2017-04-21 [1] CRAN (R 4.0.0)
    #>  pkgbuild      1.1.0      2020-07-13 [1] CRAN (R 4.0.2)
    #>  pkgload       1.1.0      2020-05-29 [1] CRAN (R 4.0.0)
    #>  prettyunits   1.1.1      2020-01-24 [1] CRAN (R 4.0.0)
    #>  processx      3.4.3      2020-07-05 [1] CRAN (R 4.0.0)
    #>  ps            1.3.4      2020-08-11 [1] CRAN (R 4.0.2)
    #>  qwraps2     * 0.4.2.9006 2020-08-26 [1] local         
    #>  R6            2.4.1      2019-11-12 [1] CRAN (R 4.0.0)
    #>  Rcpp          1.0.5      2020-07-06 [1] CRAN (R 4.0.0)
    #>  remotes       2.2.0      2020-07-21 [1] CRAN (R 4.0.2)
    #>  rlang         0.4.7      2020-07-09 [1] CRAN (R 4.0.2)
    #>  rmarkdown     2.3        2020-06-18 [1] CRAN (R 4.0.0)
    #>  rprojroot     1.3-2      2018-01-03 [1] CRAN (R 4.0.0)
    #>  sessioninfo   1.1.1      2018-11-05 [1] CRAN (R 4.0.0)
    #>  stringi       1.4.6      2020-02-17 [1] CRAN (R 4.0.0)
    #>  stringr       1.4.0      2019-02-10 [1] CRAN (R 4.0.0)
    #>  testthat      2.3.2      2020-03-02 [1] CRAN (R 4.0.0)
    #>  usethis       1.6.1      2020-04-29 [1] CRAN (R 4.0.0)
    #>  withr         2.2.0      2020-04-20 [1] CRAN (R 4.0.0)
    #>  xfun          0.16       2020-07-24 [1] CRAN (R 4.0.2)
    #>  yaml          2.2.1      2020-02-01 [1] CRAN (R 4.0.0)
    #> 
    #> [1] /Library/Frameworks/R.framework/Versions/4.0/Resources/library

</details>
