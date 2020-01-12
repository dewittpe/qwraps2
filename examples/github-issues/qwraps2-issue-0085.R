reprex::reprex(
{

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

},
outfile = "foo"
)
