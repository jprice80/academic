#' Produce Output using Pandoc tables
#'
#' @description The \code{report} function will produce ASCII formated output. This includes both custom and pandoc tables and figures.
#'
#' @param object An R object from an aca analysis.
#' @param style a pandoc table style: \code{simple, multiline, grid or rmarkdown}.
#' @param round the number of digits to round too when producing the report.
#'
#' @return a printed pander table
#' @export
#'
#' @examples
#' out<-descriptives(data=mtcars, vars=c("mpg","disp"), groupby=c("gear","am"))
#' report(out)
report<-function(object, style="multiline", round=4){
  cls<-class(object)

  if(cls=="aca_desc"){

    report_aca_desc(object, style, round)

  } else {

   stop(paste("Not an aca object"))

  }
}


#======================================Descriptive Statistics===================================================
#' @export
report_aca_desc<-function(object, style="multiline", round=4){

  if(class(object) != "aca_desc"){
    stop(paste(object, "not an aca_desc object"))
  }

  cat("\n")
  cat("===================================================================================================================")
  cat("\n")
  cat("Descriptive Statistics")
  cat("\n")
  cat("===================================================================================================================")
  cat("\n")


  for(i in 1:length(object)){
    out<-object[[i]]
    pander::pandoc.table(out, style=style, split.tables=110, round=round, keep.trailing.zeros=TRUE)
  }
}
