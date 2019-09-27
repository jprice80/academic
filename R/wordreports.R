#' Write Formatted Output to a *.docx File
#'
#' @description The write.report function will produce an MS Word document using the \code{\link{officer}}.
#'
#' @param object An R object from an aca analysis.
#' @param path Path or connection to write to.
#'
#' @return will produce an *.docx file.
#' @export
#'
#' @examples
#' out<-descriptives(data=mtcars, vars=c("mpg","disp"), groupby=c("gear","am"))
#' write.report(out, path="C:/Temp/Output.docx")
write.report<-function(object, path){
  cls<-class(object)

  if(cls=="aca_desc"){

    write_aca_desc(object, path)

  } else {

    stop(paste("Not an aca object"))

  }
}


#======================================Descriptive Statistics===================================================
#' @export
write_aca_desc<-function(object, path, round=NULL){

  if(class(object) != "aca_desc"){
    stop(paste(descriptives, "not an aca_desc_desc object"))
  }

  format_table_title<-fp_text(color="black", font.size=16, bold=TRUE, italic=FALSE, underlined = FALSE,
                              font.family = "Arial", vertical.align = "baseline", shading.color = "transparent")

  doc <- read_docx()
  doc %>% body_add_fpar(fpar(ftext('Descriptive Statistics', prop=format_table_title)), style='centered')
  doc %>% body_add_par("")

  if(!is.null(round)){

  }

  for(i in 1:length(object)){
    doc %>% body_add_table(object[[i]], style = "table_template", no_vband=FALSE)
    doc %>% body_add_par("")
  }

  doc %>% body_end_section_landscape()
  print(doc, target = path)
}
