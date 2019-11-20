#' @export
roxy_tag_parse.roxy_tag_s4methods<- function(x) {
  #tag_words_line(x)
  x$val<-NULL
  x
}

#' @export
roxy_tag_rd.roxy_tag_s4methods<- function(x, base_path, env) {
  l<-x$val
  browser()
  if(l$type=="class"){
    # we are in the documentation of a S4class
    res<-rd_section("s4methodsOfClass",value=x$val)
  }
  if(l$type=="generic"){
    res<-rd_section("s4methodsOfGeneric",value=x$val)
  }
  res
}

#helper
method_link_lines<-function(method_names,section_title){
  c(
    "      \\itemize{",
    unlist(
      lapply(
        method_names,
        function(name){
          paste0("        \\item \\code{\\link{",name,"}}",collapse="")
        }
      )
    ),
    "      }"
  )
}

#' @export
format.rd_section_s4methodsOfClass<- function(x, ...) {
  # we are documenting a S4class
  l<-x$value
  source_class_sub_list <-l[['direct_record']]
  super_class_sub_list  <-l[['super_class_record_list']]
  lines=character(0)
  if (length(source_class_sub_list$methods)>0){
    title<-paste0(
      "S4-methods with class \\code{",
      source_class_sub_list$class,
      "} in their signature:"
    )
    lines<- append(
      lines,
      c(
        paste0("  \\subsection{",title,"}{",collapse=""),
        method_link_lines(source_class_sub_list$methods),
        "  }"
      )
    )
  }
  if (length(super_class_sub_list)>0){
    super_class_lines<- unlist(
      purrr::keep(
        lapply(
          super_class_sub_list,
          function(sublist){
            if (length(sublist$methods)<1){
              return(NULL)
            }
            title<-paste( "superclass", sublist$class,collapse=" ")
            c(
              paste0( "    \\subsubsection{",title,"}{"),
              method_link_lines(sublist$methods),
              "    }"
            )
          }
        ),
        .p=function(lines) !is.null(lines)
      )
    )
    title<-paste0(
      "S4-methods with superclasses of class ",
      "\\code{",source_class_sub_list$class,"} ",
      "in their signature:"
    )
    lines<- append(
      lines,
      c(
        paste0("  \\subsection{",title,"}{"),
          super_class_lines,
        "  }"
      )
    )
  }
  if(length(lines)>0){
    lines<- c(
      "\\section{S4-methods}{",
      lines,
      "}"
    )
  }
  lines
}


#' @export
format.rd_section_s4methodsOfGeneric<- function(x, ...) {
  l<-x$value
  lines<-c(
    "\\section{S4-methods}{",
      method_link_lines(l$methods),
    "}"
  )
  lines
}
