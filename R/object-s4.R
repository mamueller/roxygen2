#' @export
roxy_tag_parse.roxy_tag_s4methods<- function(x) {
  #tag_words_line(x)
  x$val<-NULL
  x
}

#' @export
roxy_tag_rd.roxy_tag_s4methods<- function(x, base_path, env) {
  l<-x$val
  if(l$type=="class"){
    # we are in the documentation of a S4class
    return(rd_section("s4methodsOfClass",value=x$val))
  }
  if(l$type=="generic"){
    return(rd_section("s4methodsOfGeneric",value=x$val))
  }
}

#helper
link_lines<-function(method_names,rd_link_string='link'){
  c(
    "      \\itemize{",
    unlist(
      lapply(
        method_names,
        function(name){
          paste0("        \\item \\code{\\",rd_link_string,"{",name,"}}",collapse="")
        }
      )
    ),
    "      }"
  )
}
#helper
method_link_lines<-link_lines
#helper
class_link_lines<-function(names){
  link_lines(names,rd_link_string='linkS4class')
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
              paste0( "    \\subsection{",title,"}{"),
              method_link_lines(sublist$methods),
              "    }"
            )
          }
        ),
        .p=function(lines) !is.null(lines)
      )
    )
    title<-paste0(
      "S4-methods with superclasses (in the package) of class ",
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


#' Show known subclasses of a given S4 class
#' @export
roxy_tag_parse.roxy_tag_s4subclasses<- function(x) {
  #tag_words_line(x)
  x$val<-NULL
  x
}

#' Show known subclasses of a given S4 class
#'
#' @export
roxy_tag_rd.roxy_tag_s4subclasses<- function(x, base_path, env) {
  l<-x$val
  rd_section("s4subclasses",value=x$val)
}

#' @export
format.rd_section_s4subclasses<- function(x, ...) {
  l<-x$val 
  if(length(l)>1){
    lines<-c(
      "\\section{S4-subclasses}{",
        class_link_lines(l),
      "}"
    )
  }else{
    lines=character(0)
  }
  lines
}

#' Show known superclasses (in the package) of a given S4 class
#'
#' @export
roxy_tag_parse.roxy_tag_s4superclasses<- function(x) {
  #tag_words_line(x)
  x$val<-NULL
  x
}

#' @export
roxy_tag_rd.roxy_tag_s4superclasses<- function(x, base_path, env) {
  l<-x$val
  rd_section("s4superclasses",value=x$val)
}

#' @export
format.rd_section_s4superclasses<- function(x, ...) {
  l<-x$val 
  if (length(l)>1){
    lines<-c(
      "\\section{S4-superclasses (in the package)}{",
        class_link_lines(l),
      "}"
    )
  }else{
    lines=character(0)
  }
  lines
}

