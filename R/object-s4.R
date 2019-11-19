#' @export
roxy_tag_parse.roxy_tag_s4methods<- function(x) {
  #tag_words_line(x)
  x$val<-NULL
  x
}

#' @export
roxy_tag_rd.roxy_tag_s4methods<- function(x, base_path, env) {
  rd_section("s4methods",value=x$val)
}

#' @export
format.rd_section_s4methods<- function(x, ...) {
  #define a helper funciton
  subsection<-function(sublist,section_title){
      class<-sublist$class
      methods<-sublist$methods
      res <- paste0(
        "  \\itemize{\n",
        paste0("    \\item ", "\\code{\\link{",method_names,"}}","\n", collapse = ""),
        "  }\n"
      )
      paste0(
          "\\section{",section_title,"}{\n",
          res,
          "}\n"
      )
  }
  l<-x$value
  section_names<-names(l)
  browser()
  if(any(c("class","super_classes") %in% section_names)){
    # we are in the documentation of a S4class
    lines<-subsection(l$class,paste("S4-methods directly using",l$class$class))
    super_class_lines<-unlist(
      lapply(
        l$super_classes,
        function(sublist){
          subsection(sublist,paste("S4-methods using the superclass",sublist$class))
        }
      )
    )
    lines<-c(lines,super_class_lines)
  }
  if(any("generic" %in% section_names)){
    lines<-subsection(l$class,paste("S4-methods for generic",l$generic))
  }
  lines
}
