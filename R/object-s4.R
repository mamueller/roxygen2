#' @export
roxy_tag_parse.roxy_tag_s4methods<- function(x) {
  #tag_words_line(x)
  x$val<-NULL
  x
}

#' @export
roxy_tag_rd.roxy_tag_s4methods<- function(x, base_path, env) {
  rd_section("s4methods",x$val)
}
format.rd_section_s4methods <- function(x, ...) {
  paste0(
    "\\section{S4 methods for this generic}{\n",
    "\\itemize{\n",
    paste0("  \\item ", "\\code{\\link{",x$value,"}}","\n", collapse = ""),
    "}\n",
    "}\n"
  )
}
