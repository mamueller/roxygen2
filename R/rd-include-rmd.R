#' @export
roxy_tag_parse.roxy_tag_includeRmd <- function(x) {
  if (!is_installed("rmarkdown")) {
    roxy_tag_warning(x, "Needs the rmarkdown package")
    return()
  }

  tag_value(x)
}

#' @export
roxy_tag_rd.roxy_tag_includeRmd <- function(x, base_path, env) {
  rmd <- x$val
  stopifnot(is.character(rmd), length(rmd) == 1, !is.na(rmd))

  rmd_path <- tempfile(fileext = ".Rmd")
  md_path <- tempfile(fileext = ".md")
  on.exit(unlink(c(rmd_path, md_path), recursive = TRUE), add = TRUE)

  wd <- getwd()
  setwd(base_path)
  on.exit(setwd(wd), add = TRUE)

  # This will create an absolute path
  rmd <- normalizePath(rmd)

  cache_path <- paste0(sub("\\.Rmd$", "", rmd), "_cache/")
  fig_path <- file.path(dirname(rmd), "figure/")
  linkrefs <- rmd_linkrefs_from_file(rmd)
  opts <- c(
    root.dir = dirname(rmd),
    cache.path = cache_path,
    fig.path = fig_path,
    child = rmd
  )
  optss <- paste0(names(opts), "=", encodeString(opts, quote = '"'))
  txt <- sprintf(
    "```{r %s}\n```\n\n%s\n",
    paste(optss, collapse = ","),
    linkrefs
  )
  cat(txt, file = rmd_path)

  rmarkdown::render(
    rmd_path,
    output_format = rmarkdown::github_document(),
    output_file = md_path,
    quiet = TRUE
  )

  value <- rmd_eval_rd(md_path, x)
  rd_section_markdown("details", value)
}

# Helpers -----------------------------------------------------------------

rmd_linkrefs_from_file <- function(path) {
  lines <- read_lines(path)
  txt <- paste(lines, collapse = "\n")
  paste(get_md_linkrefs(txt), collapse = "\n")
}

rmd_eval_rd <- function(path, tag) {
  mdtxt <- paste(read_lines(path), collapse = "\n")
  mdesc <- add_linkrefs_to_md(mdtxt)
  mdxml <- md_to_mdxml(mdesc)
  state <- new.env(parent = emptyenv())
  state$tag <- tag
  state$has_sections <- TRUE
  rd <- mdxml_children_to_rd_top(mdxml, state = state)
  rd
}
