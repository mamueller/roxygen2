autotag_roclet <- function() {
  roclet("autotag")
}
#' @export
roclet_preprocess.roclet_autotag <- function(x, blocks, base_path) {
  browser()
  #lines <- blocks_to_ns(blocks, env, import_only = TRUE)
  #NAMESPACE <- file.path(base_path, "NAMESPACE")

  #if (length(lines) == 0 && !made_by_roxygen(NAMESPACE)) {
  #  return(x)
  #}

  #results <- c(made_by("#"), lines)
  #write_if_different(NAMESPACE, results, check = TRUE)

  invisible(x)
}

#' @export
roclet_process.roclet_autotag <- function(x, blocks, env, base_path) {
  blocks_to_ns(blocks, env)
}

#' @export
roclet_output.roclet_autotag <- function(x, results, base_path, ...) {
  #NAMESPACE <- file.path(base_path, "NAMESPACE")
  #results <- c(made_by("#"), results)

  ## Always check for roxygen2 header before overwriting NAMESPACE (#436),
  ## even when running for the first time
  #write_if_different(NAMESPACE, results, check = TRUE)

  #NAMESPACE
}

#' @export
roclet_clean.roclet_autotag <- function(x, base_path) {
  #NAMESPACE <- file.path(base_path, "NAMESPACE")
  #if (made_by_roxygen(NAMESPACE)) {
  #  unlink(NAMESPACE)
  #}
}
