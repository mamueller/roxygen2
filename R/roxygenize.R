#' Process a package with the Rd, namespace and collate roclets.
#'
#' This is the workhorse function that uses roclets, the built-in document
#' transformation functions, to build all documentation for a package. See
#' the documentation for the individual roclets, [rd_roclet()],
#' [namespace_roclet()], and for [update_collate()],
#' for more details.
#'
#' Note that roxygen2 is a dynamic documentation system: it works by
#' inspecting loaded objects in the package. This means that you must
#' be able to load the package in order to document it: see [load] for
#' details.
#'
#' @param package.dir Location of package top level directory. Default is
#'   working directory.
#' @param roclets Character vector of roclet names to use with package.
#'   The default, `NULL`, uses the roxygen `roclets` option,
#'   which defaults to `c("collate", "namespace", "rd")`.
#' @param load_code A function used to load all the R code in the package
#'   directory. The default, `NULL`, uses the strategy defined by
#'   the `load` roxygen option, which defaults to [load_pkgload()].
#'   See [load] for more details.
#' @param clean If `TRUE`, roxygen will delete all files previously
#'   created by roxygen before running each roclet.
#' @return `NULL`
#' @export
#' @importFrom stats setNames
roxygenize <- function(package.dir = ".",
                       roclets = NULL,
                       load_code = NULL,
                       clean = FALSE) {

  base_path <- normalizePath(package.dir)
  is_first <- roxygen_setup(base_path)

  encoding <- desc::desc_get("Encoding", file = base_path)[[1]]
  if (!identical(encoding, "UTF-8")) {
    warning("roxygen2 requires Encoding: UTF-8", call. = FALSE)
  }

  roxy_meta_load(base_path)

  roclets <- roclets %||% roxy_meta_get("roclets")
  # Special case collate: it doesn't need to execute code, and must be run
  # first to ensure that code can be executed
  if ("collate" %in% roclets) {
    update_collate(base_path)
    roclets <- setdiff(roclets, "collate")
  }


  
  if (length(roclets) == 0)
    return(invisible())

  roclets <- lapply(roclets, roclet_find)
  # Tokenise each file
  blocks <- parse_package(base_path, env = NULL)

  if (clean) {
    purrr::walk(roclets, roclet_clean, base_path = base_path)
  }

  roclets <- lapply(roclets, roclet_preprocess,
    blocks = blocks,
    base_path = base_path
  )

  # Now load code
  load_code <- find_load_strategy(load_code)
  env <- load_code(base_path)

  blocks <- lapply(blocks, block_set_env, env = env)

  # Special case autotag: 
  # The _roclet changes the source code by
  # adding" #' @auto " before yet undocumented calls 
  # in the source code. It thereby increases the number of blocks 
  # that will be discovered by parsing and consequently has to run
  # before the rd_roclet that will produce some minimal documentation
  # for those objects.
  x<-roclet_find("autotag_roclet")
  if (
    any(
      as.logical(lapply(
        roclets,
        function(r){all(class(x)==class(r))}
      )
    ))
  ){
    # setdiff does not work on list of roclet objects so we implement it
    roclets <- purrr:::keep(
      roclets,
      function(r){!all(class(x)==class(r))}
    )
    untagged_objects<-roclet_process(x,blocks,env,base_path)
    roclet_output(x, untagged_objects, base_path)
    # parse again
    blocks <- parse_package(base_path, env = NULL)
    blocks <- lapply(blocks, block_set_env, env = env)
  }
  # Special case remove_autotag: 
  # The _roclet changes the source code by
  # removing the lines containing" #' @auto " 
  # in the source code. 
  # It thereby decreases the number of blocks 
  # that will be discovered by parsing and consequently has to run
  # before the rd_roclet 
  x<-roclet_find("remove_autotag_roclet")
  if (
    any(
      as.logical(
        lapply(
          roclets,
          function(r){all(class(x)==class(r))}
        )
      )
    )
  ){
    # setdiff does not work on list of roclet objects so we implement it
    roclets <- purrr:::keep(
      roclets,
      function(r){!all(class(x)==class(r))}
    )
    blocks_with_auto <-roclet_process(x,blocks,env,base_path)
    roclet_output(x, blocks_with_auto , base_path)
    # parse again
    blocks <- parse_package(base_path, env = NULL)
    blocks <- lapply(blocks, block_set_env, env = env)
  }
  
  # Special case auto_comment: 
  # The roclet changes the source code by inserting
  # roxygen comments before yet undocumented calls 
  # in the source code. 
  # It thereby increases the number of blocks 
  # that will be discovered by parsing and consequently has to run
  # before the rd_roclet that will produce documentation
  # for those objects.
  x<-roclet_find("auto_comment_roclet")
  if (
    any(
      as.logical(lapply(
        roclets,
        function(r){all(class(x)==class(r))}
      )
    ))
  ){
    # setdiff does not work on list of roclet objects so we implement it
    roclets <- purrr:::keep(
      roclets,
      function(r){!all(class(x)==class(r))}
    )

    untagged_objects<-roclet_process(x,blocks,env,base_path)
    roclet_output(x, results=untagged_objects, base_path=base_path)
    # parse again
    blocks <- parse_package(base_path, env = NULL)
    blocks <- lapply(blocks, block_set_env, env = env)
  }

  # Special case update_auto_comment: 
  # The roclet changes the source code by changing 
  # roxygen comments of objects that have the @auto_comments
  # tag in their roxygenblock 
  # It has to run before the rd_roclet that will produce documentation
  # for those objects.
  x<-roclet_find("update_auto_comment_roclet")
  if (
    any(
      as.logical(lapply(
        roclets,
        function(r){all(class(x)==class(r))}
      )
    ))
  ){
    # setdiff does not work on list of roclet objects so we implement it
    roclets <- purrr::discard(
      roclets,
      function(r){all(class(x)==class(r))}
    )
    blocks_with_auto_comment_tag<-roclet_process(x,blocks,env,base_path)
    roclet_output(x, results=blocks_with_auto_comment_tag, base_path)
    # parse again
    blocks <- parse_package(base_path, env = NULL)
    blocks <- lapply(blocks, block_set_env, env = env)
  }

  results <- lapply(roclets, roclet_process,
    blocks = blocks,
    env = env,
    base_path = base_path
  )

  out <- purrr::map2(
    roclets, results,
    roclet_output,
    base_path = base_path,
    is_first = is_first
  )
  invisible(out)
}

#' @rdname roxygenize
#' @export
roxygenise <- roxygenize

roxygen_setup <- function(base_path) {
  is_first <- first_time(base_path)
  if (is_first) {
    message("First time using roxygen2. Upgrading automatically...")
  }

  man_path <- file.path(base_path, "man")
  dir.create(man_path, recursive = TRUE, showWarnings = FALSE)
  #update_roxygen_version(base_path)

  is_first
}

roxy_warning <- function(..., file = NA, line = NA) {
  print('################')
  print(as.list(...))
  message <- paste0(
    if (!is.na(file)) paste0("[", file, ":", line, "] "),
    ...,
    collapse = " "
  )

  warning(message, call. = FALSE, immediate. = TRUE)
  NULL
}
