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
  #objectsWithBlocks<-lapply(blocks,function(block){block$object$value})
  ## find those objects that do not have a block but have to be documented.
  ## and create Rd files automatically. 
  ## This is much simpler than creating blocks first since 
  ## for a missing documentation we do not have to take care
  ## of the (nonexisting) tags 
  #rds<-list()
  #genNames<-getGenerics(env)
  #for (genName in genNames){
  #    gen<-methods::getGeneric(genName,env)
  #    if (!(c(gen) %in% as.vector(objectsWithBlocks))){
  #      # create the doc object as in parser_setGeneric
  #      obj<-object(gen,NULL,"s4generic")
  #      defaults <- object_defaults(obj)
  #      default_tags <- map_chr(defaults, "tag")
  #      function_args<-names(formals(obj$value))
  #      extra_tags <- lapply(
  #        function_args
  #        ,function(arg){
  #          roxy_tag(
  #            tag='param'
  #            ,raw=paste(arg,': see method arguments')
  #            ,val=list("name"=arg,description="see method arguments")
  #          )
  #        }
  #      )
  #      topic<-RoxyTopic$new() 
  #      for (tag in append(default_tags,extra_tags)) {
  #        topic$add(roxy_tag_rd(tag, env = env, base_path = base_path))
  #      }
  #      topic$add(rd_section("name", genName))
  #      topic$add(rd_section("alias", genName))
  #      rds<-append(rds,topic)
  #    }
  #    loms<-findMethods(gen,where=env)
  #    mrds<-lapply(
  #      setdiff(loms,objectsWithBlocks),
  #      function(md){
  #          # create the doc object as in parser_setGeneric
  #          obj<-object(md,NULL,"s4method")
  #          defaults <- object_defaults(obj)
  #          #default_tags <- map_chr(defaults, "tag")
  #          default_tags <-defaults
  #          function_args<-names(formals(obj$value))
  #          sig <- obj$value@defined
  #          extra_tags <- lapply(
  #            function_args
  #            ,function(arg){
  #              roxy_tag(
  #                tag='param',
  #                raw=paste(arg,':',sep=''),
  #                val=list("name"=arg,description="")
  #              )
  #            }
  #          )
  #          # extend description for those args that have entries in the signature
  #          # (which does not have to include all formals)
  #          extra_tags<-lapply(
  #            extra_tags,
  #            function(tag){
  #              v<-tag$val
  #              if (v$name %in% names(sig)){
  #                d <- paste("object of class:", "\\code{",sig[[v$name]],"}",'. ' ,v$description,sep='')
  #                tag <- roxy_tag(
  #                  tag='param',
  #                  raw=paste(v$name,': ',d,sep=''),
  #                  val=list("name"=v$name,description=d)
  #                )
  #              }
  #              return(tag)
  #            }
  #          )
  #          topic<-RoxyTopic$new() 
  #          tags<-append(default_tags,extra_tags)
  #          for (tag in tags) {
  #            topic$add(roxy_tag_rd(tag, env = env, base_path = base_path))
  #          }
  #          name_str <- obj$topic
  #          topic$add(rd_section("name",name_str))
  #          topic$add(rd_section("alias",name_str))
  #          topic$filename <-paste0(nice_name(name_str),".Rd")
  #      }
  #    )
  #    rds<-append(rds,mrds)
  #  }

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
  message <- paste0(
    if (!is.na(file)) paste0("[", file, ":", line, "] "),
    ...,
    collapse = " "
  )

  warning(message, call. = FALSE, immediate. = TRUE)
  NULL
}
