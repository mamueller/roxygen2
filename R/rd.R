#' @include tag-registry.R
#' @import stringr
NULL

register_tags(
  aliases = parse.value,
  author = parse.value.markdown,
  backref = parse.value,
  concept = parse.value.markdown,
  describeIn = parse.name.description,
  description = parse.value.markdown,
  details = parse.value.markdown,
  docType = parse.name,
  encoding = parse.value,
  evalRd = parse.code,
  example = parse.value,
  examples = parse.examples,
  family = parse.value,
  field = parse.name.description,
  format = parse.value.markdown,
  inheritParams = parse.value,
  keywords = parse.value,
  method = parse.name.description.nomarkdown,
  name = parse.value,
  md = parse.toggle,
  noRd = parse.toggle,
  note = parse.value.markdown,
  param = parse.name.description,
  rdname = parse.value,
  rawRd = parse.value,
  references = parse.value.markdown,
  return = parse.value.markdown,
  section = parse.value.markdown,
  seealso = parse.value.markdown,
  slot = parse.name.description,
  source = parse.value.markdown,
  title = parse.value.restricted.markdown,
  usage = parse.value
)

#' Roclet: make Rd files.
#'
#' This roclet is the workhorse of \pkg{roxygen}, producing the Rd files that
#' document that functions in your package.
#'
#' @family roclets
#' @seealso \code{vignette("rd", package = "roxygen2")}
#' @export
rd_roclet <- function() {
  new_roclet(list(), "rd_roclet")
}

#' @export
roc_process.rd_roclet <- function(roclet, parsed, base_path, options = list()) {
  # Look at all blocks with roxygen comments
  blocks <- Filter(function(x) length(x) > 1, parsed$blocks)

  topics <- list()
  for (block in blocks) {
    rd <- block_to_rd(block, base_path, parsed$env)
    if (is.null(rd)) next

    if (rd$filename %in% names(topics)) {
      topics[[rd$filename]]$add(rd)
    } else {
      topics[[rd$filename]] <- rd
    }
  }

  # Drop any topics that don't have a title
  for (topic in names(topics)) {
    has_name_title <- topics[[topic]]$has_tag(c("title", "name"))
    if (!all(has_name_title)) {
      warning(topic, " is missing name/title. Skipping", call. = FALSE)
      topics[[topic]] <- NULL
    }
  }

  topics <- process_family(topics)
  topics <- process_inherit_params(topics)
  fix_params_order(topics)
}

block_to_rd <- function(block, base_path, env) {
  # Must start by processing templates
  block <- process_templates(block, base_path)

  # Does this block get an Rd file?
  if (any(names(block) == "noRd")) {
    return()
  }

  key_tags <- c("description", "param", "return", "title", "example",
    "examples", "name", "rdname", "usage", "details", "introduction",
    "describeIn")
  if (!any(names(block) %in% key_tags)) {
    return()
  }

  rd <- RdTopic$new()

  # Determine name
  name <- block$name %||% object_topic(block$object)
  if (is.null(name)) {
    return(block_warning(block, "Missing name"))
  }
  rd$add(new_tag("name", name))

  # Add backreference to source
  if (!is.null(block$backref)) {
    rd$add(process_tag(block, "backref"))
  } else {
    rd$add(new_tag("backref", block$srcref$filename))
  }

  if (is.function(block$object$value)) {
    formals <- formals(block$object$value)
    rd$add(new_tag("formals", names(formals)))
  }

  # Note that order of operations here doesn't matter: always reordered
  # by format.rd_file
  rd$add(new_tag("encoding", block$encoding))
  rd$add(process_alias(block, name, block$object$alias))
  rd$add(process_methods(block))
  rd$add(process_usage(block))
  rd$add(process_param(block))
  rd$add(process_slot(block))
  rd$add(process_field(block))
  rd$add(process_doc_type(block))
  rd$add(process_tag(block, "rawRd"))
  rd$add(process_tag(block, "evalRd", function(tag, param) {
    expr <- parse(text = param)
    out <- eval(expr, envir = env)
    new_tag("rawRd", as.character(out))
  }))
  rd$add(process_tag(block, "title"))
  rd$add(process_tag(block, "description"))
  rd$add(process_tag(block, "details"))
  rd$add(process_tag(block, "note"))
  rd$add(process_tag(block, "family"))
  rd$add(process_tag(block, "inheritParams"))
  rd$add(process_tag(block, "author"))
  rd$add(process_tag(block, "format"))
  rd$add(process_tag(block, "source"))
  rd$add(process_tag(block, "seealso"))
  rd$add(process_tag(block, "references"))
  rd$add(process_tag(block, "concept"))
  rd$add(process_tag(block, "reexport"))
  rd$add(process_tag(block, "return", function(tag, param) {
    new_tag("value", param)
  }))
  rd$add(process_tag(block, "keywords", function(tag, param) {
    new_tag("keyword", str_split(str_trim(param), "\\s+")[[1]])
  }))
  rd$add(process_tag(block, "section", process_section, block))
  rd$add(process_examples(block, base_path))

  describe_in <- process_describe_in(block, env)
  rd$add(describe_in$tag)

  rd$filename <- paste0(describe_in$rdname %||% block$rdname %||%
    nice_name(name), ".Rd")

  rd
}

#' @export
roc_output.rd_roclet <- function(roclet, results, base_path, options = list(),
                           check = TRUE) {
  man <- normalizePath(file.path(base_path, "man"))

  contents <- vapply(results, format, wrap = options$wrap,
    FUN.VALUE = character(1))

  paths <- file.path(man, names(results))
  mapply(write_if_different, paths, contents, MoreArgs = list(check = check))

  if (check) {
    # Automatically delete any files in man directory that were generated
    # by roxygen in the past, but weren't generated in this sweep.

    old_paths <- setdiff(dir(man, full.names = TRUE), paths)
    old_paths <- old_paths[!file.info(old_paths)$isdir]
    old_roxygen <- Filter(made_by_roxygen, old_paths)
    if (length(old_roxygen) > 0) {
      cat(paste0("Deleting ", basename(old_roxygen), collapse = "\n"), "\n", sep = "")
      unlink(old_roxygen)
    }
  }

  paths
}

#' @export
clean.rd_roclet <- function(roclet, base_path) {
  rd <- dir(file.path(base_path, "man"), full.names = TRUE)
  rd <- rd[!file.info(rd)$isdir]
  made_by_me <- vapply(rd, made_by_roxygen, logical(1))

  unlink(rd[made_by_me])
}


# Tag processing functions ------------------------------------------------

process_methods <- function(block) {
  obj <- block$object
  if (!inherits(obj, "rcclass")) return()

  methods <- obj$methods
  if (is.null(obj$methods)) return()

  desc <- lapply(methods, function(x) docstring(x$value@.Data))
  usage <- vapply(methods, function(x) {
    usage <- function_usage(x$value@name, formals(x$value@.Data))
    as.character(wrap_string(usage))
  }, character(1))

  has_docs <- !vapply(desc, is.null, logical(1))
  desc <- desc[has_docs]
  usage <- usage[has_docs]

  new_tag("rcmethods", setNames(desc, usage))
}


# If \code{@@examples} is provided, use that; otherwise, concatenate
# the files pointed to by each \code{@@example}.
process_examples <- function(block, base_path) {
  out <- list()
  if (!is.null(block$examples)) {
    out <- c(out, process_tag(block, "examples"))
  }

  paths <- unlist(block[names(block) == "example"])
  if (length(paths) > 0) {
    paths <- file.path(base_path, str_trim(paths))

    # Check that haven't accidentally used example instead of examples
    nl <- str_count(paths, "\n")
    if (any(nl) > 0) {
      return(block_warning(
        block,
        "@example spans multiple lines. Do you want @examples?"
      ))
    }

    examples <- unlist(lapply(paths, readLines))
    examples <- escape_examples(examples)

    out <- c(out, list(new_tag("examples", examples)))
  }
  out
}

process_section <- function(key, value, block) {
  pieces <- str_split_fixed(value, ":", n = 2)[1, ]

  title <- str_split(pieces[1], "\n")[[1]]
  if (length(title) > 1) {
    return(block_warning(
      block,
      "Section title spans multiple lines: \n", "@section ", title[1]
    ))
  }

  new_tag("section", list(list(name = pieces[1], content = pieces[2])))
}

process_doc_type <- function(block) {
  doctype <- block$docType

  if (is.null(doctype)) return()
  tags <- list(new_tag("docType", doctype))

  if (doctype == "package") {
    name <- block$name
    if (!str_detect(name, "-package")) {
      tags <- c(tags, list(new_tag("alias", package_suffix(name))))
    }
  }

  tags
}

package_suffix <- function(name) {
  paste0(name, "-package")
}

process_tag <- function(block, tag, f = new_tag, ...) {
  matches <- block[names(block) == tag]
  if (length(matches) == 0) return()

  lapply(matches, function(p) f(tag, p, ...))
}

# Name + description tags ------------------------------------------------------

process_param <- function(block) {
  process_def_tag(block, "param")
}

process_slot <- function(block) {
  process_def_tag(block, "slot")
}

process_field <- function(block) {
  process_def_tag(block, "field")
}

process_def_tag <- function(block, tag) {
  tags <- block[names(block) == tag]
  if (length(tags) == 0) return()

  desc <- str_trim(sapply(tags, "[[", "description"))
  names(desc) <- sapply(tags, "[[", "name")

  new_tag(tag, desc)
}