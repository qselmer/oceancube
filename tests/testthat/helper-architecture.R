.strip_architecture_parentheses <- function(x) {
  while (is.call(x) &&
         identical(x[[1L]], as.name("(")) &&
         length(x) == 2L) {
    x <- x[[2L]]
  }
  x
}

.is_architecture_x <- function(x) {
  identical(.strip_architecture_parentheses(x), as.name("x"))
}

.architecture_call_name <- function(x) {
  if (is.symbol(x)) {
    return(as.character(x))
  }
  if (is.call(x) &&
      as.character(x[[1L]]) %in% c("::", ":::") &&
      length(x) == 3L) {
    return(as.character(x[[3L]]))
  }
  ""
}

.contains_direct_cube_data_access <- function(expr) {
  if (!is.call(expr) && !is.expression(expr) && !is.pairlist(expr)) {
    return(FALSE)
  }

  if (is.call(expr)) {
    call_name <- .architecture_call_name(expr[[1L]])

    if (identical(call_name, "$") &&
        length(expr) >= 3L &&
        .is_architecture_x(expr[[2L]]) &&
        identical(as.character(expr[[3L]]), "data")) {
      return(TRUE)
    }

    if (identical(call_name, "[[") &&
        length(expr) >= 3L &&
        .is_architecture_x(expr[[2L]]) &&
        is.character(expr[[3L]]) &&
        length(expr[[3L]]) == 1L &&
        identical(expr[[3L]], "data")) {
      return(TRUE)
    }

    if (identical(call_name, "getElement") &&
        length(expr) >= 3L &&
        .is_architecture_x(expr[[2L]]) &&
        is.character(expr[[3L]]) &&
        length(expr[[3L]]) == 1L &&
        identical(expr[[3L]], "data")) {
      return(TRUE)
    }
  }

  any(vapply(
    as.list(expr),
    .contains_direct_cube_data_access,
    logical(1)
  ))
}

.architecture_expression_name <- function(expr) {
  if (is.call(expr) &&
      as.character(expr[[1L]]) %in% c("<-", "=") &&
      length(expr) == 3L &&
      is.symbol(expr[[2L]])) {
    return(as.character(expr[[2L]]))
  }
  "<top-level>"
}

.direct_access_violations <- function(path) {
  parsed <- parse(path, keep.source = TRUE)
  function_names <- vapply(
    parsed,
    .architecture_expression_name,
    character(1)
  )
  has_access <- vapply(
    parsed,
    .contains_direct_cube_data_access,
    logical(1)
  )

  function_names[has_access]
}
