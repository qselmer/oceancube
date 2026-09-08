.oceancube_deprecate <- function(old, replacement = NULL, details = NULL) {
  verbosity <- getOption("oceancube.lifecycle_verbosity", "warning")
  if (identical(verbosity, "quiet")) {
    return(invisible(FALSE))
  }

  message <- paste0("`", old, "()` is deprecated as of oceancube 0.3.0.")
  if (!is.null(replacement)) {
    message <- paste0(message, " Use `", replacement, "` instead.")
  }
  if (!is.null(details)) {
    message <- paste(message, details)
  }
  warning(message, call. = FALSE)
  invisible(TRUE)
}
