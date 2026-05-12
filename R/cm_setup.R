#' Set up the Python environment used by Copernicus Marine
#'
#' This function creates or activates a Python virtual environment and optionally
#' installs the `copernicusmarine` Python module. It does not store credentials.
#'
#' @param env Virtual environment name.
#' @param install Logical. Install `copernicusmarine` if missing?
#' @param module Python module name.
#' @param verbose Logical. Print progress messages?
#'
#' @return Invisibly returns the imported Python module when available.
#' @export
cm_setup <- function(env = "oceancube-copernicus", install = TRUE,
                     module = "copernicusmarine", verbose = TRUE) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    rlang::abort("Package `reticulate` is required.")
  }

  envs <- reticulate::virtualenv_list()
  if (!env %in% envs) {
    if (!isTRUE(install)) {
      rlang::abort(paste0("Virtualenv `", env, "` does not exist. Run with install = TRUE."))
    }
    if (verbose) .message_info("Creating Python virtualenv: ", env)
    reticulate::virtualenv_create(envname = env)
  }

  reticulate::use_virtualenv(env, required = TRUE)

  if (!reticulate::py_module_available(module)) {
    if (!isTRUE(install)) {
      rlang::abort(paste0("Python module `", module, "` is not available."))
    }
    if (verbose) .message_info("Installing Python module: ", module)
    reticulate::py_install(module, envname = env, pip = TRUE)
  }

  if (verbose) .message_done("Copernicus Python environment ready: ", env)
  invisible(reticulate::import(module, delay_load = TRUE))
}

#' Connect to the Copernicus Marine Python module
#'
#' @param env Virtual environment name.
#' @param required Logical. Should activation fail if the environment is missing?
#' @param module Python module name.
#' @param verbose Logical. Print progress messages?
#'
#' @return Imported Python module.
#' @export
cm_connect <- function(env = "oceancube-copernicus", required = TRUE,
                       module = "copernicusmarine", verbose = TRUE) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    rlang::abort("Package `reticulate` is required.")
  }

  reticulate::use_virtualenv(env, required = required)
  if (verbose) .message_info("Importing Python module: ", module)
  out <- reticulate::import(module, delay_load = TRUE)
  if (verbose) .message_done("Connected to Python module: ", module)
  out
}
