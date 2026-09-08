test_that("the lifecycle helper warns by default and supports quiet tests", {
  withr::local_options(list(oceancube.lifecycle_verbosity = "warning"))
  expect_warning(
    oceancube:::.oceancube_deprecate("old_name", "new_name()"),
    "`old_name\\(\\)` is deprecated as of oceancube 0.3.0"
  )

  withr::local_options(list(oceancube.lifecycle_verbosity = "quiet"))
  expect_silent(oceancube:::.oceancube_deprecate("old_name", "new_name()"))
})

expect_oceancube_deprecation <- function(code, function_name) {
  warnings <- character()
  value <- withCallingHandlers(
    code,
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl(
    paste0("`", function_name, "\\(\\)` is deprecated"), warnings
  )))
  invisible(value)
}

test_that("legacy temporal exports emit bounded deprecation warnings", {
  withr::local_options(list(oceancube.lifecycle_verbosity = "warning"))
  time <- as.Date(c("2020-01-01", "2020-02-01", "2021-01-01", "2021-02-01"))
  x <- ocean_cube(
    lon = -80, lat = -12, depth = 0, time = time,
    data = array(c(1, 2, 3, 4), dim = c(1, 1, 1, 4, 1)),
    vars = "temperature", units = "degC"
  )
  monthly_clim <- suppressWarnings(clim_month(x))

  expect_oceancube_deprecation(annual_index(x), "annual_index")
  expect_oceancube_deprecation(clim_day(x), "clim_day")
  expect_oceancube_deprecation(clim_month(x), "clim_month")
  expect_oceancube_deprecation(anom_diff(x, monthly_clim), "anom_diff")
  expect_oceancube_deprecation(anom_z(x, monthly_clim), "anom_z")
  expect_oceancube_deprecation(signal_noise(x, monthly_clim), "signal_noise")
  expect_oceancube_deprecation(to_month(x), "to_month")
})

test_that("legacy provider exports warn without provider or network access", {
  skip_if_not_installed("reticulate")
  withr::local_options(list(oceancube.lifecycle_verbosity = "warning"))
  module <- structure(list(name = "copernicusmarine"), class = "mock_module")
  local_mocked_bindings(
    virtualenv_list = function() "release-env",
    use_virtualenv = function(...) invisible(NULL),
    py_module_available = function(...) TRUE,
    import = function(...) module,
    .package = "reticulate"
  )

  expect_oceancube_deprecation(
    cm_setup(env = "release-env", install = FALSE, verbose = FALSE),
    "cm_setup"
  )
  expect_oceancube_deprecation(
    cm_connect(env = "release-env", verbose = FALSE),
    "cm_connect"
  )

  outdir <- withr::local_tempdir(pattern = "oceancube-deprecation-")
  cached <- file.path(outdir, "cached.nc")
  expect_true(file.create(cached))
  expect_oceancube_deprecation(
    download_nc(
      "dataset", "thetao", outdir = outdir, filename = basename(cached),
      verbose = FALSE
    ),
    "download_nc"
  )
})
