test_that("cm_setup isolates activation and import arguments without Python changes", {
  calls <- list()
  module <- structure(list(name = "copernicusmarine"), class = "a2_python_module")
  local_mocked_bindings(
    virtualenv_list = function() "a2-existing-env",
    use_virtualenv = function(env, required) {
      calls$use <<- list(env = env, required = required)
    },
    py_module_available = function(name) {
      calls$available <<- name
      TRUE
    },
    import = function(name, delay_load) {
      calls$import <<- list(name = name, delay_load = delay_load)
      module
    },
    virtualenv_create = function(...) stop("must not create an environment"),
    py_install = function(...) stop("must not install a package"),
    .package = "reticulate"
  )

  expect_silent(result <- withVisible(cm_setup(
    env = "a2-existing-env", install = FALSE,
    module = "copernicusmarine", verbose = FALSE
  )))
  expect_false(result$visible)
  expect_identical(result$value, module)
  expect_identical(calls$use, list(env = "a2-existing-env", required = TRUE))
  expect_identical(calls$available, "copernicusmarine")
  expect_identical(
    calls$import,
    list(name = "copernicusmarine", delay_load = TRUE)
  )
})

test_that("cm_setup error branches remain local and informative", {
  local_mocked_bindings(
    virtualenv_list = function() character(),
    virtualenv_create = function(...) stop("must not create an environment"),
    .package = "reticulate"
  )
  expect_error(
    cm_setup(env = "a2-missing-env", install = FALSE, verbose = FALSE),
    "Virtualenv `a2-missing-env` does not exist"
  )
})

test_that("cm_setup installation branch is fully mocked and deterministic", {
  calls <- list()
  module <- structure(list(name = "custom_module"), class = "a2_python_module")
  local_mocked_bindings(
    virtualenv_list = function() character(),
    virtualenv_create = function(envname) {
      calls$create <<- envname
      invisible(envname)
    },
    use_virtualenv = function(env, required) {
      calls$use <<- list(env = env, required = required)
    },
    py_module_available = function(name) FALSE,
    py_install = function(module, envname, pip) {
      calls$install <<- list(module = module, envname = envname, pip = pip)
    },
    import = function(name, delay_load) {
      calls$import <<- list(name = name, delay_load = delay_load)
      module
    },
    .package = "reticulate"
  )

  expect_silent(result <- cm_setup(
    env = "a2-new-env", install = TRUE,
    module = "custom_module", verbose = FALSE
  ))
  expect_identical(result, module)
  expect_identical(calls$create, "a2-new-env")
  expect_identical(calls$use, list(env = "a2-new-env", required = TRUE))
  expect_identical(
    calls$install,
    list(module = "custom_module", envname = "a2-new-env", pip = TRUE)
  )
  expect_identical(calls$import, list(name = "custom_module", delay_load = TRUE))
})

test_that("cm_setup rejects a missing module without installing it", {
  local_mocked_bindings(
    virtualenv_list = function() "a2-env",
    use_virtualenv = function(...) invisible(NULL),
    py_module_available = function(name) FALSE,
    py_install = function(...) stop("must not install a package"),
    .package = "reticulate"
  )
  expect_error(
    cm_setup(env = "a2-env", install = FALSE, module = "missing", verbose = FALSE),
    "Python module `missing` is not available"
  )
})

test_that("cm_connect passes activation and import contracts without messages", {
  calls <- list()
  module <- structure(list(name = "copernicusmarine"), class = "a2_python_module")
  local_mocked_bindings(
    use_virtualenv = function(env, required) {
      calls$use <<- list(env = env, required = required)
    },
    import = function(name, delay_load) {
      calls$import <<- list(name = name, delay_load = delay_load)
      module
    },
    .package = "reticulate"
  )

  expect_silent(result <- cm_connect(
    env = "a2-env", required = FALSE,
    module = "copernicusmarine", verbose = FALSE
  ))
  expect_identical(result, module)
  expect_identical(calls$use, list(env = "a2-env", required = FALSE))
  expect_identical(calls$import, list(name = "copernicusmarine", delay_load = TRUE))
})

test_that("download_nc validates arguments before filesystem or provider access", {
  import_calls <- 0L
  local_mocked_bindings(
    import = function(...) {
      import_calls <<- import_calls + 1L
      stop("provider import must not occur")
    },
    .package = "reticulate"
  )
  outdir <- file.path(tempdir(), "oceancube-a2-validation-must-not-exist")
  withr::defer(unlink(outdir, recursive = TRUE))

  invalid <- list(
    function() download_nc(42, "thetao", outdir = outdir),
    function() download_nc(c("one", "two"), "thetao", outdir = outdir),
    function() download_nc("dataset", character(), outdir = outdir),
    function() download_nc("dataset", 42, outdir = outdir),
    function() download_nc("dataset", "thetao", lon = -80, outdir = outdir),
    function() download_nc("dataset", "thetao", lon = c(-70, -80), outdir = outdir),
    function() download_nc("dataset", "thetao", lat = c(-12, NA), outdir = outdir),
    function() download_nc("dataset", "thetao", depth = c(100, 0), outdir = outdir),
    function() download_nc("dataset", "thetao", time = as.Date("2020-01-01"), outdir = outdir),
    function() download_nc("dataset", "thetao", fmt = "invalid", outdir = outdir)
  )
  for (call in invalid) expect_error(call())
  expect_identical(import_calls, 0L)
  expect_false(dir.exists(outdir))
})

test_that("download_nc deterministic skip-existing returns before provider import", {
  outdir <- withr::local_tempdir(pattern = "oceancube-a2-download-")
  expected_name <- .make_filename(
    dataset_id = "dataset-id", vars = c("thetao", "so"),
    lon = c(-80, -79), lat = c(-12, -11),
    time = as.Date(c("2020-01-01", "2020-01-02")),
    depth = c(0, 50), ext = "nc"
  )
  expected_path <- file.path(normalizePath(outdir), expected_name)
  expect_true(file.create(expected_path))
  local_mocked_bindings(
    import = function(...) stop("provider import must not occur"),
    .package = "reticulate"
  )

  result <- download_nc(
    dataset_id = "dataset-id", vars = c("thetao", "so"),
    lon = c(-80, -79), lat = c(-12, -11),
    time = as.Date(c("2020-01-01", "2020-01-02")), depth = c(0, 50),
    outdir = outdir, skip_existing = TRUE, overwrite = FALSE,
    verbose = FALSE
  )

  expect_identical(result, expected_path)
  expect_identical(basename(result), expected_name)
})

test_that("download_nc overwrite precedence and request mapping use only mocks", {
  calls <- list()
  module <- new.env(parent = emptyenv())
  module$subset <- function(...) {
    calls$args <<- list(...)
    invisible(NULL)
  }
  local_mocked_bindings(
    import = function(name, delay_load) {
      calls$import <<- list(name = name, delay_load = delay_load)
      module
    },
    .package = "reticulate"
  )
  root <- withr::local_tempdir(pattern = "oceancube-a2-request-")
  outdir <- file.path(root, "nested", "output")
  filename <- "existing.nc"
  dir.create(outdir, recursive = TRUE)
  expect_true(file.create(file.path(outdir, filename)))

  result <- download_nc(
    dataset_id = "dataset-id", vars = c("thetao", "so"),
    lon = c(-80, -79), lat = c(-12, -11),
    time = as.Date(c("2020-01-01", "2020-01-02")), depth = c(0, 50),
    outdir = outdir, fmt = "netcdf", overwrite = TRUE,
    skip_existing = TRUE, dry_run = TRUE, filename = filename,
    verbose = FALSE
  )

  expect_identical(result, file.path(normalizePath(outdir), filename))
  expect_identical(calls$import, list(name = "copernicusmarine", delay_load = TRUE))
  expect_identical(calls$args$dataset_id, "dataset-id")
  expect_identical(calls$args$variables, as.list(c("thetao", "so")))
  expect_identical(calls$args$output_filename, filename)
  expect_identical(calls$args$file_format, "netcdf")
  expect_true(calls$args$overwrite)
  expect_true(calls$args$skip_existing)
  expect_true(calls$args$dry_run)
  expect_true(calls$args$disable_progress_bar)
  expect_identical(calls$args$minimum_longitude, -80)
  expect_identical(calls$args$maximum_longitude, -79)
  expect_identical(calls$args$minimum_latitude, -12)
  expect_identical(calls$args$maximum_latitude, -11)
  expect_identical(calls$args$minimum_depth, 0)
  expect_identical(calls$args$maximum_depth, 50)
  expect_identical(calls$args$start_datetime, "2020-01-01")
  expect_identical(calls$args$end_datetime, "2020-01-02")
})
