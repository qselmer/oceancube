make_cf_climatology_b6_fixture <- function(
    n = 4L,
    units = "months since 2000-01-01 00:00:00",
    calendar = "standard",
    representative = seq_len(n) + 5,
    support_start = seq_len(n) - 1,
    support_end = support_start + 12,
    target = "time_clim",
    add_target = TRUE,
    add_ordinary_bounds = FALSE,
    cell_methods = "time: mean within years time: mean over years",
    coverage_start = NULL) {
  file <- tempfile("oceancube-b6-climatology-", tmpdir = tempdir(), fileext = ".nc")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", c(-80, -79))
  lat <- ncdf4::ncdim_def("lat", "degrees_north", c(-12, -11))
  time <- ncdf4::ncdim_def("time", units, representative)
  nv <- ncdf4::ncdim_def("nv", "", 1:2, create_dimvar = FALSE)
  definitions <- list(
    ncdf4::ncvar_def("temperature", "K", list(lon, lat, time))
  )
  if (isTRUE(add_target)) {
    definitions <- c(
      list(ncdf4::ncvar_def(target, units, list(nv, time))),
      definitions
    )
  }
  if (isTRUE(add_ordinary_bounds)) {
    definitions <- c(
      list(ncdf4::ncvar_def("time_bnds", units, list(nv, time))),
      definitions
    )
  }
  nc <- ncdf4::nc_create(file, definitions, force_v4 = TRUE)
  tryCatch({
    ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.13")
    if (!is.null(coverage_start)) {
      ncdf4::ncatt_put(nc, 0, "time_coverage_start", coverage_start)
    }
    ncdf4::ncatt_put(nc, "lon", "standard_name", "longitude")
    ncdf4::ncatt_put(nc, "lat", "standard_name", "latitude")
    ncdf4::ncatt_put(nc, "time", "standard_name", "time")
    ncdf4::ncatt_put(nc, "time", "calendar", calendar)
    ncdf4::ncatt_put(nc, "time", "climatology", target)
    if (isTRUE(add_ordinary_bounds)) {
      ncdf4::ncatt_put(nc, "time", "bounds", "time_bnds")
      ncdf4::ncvar_put(
        nc, "time_bnds", rbind(support_start, support_end)
      )
    }
    if (isTRUE(add_target)) {
      ncdf4::ncvar_put(nc, target, rbind(support_start, support_end))
    }
    ncdf4::ncatt_put(nc, "temperature", "cell_methods", cell_methods)
    ncdf4::ncvar_put(
      nc, "temperature", array(seq_len(4L * n), dim = c(2L, 2L, n))
    )
  }, finally = ncdf4::nc_close(nc))
  file
}

test_that("UDUNITS month and year are exact elapsed durations, not civil periods", {
  month <- .decode_cf_time(0:1, "months since 2001-01-01", "standard")
  year <- .decode_cf_time(0:1, "years since 2001-01-01", "standard")

  expect_identical(month$unit, "udunits_month")
  expect_identical(year$unit, "udunits_year")
  expect_equal(
    diff(.time_key(month$decoded_values)),
    365.242198781 / 12 * 86400,
    tolerance = 1e-7
  )
  expect_equal(
    diff(.time_key(year$decoded_values)),
    365.242198781 * 86400,
    tolerance = 1e-7
  )
  expect_false(
    identical(
      format(month$decoded_values[[2L]], "%Y-%m-%d", tz = "UTC"),
      "2001-02-01"
    )
  )
})

test_that("ordinary UDUNITS-month axes retain eager and deferred parity", {
  file <- make_netcdf_backend_fixture(
    time_units = "months since 2001-01-01",
    time_values = 0:3
  )
  withr::local_file(file)
  eager <- read_nc(file, vars = "temperature")
  deferred <- cube_open(file, vars = "temperature")
  collected <- cube_collect(deferred)

  expect_identical(.cube_chronology_kind(eager), "ordinary")
  expect_equal(eager$time, deferred$time)
  expect_equal(eager$time, collected$time)
})

test_that("generic CF climatologies are represented in current CF metadata", {
  cases <- list(
    annual = list(n = 1L, start = 0, end = 12, rep = 6),
    seasonal = list(n = 4L, start = 0:3, end = 12:15, rep = 6:9),
    monthly = list(n = 12L, start = 0:11, end = 12:23, rep = 6:17)
  )
  for (case in cases) {
    file <- make_cf_climatology_b6_fixture(
      n = case$n,
      representative = case$rep,
      support_start = case$start,
      support_end = case$end
    )
    withr::local_file(file)
    eager <- read_nc(file, vars = "temperature")
    deferred <- cube_open(file, vars = "temperature")
    collected <- cube_collect(deferred)
    chronology <- eager$metadata$cf$current$chronology

    expect_identical(.cube_chronology_kind(eager), "climatological")
    expect_identical(.cube_chronology_kind(deferred), "climatological")
    expect_identical(chronology$climatology_target, "time_clim")
    expect_identical(
      chronology$runtime_status,
      "CLIMATOLOGY_RUNTIME_SUPPORTED"
    )
    expect_equal(as.numeric(eager$data), as.numeric(collected$data))
  }
})

test_that("climatology support works with elapsed days and guards analytics", {
  file <- make_cf_climatology_b6_fixture(
    units = "days since 2000-01-01",
    representative = 100:103,
    support_start = 0:3,
    support_end = 365:368
  )
  withr::local_file(file)
  cube <- read_nc(file, vars = "temperature")
  selected <- cube_slice(cube, time = 2:3, by = "index")

  expect_length(
    selected$metadata$cf$current$chronology$representative_time,
    2L
  )
  expect_length(selected$metadata$cf$current$chronology$support_start, 2L)

  expect_error(
    cube_aggregate_time(cube, by = "month"),
    "requires ordinary chronology"
  )
  expect_error(
    cube_climatology(cube, by = "month"),
    "requires ordinary chronology"
  )
  expect_error(cube_trend(cube), "requires ordinary chronology")
  expect_error(to_month(cube), "requires ordinary chronology")
  expect_error(annual_index(cube), "requires ordinary chronology")

  non_gregorian <- make_cf_climatology_b6_fixture(
    units = "days since 2001-01-01",
    calendar = "360_day",
    representative = 100:103,
    support_start = 0:3,
    support_end = 360:363
  )
  withr::local_file(non_gregorian)
  expect_identical(
    .cube_chronology_kind(read_nc(non_gregorian, vars = "temperature")),
    "climatological"
  )
})

test_that("malformed or ambiguous climatology metadata is rejected", {
  missing <- make_cf_climatology_b6_fixture(add_target = FALSE)
  malformed <- make_cf_climatology_b6_fixture(
    support_start = 0:3,
    support_end = 0:3
  )
  conflict <- make_cf_climatology_b6_fixture(add_ordinary_bounds = TRUE)
  methods <- make_cf_climatology_b6_fixture(cell_methods = "time: mean")
  withr::local_file(missing)
  withr::local_file(malformed)
  withr::local_file(conflict)
  withr::local_file(methods)

  expect_error(read_nc(missing), "target `time_clim` is missing")
  expect_error(read_nc(malformed), "start before end")
  expect_error(read_nc(conflict), "ordinary bounds and climatology bounds")
  expect_error(read_nc(methods), "recognized `time: ... within years")
})

test_that("governed WOA annual encoding is rejected without provider repair", {
  path <- test_path(
    "fixtures", "real-data", "noaa-woa23-vertical-fv1.nc"
  )
  expect_error(
    read_nc(path, vars = c("t_an", "s_an")),
    "begins at 2306-01-01T00:16:57"
  )
  expect_error(
    cube_open(path, vars = c("t_an", "s_an")),
    "Core will not infer a provider-specific offset correction"
  )
})
