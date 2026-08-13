make_contract_cube <- function(
    lon = c(-80, -79, -78),
    lat = c(-12, -11),
    depth = c(0, 50),
    time = as.Date(c(
      "2020-01-01", "2020-02-01",
      "2021-01-01", "2021-02-01"
    )),
    vars = c("temperature", "oxygen"),
    units = c(temperature = "degC", oxygen = "mmol m-3"),
    data = NULL,
    ...) {
  if (is.null(data)) {
    data <- array(
      seq_len(length(lon) * length(lat) * length(depth) * length(time) * length(vars)),
      dim = c(length(lon), length(lat), length(depth), length(time), length(vars))
    )
  }

  ocean_cube(
    lon = lon,
    lat = lat,
    depth = depth,
    time = time,
    vars = vars,
    units = units,
    data = data,
    ...
  )
}

test_that("normal and surface memory cubes satisfy the internal contract", {
  cube <- make_contract_cube()
  surface <- ocean_cube(
    lon = c(-80, -79),
    lat = c(-12, -11),
    depth = NA_real_,
    time = as.Date("2020-01-01"),
    data = array(1:4, dim = c(2, 2, 1, 1, 1)),
    vars = "temperature",
    units = "degC"
  )

  expect_s3_class(cube, "ocean_cube")
  expect_type(cube, "list")
  expect_true(.check_cube(cube))
  expect_identical(.cube_backend(cube), "memory")
  expect_true(.check_cube(surface))
  expect_identical(surface$depth, NA_real_)
  expect_identical(unname(dim(surface$data)), c(2L, 2L, 1L, 1L, 1L))
})

test_that("supported coordinate conventions are preserved without reordering", {
  lon_180 <- make_contract_cube(lon = c(-180, 180, 0))
  lon_360 <- make_contract_cube(lon = c(360, 0, 180))
  negative_depth <- make_contract_cube(depth = c(0, -50))
  duplicated_coordinates <- make_contract_cube(
    lon = c(-80, -80, -79),
    lat = c(-12, -12),
    depth = c(0, 0)
  )
  posix_time <- make_contract_cube(
    time = as.POSIXct(c(
      "2020-01-01", "2020-02-01",
      "2021-01-01", "2021-02-01"
    ), tz = "UTC")
  )

  expect_identical(lon_180$lon, c(-180, 180, 0))
  expect_identical(lon_360$lon, c(360, 0, 180))
  expect_identical(negative_depth$depth, c(0, -50))
  expect_identical(duplicated_coordinates$lon, c(-80, -80, -79))
  expect_identical(duplicated_coordinates$lat, c(-12, -12))
  expect_identical(duplicated_coordinates$depth, c(0, 0))
  expect_error(
    make_contract_cube(time = as.Date(c(
      "2020-02-01", "2020-01-01", "2020-01-01", "2021-01-01"
    ))),
    "unique|strictly increasing"
  )
  expect_s3_class(posix_time$time, "POSIXct")
  expect_identical(attr(posix_time$time, "tzone"), "UTC")
})

test_that("optional metadata and units are preserved without changing dimensions", {
  provenance <- list(provider = "test-provider", request = "contract")
  qa <- list(status = "unchecked")
  mask <- list(label = "test-mask")
  dc <- matrix(1:6, nrow = 3, ncol = 2)
  cube <- make_contract_cube(
    units = list(oxygen = "mmol m-3", temperature = "degC"),
    source = "synthetic",
    dataset_id = "contract-fixture",
    spatial_extent = c(-90, -70, -20, 0),
    temporal_extent = as.Date(c("2019-01-01", "2022-01-01")),
    depth_extent = c(-10, 100),
    mask = mask,
    dc = dc,
    climatology = list(scale = "month"),
    anomaly = list(method = "difference"),
    provenance = provenance,
    qa = qa
  )

  expect_identical(unname(dim(cube$data)), c(3L, 2L, 2L, 4L, 2L))
  expect_identical(cube$source, "synthetic")
  expect_identical(cube$dataset_id, "contract-fixture")
  expect_identical(cube$mask, mask)
  expect_identical(cube$dc, dc)
  expect_identical(cube$provenance$provider, provenance$provider)
  expect_identical(cube$provenance$request, provenance$request)
  expect_identical(cube$provenance$time$canonical_class, "Date")
  expect_identical(cube$qa, qa)
  expect_identical(cube$units, list(oxygen = "mmol m-3", temperature = "degC"))
  expect_true(.check_cube(cube))
})

test_that("dimension labels are descriptive and not part of the primary contract", {
  dimension_names <- make_contract_cube()
  dimnames_names <- make_contract_cube()
  names(dim(dimension_names$data)) <- c("x", "y", "z", "t", "v")
  names(dimnames(dimnames_names$data)) <- c("X", "Y", "Z", "T", "V")

  expect_true(.check_cube(dimension_names))
  expect_true(.check_cube(dimnames_names))
  expect_identical(unname(dim(dimension_names$data)), c(3L, 2L, 2L, 4L, 2L))
})

test_that("empty coordinates and variables identify the responsible component", {
  expect_error(
    make_contract_cube(lon = numeric(0)),
    "Invalid `lon`: must not be empty"
  )
  expect_error(
    make_contract_cube(lat = numeric(0)),
    "Invalid `lat`: must not be empty"
  )
  expect_error(
    make_contract_cube(depth = numeric(0)),
    "Invalid `depth`: must not be empty"
  )
  expect_error(
    make_contract_cube(time = as.Date(character(0))),
    "Invalid `time`: must not be empty"
  )
  expect_error(
    make_contract_cube(vars = character(0), units = NULL),
    "Invalid `vars`: must contain at least one"
  )
})

test_that("invalid coordinate values and variable names are rejected clearly", {
  expect_error(
    make_contract_cube(lon = c("west", "east", "zero")),
    "Invalid `lon`: must be numeric"
  )
  expect_error(
    make_contract_cube(lon = c(-181, 0, 361)),
    "Invalid `lon`: values must follow"
  )
  expect_error(
    make_contract_cube(lat = c(-91, 0)),
    "Invalid `lat`: values must be between -90 and 90"
  )
  expect_error(
    make_contract_cube(time = c(1, 2, 3, 4)),
    "Invalid `time`: must be Date, POSIXct, or unambiguous ISO character"
  )
  expect_error(
    make_contract_cube(vars = c("temperature", "temperature"), units = NULL),
    "Invalid `vars`: must not contain duplicates"
  )
  expect_error(
    make_contract_cube(vars = c("temperature", ""), units = NULL),
    "Invalid `vars`: must contain non-empty"
  )
})

test_that("storage and dimensional errors report expected and obtained shapes", {
  expect_error(
    make_contract_cube(data = seq_len(96)),
    "Invalid `data`: must be a numeric array"
  )
  expect_error(
    make_contract_cube(data = array(letters[1:24], dim = c(3, 2, 2, 2))),
    "Invalid `data`: must be a numeric array"
  )
  expect_error(
    make_contract_cube(data = array(1, dim = c(3, 2, 8))),
    "must have 5 dimensions.*or 4 dimensions"
  )
  expect_error(
    make_contract_cube(data = array(1, dim = c(3, 2, 2, 4, 2, 1))),
    "must have 5 dimensions.*or 4 dimensions"
  )
  expect_error(
    make_contract_cube(data = array(1:96, dim = c(2, 3, 2, 4, 2))),
    "expected.*3 x 2 x 2 x 4 x 2.*obtained.*2 x 3 x 2 x 4 x 2"
  )
  expect_error(
    make_contract_cube(data = array(numeric(0), dim = c(3, 2, 2, 0, 2))),
    "expected.*3 x 2 x 2 x 4 x 2.*obtained.*3 x 2 x 2 x 0 x 2"
  )
})

test_that("units must map unambiguously to variables", {
  expect_error(
    make_contract_cube(units = "degC"),
    "Invalid `units`: length must match"
  )
  expect_error(
    make_contract_cube(units = c(foo = "degC", bar = "1")),
    "Invalid `units`: names must match"
  )
  expect_error(
    make_contract_cube(units = c(temperature = "degC", temperature = "1")),
    "Invalid `units`: names must be unique"
  )
  expect_error(
    make_contract_cube(units = 1:2),
    "Invalid `units`: must be NULL, a character vector, or a list"
  )
})

test_that("extents must be finite, ordered, and cover their coordinates", {
  expect_error(
    make_contract_cube(spatial_extent = c(-Inf, Inf, -20, 0)),
    "Invalid `spatial_extent`: must contain four finite"
  )
  expect_error(
    make_contract_cube(spatial_extent = c(-79, -78, -20, 0)),
    "Invalid `spatial_extent`: must contain the longitude and latitude"
  )
  expect_error(
    make_contract_cube(temporal_extent = as.Date(c("2020-02-01", "2020-01-01"))),
    "Invalid `temporal_extent`: must be ordered"
  )
  expect_error(
    make_contract_cube(temporal_extent = as.Date(c("2020-01-15", "2022-01-01"))),
    "Invalid `temporal_extent`: must contain the time coordinate"
  )
  expect_error(
    make_contract_cube(depth_extent = c(-Inf, Inf)),
    "Invalid `depth_extent`: must contain two finite"
  )
  expect_error(
    make_contract_cube(depth_extent = c(60, 100)),
    "Invalid `depth_extent`: must contain the depth coordinate"
  )
})

test_that("the validator diagnoses mutated or incomplete objects", {
  cube <- make_contract_cube()
  missing_lon <- cube
  missing_lon$lon <- NULL
  permuted <- cube
  dim(permuted$data) <- c(2, 3, 2, 4, 2)
  bad_storage <- cube
  bad_storage$data <- list(values = cube$data)

  expect_error(
    .check_cube(missing_lon),
    "missing fields: lon"
  )
  expect_error(
    .check_cube(permuted),
    "expected.*3 x 2 x 2 x 4 x 2.*obtained.*2 x 3 x 2 x 4 x 2"
  )
  expect_error(
    .cube_backend(bad_storage),
    "Cannot determine.*backend.*`x\\$data`"
  )
  expect_error(
    .cube_backend(list(data = cube$data)),
    "must inherit from <ocean_cube>"
  )
})

test_that("construction is observably immutable and the public signature is stable", {
  longitude <- c(-80, -79, -78)
  latitude <- c(-12, -11)
  depth <- c(0, 50)
  time <- as.Date(c(
    "2020-01-01", "2020-02-01",
    "2021-01-01", "2021-02-01"
  ))
  vars <- c("temperature", "oxygen")
  data <- array(seq_len(96), dim = c(3, 2, 2, 4, 2))
  originals <- list(longitude, latitude, depth, time, vars, data)

  cube <- ocean_cube(
    lon = longitude,
    lat = latitude,
    depth = depth,
    time = time,
    vars = vars,
    data = data
  )

  expect_identical(longitude, originals[[1]])
  expect_identical(latitude, originals[[2]])
  expect_identical(depth, originals[[3]])
  expect_identical(time, originals[[4]])
  expect_identical(vars, originals[[5]])
  expect_identical(data, originals[[6]])
  expect_s3_class(cube, "ocean_cube")
  expect_identical(
    names(formals(ocean_cube)),
    c(
      "lon", "lat", "time", "data", "depth", "vars", "units", "source",
      "dataset_id", "spatial_extent", "temporal_extent", "depth_extent",
      "mask", "dc", "climatology", "anomaly", "provenance", "qa"
    )
  )
})

test_that("existing scientific operations remain compatible with memory cubes", {
  cube <- .make_baseline_fixture()$cube
  monthly <- clim_month(cube)
  layer <- layer_mean(cube, c(0, 50))
  linked <- link_events(
    cube,
    data.frame(
      lon = -80,
      lat = -12,
      date = as.Date("2020-01-01")
    ),
    vars = "temperature"
  )

  expect_no_error(print(cube))
  expect_no_error(summary(cube))
  expect_s3_class(monthly, "ocean_clim")
  expect_equal(monthly$mean[1, 1, 1, 1, 1], 12111)
  expect_equal(layer$data[1, 1, 1, 1, 1], 11161)
  expect_equal(linked$temperature_value, 11111)
  expect_identical(.cube_backend(cube), "memory")
})
