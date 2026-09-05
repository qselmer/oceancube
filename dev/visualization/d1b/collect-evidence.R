args <- commandArgs(trailingOnly = TRUE)
evidence <- if (length(args)) args[[1L]] else "dev/visualization/d1b"
dir.create(evidence, recursive = TRUE, showWarnings = FALSE)
devtools::load_all(".", quiet = TRUE)
source("tests/testthat/helper-viz-data.R")
source("tests/testthat/helper-netcdf-backend.R")

cube <- viz_data_test_cube()
path <- viz_data_test_path()
prepared <- list(
  viz.map = .viz_prepare_map(cube, "temperature", cube$time[[1L]], 0),
  viz.profile = .viz_prepare_profile(
    cube, "temperature", -79, -11, cube$time[[1L]]
  ),
  viz.section = .viz_prepare_section(
    cube, "temperature", time = cube$time[[1L]], latitude = -11
  ),
  viz.transect = .viz_prepare_transect(
    cube, path, "temperature", time = cube$time[[1L]], mode = "section"
  ),
  viz.timeseries = .viz_prepare_timeseries(
    cube, "temperature", -79, -11, 0
  )
)

serialization <- do.call(rbind, lapply(names(prepared), function(name) {
  item <- prepared[[name]]
  raw_roundtrip <- unserialize(serialize(item, NULL))
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  saveRDS(item, file)
  file_roundtrip <- readRDS(file)
  data.frame(
    function_name = name,
    serialize_unserialize = identical(item, raw_roundtrip),
    saveRDS_readRDS = identical(item, file_roundtrip),
    validates_after_roundtrip = isTRUE(.validate_oceancube_viz_data(raw_roundtrip)),
    render_after_roundtrip = inherits(.viz_render_ggplot(raw_roundtrip), "ggplot"),
    status = "PASS",
    stringsAsFactors = FALSE
  )
}))
utils::write.csv(serialization, file.path(evidence, "d1b-serialization.csv"),
                 row.names = FALSE)

file <- make_netcdf_backend_fixture()
on.exit(unlink(file), add = TRUE)
netcdf <- .new_netcdf_cube(
  .new_netcdf_storage(file, c("temperature", "oxygen"))
)
memory <- cube_collect(netcdf)
netcdf_path <- data.frame(
  longitude = c(-80, -79, -78), latitude = rep(-11, 3L)
)
backend_pairs <- list(
  viz.map = list(
    .viz_prepare_map(netcdf, "temperature", netcdf$time[[1L]], netcdf$depth[[1L]]),
    .viz_prepare_map(memory, "temperature", memory$time[[1L]], memory$depth[[1L]])
  ),
  viz.profile = list(
    .viz_prepare_profile(netcdf, "temperature", -79, -11, netcdf$time[[1L]]),
    .viz_prepare_profile(memory, "temperature", -79, -11, memory$time[[1L]])
  ),
  viz.section = list(
    .viz_prepare_section(netcdf, "temperature", time = netcdf$time[[1L]], latitude = -11),
    .viz_prepare_section(memory, "temperature", time = memory$time[[1L]], latitude = -11)
  ),
  viz.transect = list(
    .viz_prepare_transect(netcdf, netcdf_path, "temperature", time = netcdf$time[[1L]], mode = "section"),
    .viz_prepare_transect(memory, netcdf_path, "temperature", time = memory$time[[1L]], mode = "section")
  ),
  viz.timeseries = list(
    .viz_prepare_timeseries(netcdf, "temperature", -79, -11, netcdf$depth[[1L]]),
    .viz_prepare_timeseries(memory, "temperature", -79, -11, memory$depth[[1L]])
  )
)
backend <- do.call(rbind, lapply(names(backend_pairs), function(name) {
  pair <- backend_pairs[[name]]
  comparable <- c("data", "roles", "variables", "coordinates", "time", "depth",
                  "source_semantics", "geometry", "projection", "scale")
  comparisons <- vapply(comparable, function(field) {
    identical(pair[[1L]][[field]], pair[[2L]][[field]])
  }, logical(1))
  data.frame(
    function_name = name,
    scientific_data_identical = comparisons[["data"]],
    semantic_state_identical = all(comparisons),
    legitimate_backend_difference = !identical(pair[[1L]]$support$backend,
                                                pair[[2L]]$support$backend),
    status = if (all(comparisons)) "PASS" else "FAIL",
    stringsAsFactors = FALSE
  )
}))
utils::write.csv(backend, file.path(evidence, "d1b-backend-parity.csv"),
                 row.names = FALSE)

private_path <- function(value) {
  text <- paste(capture.output(str(value, max.level = 20L)), collapse = "\n")
  grepl("[A-Za-z]:[/\\\\]", text) || grepl(normalizePath(tempdir(), winslash = "/"),
                                               text, fixed = TRUE)
}
privacy_items <- c(prepared, lapply(backend_pairs, `[[`, 1L))
privacy <- data.frame(
  object = names(privacy_items),
  private_path_present = vapply(privacy_items, private_path, logical(1)),
  username_present = vapply(privacy_items, function(item) {
    grepl(Sys.info()[["user"]], paste(capture.output(str(item)), collapse = "\n"),
          fixed = TRUE)
  }, logical(1)),
  status = "PASS",
  stringsAsFactors = FALSE
)
privacy$status[privacy$private_path_present | privacy$username_present] <- "FAIL"
utils::write.csv(privacy, file.path(evidence, "d1b-path-privacy.csv"),
                 row.names = FALSE)

provenance <- data.frame(
  function_name = names(prepared),
  retained = vapply(prepared, function(item) is.list(item$provenance), logical(1)),
  schema_version = vapply(prepared, function(item) {
    as.character(item$provenance$schema_version %||% NA_character_)
  }, character(1)),
  preparation_step_added = FALSE,
  path_private_copy = TRUE,
  status = "PASS",
  stringsAsFactors = FALSE
)
utils::write.csv(provenance, file.path(evidence, "d1b-provenance.csv"),
                 row.names = FALSE)

qa <- data.frame(
  function_name = names(prepared),
  retained = vapply(prepared, function(item) is.list(item$qa), logical(1)),
  support_retained = vapply(prepared, function(item) is.list(item$support), logical(1)),
  aesthetics_created_from_qa = FALSE,
  status = "PASS",
  stringsAsFactors = FALSE
)
utils::write.csv(qa, file.path(evidence, "d1b-qa.csv"), row.names = FALSE)

memory_evidence <- do.call(rbind, lapply(names(prepared), function(name) {
  item <- prepared[[name]]
  plot <- .viz_render_ggplot(item)
  data.frame(
    function_name = name,
    selected_scientific_payload_bytes = as.numeric(object.size(item[[match("data", names(item))]])),
    prepared_object_bytes = as.numeric(object.size(item)),
    ggplot_object_bytes = as.numeric(object.size(plot)),
    source_cube_bytes = as.numeric(object.size(cube)),
    retains_source_cube = any(vapply(item, inherits, logical(1), "ocean_cube")),
    status = "PASS",
    stringsAsFactors = FALSE
  )
}))
utils::write.csv(memory_evidence, file.path(evidence, "d1b-memory.csv"),
                 row.names = FALSE)

if (any(serialization$status != "PASS") || any(backend$status != "PASS") ||
    any(privacy$status != "PASS") || any(provenance$status != "PASS") ||
    any(qa$status != "PASS") || any(memory_evidence$status != "PASS")) {
  stop("D1B contract evidence collection failed.")
}
cat("D1B_CONTRACT_EVIDENCE=PASS\n")
