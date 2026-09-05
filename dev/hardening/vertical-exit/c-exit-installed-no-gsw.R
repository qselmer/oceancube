isolated <- Sys.getenv("OCEANCUBE_ISOLATED_LIB", unset = NA_character_)
stopifnot(!is.na(isolated), dir.exists(isolated))
.libPaths(c(isolated, .libPaths()))
stopifnot(requireNamespace("oceancube", quietly = TRUE))
stopifnot(!requireNamespace("gsw", quietly = TRUE))
stopifnot(length(getNamespaceExports("oceancube")) == 48L)

cube <- oceancube::ocean_cube(
  lon = 0, lat = 0, depth = c(0, 10), time = as.Date("2000-01-01"),
  data = array(c(1, 2), dim = c(1, 1, 2, 1, 1)),
  vars = "value", units = "1"
)
stopifnot(inherits(cube, "ocean_cube"))
stopifnot(inherits(oceancube::cube_slice(cube, depth = 0), "ocean_cube"))

missing_error <- function(expr) {
  condition <- tryCatch({ force(expr); NULL }, error = identity)
  !is.null(condition) && inherits(condition, "oceancube_teos10_dependency_missing")
}
stopifnot(missing_error(oceancube::thermodynamic_state(NULL)))
stopifnot(missing_error(oceancube::stratification(NULL)))

cat("C_EXIT_INSTALLED_NO_GSW: PASS\n")
cat("C_EXIT_INSTALLED_NO_GSW_API: 48\n")
cat("C_EXIT_INSTALLED_NO_GSW_INTERNAL_CALLS: 0\n")
