test_that("the architecture detector recognizes code but ignores text", {
  expect_true(.contains_direct_cube_data_access(parse(text = "x$data")[[1L]]))
  expect_true(.contains_direct_cube_data_access(parse(text = "x[[\"data\"]]")[[1L]]))
  expect_true(.contains_direct_cube_data_access(parse(text = "x[['data']]")[[1L]]))
  expect_true(
    .contains_direct_cube_data_access(
      parse(text = "getElement(x, \"data\")")[[1L]]
    )
  )
  expect_true(
    .contains_direct_cube_data_access(
      parse(text = "base::getElement((x), 'data')")[[1L]]
    )
  )
  expect_false(
    .contains_direct_cube_data_access(
      parse(text = "message('x$data')")[[1L]]
    )
  )
  expect_false(
    .contains_direct_cube_data_access(
      parse(text = "x$dataset_id")[[1L]]
    )
  )
  expect_false(
    .contains_direct_cube_data_access(
      parse(text = "function(data) dim(data)")[[1L]]
    )
  )
  expect_false(
    .contains_direct_cube_data_access(
      parse(text = "# x$data is documentation\n1")[[1L]]
    )
  )
})

test_that("direct ocean_cube storage access is confined to backend-memory.R", {
  r_directory <- testthat::test_path("..", "..", "R")
  files <- list.files(
    r_directory,
    pattern = "\\.[Rr]$",
    full.names = TRUE
  )
  authorized <- "backend-memory.R"
  inspected <- files[basename(files) != authorized]
  violations <- unlist(
    lapply(
      inspected,
      function(path) {
        functions <- .direct_access_violations(path)
        if (length(functions) == 0L) {
          return(character())
        }
        paste0("R/", basename(path), ": ", functions)
      }
    ),
    use.names = FALSE
  )

  if (length(violations) > 0L) {
    testthat::fail(
      paste(
        c(
          "Direct access to ocean_cube storage is not allowed outside backend files:",
          violations
        ),
        collapse = "\n"
      )
    )
  }
  expect_length(violations, 0L)
})
