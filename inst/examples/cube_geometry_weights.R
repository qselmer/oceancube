# Geometría cuantitativa para un ocean_cube.
#
# La geometría describe celdas e intersecciones. Un indicador resume valores
# oceanográficos con una definición científica. oceancube produce lo primero;
# el cálculo e inferencia de indicadores 2D/3D pertenece a spatind.

if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Este ejemplo requiere el paquete sugerido `sf`.")
}

# 1. Cubo pequeño con bounds horizontales explícitos.
lon <- structure(c(-80, -79), bounds = c(-80.5, -79.5, -78.5))
lat <- structure(c(-12, -10), bounds = c(-13, -11, -9))
depth <- structure(c(5, 20), units = "m", positive = "down")
values <- array(
  seq_len(2 * 2 * 2),
  dim = c(2, 2, 2, 1, 1)
)
cube <- ocean_cube(
  lon = lon,
  lat = lat,
  depth = depth,
  time = as.Date("2020-01-01"),
  data = values,
  vars = "temperature"
)

# 2. Área: la forma es longitud × latitud y cambia con la latitud.
area_m2 <- cube_cell_area(cube, unit = "m2")
area_km2 <- cube_cell_area(cube, unit = "km2")
stopifnot(
  identical(dim(area_m2), c(2L, 2L)),
  isTRUE(all.equal(as.numeric(area_m2) / 1e6, as.numeric(area_km2))),
  area_m2[1, 1] != area_m2[1, 2]
)

# 3. Bounds verticales explícitos: nunca se infieren desde los centros.
depth_bounds <- structure(c(0, 10, 30), units = "m")
thickness_m <- cube_layer_thickness(cube, depth_bounds, unit = "m")
volume_m3 <- cube_cell_volume(cube, depth_bounds, unit = "m3")
stopifnot(
  identical(as.numeric(thickness_m), c(10, 20)),
  isTRUE(all.equal(
    as.numeric(volume_m3[, , 1]),
    as.numeric(area_m2) * thickness_m[1]
  ))
)

# 4. Tres features separadas: dos superpuestas y una parcialmente externa.
rectangle <- function(xmin, ymin, xmax, ymax) {
  sf::st_polygon(list(rbind(
    c(xmin, ymin), c(xmax, ymin), c(xmax, ymax),
    c(xmin, ymax), c(xmin, ymin)
  )))
}
shell <- rbind(
  c(-80.5, -13), c(-79.5, -13), c(-79.5, -11),
  c(-80.5, -11), c(-80.5, -13)
)
hole <- rbind(
  c(-80.2, -12.4), c(-80.2, -11.6), c(-79.8, -11.6),
  c(-79.8, -12.4), c(-80.2, -12.4)
)
polygons <- sf::st_sf(
  feature = c("con_hueco", "superpuesto", "parcial"),
  geometry = sf::st_sfc(
    sf::st_polygon(list(shell, hole)),
    rectangle(-80.5, -13, -79.5, -11),
    rectangle(-81, -13, -80, -11),
    crs = 4326
  )
)

# 5. La intersección polígono-celda genera fracción y área efectiva.
weights_2d <- cube_polygon_weights(
  cube,
  polygons,
  id_col = "feature",
  dimension = "2d"
)
stopifnot(
  all(weights_2d$fraction_cell_covered >= 0),
  all(weights_2d$fraction_cell_covered <= 1),
  isTRUE(all.equal(
    weights_2d$effective_area_m2,
    weights_2d$cell_area_m2 * weights_2d$fraction_cell_covered
  )),
  length(unique(weights_2d$feature_order)) == 3L,
  any(weights_2d$fraction_polygon_covered_by_grid < 1)
)

# 6. El formato 3D añade profundidad, espesor y volumen efectivo.
weights_3d <- cube_polygon_weights(
  cube,
  polygons,
  id_col = "feature",
  dimension = "3d",
  depth_bounds = depth_bounds
)
stopifnot(
  all(c("depth_index", "cell_volume_m3", "effective_volume_m3") %in%
        names(weights_3d)),
  isTRUE(all.equal(
    weights_3d$effective_volume_m3,
    weights_3d$cell_volume_m3 * weights_3d$fraction_cell_covered
  ))
)

# 7. Las claves permiten enlazar una tabla de valores por índice. El enlace y
# cualquier indicador posterior se deciden fuera de estas primitivas.
keys_2d <- c("longitude_index", "latitude_index")
keys_3d <- c(keys_2d, "depth_index")
stopifnot(
  all(keys_2d %in% names(weights_2d)),
  all(keys_3d %in% names(weights_3d))
)

# Estas llamadas usaron únicamente coordenadas, bounds y polígonos: no leyeron
# el array científico del backend. La procedencia confirma que el producto es
# sólo geométrico y que no se calculó un indicador. Conceptualmente, una tabla
# de valores enlazada con estas claves y pesos puede pasar después a spatind.
weights_2d_provenance <- attr(weights_2d, "oceancube_provenance")
weights_3d_provenance <- attr(weights_3d, "oceancube_provenance")
weights_2d_operation <- tail(weights_2d_provenance$history, 1L)[[1L]]
weights_3d_operation <- tail(weights_3d_provenance$history, 1L)[[1L]]
stopifnot(
  grepl("no indicator", weights_2d_operation$parameters$resolved$role),
  identical(weights_3d_operation$software$package, "oceancube")
)

cat(
  "GEOMETRÍA Y PESOS COMPLETADOS: oceancube generó insumos 2D/3D sin calcular indicadores espaciales.\n"
)
