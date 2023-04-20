if (requireNamespace('terra', quietly = TRUE)) {

  w <- id_to_huc(c("071000050101", "071000050102",
                   "071000050103", "071000050104"), layerid = 6)
  # terra::plot(w)
  expect_true(inherits(w, 'SpatVector'))

  x <- point_to_huc(-94.0671, 43.026, layerid = 5)
  # terra::plot(x)
  expect_true(inherits(x, 'SpatVector'))

  # bounding box/envelope numeric input
  y <- envelope_to_huc(terra::ext(x), layerid = 5)
  # terra::plot(y)
  expect_true(inherits(y, 'SpatVector'))

  # SpatVector polygon (Prairie Creek rect extent) as input
  z <- polygon_to_huc(terra::as.polygons(y[1,], ext = TRUE), layerid = 5)
  # terra::plot(z)
  # terra::plot(terra::as.polygons(y[1,], ext = TRUE), col = "RED", add = TRUE)
  expect_true(inherits(z, 'SpatVector'))
}
