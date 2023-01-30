library(sf)
testthat::test_that("Check if output contains only one geometry type",{
  expect_length(
  (illuminate::merge_kml("G:\\My Drive\\01_Professional\\08_REACH_IRAQ_UPDATE\\mh1\\01_DS\\02_ICRC\\Telafar assessment bundle\\kml_export/all/")|>
     st_geometry_type() |> unique() |> length()),1
  )
})

