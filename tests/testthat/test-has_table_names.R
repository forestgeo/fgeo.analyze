context("has_table_names")

test_that("returns TRUE with a tree table and FALSE with other tables", {
  skip_if_not_installed("fgeo.data")

  tree <- fgeo.data::luquillo_tree6_1ha
  stem <- fgeo.data::luquillo_stem6_1ha
  vft <- fgeo.data::luquillo_vft_4quad
  taxa <- fgeo.data::luquillo_taxa
  spp <- fgeo.data::luquillo_species

  expect_false(has_table_names(tree)(stem))
  expect_true(has_table_names(tree)(tree))
  expect_true(has_table_names(stem)(stem))
  expect_true(has_table_names(vft)(vft))
  expect_true(has_table_names(spp)(spp))
  expect_true(has_table_names(taxa)(taxa))
})
