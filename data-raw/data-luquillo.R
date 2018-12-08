cns <- fgeo.data::luquillo_tree6_random
top3_sp <- cns %>%
  dplyr::count(sp) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::top_n(3) %>%
  dplyr::pull(sp)
luquillo_top3_sp <- dplyr::filter(cns, sp %in% top3_sp)
use_data(luquillo_top3_sp, overwrite = TRUE)
