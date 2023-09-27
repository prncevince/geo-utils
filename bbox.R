library(sf)
library(tibble)

#' @param lon1 left longitude [-180, 180]
#' @param lon2 right longitude [-180, 180]
#' @param lat1 bottom latitude [-90, 90]
#' @param lat2 top latitude [-90, 90]
bbox_d <- function(lon1, lon2, lat1, lat2, crs = 4326) {
  d_bbox <- tibble(
    st_p_tl = st_sfc(list(st_point(c(lon1, lat2)))),
    st_p_tr = st_sfc(list(st_point(c(lon2, lat2)))),
    st_p_br = st_sfc(list(st_point(c(lon2, lat1)))),
    st_p_bl = st_sfc(list(st_point(c(lon1, lat1))))
  )
  d_bbox <- d_bbox %>% mutate(st_sfc_mp_box = st_sfc(
      list(c(st_p_tl[[1]], st_p_tr[[1]], st_p_br[[1]], st_p_bl[[1]], st_p_tl[[1]])),
      crs = crs
  ))
  d_bbox <- d_bbox %>% mutate(st_sfc_ls_box = st_sfc(
      list(st_linestring(st_sfc_mp_box[[1]])), crs = crs
  ))
  d_bbox <- d_bbox %>% mutate(st_sfc_poly = st_sfc(
    st_polygon(st_sfc_ls_box), crs = crs
  ))
  return(d_bbox)
}
