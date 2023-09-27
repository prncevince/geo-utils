library(sf)
library(dplyr)
library(purrr)
library(terra)
library(tidyr)
library(tools)
library(tibble)
library(tictoc)
library(ggplot2)
library(ggrepel)
library(jsonlite)
library(base64enc)
library(RStoolbox)

# overlays simple features onto RGB GeoTIFF & adds labels from metadata 
# takes a larger colorized GeoTIFF as a basemap & crops down to the extent of another fully contained GeoTIFF
#' @param color_label e.g. "blue"
#' @param d_sf_1 sf dataframe with POINT type geometry
#' @param d_sf_2 sf dataframe of POLYGON type geometries
plot_overlay_gg <- function(d_p, d_sf_1, d_sf_2, dpi, color_label_1, color_label_2) {
  sr <- rast(d_p[['p_large']])
  sr_ext <- rast(d_p[['p_small']])
  bbox <- ext(sr_ext) |> project(from = crs(sr_ext), to = crs(sr)) |> as.vector()
  # png(d_p[['p_out']], width = ncol(sr_ext), height = nrow(sr_ext))
  d_sf_1 <- d_sf_1 |> filter(image_name == d_p[['f_in']]) |>
    st_transform(crs(sr, proj=T, describe=T) |> pluck('code') |> as.integer())
  d_sf_2 <- d_sf_2 |> filter(image_name == d_p[['f_in']]) |>
    st_transform(crs(sr, proj=T, describe=T) |> pluck('code') |> as.integer())
  g <- ggRGB(sr, r = 1, g = 2, b = 3, maxpixels = Inf) + # 1e6, stretch = 'hist' 
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + 
    theme_void() +
    geom_sf(
      data = d_sf_2,
      fill = scales::alpha(color_label_2, alpha = 0.25), color = color_label_2
    )
  if (nrow(d_sf_1) > 0) {
    g <- g + 
      geom_sf(
        data = d_sf_1, color = color_label_1
      ) +
      geom_label_repel(
        data = bind_cols(d_sf_1, st_coordinates(d_sf_1)), 
        aes(x = X, y = Y, label = paste0(toTitleCase(label_1), '\n', toTitleCase(label_2), '\n', label_3)),
        hjust = 0,
        fill = scales::alpha(color_label_1, alpha = 0.35),
        segment.color = color_label_1,
        seed = 1,
        point.padding = 0.5, box.padding = 1.5, 
        nudge_x = .15,
        nudge_y = -.5,
        segment.linetype = 6,
        segment.curvature = 1e-20,
        arrow = arrow(length = unit(2, "mm")),
        max.overlaps = Inf
      )
  }
  g <- g + coord_sf(xlim =  bbox[c("xmin", "xmax")], ylim = bbox[c("ymin", "ymax")])
  # print(g)
  # dev.off()
  ggsave(
    plot = g, filename = d_p[['p_out']], limitsize = FALSE,
    width = ncol(sr_ext), height = nrow(sr_ext), units = "px", dpi = dpi
  )
}

tic()
sr <- rast(p_in)
png(p_out, width = ncol(sr), height = nrow(sr))
# parameters that can be changed `maxcell` & `stretch`
# plotRGB(sr, stretch='hist', maxcell=Inf)
# plotRGB(sr, stretch='lin')
plotRGB(stretch(sr, minq = 0.001, maxq = 0.999))
plot(
  d_sf |> pull(geometry),
  border = "green3", col = scales::alpha("green3", alpha = 0.10), add = TRUE
)
dev.off()
toc()
