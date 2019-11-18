#' Plot a timeline of earthquake
#'
#' Geom for plotting a time line of earthquakes ranging from xmin to xmaxdates
#' with a point for each earthquake
#'
#' @importFrom ggplot2 layer
#'
#' @inheritParams ggplot2::layer
#' @param mapping Set of aesthetic mappings created
#' @param data  The data to be displayed
#' @param stat  The statistical transformation to use on the data for this layer,
#' as a string.
#' @param position Position adjustment, either as a string, or the result of a
#' call to a position adjustment function.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#' a warning. If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than
#' combining with them.
#' @param ... Other arguments passed on to the layer. it can be used to set an
#' aesthetic to a fixed value.
#'
#' @return This function returns a timeline plot
#'
#' @examples
#' \dontrun{
#' df %>% filter(COUNTRY %in% c("RUSSIA", "ISRAEL")) %>%
#'   ggplot(aes(x = date, size = EQ_PRIMARY, y = COUNTRY, col= DEATHS)) +
#'       geom_timeline() +
#'       scale_size_continuous(name = 'Richter scale value') +
#'       scale_color_continuous(name = '# deaths')
#'}
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", show.legend = NA, na.rm = FALSE,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomTimeline,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )}

#' New ggproto class for ggplot2: timeline
#'
#' @importFrom ggplot2 ggproto aes draw_key_point
#' @importFrom grid gList pointsGrob gpar segmentsGrob gpar
#'
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c('x'),
                                 default_aes = ggplot2::aes(y = NULL, color = 'black', shape = 21, stroke = 0.5,
                                                            fill = 'black', size = 5, alpha = 0.5),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_group = function(data, panel_params, coord) {
                                   coords <- coord$transform(data, panel_params)
                                   segment <- grid::segmentsGrob(
                                     x0 = min(coords$x),
                                     x1 = max(coords$x),
                                     y0 = coords$y,
                                     y1 = coords$y,
                                     gp = grid::gpar(
                                       col = "gray50",
                                       lwd = 1))
                                   points <- grid::pointsGrob(
                                     x = coords$x,
                                     y = coords$y,
                                     size = unit(coords$size/4, "char"),
                                     pch = coords$shape,
                                     gp = grid::gpar(
                                       col = coords$colour,
                                       fill = coords$fill,
                                       alpha = coords$alpha))
                                   grid::gTree(children = grid::gList(segment, points))
                                 }
)


#' Plot a timeline of earthquake with annotations
#'
#' Geom for plotting a time line of earthquakes ranging from xmin to xmaxdates
#' with a point for each earthquake. Additionally, this geom adds a vertical
#' line to each data point with a text annotation (e.g. the location of the
#' earthquake) attached to each line.
#'
#' @importFrom ggplot2 layer
#'
#' @inheritParams ggplot2::layer
#' @param mapping Set of aesthetic mappings created
#' @param data  The data to be displayed
#' @param stat  The statistical transformation to use on the data for this layer,
#' as a string.
#' @param position Position adjustment, either as a string, or the result of a
#' call to a position adjustment function.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#' a warning. If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than
#' combining with them.
#' @param ... Other arguments passed on to the layer. it can be used to set an
#' aesthetic to a fixed value.
#'
#' @return This function returns a timeline plot with annotations
#'
#' @examples
#' \dontrun{
#' df %>% filter(COUNTRY %in% c("RUSSIA", "ISRAEL"), date > "2000-01-01") %>%
#'  ggplot(aes(x = date, size = EQ_PRIMARY, y = COUNTRY, col = DEATHS)) +
#'  geom_timeline() +
#'  scale_size_continuous(name = 'Richter scale value') +
#'  scale_color_continuous(name = '# deaths')+
#'  geom_timeline_label(aes(label=LOCATION_NAME), n_max=1)
#'  }
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", show.legend = NA, na.rm = FALSE,
                                inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomTimelineLabel,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' New ggproto class for ggplot2: timelinelabel
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom dplyr group_by top_n
#' @importFrom magrittr "%>%"
#' @importFrom grid polylineGrob textGrob  gpar gTree gList
#'
#' @export
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                      required_aes = c("x", "label"),
                                      default_aes = ggplot2::aes(y=NULL, n_max=3),
                                      draw_key = ggplot2::draw_key_point,
                                      draw_panel = function(data, panel_params, coord) {
                                        data = data %>% group_by(y) %>% top_n(data$n_max[1], size)
                                        coords <- coord$transform(data, panel_params)
                                        lines <- grid::polylineGrob(
                                          x = unit(c(coords$x, coords$x), "npc"),
                                          y = unit(c(coords$y, coords$y + 0.1), "npc"),
                                          id = rep(1:dim(coords)[1], 2),
                                          gp = grid::gpar(col = "grey"))
                                        
                                        names <- grid::textGrob(label = coords$label,
                                                                x = unit(coords$x, "npc"),
                                                                y = unit(coords$y + 0.1, "npc"),
                                                                just = c("left", "bottom"),
                                                                rot = 30,
                                                                gp=grid::gpar(
                                                                  fontsize=8,
                                                                  col = "black"))
                                        grid::gTree(children = grid::gList(lines, names))
                                      }
)