#' Vertical Terrorbars
#'
#' Creates errorbars inspired by Figure 9 of the now retracted classic *["Monitoring of Sports Health Indicators Based on Wearable Nanobiosensors"](https://www.hindawi.com/journals/amse/2022/3802603/)* (Gong & Liu, retracted). Unlike the original figure, graphics created with `geom_terrorbar` are actual errorbars.
#'
#' @param mapping Set of aesthetic mappings created by [`ggplot2::aes()`]. If specified and `inherit.aes = TRUE` (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#' If `NULL`, the default, the data is inherited from the plot data as specified in the call to [`ggplot2::ggplot()`].
#' A `data.frame`, or other object, will override the plot data. All objects will be fortified to produce a data frame. See [`ggplot2::fortify()`] for which variables will be created.
#' A `function` will be called with a single argument, the plot data. The return value must be a `data.frame`, and will be used as the layer data. A `function` can be created from a formula (*e.g.,* `~ head(.x, 10)`).
#' @param stat The statistical transformation to use on the data for this layer, either as a `ggproto Geom` subclass or as a string naming the stat stripped of the `stat_` prefix (*e.g.,* `"count"` rather than `"stat_count"`)
#' @param connect logical. Whether or not the Ts should be connected with a line
#' @param position Position adjustment, either as a string naming the adjustment (*e.g.,* `"jitter"` to use `position_jitter`), or the result of a call to a position adjustment function. Use the latter if you need to change the settings of the adjustment.
#' @param ... Other arguments passed on to [`ggplot2::layer()`]. These are often aesthetics, used to set an aesthetic to a fixed value, like `colour = "red"` or `size = 3`. They may also be parameters to the paired geom/stat.
#' @param na.rm If `FALSE`, the default, missing values are removed with a warning. If `TRUE`, missing values are silently removed.
#' @param orientation The orientation of the layer. The default (`NA`) automatically determines the orientation from the aesthetic mapping. In the rare event that this fails it can be given explicitly by setting orientation to either `"x"` or `"y"`. See the *Orientation` section for more detail.
#' @param show.legend logical. Should this layer be included in the legends? `NA`, the default, includes if any aesthetics are mapped. `FALSE` never includes, and `TRUE` always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, *e.g.,* [`ggplot2::borders()`].
#'
#' @details
#' # Orientation
#' This geom treats each axis differently and, thus, can thus have two orientations. Often the orientation is easy to deduce from a combination of the given mappings and the types of positional scales in use. Thus, ggplot2 will by default try to guess which orientation the layer should have. Under rare circumstances, the orientation is ambiguous and guessing may fail. In that case the orientation can be specified directly using the `orientation` parameter, which can be either `"x"` or `"y"`. The value gives the axis that the geom should run along, `"x"` being the default orientation you would expect for the geom.
#'
#' # Aesthetics
#' `geom_terrorbar()` understands the following aesthetics (required aesthetics are in bold):
#' - **`x`** or **`y`**
#' - **`ymin`** or **`xmin`**
#' - **`ymax`** or **`xmax`**
#' - `alpha`
#' - `colour`
#' - `group`
#' - `fontface` - one of `c("plain", "bold", "italic")`
#' - `size` - size of the Ts in pt units
#' - `line_type` - type of connecting line when `connect = TRUE`
#' - `line_width` - width of connecting line when `connect = TRUE`
#' - `line_alpha` - alpha for connecting line when `connect = TRUE`. By default = 0.6 * `alpha`
#'
#' @seealso This is just a frivolous edit of [`ggplot2::geom_errorbar`]
#' @export
geom_terrorbar <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           na.rm = FALSE,
                           orientation = NA,
                           connect = TRUE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTerrorbar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      connect = connect,
      ...
    )
  )
}

#' @export
GeomTerrorbar <- ggplot2::ggproto(
  "GeomTerrorbar", ggplot2::Geom,
  default_aes = ggplot2::aes(
    colour = "black", fontface = "plain", size = 5,
    alpha = 1, line_type=2, line_width=.5, line_alpha = NA, connect = TRUE),
  draw_key = function (data, params, size) {
    if (is.null(data$label)) data$label <- "a"
    textGrob(
      data$label,
      0.5,
      0.5,
      rot = data$angle %||% 0,
      gp = gpar(col = alpha(data$colour %||% data$fill %||% "black", data$alpha), fontfamily = data$family %||% "", fontface = data$fontface %||% 1, fontsize = (data$size %||% 3.88) * .pt))
  },
  required_aes = c("x|y", "ymin|xmin", "ymax|xmax"),
  setup_params = function(data, params) {
    ggplot2::GeomText$setup_params(data, params)
  },
  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data <- transform(
      data,
      xmin = x, xmax = x
    )
    flip_data(data, params$flipped_aes)
  },
  extra_params = c("na.rm", "orientation", "connect"),

  draw_panel = function(data, panel_params, coord, parse = FALSE, na.rm = FALSE, check_overlap = FALSE, flipped_aes = FALSE, lineend = "butt", connect = params$connect) {
    data <- flip_data(data, flipped_aes)
    x <- vctrs::vec_interleave(data$x, data$x)
    y <- vctrs::vec_interleave(data$ymax, data$ymin)
    len <- length(x)
    t_data <- ggplot2:::data_frame0(
      x = x,
      y = y,
      colour = rep(data$colour, each = 2),
      alpha = rep(data$alpha, each = 2),
      label = rep("T", len),
      vjust = rep(.95, len),
      hjust = rep(c(.47, .48), len/2),
      angle = rep(c(0, 180), len/2),
      group = rep(seq_len(nrow(data)), each = 2),
      size = rep(data$size, 2),
      family = rep("serif", len),
      fontface = rep(data$fontface, 2),
      lineheight = rep(1, len),
      .size = nrow(data) * 2
    )

    t_data <- flip_data(t_data, flipped_aes)

    t_grob <- ggplot2::GeomText$draw_panel(t_data, panel_params, coord, parse, na.rm, check_overlap)
    if (!connect) {
      ggplot2:::ggname("geom_errorbar", grid::grobTree(t_grob))
    } else {

      x <- vctrs::vec_interleave(data$x, data$x, NA, data$x, data$x)
      y <- vctrs::vec_interleave(data$y, data$ymax - (data$ymax - data$y) * .05, NA, data$y, data$ymin + (data$y - data$ymin) * .05)
      len <- length(x)
      line_data <- ggplot2:::data_frame0(
        x = x,
        y = y,
        colour = rep(data$colour, each = 5),
        alpha = rep(ifelse(is.na(data$line_alpha), data$alpha * .4, data$line_alpha), each = 5),
        group = rep(seq_len(nrow(data)), each = 5),
        linewidth = rep(data$line_width, 5),
        linetype = rep(data$line_type, 5),
        .size = nrow(data) * 5
      )

      line_data <- flip_data(line_data, flipped_aes)

      line_grob <- ggplot2::GeomPath$draw_panel(line_data, panel_params, coord, lineend = lineend)

      ggplot2:::ggname("geom_errorbar", grid::grobTree(line_grob, t_grob))
    }
  }
)
