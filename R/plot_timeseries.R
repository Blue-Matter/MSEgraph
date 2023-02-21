make_df <- function(x, quantiles=c(0.025, 0.975), func=get_SSB) {
  df <- func(x)
  if (!is.null(df[['MP']])) {
    df <- df %>%  group_by(Year, MP,Name)
  } else {
    df <- df %>%  group_by(Year,Name)
  }
  if (length(quantiles)==1) {
    if(quantiles==0.5) {
      quantiles <- c(0.5,0.5)
    } else {
      stop('`quantiles` must be length 2 or 0.5')
    }
  }
  df %>%
    summarize(Lower=quantile(Value, quantiles[1]),
              Median=median(Value),
              Upper=quantile(Value, quantiles[2]),
              .groups='drop')
}

#' Title
#'
#' @param df
#' @param xlab
#' @param ylab
#' @param title
#' @param alpha
#' @param lwd
#' @param inc_legend
#' @param use_theme
#' @param colpalette
#'
#' @return
#' @export
#'
#' @examples
plot_ts_quants <- function(df,
                           xlab='Year',
                           ylab='',
                           title='',
                           alpha=0.1,
                           lwd=1,
                           inc_legend=FALSE,
                           use_theme=NULL,
                           colpalette='Dark2') {

  n_scens <- length(unique(df$Name))
  do_facet <- ifelse(n_scens>1, TRUE, FALSE)

  if (!is.null(df[['MP']])) {
    p <- ggplot(df, aes(x=Year, y=Median, ymin=Lower, ymax=Upper,
                        linetype=MP)) +
      geom_ribbon(alpha=alpha, aes(fill=MP)) +
      geom_line(linewidth=lwd, aes(color=MP))

  } else {
    p <- ggplot(df, aes(x=Year, y=Median, ymin=Lower, ymax=Upper))+
      geom_ribbon(alpha=alpha, aes(fill=Name)) +
      geom_line(linewidth=lwd, aes(color=Name)) +
      guides(fill='none', color='none')
  }
  p <- p  +
    labs(x = xlab, y = ylab, title= title) +
    scale_fill_brewer(palette=colpalette) +
    scale_color_brewer(palette=colpalette) +
    expand_limits(y=0)
  if (do_facet)
    p <- p + facet_wrap(vars(Name))

  if (is.null(use_theme)) {
    p <- p + theme_default()
  } else {
    p <- p + use_theme()
  }
  if (!inc_legend)
    p <- p + guides(linetype='none')
  p
}



#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_SSB <- function(x,
                     title='',
                     xlab='Year',
                     ylab='Spawning Biomass',
                     quantiles=c(0.025, 0.975),
                     ...) {
  df <- make_df(x, quantiles=quantiles, get_SSB)
  plot_ts_quants(df, xlab=xlab, ylab=ylab, title=title, inc_legend = TRUE, ...)
}

#' @export
#' @rdname plot_SSB
plot_SSB.Hist <- function(x, ...) {
  UseMethod("plot_SSB")
}

#' @export
#' @rdname plot_SSB
plot_SSB.list <- function(x, ...) {
  UseMethod("plot_SSB")
}

#' @export
#' @rdname plot_SSB
plot_SSB.MSE <- function(x, ...) {
  UseMethod("plot_SSB")
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_Biomass <- function(x,
                         title='',
                         xlab='Year',
                         ylab='Biomass',
                         quantiles=c(0.025, 0.975),
                         ...) {
  df <- make_df(x, quantiles=quantiles, get_Biomass)
  plot_ts_quants(df, xlab=xlab, ylab=ylab, title=title, inc_legend = TRUE, ...)

}

#' @export
#' @rdname plot_Biomass
plot_Biomass.Hist <- function(x, ...) {
  UseMethod("plot_Biomass")
}

#' @export
#' @rdname plot_Biomass
plot_Biomass.list <- function(x, ...) {
  UseMethod("plot_Biomass")
}

#' @export
#' @rdname plot_Biomass
plot_Biomass.MSE <- function(x, ...) {
  UseMethod("plot_Biomass")

}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_Landings <- function(x,
                         title='',
                         xlab='Year',
                         ylab='Landings',
                         quantiles=c(0.025, 0.975),
                         ...) {
  df <- make_df(x, quantiles=quantiles, get_Landings)
  plot_ts_quants(df, xlab=xlab, ylab=ylab, title=title, inc_legend = TRUE, ...)

}

#' @export
#' @rdname plot_Landings
plot_Landings.Hist <- function(x, ...) {
  UseMethod("plot_Landings")
}

#' @export
#' @rdname plot_Landings
plot_Landings.list <- function(x, ...) {
  UseMethod("plot_Landings")
}

#' @export
#' @rdname plot_Landings
plot_Landings.MSE <- function(x, ...) {
  UseMethod("plot_Landings")

}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_Removals <- function(x,
                          title='',
                          xlab='Year',
                          ylab='Removals',
                          quantiles=c(0.025, 0.975),
                          ...) {
  df <- make_df(x, quantiles=quantiles, get_Removals)
  plot_ts_quants(df, xlab=xlab, ylab=ylab, title=title, inc_legend = TRUE, ...)

}

#' @export
#' @rdname plot_Removals
plot_Removals.Hist <- function(x, ...) {
  UseMethod("plot_Removals")
}

#' @export
#' @rdname plot_Removals
plot_Removals.list <- function(x, ...) {
  UseMethod("plot_Removals")
}

#' @export
#' @rdname plot_Removals
plot_Removals.MSE <- function(x, ...) {
  UseMethod("plot_Removals")

}
