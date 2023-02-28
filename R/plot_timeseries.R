summary_df <- function(x, quantiles=c(0.025, 0.975), func=get_SSB, scale=NULL) {

  if (is.null(formals(func)$scale)) {
    df <- func(x)
  } else {
    df <- func(x, scale=scale)
  }

  if (!is.null(df[['MP']])) {
    df <- df %>%  group_by(Year, MP,Variable, Period, Model)
  } else {
    if (!is.null(df[['Fleet']])) {
      df <- df %>%  group_by(Year,Variable, Period, Model, Fleet)
    } else {
      df <- df %>%  group_by(Year,Variable, Period, Model)
    }

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
                           ylab=NULL,
                           title='',
                           alpha=0.1,
                           lwd=1,
                           use_theme=NULL,
                           colpalette='Dark2',
                           print=TRUE,
                           inc.Hist=FALSE,
                           facet=TRUE,
                           inc.Legend=!facet) {

  n_scens <- length(unique(df$Model))
  n_variables <- length(unique(df$Variable))

  if (!inc.Hist & 'Projection' %in% df$Period) {
    df <- df %>% dplyr::filter(Period!='Historical')
  }

  p <- ggplot(df, aes(x=Year, y=Median, ymin=Lower, ymax=Upper)) +
    geom_ribbon(alpha=alpha) +
    geom_line(linewidth=lwd) +
    facet_wrap(~Variable, scales='free_y')


  if (is.null(ylab))
    ylab <- unique(df$Variable)

  if (!is.null(df[['MP']])) {
    p <- ggplot(df, aes(x=Year, y=Median, ymin=Lower, ymax=Upper)) +
      geom_ribbon(alpha=alpha, aes(fill=MP)) +
      geom_line(linewidth=lwd, aes(color=MP))

    if (facet) {
      p <- p + facet_grid(MP~Model)
      if ('Historical' %in% df$Period) {
        lastHistYr <- max(df$Year[df$Period=='Historical'])
        p <- p + geom_vline(xintercept = lastHistYr, linetype=2, color='darkgray')
      }

    } else {
      if (!is.null(df[['Fleet']])) {
        p <- p + facet_wrap(Fleet~Model)
      } else {
        p <- p + facet_wrap(~Model)
      }

    }

  } else {
    p <- ggplot(df, aes(x=Year, y=Median, ymin=Lower, ymax=Upper))+
      geom_ribbon(alpha=alpha, aes(fill=Model)) +
      geom_line(linewidth=lwd, aes(color=Model))

    if (facet) {
      if (!is.null(df[['Fleet']])) {
        p <- p + facet_grid(Model~Fleet, scales='free_y')
      } else {
        p <- p + facet_wrap(~Model)
      }
    }


  }
  p <- p  +
    labs(x = xlab, y = ylab, title= title) +
    scale_fill_brewer(palette=colpalette) +
    scale_color_brewer(palette=colpalette) +
    expand_limits(y=0)

  if (is.null(use_theme)) {
    p <- p + theme_default()
  } else {
    p <- p + use_theme()
  }
  if (!inc.Legend)
    p <- p + guides(color='none', fill='none')
  if (print)
    print(p)
  invisible(list(plot=p, df=df))
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
                     scale=NULL,
                     ...) {
  df <- summary_df(x, quantiles=quantiles, get_SSB, scale=scale)
  plot_ts_quants(df, xlab=xlab, ylab=ylab, title=title, ...)
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
                         scale=NULL,
                         ...) {
  df <- summary_df(x, quantiles=quantiles, get_Biomass, scale=scale)
  plot_ts_quants(df, xlab=xlab, ylab=ylab, title=title, ...)

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
                         scale=NULL,
                         ...) {
  df <- summary_df(x, quantiles=quantiles, get_Landings, scale=scale)
  plot_ts_quants(df, xlab=xlab, ylab=ylab, title=title, ...)

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
                          scale=NULL,
                          ...) {
  df <- summary_df(x, quantiles=quantiles, get_Removals, scale=scale)
  plot_ts_quants(df, xlab=xlab, ylab=ylab, title=title, ...)

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


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_Recruits <- function(x,
                          title='',
                          xlab='Year',
                          ylab='Recruits',
                          quantiles=c(0.025, 0.975),
                          scale=NULL,
                          ...) {
  df <- summary_df(x, quantiles=quantiles, get_Recruits, scale=scale)
  plot_ts_quants(df, xlab=xlab, ylab=ylab, title=title, ...)

}

#' @export
#' @rdname plot_Recruits
plot_Recruits.Hist <- function(x, ...) {
  UseMethod("plot_Recruits")
}

#' @export
#' @rdname plot_Recruits
plot_Recruits.list <- function(x, ...) {
  UseMethod("plot_Recruits")
}

#' @export
#' @rdname plot_Recruits
plot_Recruits.MSE <- function(x, ...) {
  UseMethod("plot_Recruits")

}
