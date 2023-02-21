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
      geom_line(lwd=1, aes(color=MP))

  } else {
    p <- ggplot(df, aes(x=Year, y=Median, ymin=Lower, ymax=Upper))+
      geom_ribbon(alpha=alpha, aes(fill=Name)) +
      geom_line(lwd=1, aes(color=Name)) +
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



plot_SSB <- function(x, ...) {
  UseMethod("plot_SSB")
}

plot_SSB.Hist <- function(x,
                          title='',
                          xlab='Year',
                          ylab='Spawning Biomass',
                          quantiles=c(0.025, 0.975),
                          ...) {
  df <- get_SSB(x) %>%
    group_by(Year, Name) %>%
    summarize(Lower=quantile(Value, quantiles[1]),
              Median=median(Value),
              Upper=quantile(Value, quantiles[2]),
              .groups='drop')

  plot_ts_quants(df, xlab=xlab, ylab=ylab, title=title, ...)

}

plot_SSB.list <- function(x,
                          title='',
                          xlab='Year',
                          ylab='Spawning Biomass',
                          quantiles=c(0.025, 0.975),
                          ...) {
  if (is.null(names(x))) {
    message("List elements are not named. Using numeric values")
    names(x) <- 1:length(x)
  }

  df <- purrr::map2_df(x, names(x), get_SSB)
  if (!is.null(df[['MP']])) {
    df <- df %>%  group_by(Year, MP,Name)
  } else {
    df <- df %>%  group_by(Year,Name)
  }
  df <- df %>%
    summarize(Lower=quantile(Value, quantiles[1]),
              Median=median(Value),
              Upper=quantile(Value, quantiles[2]),
              .groups='drop')

  plot_ts_quants(df, xlab=xlab, ylab=ylab, title=title, inc_legend = TRUE, ...)
}

plot_SSB.MSE <- function(x,
                         title='',
                         xlab='Year',
                         ylab='Spawning Biomass',
                         quantiles=c(0.025, 0.975),
                         ...) {
  df <- get_SSB(x) %>%
    group_by(Year, MP, Name) %>%
    summarize(Lower=quantile(Value, quantiles[1]),
              Median=median(Value),
              Upper=quantile(Value, quantiles[2]),
              .groups='drop')

  plot_ts_quants(df, xlab=xlab, ylab=ylab, title=title, inc_legend = TRUE, ...)

}
