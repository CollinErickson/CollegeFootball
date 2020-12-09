plot_intervals <- function(df, mn, low, high, yname=NULL, imgurl=NULL, imgsize=1) {
  stopifnot(is.data.frame(df), nrow(df) > 0)
  if (is.null(yname)) {
    df <- df %>% mutate(yname_=1:n())
    yname <- "yname_"
  }
  df <- df %>% mutate(yorder=rev(1:n()))
  df$yplot <- factor(df[[yname]], levels=df[[yname]])
  yplot_ <- "yplot"
  # yyy_ <- "yyy"
  # print(df)
  # browser()
  p <- ggplot(df) +
    xlab(mn) + # geom_point(aes_string(x=mn, y=yplot_), size=0) +
    ylab(yname) +
    geom_segment(aes_string(x=low, xend=high, y=yplot_, yend=yplot_))
  if (is.null(imgurl)) {
    p <- p +
      geom_point(aes_string(x=mn, y=yplot_), size=4)
  } else {
    p <- p + ggimage::geom_image(aes_string(image=imgurl, x=mn, y=yplot_), size=.1 * imgsize)
  }
  p
}
if (F) {
  plot_intervals(sumteam %>% sample_n(30) %>% arrange((mean)), 'mean', '`2.5%`', '`97.5%`', yname="team")
  plot_intervals(sumteam %>% sample_n(25) %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")
}

