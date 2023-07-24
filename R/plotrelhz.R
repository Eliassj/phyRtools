#' Title
#'
#' @param x An object returned by \link{\code{rel_response}}
#' @param by
#' @param clstrs
#' @param CS
#' @param US
#' @param abs
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom ggtext element_markdown
plotrelhz <- function(x, by = c("cluster", "trigger", "depthraster", "depthline"), clstrs = NA, CS = 0, US = 300, abs = FALSE, morelines = FALSE)
{
  tmin <- -attr(x, "min_t") / attr(x, "tfactorms")
  tmax <- attr(x, "max_t") / attr(x, "tfactorms")

  if (by[1] == "cluster") {
    if (all(is.na(clstrs))){stop("Clusters not specified")}
    p=ggplot(x$clustermeans[cluster %in% clstrs])+
        geom_line(
          aes(
            x = time / attr(x, "tfactorms"),
            y = relhz
          ),
          stat = "identity"
        )+
      facet_wrap(~cluster, ncol = 1)+
      relhztheme
  }
  if (by[1] == "depthline") {
    if (!all(is.na(clstrs))){warning("Cluster specification will be ignored when plotting spike rate by depth")}
    p=ggplot(x$depthmeans)+
      geom_line(
        aes(
           x = time / attr(x, "tfactorms"),
           if (abs == FALSE){y = relhz} else {y = hz}
        ),
        linewidth = 1
      )+
      facet_grid(rows = vars(depth),
                 switch = "y")+
      scale_x_continuous(
        breaks = c(tmin, CS-US, CS, US, tmax),
        labels = c(tmin, CS-US, paste0(CS, " **(CS)**"), paste0(US, " **(US)**"), tmax),
        limits = c(tmin, tmax)
      )+
      labs(
        x = "Time(ms)"
      )+
      relhztheme+
      theme(
        legend.position = "none",
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        strip.text.y.left = element_text(
          angle = 0,
          color = "#000000"),
        axis.text.x = element_markdown(color = "#000000")
      )
  }
  if (by[1] == "depthraster") {
    w <- attr(x, "period") / attr(x, "tfactorms")
    p=ggplot(x$depthmeans
             )+
      geom_rect(aes(
        xmin = time / attr(x, "tfactorms"),
        xmax = (time / attr(x, "tfactorms")) + w,
        ymin = nodelist - 0.5,
        ymax = nodelist + 0.5,
        fill = relhz
      ))+
      scale_y_continuous(
        labels = attr(x, "lvls"),
        breaks = seq(1:length(attr(x, "lvls")))
      )+
      scale_x_continuous(
        breaks = c(tmin, CS-US, CS, US, tmax),
        labels = c(tmin, CS-US, paste0(CS, " **(CS)**"), paste0(US, " **(US)**"), tmax),
        limits = c(tmin, tmax)
      )+
      relhztheme+
      labs(
        x = "Time(ms)"
      )+
      theme(
        legend.position = "top",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_markdown(color = "#000000"),
        axis.text.y = element_text(color = "#000000")
      )+
      guides(
       fill =  guide_colorbar(
         title = "Relative frequency",
         title.position = "top",
         title.hjust = .5,
         barwidth = unit(20, "lines"),
         barheight = .7,
       )
      )+
    scale_fill_gradient(
      low = "#FFFFFF",
      high = "#000000",
      limits = c(0, ceiling(max(x$depthmeans$relhz)*10)/10),
      breaks = c(seq(0, ceiling(max(x$depthmeans$relhz)*10)/10), ceiling(max(x$depthmeans$relhz)*10)/10)
    )
  }


  return(p+
         geom_vline(xintercept = c(CS, US),
                    alpha = .25)
         )
}