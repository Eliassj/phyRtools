#' Plot the relative response to CS and US
#'
#' @param x An object returned by [rel_response]
#' @param by The type of plot to be returned
#' @param clstrs What clusters to include (If plotting by cluster instead of depth)
#' @param CS Timing of CS
#' @param US Timing of US
#'
#' @return A plot depending on \code{by}
#' @export
#'
#'
#' @importFrom ggtext element_markdown
plotrelhz <- function(x, by = c("cluster", "depthraster", "depthline"), clstrs = NA, CS = 0, US = 300) {
    tmin <- -attr(x, "min_t") / attr(x, "tfactorms")
    tmax <- attr(x, "max_t") / attr(x, "tfactorms")

    if (by[1] == "cluster") {
        if (all(is.na(clstrs))) {
            stop("Cluster(s) not specified")
        }
        p <- ggplot(x$clustermeans[cluster %in% clstrs]) +
            geom_line(
                aes(
                    x = time / attr(x, "tfactorms"),
                    y = relhz
                ),
                stat = "identity",
                linewidth = 1
            ) +
            facet_wrap(~cluster, ncol = 1) +
            labs(
                x = "Time(ms)"
            ) +
            relhztheme +
            theme(
                strip.background = element_blank(),
                strip.text = element_text(face = "bold"),
                axis.line.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank()
            )
    }

    #  if (by[1] == "trigger") {
    #    w <- attr(x, "period") / attr(x, "tfactorms")
    #    p=ggplot(x$spiketimes[cluster == clstrs])+
    #      geom_rect(aes(
    #        xmin = time / attr(x, "tfactorms"),
    #        xmax = (time / attr(x, "tfactorms")) + w,
    #        ymin = ntrig - 0.5,
    #        ymax = ntrig + 0.5,
    #        fill = relhzcluster
    #        )
    #      )+
    #      scale_x_continuous(
    #        breaks = c(tmin, CS-US, CS, US, tmax),
    #        labels = c(tmin, CS-US, paste0(CS, " **(CS)**"), paste0(US, " **(US)**"), tmax),
    #        limits = c(tmin, tmax)
    #      )+
    #      relhztheme+
    #      labs(
    #        x = "Time(ms)"
    #      )+
    #      theme(
    #        legend.position = "top",
    #        axis.line.y = element_blank(),
    #        axis.ticks.y = element_blank(),
    #        axis.title.y = element_blank(),
    #        axis.text.x = element_markdown(color = "#000000"),
    #        axis.text.y = element_text(color = "#000000")
    #      )+
    #      guides(
    #        fill =  guide_colorbar(
    #          title = "Relative frequency",
    #          title.position = "top",
    #          title.hjust = .5,
    #          barwidth = unit(20, "lines"),
    #          barheight = .7,
    #        )
    #      )+
    #      scale_fill_gradient(
    #        low = "#FFFFFF",
    #        high = "#000000",
    #        limits = c(0, ceiling(max(x$depthmeans$relhz)*10)/10),
    #        breaks = c(seq(0, ceiling(max(x$depthmeans$relhz)*10)/10), ceiling(max(x$depthmeans$relhz)*10)/10)
    #      )
    #  }

    if (by[1] == "depthline") {
        if (!all(is.na(clstrs))) {
            warning("Cluster specification will be ignored when plotting spike rate by depth")
        }
        m <- x$depthmeans[, max(relhz)] + 0.05 + 0.55

        p <- ggplot(x$depthmeans) +
            labs(
                x = "Time(ms)"
            ) +
            geom_segment(
                aes(
                    x = CS, xend = CS,
                    y = 0, yend = m
                ),
                alpha = .3,
                color = "grey"
            ) +
            labs(
                x = "Time(ms)"
            ) +
            geom_segment(
                aes(
                    x = US, xend = US,
                    y = 0, yend = m
                ),
                alpha = .7,
                color = "grey"
            ) +
            geom_line(
                aes(
                    x = time / attr(x, "tfactorms"),
                    y = relhz
                ),
                linewidth = 1
            ) +
            facet_grid(
                rows = vars(depth),
                switch = "y"
            ) +
            scale_x_continuous(
                breaks = c(tmin, CS - US, CS, US, tmax),
                labels = c(tmin, CS - US, paste0(CS, " **(CS)**"), paste0(US, " **(US)**"), tmax),
                limits = c(tmin, tmax)
            ) +
            coord_cartesian(clip = "off", ylim = c(0, max(x$depthmeans$relhz))) +
            relhztheme +
            theme(
                legend.position = "none",
                axis.line.y = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                strip.background = element_blank(),
                strip.text.y.left = element_text(
                    angle = 0,
                    color = "#000000"
                ),
                axis.text.x = element_markdown(color = "#000000")
            )
    }
    if (by[1] == "depthraster") {
        w <- attr(x, "period") / attr(x, "tfactorms")
        p <- ggplot(x$depthmeans) +
            geom_rect(aes(
                xmin = time / attr(x, "tfactorms"),
                xmax = (time / attr(x, "tfactorms")) + w,
                ymin = nodelist - 0.5,
                ymax = nodelist + 0.5,
                fill = relhz
            )) +
            scale_y_continuous(
                labels = rev(attr(x, "lvls")),
                breaks = seq(1:length(attr(x, "lvls"))),
                trans = "reverse"
            ) +
            scale_x_continuous(
                breaks = c(tmin, CS - US, CS, US, tmax),
                labels = c(tmin, CS - US, paste0(CS, " **(CS)**"), paste0(US, " **(US)**"), tmax),
                limits = c(tmin, tmax)
            ) +
            relhztheme +
            labs(
                x = "Time(ms)"
            ) +
            theme(
                legend.position = "top",
                axis.line.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_markdown(color = "#000000"),
                axis.text.y = element_text(color = "#000000")
            ) +
            guides(
                fill = guide_colorbar(
                    title = "Relative frequency",
                    title.position = "top",
                    title.hjust = .5,
                    barwidth = unit(20, "lines"),
                    barheight = .7,
                )
            ) +
            scale_fill_gradient(
                low = "#FFFFFF",
                high = "#000000",
                limits = c(0, ceiling(max(x$depthmeans$relhz) * 10) / 10),
                breaks = c(seq(0, ceiling(max(x$depthmeans$relhz) * 10) / 10), ceiling(max(x$depthmeans$relhz) * 10) / 10)
            )
    }


    return(p)
}
