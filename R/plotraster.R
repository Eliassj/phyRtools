plotraster <- function(x, clstr)
{
  ggplot(x[cluster == clstr])+
    geom_point(
      aes(
        x = time,
        y = ntrig
      )
    )
}
