rastertheme <- theme_classic()+
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_text(hjust = 0.3, vjust = -7)
  )

ffttheme <- theme_classic()+
  theme(
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_line(linetype = 2)
  )

corrgramtheme <- theme_classic()+
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

hztheme <- theme_classic()+
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )

isitheme <- theme_classic()+
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank()
  )

standardcol <- "#E65D2FFF"

standarddisc <- viridis::scale_color_viridis(discrete = TRUE, option = "B", begin = .25, end = .75)

#  [1] "#000004FF" "#010107FF" "#02020CFF" "#040311FF" "#050418FF" "#08051DFF" "#0A0723FF" "#0D0829FF"
#[9] "#110A2FFF" "#140B35FF" "#180C3BFF" "#1B0C42FF" "#1F0C48FF" "#230C4DFF" "#280B53FF" "#2C0B58FF"
#[17] "#310A5CFF" "#360961FF" "#3A0963FF" "#3E0966FF" "#430A68FF" "#470B6AFF" "#4B0C6BFF" "#500D6CFF"
#[25] "#540F6DFF" "#58106EFF" "#5C126EFF" "#60136EFF" "#64156EFF" "#69166EFF" "#6C186EFF" "#71196EFF"
#[33] "#741A6EFF" "#781C6DFF" "#7D1E6DFF" "#801F6CFF" "#85216BFF" "#88226AFF" "#8D2369FF" "#912568FF"
#[41] "#952667FF" "#992766FF" "#9D2964FF" "#A22B62FF" "#A52C60FF" "#A92E5EFF" "#AD305CFF" "#B1325AFF"
#[49] "#B53458FF" "#B93556FF" "#BD3853FF" "#C03A51FF" "#C43C4EFF" "#C73F4BFF" "#CB4149FF" "#CF4446FF"
#[57] "#D24644FF" "#D54A41FF" "#D84C3EFF" "#DB503BFF" "#DF5237FF" "#E15635FF" "#E45A32FF" "#E65D2FFF"
#[65] "#E9612BFF" "#EB6529FF" "#ED6925FF" "#EF6D22FF" "#F1711FFF" "#F3751BFF" "#F47918FF" "#F67E14FF"
#[73] "#F78311FF" "#F8870EFF" "#F98C0AFF" "#FA9008FF" "#FB9606FF" "#FB9A06FF" "#FC9F07FF" "#FCA409FF"
#[81] "#FCA80DFF" "#FCAD12FF" "#FCB216FF" "#FBB81CFF" "#FBBD22FF" "#FAC228FF" "#F9C72EFF" "#F9CB35FF"
#[89] "#F7D03CFF" "#F6D544FF" "#F5DB4BFF" "#F4E054FF" "#F3E55DFF" "#F2E967FF" "#F1ED71FF" "#F2F27CFF"
#[97] "#F3F587FF" "#F5F991FF" "#F9FB9BFF" "#FCFFA4FF"#
