##########################################################################################
# DESIGN
##########################################################################################
# Three ramps of 13 steps, 100 (lightest) to 700 (darkest), one per palette. The blue is a blue
# sequential ramp; the red and the green have the same OKLCH lightness and chroma at every step,
# at the hues of red and green, so that every palette weighs the same and a step is as light in
# one as in the others.
snp_ramps<-list(
  blues=c("#cde2fb","#b7d3f6","#9ec5f4","#86b6ef","#6da7ec","#5598e7","#3987e5","#2a78d6","#256abf","#1c5cab","#184f95","#104281","#0d366b"),
  reds=c("#fad6d2","#f4c2be","#f1aea8","#ea9a93","#e4857e","#dd716a","#d75853","#c74845","#b13f3c","#9e3432","#892b2a","#762221","#621b1a"),
  greens=c("#d1e7ce","#bbdab8","#a5cfa0","#8ec289","#76b771","#5faa5a","#429e3d","#31902c","#2a7f26","#1f701c","#196116","#10520e","#0d440b"))
#' @title Steps of a ramp
#' @param palette "blues", "reds" or "greens".
#' @param steps Steps from 100 to 700, by 50.
#' @noRd
snp_step<-function(palette,steps) snp_ramps[[palette]][(steps-100)/50+1]
# The inks of the light and the dark mode: the text, the names of the scales, the curve of the
# diverging palette, the area under it, the rules between the rows, the tick marks, the
# background (also the gaps between the regions under the curve) and the gray of a band
# centred on the mean in the diverging palette.
snp_inks<-list(
  light=list(text="#0b0b0b",name="#52514e",curve="#52514e",wash="#ebeae6",rule="#e1e0d9",tick="#c3c2b7",surface="#ffffff",neutral="#c3c2b7"),
  dark=list(text="#f0efec",name="#c3c2b7",curve="#c3c2b7",wash="#2c2c2a",rule="#2c2c2a",tick="#52514e",surface="#1a1a19",neutral="#52514e"))
##########################################################################################
# STYLE
##########################################################################################
#' @title The colours of a chart
#' @description In the blues, reds and greens, colour shows how common a score is: under the
#' curve, the intervals that hold more cases are darker (in the dark mode, brighter), and so
#' are the bands near the mean. In the diverging palette, the area under the curve is a neutral
#' wash, and the bands are blue below the mean and red above it, darker (in the dark mode,
#' brighter) the further from the mean.
#' @param palette "blues", "reds", "greens" or "diverging".
#' @param mode "light" or "dark".
#' @return A list: \code{ink}, the inks of the mode; \code{regions}, the fills of the intervals
#'   under the curve, from the one at the mean (34.13\%) out to the tails (0.13\%); \code{curve},
#'   the colour of the curve.
#' @noRd
snp_style<-function(palette,mode) {
  ink<-snp_inks[[mode]]
  if (palette=="diverging") return(list(ink=ink,regions=rep(ink$wash,4),curve=ink$curve))
  light<-mode=="light"
  list(ink=ink,
       regions=snp_step(palette,if (light) c(350,250,150,100) else c(450,550,650,700)),
       curve=snp_step(palette,if (light) 650 else 200))
}
##########################################################################################
# BAND COLOURS
##########################################################################################
#' @title Colours of bands
#' @description In the blues, reds and greens, by how common the scores of the band are: the
#' closer the middle of the band to the mean, the darker the band (in the dark mode, the
#' brighter). In the diverging palette, by where the band sits against the mean: gray for a
#' band centred on the mean, blue below it and red above it, darker (in the dark mode,
#' brighter) the further from the mean. The open ends of the outer bands (such as sten 1, which
#' runs from \eqn{z = -4}) are cut at 2.5 standard deviations, so that a wide outer band is
#' coloured like its neighbour.
#' @param start,end Where the bands start and end on the z axis.
#' @param palette "blues", "reds", "greens" or "diverging".
#' @param mode "light" or "dark".
#' @return Hex colours, one per band.
#' @importFrom grDevices colorRamp rgb
#' @noRd
band_colours<-function(start,end,palette="blues",mode="light") {
  if (length(start)==0) return(character(0))
  middle<-(pmax(pmin(start,end),-2.5)+pmin(pmax(start,end),2.5))/2
  ramp<-function(colours,t) rgb(colorRamp(colours,space="Lab")(t),maxColorValue=255)
  light<-mode=="light"
  if (palette=="diverging") {
    neutral<-snp_inks[[mode]]$neutral
    steps<-if (light) c(250,350,450,550) else c(550,450,350,250)
    t<-pmin(abs(middle)/2.5,1)
    colour<-ramp(c(neutral,snp_step("blues",steps)),t)
    above<-middle>0
    if (any(above)) colour[above]<-ramp(c(neutral,snp_step("reds",steps)),t[above])
    return(colour)
  }
  # how common the scores of the band are: the height of the curve at its middle, 1 at the mean
  ramp(snp_step(palette,if (light) c(150,250,350,450) else c(650,550,450,350)),exp(-middle^2/2))
}
##########################################################################################
# LABEL INK
##########################################################################################
#' @title The ink of a label on a fill
#' @description White or near-black, whichever stands out more from the fill by APCA
#' lightness contrast (version 0.0.98G).
#' @param fill Colours of the fills.
#' @return Hex colours, one per fill.
#' @importFrom grDevices col2rgb
#' @noRd
label_ink<-function(fill) {
  luminance<-function(colour) {
    y<-colSums((col2rgb(colour)/255)^2.4*c(0.2126729,0.7151522,0.072175))
    ifelse(y>=0.022,y,y+(0.022-y)^1.414)
  }
  apca<-function(text,background) {
    t<-luminance(text)
    b<-luminance(background)
    c<-ifelse(b>t,1.14*(b^0.56-t^0.57),1.14*(b^0.65-t^0.62))
    ifelse(abs(c)<0.1,0,(c-sign(c)*0.027)*100)
  }
  dark<-snp_inks$light$text
  ifelse(abs(apca("#ffffff",fill))>abs(apca(dark,fill)),"#ffffff",dark)
}
##########################################################################################
# BAND GROB
##########################################################################################
#' @title A band of a scale, as a rounded block
#' @description Fills the rectangle it is placed in with \code{ggplot2::annotation_custom()},
#' less a gap of 2 pixels (1.5 points) that separates it from the next band, with corners
#' rounded by 3 points, both at the default text size of 3 and growing with it. The gap and
#' the corners are in points, so they keep their size whatever the size of the figure.
#' @param fill Colour of the band.
#' @param k How much larger than at the default text size of 3 to make the gap and the corners.
#' @return A grid grob.
#' @noRd
band_grob<-function(fill,k=1) {
  grid::roundrectGrob(width=grid::unit(1,"npc")-grid::unit(1.5*k,"pt"),
                      r=grid::unit(3*k,"pt"),
                      gp=grid::gpar(fill=fill,col=NA))
}
