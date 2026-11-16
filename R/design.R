##########################################################################################
# DESIGN
##########################################################################################
# The colours of the chart. Text is near-black ink; the rules between the rows and the tick
# marks are two recessive grays; the curve is a dark gray line over a light gray wash.
snp_ink<-list(text="#0b0b0b",       # values and the labels of the curve
              name="#52514e",       # names of the scales
              curve="#52514e",      # the normal curve
              wash="#ebeae6",       # the area under the curve
              rule="#e1e0d9",       # hairlines between the rows
              tick="#c3c2b7",       # tick marks
              surface="#ffffff")    # the background, and the gaps between the regions and bands
# The bands of a scale are coloured by where they sit against the mean: a neutral gray for a
# band centred on the mean, blue below it and red above it, darker the further from the mean.
# The blue steps are 250, 350, 450 and 550 of a blue sequential ramp; the red steps have the
# same OKLCH lightness and chroma at the hue of red, so the two sides weigh the same; the gray
# has the lightness of the lightest blue step but one.
snp_neutral<-"#c3c2b7"
snp_below<-c("#86b6ef","#5598e7","#2a78d6","#1c5cab")
snp_above<-c("#ea9a93","#dd716a","#c74845","#9e3432")
##########################################################################################
# BAND COLOURS
##########################################################################################
#' @title Colours of bands by where they sit against the mean
#' @description Gray for a band centred on the mean, blue below it and red above it, darker
#' the further the middle of the band is from the mean, up to 2.5 standard deviations. The
#' open ends of the outer bands (such as sten 1, which runs from \eqn{z = -4}) are cut at 2.5
#' standard deviations, so that a wide outer band is not much darker than its neighbour.
#' @param start,end Where the bands start and end on the z axis.
#' @return Hex colours, one per band.
#' @importFrom grDevices colorRamp rgb
#' @noRd
band_colours<-function(start,end) {
  if (length(start)==0) return(character(0))
  middle<-(pmax(pmin(start,end),-2.5)+pmin(pmax(start,end),2.5))/2
  t<-pmin(abs(middle)/2.5,1)
  colour<-colorRamp(c(snp_neutral,snp_below),space="Lab")(t)
  above<-middle>0
  if (any(above)) colour[above,]<-colorRamp(c(snp_neutral,snp_above),space="Lab")(t[above])
  rgb(colour,maxColorValue=255)
}
##########################################################################################
# LABEL INK
##########################################################################################
#' @title The ink of a label inside a coloured band
#' @description White or near-black, whichever stands out more from the fill by APCA
#' lightness contrast (version 0.0.98G).
#' @param fill Colours of the bands.
#' @return Hex colours, one per band.
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
  ifelse(abs(apca("#ffffff",fill))>abs(apca(snp_ink$text,fill)),"#ffffff",snp_ink$text)
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
