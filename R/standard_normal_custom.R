##########################################################################################
# STANDARD NORMAL CUSTOM SCALES
##########################################################################################
#' @title Plot the standard normal curve with your own score scales
#' @description Draws the standard normal curve, divided at every standard deviation and marked
#' with the percentage of cases in each interval, with the scales you describe in a data frame
#' lined up under it. A scale can be a row of labelled tick marks (like T scores), a row of
#' coloured bands (like stanines), or both.
#' @param scales A data frame with one row per tick mark or band, and the columns:
#' \describe{
#'   \item{scale}{Name of the scale, shown at the left of its row. Rows are drawn from top to
#'     bottom in the order the names first appear.}
#'   \item{z}{Position on the z axis: where the tick mark goes, or where the band starts.}
#'   \item{label}{Text shown at the tick mark or inside the band.}
#'   \item{z_end}{Optional. Where the band ends on the z axis. Leave the column out, or use
#'     \code{NA}, for a tick mark.}
#'   \item{colour}{Optional. Fill colour of the band. Bands without a colour are coloured by
#'     where they sit against the mean: gray for a band centred on the mean, blue below it and
#'     red above it, darker the further from the mean.}
#' }
#' The default, \code{\link{standard_normal_scales_data}()}, holds the scales of
#' \code{\link{standard_normal_scales}}.
#' @param size Text size of all labels, in millimetres (ggplot2 text size). The default of 3
#'   suits figures around 7 inches wide.
#' @details The labels inside the bands are white or near-black, whichever stands out more from
#'   the colour of the band. The names of the scales are in a column of their own, just left of
#'   the scales, so a long name never runs into them. The percentages are kept off the curve by
#'   distances in heights of their text, so they keep their place whatever the size of the
#'   figure. The lines, the gaps between the bands and their rounded corners grow with
#'   \code{size}, as the text does.
#' @return A ggplot object. Print it to draw the chart, or save it with
#'   \code{ggplot2::ggsave()}. The chart keeps a fixed aspect ratio, and every scale you add
#'   makes it taller.
#' @seealso \code{\link{standard_normal_scales_data}}, \code{\link{standard_normal_scales}}
#' @import ggplot2
#' @export
#' @examples
#' standard_normal_custom()
#' # your own scales: tick marks for a test score and bands for performance levels
#' my_scales<-rbind(data.frame(scale="SCORE",z=-3:3,z_end=NA,label=seq(20,80,10)),
#'                  data.frame(scale="LEVEL",z=c(-4,-1,1),z_end=c(-1,1,4),label=c("Low","Average","High")))
#' standard_normal_custom(my_scales)
#' # start from the standard scales, drop the grades and add Wechsler scaled scores (mean 10, SD 3)
#' scales<-standard_normal_scales_data()
#' scales<-scales[scales$scale!="GRADE",]
#' scales<-rbind(scales,data.frame(scale="SCALED",z=-3:3,z_end=NA,label=seq(1,19,3),colour=NA))
#' standard_normal_custom(scales)
standard_normal_custom<-function(scales=standard_normal_scales_data(),size=3) {
  if (!is.data.frame(scales)||nrow(scales)==0||!all(c("scale","z","label")%in%names(scales)))
    stop("scales must be a data frame with the columns scale, z and label (and optionally z_end and colour)",call.=FALSE)
  if (!"z_end"%in%names(scales)) scales$z_end<-NA
  if (!"colour"%in%names(scales)) scales$colour<-NA
  if (!is.numeric(scales$z)||!(is.numeric(scales$z_end)||all(is.na(scales$z_end))))
    stop("the z and z_end columns of scales must be numeric",call.=FALSE)
  scales<-data.frame(scale=as.character(scales$scale),z=scales$z,z_end=as.numeric(scales$z_end),
                     label=as.character(scales$label),colour=as.character(scales$colour))
  density<-function(x) exp(-x^2/2)/sqrt(2*pi)
  z_score<- -4:4
  # the curve over the span of the scales, where it meets the bottom of its wash
  snd<-data.frame(x=seq(-4,4,.01))
  snd$y<-density(snd$x)
  # lines, gaps and corners grow with the text, as a larger text size is for a larger figure
  k<-size/3
  # one row per scale, .04 apart, with a hairline .02 above each row, the first included, which
  # separates the scales from the curve; tick labels sit under their tick marks and bands fill 85% of
  # their row, so rows never touch
  rows<-data.frame(label=unique(scales$scale))
  rows$y<- -.01-.04*seq_len(nrow(rows))
  scales$y<-rows$y[match(scales$scale,rows$label)]
  bands<-scales[!is.na(scales$z_end),]
  ticks<-scales[is.na(scales$z_end),]
  rows$text_y<-ifelse(rows$label%in%bands$scale,rows$y,rows$y-.005)
  band_half_height<-.017
  bands$xmin<-pmin(bands$z,bands$z_end)
  bands$xmax<-pmax(bands$z,bands$z_end)
  uncoloured<-is.na(bands$colour)
  bands$colour[uncoloured]<-band_colours(bands$xmin[uncoloured],bands$xmax[uncoloured])
  # the percentage of cases between two standard deviations, kept off the curve by distances in
  # heights of their text, so that they keep their place whatever the size of the figure: in the
  # middle, inside the curve just above its base; in the tails, just above the curve, from the
  # line of the standard deviation outwards, where the curve only falls
  inner<-data.frame(x=c(-1.5,-.5,.5,1.5),label=c("13.59%","34.13%","34.13%","13.59%"))
  tails<-data.frame(x=c(-3.1,-2.1,2.1,3.1),label=c("0.13%","2.14%","2.14%","0.13%"),hjust=c(1,1,0,0))
  normal_plot<-ggplot()+
    geom_ribbon(data=snd,aes(x=.data$x,ymin=0,ymax=.data$y),fill=snp_ink$wash)+
    # the intervals between standard deviations, divided by gaps of the background colour
    annotate("segment",x=z_score,xend=z_score,y=0,yend=density(z_score),linewidth=.6*k,colour=snp_ink$surface)+
    geom_line(data=snd,aes(x=.data$x,y=.data$y),linewidth=.75*k,colour=snp_ink$curve,linejoin="round")+
    annotate("text",x=inner$x,y=0,label=inner$label,vjust=-.6,size=size,colour=snp_ink$text)+
    annotate("text",x=tails$x,y=density(tails$x),label=tails$label,hjust=tails$hjust,vjust=-.35,size=size,colour=snp_ink$text)+
    # the rules run the width of the figure, under the names of the scales too
    annotate("segment",x=-100,xend=100,y=rows$y+.02,yend=rows$y+.02,linewidth=.3*k,colour=snp_ink$rule)
  for (i in seq_len(nrow(bands))) normal_plot<-normal_plot+
    annotation_custom(band_grob(bands$colour[i],k),xmin=bands$xmin[i],xmax=bands$xmax[i],
                      ymin=bands$y[i]-band_half_height,ymax=bands$y[i]+band_half_height)
  if (nrow(bands)>0) normal_plot<-normal_plot+
    annotate("text",x=(bands$xmin+bands$xmax)/2,y=bands$y,label=bands$label,size=size,colour=label_ink(bands$colour))
  if (nrow(ticks)>0) normal_plot<-normal_plot+
    annotate("segment",x=ticks$z,xend=ticks$z,y=ticks$y+.02,yend=ticks$y+.0125,linewidth=.35*k,colour=snp_ink$tick)+
    annotate("text",x=ticks$z,y=ticks$y-.005,label=ticks$label,size=size,colour=snp_ink$text)
  # the names of the scales are the labels of the y axis, so that ggplot2 measures them and puts
  # them in a column of their own, a gap of their own size left of the scales, which they can
  # never run into however long they are
  normal_plot<-normal_plot+
    scale_y_continuous(breaks=rows$text_y,labels=rows$label)+
    theme_void()+
    theme(legend.position="none",
          axis.text.y=element_text(hjust=0,size=size*.pt,colour=snp_ink$name,margin=margin(r=size*.pt)),
          # the names of the scales indented from the left edge by the height of their text
          plot.margin=unit(c(0,0,0,size*.pt),"pt"),
          plot.background=element_rect(fill=snp_ink$surface,colour=NA))+
    # the span of the scales, which the rules, longer, do not widen
    coord_fixed(ratio=6,xlim=range(-4,4,scales$z,scales$z_end,na.rm=TRUE),clip="off")
  return(normal_plot)
}
##########################################################################################
# STANDARD NORMAL SCALES DATA
##########################################################################################
#' @title Score scales of the standard normal chart, as a data frame
#' @description Returns the z, T, cumulative percentage, percentile, sten, stanine, grade and
#' IQ scales of
#' \code{\link{standard_normal_scales}} in the format \code{\link{standard_normal_custom}}
#' takes, so you can start from them and change, remove or add scales.
#' @return A data frame with the columns scale, z, z_end, label and colour, and one row per
#'   tick mark (z, T, cumulative percentage, percentile and IQ, with \code{z_end} and
#'   \code{colour} set to \code{NA})
#'   or per band (sten, stanine and grade, coloured by where they sit against the mean).
#' @seealso \code{\link{standard_normal_custom}}
#' @export
#' @examples
#' scales<-standard_normal_scales_data()
#' head(scales)
#' unique(scales$scale)
standard_normal_scales_data<-function() {
  z_score<- -4:4
  ticks<-function(scale,z,label) data.frame(scale=scale,z=z,z_end=NA,label=as.character(label),colour=NA)
  bands<-function(scale,z,z_end,label) data.frame(scale=scale,z=z,z_end=z_end,label=as.character(label),colour=band_colours(z,z_end))
  rbind(ticks("z",z_score,z_score),
        ticks("T",z_score,z_score*10+50),
        # the percentage of cases below each standard deviation, 100 * pnorm(-3:3), to one decimal
        ticks("CUMULATIVE %",-3:3,c("0.1","2.3","15.9","50","84.1","97.7","99.9")),
        ticks("PERCENTILE",c(-2.326,-1.645,-1.282,-0.675,0,0.675,1.282,1.645,2.326),c(1,5,10,25,50,75,90,95,99)),
        bands("STEN",c(-4,-2,-1.5,-1,-.5,0,.5,1,1.5,2),c(-2,-1.5,-1,-.5,0,.5,1,1.5,2,4),1:10),
        bands("STANINE",c(-4,-1.75,-1.25,-.75,-.25,.25,.75,1.25,1.75),c(-1.75,-1.25,-.75,-.25,.25,.75,1.25,1.75,4),1:9),
        bands("GRADE",c(-4,-1.25,-.75,.75,1.25),c(-1.25,-.75,.75,1.25,4),c("E","D","C","B","A")),
        ticks("IQ",z_score,z_score*15+100))
}
