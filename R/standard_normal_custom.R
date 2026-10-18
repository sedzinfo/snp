##########################################################################################
# STANDARD NORMAL CUSTOM SCALES
##########################################################################################
#' @title Plot the standard normal curve with your own score scales
#' @description Draws the standard normal curve, marked at every standard deviation with the
#' percentage of cases in each interval, with the scales you describe in a data frame lined up
#' under it. A scale can be a row of labelled tick marks (like T scores), a row of coloured
#' bands (like stanines), or both.
#' @param scales A data frame with one row per tick mark or band, and the columns:
#' \describe{
#'   \item{scale}{Name of the scale, shown at the left of its row. Rows are drawn from top to
#'     bottom in the order the names first appear.}
#'   \item{z}{Position on the z axis: where the tick mark goes, or where the band starts.}
#'   \item{label}{Text shown at the tick mark or inside the band.}
#'   \item{z_end}{Optional. Where the band ends on the z axis. Leave the column out, or use
#'     \code{NA}, for a tick mark.}
#'   \item{colour}{Optional. Fill colour of the band. Bands without a colour are coloured from
#'     light to dark along their row.}
#' }
#' The default, \code{\link{standard_normal_scales_data}()}, holds the scales of
#' \code{\link{standard_normal_scales}}.
#' @param size Text size of all labels, in millimetres (ggplot2 text size). The default of 3
#'   suits figures around 7 inches wide.
#' @return A ggplot object. Print it to draw the chart, or save it with
#'   \code{ggplot2::ggsave()}. The chart keeps a fixed aspect ratio, and every scale you add
#'   makes it taller.
#' @seealso \code{\link{standard_normal_scales_data}}, \code{\link{standard_normal_scales}}
#' @import ggplot2
#' @importFrom grDevices hcl.colors
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
  z_score<- -4:4
  snd<-data.frame(x=seq(-4.5,4.5,.01))
  snd$y<-exp(-snd$x^2/2)/sqrt(2*pi)
  linecolor<-"gray50"
  # one row per scale, .04 apart, with a separator .02 above each row line;
  # tick labels sit under their tick marks and bands fill 85% of their row, so rows never touch
  rows<-data.frame(label=unique(scales$scale))
  rows$y<- -.01-.04*seq_len(nrow(rows))
  scales$y<-rows$y[match(scales$scale,rows$label)]
  bands<-scales[!is.na(scales$z_end),]
  ticks<-scales[is.na(scales$z_end),]
  rows$text_y<-ifelse(rows$label%in%bands$scale,rows$y,rows$y-.005)
  band_half_height<-.017
  bands$xmin<-pmin(bands$z,bands$z_end)
  bands$xmax<-pmax(bands$z,bands$z_end)
  for (s in unique(bands$scale)) {
    i<-which(bands$scale==s&is.na(bands$colour))
    if (length(i)>0) bands$colour[i]<-hcl.colors(length(i),palette="Heat 2",alpha=1,rev=TRUE,fixup=TRUE)[rank(bands$xmin[i],ties.method="first")]
  }
  normal_plot<-ggplot()+
    geom_line(data=snd,aes(x=.data$x,y=.data$y),linewidth=2,colour=linecolor)+
    annotate("segment",x=z_score,xend=z_score,y=-.01,yend=exp(-z_score^2/2)/sqrt(2*pi),linewidth=1,colour=linecolor)+
    geom_hline(yintercept=-.01,linewidth=1,colour=linecolor)+
    annotate("text",x=c(-3:4)-.5,y=.01,label=c("0.13%","2.14%","13.59%","34.13%","34.13%","13.59%","2.14%","0.13%"),size=size)
  if (nrow(bands)>0) normal_plot<-normal_plot+
    annotate("rect",xmin=bands$xmin,xmax=bands$xmax,ymin=bands$y-band_half_height,ymax=bands$y+band_half_height,fill=bands$colour)+
    annotate("text",x=(bands$xmin+bands$xmax)/2,y=bands$y,label=bands$label,size=size)
  if (nrow(ticks)>0) normal_plot<-normal_plot+
    annotate("segment",x=ticks$z,xend=ticks$z,y=ticks$y+.02,yend=ticks$y+.01,alpha=.5)+
    annotate("text",x=ticks$z,y=ticks$y-.005,label=ticks$label,size=size)
  normal_plot<-normal_plot+
    annotate("text",x=min(-4,scales$z,scales$z_end,na.rm=TRUE)-2,y=rows$text_y,label=rows$label,hjust=0,size=size)+
    geom_hline(yintercept=rows$y+.02,alpha=.5,linewidth=.1,colour=linecolor)+
    theme_bw(base_size=size)+
    labs(x="",y="")+
    theme(legend.position="none",
          panel.grid.minor=element_blank(),
          panel.grid=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          plot.margin=unit(c(0,0,0,0),"pt"),
          plot.background=element_blank(),
          panel.border=element_blank())+
    coord_fixed(ratio=6)
  return(normal_plot)
}
##########################################################################################
# STANDARD NORMAL SCALES DATA
##########################################################################################
#' @title Score scales of the standard normal chart, as a data frame
#' @description Returns the z, T, percentile, sten, stanine, grade and IQ scales of
#' \code{\link{standard_normal_scales}} in the format \code{\link{standard_normal_custom}}
#' takes, so you can start from them and change, remove or add scales.
#' @return A data frame with the columns scale, z, z_end, label and colour, and one row per
#'   tick mark (z, T, percentile and IQ, with \code{z_end} and \code{colour} set to \code{NA})
#'   or per band (sten, stanine and grade).
#' @seealso \code{\link{standard_normal_custom}}
#' @importFrom grDevices hcl.colors
#' @export
#' @examples
#' scales<-standard_normal_scales_data()
#' head(scales)
#' unique(scales$scale)
standard_normal_scales_data<-function() {
  z_score<- -4:4
  colours<-hcl.colors(18,palette="Heat 2",alpha=1,rev=TRUE,fixup=TRUE)
  ticks<-function(scale,z,label) data.frame(scale=scale,z=z,z_end=NA,label=as.character(label),colour=NA)
  bands<-function(scale,z,z_end,label,colour) data.frame(scale=scale,z=z,z_end=z_end,label=as.character(label),colour=colour)
  rbind(ticks("z",z_score,z_score),
        ticks("T",z_score,z_score*10+50),
        ticks("PERCENTILE",c(-2.326,-1.645,-1.282,-0.675,0,0.675,1.282,1.645,2.326),c(1,5,10,25,50,75,90,95,99)),
        bands("STEN",c(-4,-2,-1.5,-1,-.5,0,.5,1,1.5,2),c(-2,-1.5,-1,-.5,0,.5,1,1.5,2,4),1:10,
              colours[c(1,2,4,6,8,11,13,15,17,18)]),
        bands("STANINE",c(-4,-1.75,-1.25,-.75,-.25,.25,.75,1.25,1.75),c(-1.75,-1.25,-.75,-.25,.25,.75,1.25,1.75,4),1:9,
              colours[c(1,3,5,7,9,12,14,16,18)]),
        bands("GRADE",c(-4,-1.25,-.75,.75,1.25),c(-1.25,-.75,.75,1.25,4),c("E","D","C","B","A"),
              colours[c(1,5,9,14,18)]),
        ticks("IQ",z_score,z_score*15+100))
}
