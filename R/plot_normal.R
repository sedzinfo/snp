##########################################################################################
# PLOT NORMAL
##########################################################################################
#' @title Plot the standard normal curve with score scales
#' @description Draws the standard normal curve, divided at every standard deviation and marked
#' with the percentage of cases in each interval, with score scales lined up under it, so that
#' a score on one scale can be read off on the others: by default the z, T, cumulative
#' percentage, percentile, sten, stanine, grade and IQ scales, or scales you describe in a data
#' frame. A scale can be a row of labelled tick marks (like T scores), a row of coloured bands
#' (like stanines), or both.
#' @param scales A data frame with one row per tick mark or band, and the columns:
#' \describe{
#'   \item{scale}{Name of the scale, shown at the left of its row. Rows are drawn from top to
#'     bottom in the order the names first appear.}
#'   \item{z}{Position on the z axis: where the tick mark goes, or where the band starts.}
#'   \item{label}{Text shown at the tick mark or inside the band.}
#'   \item{z_end}{Optional. Where the band ends on the z axis. Leave the column out, or use
#'     \code{NA}, for a tick mark.}
#'   \item{colour}{Optional. Fill colour of the band. Bands without a colour take the colours
#'     of \code{palette}.}
#' }
#' The default, \code{\link{standard_normal_scales_data}()}, holds the standard scales; its help
#' page says how each one is defined.
#' @param size Text size of all labels, in millimetres (ggplot2 text size). The default of 3
#'   suits figures around 7 inches wide; about 5 suits figures around 12 inches wide.
#' @param palette The colours of the chart:
#' \describe{
#'   \item{"blues", "reds" or "greens"}{One colour, which shows how common a score is: under
#'     the curve, the intervals that hold more cases are darker, so 34.13\% is the darkest and
#'     0.13\% the lightest, and the bands are darker the nearer the mean they are.}
#'   \item{"diverging"}{A neutral gray under the curve, and bands blue below the mean and red
#'     above it, darker the further from the mean, with gray for a band centred on the mean.}
#' }
#' @param mode \code{"light"} for dark text on a white background, or \code{"dark"} for light
#'   text on a near-black background. In the dark mode the colours that are darker in the light
#'   mode are brighter instead, so that they still stand out from the background.
#' @details The labels on the coloured areas are white or near-black, whichever stands out more
#'   from the colour behind them. The names of the scales are in a column of their own, just
#'   left of the scales, so a long name never runs into them. The percentages are on one line,
#'   each centred over its interval, raised from the base of the curve by heights of their text,
#'   so that they keep their place whatever the size of the figure. The lines, the gaps between
#'   the bands and their rounded corners grow with \code{size}, as the text does.
#' @return A ggplot object. Print it to draw the chart, or save it with
#'   \code{ggplot2::ggsave()}. The chart keeps a fixed aspect ratio, and every scale you add
#'   makes it taller. With the default scales it is about 2.2 times as wide as it is tall, so a
#'   figure with those proportions, such as 7 by 3.2 inches, has no empty margins.
#' @seealso \code{\link{standard_normal_scales_data}}
#' @import ggplot2
#' @importFrom stats dnorm
#' @export
#' @examples
#' plot_normal()
#' plot_normal(palette="reds")
#' plot_normal(palette="greens",mode="dark")
#' plot_normal(size=5)
#' # save it with the chart's proportions
#' ggsave(file.path(tempdir(),"plot_normal.png"),plot_normal(),width=7,height=3.2)
#' # your own scales: tick marks for a test score and bands for performance levels
#' my_scales<-rbind(data.frame(scale="SCORE",z=-3:3,z_end=NA,label=seq(20,80,10)),
#'                  data.frame(scale="LEVEL",z=c(-4,-1,1),z_end=c(-1,1,4),
#'                             label=c("Low","Average","High")))
#' plot_normal(my_scales,palette="diverging")
#' # start from the standard scales, drop the grades and add Wechsler scaled scores (mean 10, SD 3)
#' scales<-standard_normal_scales_data()
#' scales<-scales[scales$scale!="GRADE",]
#' scales<-rbind(scales,data.frame(scale="SCALED",z=-3:3,z_end=NA,label=seq(1,19,3),colour=NA))
#' plot_normal(scales)
plot_normal<-function(scales=standard_normal_scales_data(),size=3,
                          palette=c("blues","reds","greens","diverging"),mode=c("light","dark")) {
  palette<-match.arg(palette)
  mode<-match.arg(mode)
  if (!is.data.frame(scales)||nrow(scales)==0||!all(c("scale","z","label")%in%names(scales)))
    stop("scales must be a data frame with the columns scale, z and label (and optionally z_end and colour)",call.=FALSE)
  if (!"z_end"%in%names(scales)) scales$z_end<-NA
  if (!"colour"%in%names(scales)) scales$colour<-NA
  if (!is.numeric(scales$z)||!(is.numeric(scales$z_end)||all(is.na(scales$z_end))))
    stop("the z and z_end columns of scales must be numeric",call.=FALSE)
  scales<-data.frame(scale=as.character(scales$scale),z=scales$z,z_end=as.numeric(scales$z_end),
                     label=as.character(scales$label),colour=as.character(scales$colour))
  style<-snp_style(palette,mode)
  ink<-style$ink
  z_score<- -4:4
  # the curve over the span of the scales, where it meets the bottom of the area under it
  snd<-data.frame(x=seq(-4,4,.01))
  snd$y<-dnorm(snd$x)
  # the area under the curve, one region per interval between standard deviations, filled by
  # how many cases it holds: the regions of style$regions run from the mean outwards
  regions<-do.call(rbind,lapply(-4:3,function(from) {
    x<-seq(from,from+1,.01)
    data.frame(x=x,y=dnorm(x),region=from,fill=style$regions[min(abs(from),abs(from+1))+1])
  }))
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
  bands$colour[uncoloured]<-band_colours(bands$xmin[uncoloured],bands$xmax[uncoloured],palette,mode)
  # the percentage of cases between two standard deviations, all on one line, each centred over
  # its interval: inside the curve in the middle, in the ink that stands out from their region,
  # and just above the curve in the tails, where it is lower than the text. The line is raised
  # from the base by heights of the text, so that it keeps its place whatever the size of the figure
  percentages<-data.frame(x=-3.5:3.5,label=c("0.13%","2.14%","13.59%","34.13%","34.13%","13.59%","2.14%","0.13%"))
  percentages$ink<-ink$text
  percentages$ink[3:6]<-label_ink(style$regions[c(2,1,1,2)])
  normal_plot<-ggplot()+
    geom_ribbon(data=regions,aes(x=.data$x,ymin=0,ymax=.data$y,group=.data$region,fill=I(.data$fill)))+
    # the regions divided by gaps of the background colour
    annotate("segment",x=z_score,xend=z_score,y=0,yend=dnorm(z_score),linewidth=.6*k,colour=ink$surface)+
    geom_line(data=snd,aes(x=.data$x,y=.data$y),linewidth=.75*k,colour=style$curve,linejoin="round")+
    annotate("text",x=percentages$x,y=0,label=percentages$label,vjust=-1.9,size=size,colour=percentages$ink)+
    # the rules run the width of the figure, under the names of the scales too
    annotate("segment",x=-100,xend=100,y=rows$y+.02,yend=rows$y+.02,linewidth=.3*k,colour=ink$rule)
  for (i in seq_len(nrow(bands))) normal_plot<-normal_plot+
    annotation_custom(band_grob(bands$colour[i],k),xmin=bands$xmin[i],xmax=bands$xmax[i],
                      ymin=bands$y[i]-band_half_height,ymax=bands$y[i]+band_half_height)
  if (nrow(bands)>0) normal_plot<-normal_plot+
    annotate("text",x=(bands$xmin+bands$xmax)/2,y=bands$y,label=bands$label,size=size,colour=label_ink(bands$colour))
  if (nrow(ticks)>0) normal_plot<-normal_plot+
    annotate("segment",x=ticks$z,xend=ticks$z,y=ticks$y+.02,yend=ticks$y+.0125,linewidth=.35*k,colour=ink$tick)+
    annotate("text",x=ticks$z,y=ticks$y-.005,label=ticks$label,size=size,colour=ink$text)
  # the names of the scales are the labels of the y axis, so that ggplot2 measures them and puts
  # them in a column of their own, a gap of their own size left of the scales, which they can
  # never run into however long they are
  normal_plot<-normal_plot+
    scale_y_continuous(breaks=rows$text_y,labels=rows$label)+
    theme_void()+
    theme(legend.position="none",
          axis.text.y=element_text(hjust=0,size=size*.pt,colour=ink$name,margin=margin(r=size*.pt)),
          # the names of the scales indented from the left edge by the height of their text
          plot.margin=unit(c(0,0,0,size*.pt),"pt"),
          plot.background=element_rect(fill=ink$surface,colour=NA))+
    # the span of the scales, which the rules, longer, do not widen
    coord_fixed(ratio=6,xlim=range(-4,4,scales$z,scales$z_end,na.rm=TRUE),clip="off")
  normal_plot
}
##########################################################################################
# STANDARD NORMAL SCALES DATA
##########################################################################################
#' @title Score scales of the standard normal chart, as a data frame
#' @description Returns the z, T, cumulative percentage, percentile, sten, stanine, grade and
#' IQ scales that \code{\link{plot_normal}} draws by default, in the format of its
#' \code{scales} argument, so you can start from them and change, remove or add scales.
#' @details All scales are placed on the z axis:
#' \itemize{
#'   \item \strong{z}: standard score, mean 0 and standard deviation 1.
#'   \item \strong{T}: \eqn{50 + 10z}.
#'   \item \strong{Cumulative \%}: the percentage of cases below each standard deviation, from
#'     \eqn{z = -3} to \eqn{z = 3}: 0.1, 2.3, 15.9, 50, 84.1, 97.7 and 99.9.
#'   \item \strong{Percentile}: the z values of the 1st, 5th, 10th, 25th, 50th, 75th, 90th,
#'     95th and 99th percentiles.
#'   \item \strong{Sten}: ten bands half a standard deviation wide, sten 1 below \eqn{z = -2}
#'     and sten 10 above \eqn{z = 2} (mean 5.5, standard deviation 2).
#'   \item \strong{Stanine}: nine bands half a standard deviation wide, stanine 5 from
#'     \eqn{z = -0.25} to \eqn{z = 0.25} (mean 5, standard deviation 2).
#'   \item \strong{Grade}: groups of stanines, E (1-2), D (3), C (4-6), B (7) and A (8-9).
#'   \item \strong{IQ}: \eqn{100 + 15z}.
#' }
#' @return A data frame with the columns scale, z, z_end, label and colour, and one row per
#'   tick mark (z, T, cumulative percentage, percentile and IQ, with \code{z_end} set to
#'   \code{NA}) or per band (sten, stanine and grade). \code{colour} is \code{NA} throughout, so
#'   that the bands take the colours of the palette of the chart; set it to colour a band
#'   yourself.
#' @seealso \code{\link{plot_normal}}
#' @export
#' @examples
#' scales<-standard_normal_scales_data()
#' head(scales)
#' unique(scales$scale)
standard_normal_scales_data<-function() {
  z_score<- -4:4
  bands<-function(scale,z,z_end,label) data.frame(scale=scale,z=z,z_end=z_end,label=as.character(label),colour=NA)
  ticks<-function(scale,z,label) bands(scale,z,NA,label)
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
