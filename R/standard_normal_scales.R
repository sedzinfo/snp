##########################################################################################
# STANDARD NORMAL SCALES
##########################################################################################
#' @title Plot standard normal curve with score scales
#' @description The chart of standard_normal_plot(): the standard normal curve with the
#' z, T, percentile, sten, stanine, grade and IQ scales under it. The coloured bands are
#' sized in plot units, so the rows never overlap whatever the size of the figure, and
#' each element is drawn once, without ggplot2 warnings.
#' @param size text size
#' @import ggplot2
#' @importFrom grDevices hcl.colors
#' @export
#' @examples
#' standard_normal_scales()
#' standard_normal_scales(size=4)
standard_normal_scales<-function(size=3) {
  z_score<- -4:4
  snd<-data.frame(x=seq(-4.5,4.5,.01))
  snd$y<-exp(-snd$x^2/2)/sqrt(2*pi)
  percentile_x<-c(-2.326,-1.645,-1.282,-0.675,0,0.675,1.282,1.645,2.326)
  percentile_label<-c(1,5,10,25,50,75,90,95,99)
  colours<-hcl.colors(18,palette="Heat 2",alpha=1,rev=TRUE,fixup=TRUE)
  linecolor<-"gray50"
  # rows under the curve are .04 apart with a separator .02 above each row line;
  # scale labels sit under their tick marks and bands fill 85% of their row, so rows never touch
  rows<-data.frame(label=c("z","T","PERCENTILE","STEN","STANINE","GRADE","IQ"),
                   y=c(-.05,-.09,-.13,-.17,-.21,-.25,-.29),
                   band=c(FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE))
  rows$text_y<-ifelse(rows$band,rows$y,rows$y-.005)
  band_half_height<-.017
  scales<-data.frame(y=rep(rows$y[!rows$band],each=9),
                     text_y=rep(rows$text_y[!rows$band],each=9),
                     x=c(z_score,z_score,percentile_x,z_score),
                     label=c(z_score,z_score*10+50,percentile_label,z_score*15+100))
  bands<-data.frame(y=rep(rows$y[rows$band],c(10,9,5)),
                    xmin=c(-4,-2,-1.5,-1,-.5,0,.5,1,1.5,2,             # sten 1-10
                           -4,-1.75,-1.25,-.75,-.25,.25,.75,1.25,1.75, # stanine 1-9
                           -4,-1.25,-.75,.75,1.25),                    # grade E-A
                    xmax=c(-2,-1.5,-1,-.5,0,.5,1,1.5,2,4,
                           -1.75,-1.25,-.75,-.25,.25,.75,1.25,1.75,4,
                           -1.25,-.75,.75,1.25,4),
                    label_x=c(-3,-1.75,-1.25,-.75,-.25,.25,.75,1.25,1.75,3,
                              -3,-1.5,-1,-.5,0,.5,1,1.5,3,
                              -2,-1,0,1,2),
                    label=c(1:10,1:9,"E","D","C","B","A"),
                    colour=colours[c(1,2,4,6,8,11,13,15,17,18,
                                     1,3,5,7,9,12,14,16,18,
                                     1,5,9,14,18)])
  normal_plot<-ggplot()+
    geom_line(data=snd,aes(x=.data$x,y=.data$y),linewidth=2,colour=linecolor)+
    annotate("segment",x=z_score,xend=z_score,y=-.01,yend=exp(-z_score^2/2)/sqrt(2*pi),linewidth=1,colour=linecolor)+
    geom_hline(yintercept=-.01,linewidth=1,colour=linecolor)+
    annotate("text",x=c(-3:4)-.5,y=.01,label=c("0.13%","2.14%","13.59%","34.13%","34.13%","13.59%","2.14%","0.13%"),size=size)+
    annotate("rect",xmin=bands$xmin,xmax=bands$xmax,ymin=bands$y-band_half_height,ymax=bands$y+band_half_height,fill=bands$colour)+
    annotate("text",x=bands$label_x,y=bands$y,label=bands$label,size=size)+
    annotate("segment",x=scales$x,xend=scales$x,y=scales$y+.02,yend=scales$y+.01,alpha=.5)+
    annotate("text",x=scales$x,y=scales$text_y,label=scales$label,size=size)+
    annotate("text",x=min(z_score)-2,y=rows$text_y,label=rows$label,hjust=0,size=size)+
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
