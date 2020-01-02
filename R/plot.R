##########################################################################################
# DESCRIPTIVES
##########################################################################################
#' @title Descriptive statistics
#' @param size text size
#' @export
#' @examples
#' standard_normal_plot()
#' standard_normal_plot(size=5)
standard_normal_plot<-function(size=10){
  require(ggplot2)
  standard_normal_distribution<-function(x)
    return((1/sqrt(2*pi))*exp(-1/2*x^2))
  percentile_x<-c(-2.326,-1.645,-1.282,-0.675,0,0.675,1.282,1.645,2.326)
  percentile_label<-c(1,5,10,25,50,75,90,95,99)
  ss<-data.frame(stanine=c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9),
                 z_stanine=c(-4,-1.75,-1.75,-1.25,-1.25,-0.75,-0.75,-0.25,-0.25,0.25,0.25,0.75,0.75,1.25,1.25,1.75,1.75,4),
                 sten=c(1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10),
                 z_sten=c(-4,-2,-1.5,-1.5,-1,-1,-0.5,-0.5,0,0,0.5,0.5,1,1,1.5,1.5,2,4))
  colours<-rainbow(nrow(ss))
  z_score<--4:4
  snd<-data.frame(y=standard_normal_distribution(seq(-4,4,.01)),x=seq(-4,4,.01))
  normal_plot<-ggplot(snd,aes(x=x,y=y))+
    geom_line(size=2,alpha=.3)+
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
          panel.border=element_blank())

  for(i in 1:nrow(ss)){
    normal_plot<-normal_plot+geom_segment(x=ss$z_stanine[i],y=-.18,xend=ss$z_stanine[i+1],yend=-.18,size=rel(size*2.3),colour=colours[i],alpha=.009,na.rm=TRUE)
    normal_plot<-normal_plot+geom_segment(x=ss$z_sten[i],y=-.13,xend=ss$z_sten[i+1],yend=-.13,size=rel(size*2.3),colour=colours[i],alpha=.009,na.rm=TRUE)
  }

  normal_plot<-normal_plot+
    geom_segment(aes(x=z_score[1],y=0,xend=z_score[1],yend=snd[which(snd$x==z_score[1]),"y"]),alpha=.01,na.rm=TRUE,size=.1)+
    geom_segment(aes(x=z_score[2],y=0,xend=z_score[2],yend=snd[which(snd$x==z_score[2]),"y"]),alpha=.01,na.rm=TRUE,size=.1)+
    geom_segment(aes(x=z_score[3],y=0,xend=z_score[3],yend=snd[which(snd$x==z_score[3]),"y"]),alpha=.01,na.rm=TRUE,size=.1)+
    geom_segment(aes(x=z_score[4],y=0,xend=z_score[4],yend=snd[which(snd$x==z_score[4]),"y"]),alpha=.01,na.rm=TRUE,size=.1)+
    geom_segment(aes(x=z_score[5],y=0,xend=z_score[5],yend=snd[which(snd$x==z_score[5]),"y"]),alpha=.01,na.rm=TRUE,size=.1)+
    geom_segment(aes(x=z_score[6],y=0,xend=z_score[6],yend=snd[which(snd$x==z_score[6]),"y"]),alpha=.01,na.rm=TRUE,size=.1)+
    geom_segment(aes(x=z_score[7],y=0,xend=z_score[7],yend=snd[which(snd$x==z_score[7]),"y"]),alpha=.01,na.rm=TRUE,size=.1)+
    geom_segment(aes(x=z_score[8],y=0,xend=z_score[8],yend=snd[which(snd$x==z_score[8]),"y"]),alpha=.01,na.rm=TRUE,size=.1)+
    geom_segment(aes(x=z_score[9],y=0,xend=z_score[9],yend=snd[which(snd$x==z_score[9]),"y"]),alpha=.01,na.rm=TRUE,size=.1)+

    annotate("text",x=c(-3:4)-.5,y=rep(0.01,8),label=c("0.13%","2.14%","13.59%","34.13%","34.13%","13.59%","2.14%","0.13%"),size=size)+

    annotate("text",x=min(z_score)-1,y=-.05,label="z - σ",hjust=0,vjust=0,size=size)+
    annotate("text",x=z_score,y=rep(-.05,length(z_score)),label=z_score,vjust=0,size=size)+
    annotate("segment",x=z_score,xend=z_score,y=rep(-.055,length(z_score)),yend=rep(-.052,length(z_score)),alpha=.5)+

    annotate("text",x=min(z_score)-1,y=-.25,label="PERCENTILE",hjust=0,vjust=0,size=size)+
    annotate("text",x=percentile_x,y=rep(-.25,length(percentile_x)),label=percentile_label,vjust=0,size=size)+
    annotate("segment",x=percentile_x,xend=percentile_x,y=rep(-.255,length(percentile_x)),yend=rep(-.252,length(percentile_x)),alpha=.5)+

    annotate("text",x=min(z_score)-1,y=-.10,label="T",hjust=0,vjust=0,size=size)+
    annotate("text",x=z_score,y=rep(-.10,length(z_score)),label=(z_score*10)+50,vjust=0,size=size)+
    annotate("segment",x=z_score,xend=z_score,y=rep(-.105,length(z_score)),yend=rep(-.102,length(z_score)),alpha=.5)+

    annotate("text",x=min(z_score)-1,y=-.15,label="STEN",hjust=0,vjust=0,size=size)+
    annotate("text",x=c(-3,-1.5-.25,-1-.25,-.5-.25,0-.25,0+.25,.5+.25,1+.25,1.5+.25,3),y=rep(-.15,10),label=1:10,vjust=0,size=size)+

    annotate("text",x=min(z_score)-1,y=-.20,label="STANINE",hjust=0,vjust=0,size=size)+
    annotate("text",x=c(-3,-1.75+.25,-1.25+.25,-0.75+.25,-0.25+.25,0.25-.25,0.75-.25,1.25-.25,1.75-.25,3),y=rep(-.20,10),label=c(1:5,5:9),vjust=0,size=size)+


    geom_hline(yintercept=-.002,alpha=.5,size=.1)+
    geom_hline(yintercept=-.055,alpha=.5,size=.1)+
    geom_hline(yintercept=-.105,alpha=.5,size=.1)+
    geom_hline(yintercept=-.155,alpha=.5,size=.1)+
    geom_hline(yintercept=-.205,alpha=.5,size=.1)+
    geom_hline(yintercept=-.255,alpha=.5,size=.1)+
    coord_fixed(ratio=7)

  return(normal_plot)

}

