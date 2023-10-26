##########################################################################################
# STANDARD NORMAL PLOT
##########################################################################################
#' @title Plot standard normal plot
#' @param size text size
#' @import ggplot2
#' @importFrom grDevices rainbow
#' @export
#' @examples
#' standard_normal_plot()
#' standard_normal_plot(size=4)
standard_normal_plot<-function(size=5) {
  standard_normal_distribution<-function(x) {
    return((1/sqrt(2*pi))*exp(-1/2*x^2))
  }
  percentile_x<-c(-2.326,-1.645,-1.282,-0.675,0,0.675,1.282,1.645,2.326)
  percentile_label<-c(1,5,10,25,50,75,90,95,99)
  colours<-hcl.colors(18,palette="Heat 2",alpha=1,rev=TRUE,fixup=TRUE)
  ss<-data.frame(stanine=c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9),
                 z_stanine=c(-4,-1.75,-1.75,-1.25,-1.25,-0.75,-0.75,-0.25,-0.25,0.25,0.25,0.75,0.75,1.25,1.25,1.75,1.75,4),
                 sten=c(1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10),
                 z_sten=c(-4,-2,-1.5,-1.5,-1,-1,-0.5,-0.5,0,0,0.5,0.5,1,1,1.5,1.5,2,4),
                 colours=colours)
  z_score<--4:4
  snd<-data.frame(y=standard_normal_distribution(seq(-4,4,.01)),x=seq(-4,4,.01))
  line_1<--.05
  line_2<--.09
  line_3<--.13
  line_4<--.17
  line_5<--.21
  line_6<--.25
  line_7<--.29
  normal_plot<-ggplot(snd,aes(x=x,y=y))+
    geom_line(linewidth=2,alpha=.3)+
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
  
  # for (i in z_score) {
  #   print(i)
  #   yend<-snd[which(snd$x==i),]$y
  #   normal_plot<-normal_plot+geom_segment(aes(x=i,y=0,xend=i,yend=yend),alpha=.5,na.rm=TRUE,size=.2,color="gray25")
  # }
  
  normal_plot<-normal_plot+
    geom_segment(aes(x=z_score[1],y=0,xend=z_score[1],yend=snd[which(snd$x==z_score[1]),"y"]),alpha=.01,na.rm=TRUE,size=.2,color="gray25")+
    geom_segment(aes(x=z_score[2],y=0,xend=z_score[2],yend=snd[which(snd$x==z_score[2]),"y"]),alpha=.01,na.rm=TRUE,size=.2,color="gray25")+
    geom_segment(aes(x=z_score[3],y=0,xend=z_score[3],yend=snd[which(snd$x==z_score[3]),"y"]),alpha=.01,na.rm=TRUE,size=.2,color="gray25")+
    geom_segment(aes(x=z_score[4],y=0,xend=z_score[4],yend=snd[which(snd$x==z_score[4]),"y"]),alpha=.01,na.rm=TRUE,size=.2,color="gray25")+
    geom_segment(aes(x=z_score[5],y=0,xend=z_score[5],yend=snd[which(snd$x==z_score[5]),"y"]),alpha=.01,na.rm=TRUE,size=.2,color="gray25")+
    geom_segment(aes(x=z_score[6],y=0,xend=z_score[6],yend=snd[which(snd$x==z_score[6]),"y"]),alpha=.01,na.rm=TRUE,size=.2,color="gray25")+
    geom_segment(aes(x=z_score[7],y=0,xend=z_score[7],yend=snd[which(snd$x==z_score[7]),"y"]),alpha=.01,na.rm=TRUE,size=.2,color="gray25")+
    geom_segment(aes(x=z_score[8],y=0,xend=z_score[8],yend=snd[which(snd$x==z_score[8]),"y"]),alpha=.01,na.rm=TRUE,size=.2,color="gray25")+
    geom_segment(aes(x=z_score[9],y=0,xend=z_score[9],yend=snd[which(snd$x==z_score[9]),"y"]),alpha=.01,na.rm=TRUE,size=.2,color="gray25")+
    
    annotate("text",x=c(-3:4)-.5,y=rep(0.01,8),label=c("0.13%","2.14%","13.59%","34.13%","34.13%","13.59%","2.14%","0.13%"),size=size)+
    geom_hline(yintercept=0,alpha=.5,size=1)+
    
    annotate("text",x=min(z_score)-2,y=line_1,label="z - σ",hjust=0,vjust=1,size=size)+
    annotate("text",x=z_score,y=rep(line_1,length(z_score)),label=z_score,hjust=.5,vjust=1,size=size)+
    annotate("segment",x=z_score,xend=z_score,y=rep(line_1+.02,length(z_score)),yend=rep(line_1+.01,length(z_score)),alpha=.5,hjust=.5,vjust=1)+
    geom_hline(yintercept=line_1+.02,alpha=.5,size=.1)+
    
    annotate("text",x=min(z_score)-2,y=line_2,label="T",hjust=0,vjust=1,size=size)+
    annotate("text",x=z_score,y=rep(line_2,length(z_score)),label=(z_score*10)+50,hjust=.5,vjust=1,size=size)+
    annotate("segment",x=z_score,xend=z_score,y=rep(line_2+.02,length(z_score)),yend=rep(line_2+.01,length(z_score)),alpha=.5)+
    geom_hline(yintercept=line_2+.02,alpha=.5,size=.1)+
    
    annotate("text",x=min(z_score)-2,y=line_3,label="PERCENTILE",hjust=0,vjust=1,size=size)+
    annotate("text",x=percentile_x,y=rep(line_3,length(percentile_x)),label=percentile_label,hjust=.5,vjust=1,size=size)+
    annotate("segment",x=percentile_x,xend=percentile_x,y=rep(line_3+.02,length(percentile_x)),yend=rep(line_3+.01,length(percentile_x)),alpha=.5)+
    geom_hline(yintercept=line_3+.02,alpha=.5,size=.1)+
    
    geom_segment(x=ss$z_sten[1],y=line_4,xend=ss$z_sten[2],yend=line_4,size=rel(size*2.3),colour=ss$colours[1],alpha=1,na.rm=TRUE)+
    geom_segment(x=ss$z_sten[2],y=line_4,xend=ss$z_sten[3],yend=line_4,size=rel(size*2.3),colour=ss$colours[2],alpha=1,na.rm=TRUE)+
    geom_segment(x=ss$z_sten[3],y=line_4,xend=ss$z_sten[4],yend=line_4,size=rel(size*2.3),colour=ss$colours[3],alpha=1,na.rm=TRUE)+
    geom_segment(x=ss$z_sten[4],y=line_4,xend=ss$z_sten[5],yend=line_4,size=rel(size*2.3),colour=ss$colours[4],alpha=1,na.rm=TRUE)+
    geom_segment(x=ss$z_sten[5],y=line_4,xend=ss$z_sten[6],yend=line_4,size=rel(size*2.3),colour=ss$colours[5],alpha=1,na.rm=TRUE)+
    geom_segment(x=ss$z_sten[6],y=line_4,xend=ss$z_sten[7],yend=line_4,size=rel(size*2.3),colour=ss$colours[6],alpha=1,na.rm=TRUE)+
    geom_segment(x=ss$z_sten[7],y=line_4,xend=ss$z_sten[8],yend=line_4,size=rel(size*2.3),colour=ss$colours[7],alpha=1,na.rm=TRUE)+
    geom_segment(x=ss$z_sten[8],y=line_4,xend=ss$z_sten[9],yend=line_4,size=rel(size*2.3),colour=ss$colours[8],alpha=1,na.rm=TRUE)+
    geom_segment(x=ss$z_sten[9],y=line_4,xend=ss$z_sten[10],yend=line_4,size=rel(size*2.3),colour=ss$colours[9],alpha=1,na.rm=TRUE)+
    geom_segment(x=ss$z_sten[10],y=line_4,xend=ss$z_sten[11],yend=line_4,size=rel(size*2.3),colour=ss$colours[11],alpha=1,na.rm=TRUE)+
    geom_segment(x=ss$z_sten[11],y=line_4,xend=ss$z_sten[12],yend=line_4,size=rel(size*2.3),colour=ss$colours[12],alpha=1,na.rm=TRUE)+
    geom_segment(x=ss$z_sten[12],y=line_4,xend=ss$z_sten[13],yend=line_4,size=rel(size*2.3),colour=ss$colours[13],alpha=1,na.rm=TRUE)+
    geom_segment(x=ss$z_sten[13],y=line_4,xend=ss$z_sten[14],yend=line_4,size=rel(size*2.3),colour=ss$colours[14],alpha=1,na.rm=TRUE)+
    geom_segment(x=ss$z_sten[14],y=line_4,xend=ss$z_sten[15],yend=line_4,size=rel(size*2.3),colour=ss$colours[15],alpha=1,na.rm=TRUE)+
    geom_segment(x=ss$z_sten[15],y=line_4,xend=ss$z_sten[16],yend=line_4,size=rel(size*2.3),colour=ss$colours[16],alpha=1,na.rm=TRUE)+
    geom_segment(x=ss$z_sten[16],y=line_4,xend=ss$z_sten[17],yend=line_4,size=rel(size*2.3),colour=ss$colours[17],alpha=1,na.rm=TRUE)+
    geom_segment(x=ss$z_sten[17],y=line_4,xend=ss$z_sten[18],yend=line_4,size=rel(size*2.3),colour=ss$colours[18],alpha=1,na.rm=TRUE)+
    annotate("text",x=min(z_score)-2,y=line_4,label="STEN",hjust=0,vjust=1,size=size)+
    annotate("text",x=c(-3,-1.5-.25,-1-.25,-.5-.25,0-.25,0+.25,.5+.25,1+.25,1.5+.25,3),y=rep(line_4,10),label=1:10,hjust=.5,vjust=.5,size=size)+
    geom_hline(yintercept=line_4+.02,alpha=.5,size=.1)+
    
    geom_segment(x=ss$z_stanine[1],y=line_5,xend=ss$z_stanine[2],yend=line_5,size=rel(size*2.3),colour=ss$colours[1],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[2],y=line_5,xend=ss$z_stanine[3],yend=line_5,size=rel(size*2.3),colour=ss$colours[2],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[3],y=line_5,xend=ss$z_stanine[4],yend=line_5,size=rel(size*2.3),colour=ss$colours[3],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[4],y=line_5,xend=ss$z_stanine[5],yend=line_5,size=rel(size*2.3),colour=ss$colours[4],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[5],y=line_5,xend=ss$z_stanine[6],yend=line_5,size=rel(size*2.3),colour=ss$colours[5],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[6],y=line_5,xend=ss$z_stanine[7],yend=line_5,size=rel(size*2.3),colour=ss$colours[6],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[7],y=line_5,xend=ss$z_stanine[8],yend=line_5,size=rel(size*2.3),colour=ss$colours[7],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[8],y=line_5,xend=ss$z_stanine[9],yend=line_5,size=rel(size*2.3),colour=ss$colours[8],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[9],y=line_5,xend=ss$z_stanine[10],yend=line_5,size=rel(size*2.3),colour=ss$colours[9],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[10],y=line_5,xend=ss$z_stanine[11],yend=line_5,size=rel(size*2.3),colour=ss$colours[11],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[11],y=line_5,xend=ss$z_stanine[12],yend=line_5,size=rel(size*2.3),colour=ss$colours[12],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[12],y=line_5,xend=ss$z_stanine[13],yend=line_5,size=rel(size*2.3),colour=ss$colours[13],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[13],y=line_5,xend=ss$z_stanine[14],yend=line_5,size=rel(size*2.3),colour=ss$colours[14],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[14],y=line_5,xend=ss$z_stanine[15],yend=line_5,size=rel(size*2.3),colour=ss$colours[15],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[15],y=line_5,xend=ss$z_stanine[16],yend=line_5,size=rel(size*2.3),colour=ss$colours[16],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[16],y=line_5,xend=ss$z_stanine[17],yend=line_5,size=rel(size*2.3),colour=ss$colours[17],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[17],y=line_5,xend=ss$z_stanine[18],yend=line_5,size=rel(size*2.3),colour=ss$colours[18],alpha=.1,na.rm=TRUE)+
    annotate("text",x=min(z_score)-2,y=line_5,label="STANINE",hjust=0,vjust=1,size=size)+
    annotate("text",x=c(-3,-1.75+.25,-1.25+.25,-0.75+.25,-0.25+.25,0.25-.25,0.75-.25,1.25-.25,1.75-.25,3),y=rep(line_5,10),label=c(1:5,5:9),hjust=.5,vjust=.5,size=size)+
    geom_hline(yintercept=line_5+.02,alpha=.5,size=.1)+
    
    geom_segment(x=ss$z_stanine[1],y=line_6,xend=ss$z_stanine[4],yend=line_6,size=rel(size*2.3),colour=ss$colours[1],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[4],y=line_6,xend=ss$z_stanine[6],yend=line_6,size=rel(size*2.3),colour=ss$colours[5],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[6],y=line_6,xend=ss$z_stanine[12],yend=line_6,size=rel(size*2.3),colour=ss$colours[9],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[12],y=line_6,xend=ss$z_stanine[14],yend=line_6,size=rel(size*2.3),colour=ss$colours[14],alpha=.1,na.rm=TRUE)+
    geom_segment(x=ss$z_stanine[14],y=line_6,xend=ss$z_stanine[18],yend=line_6,size=rel(size*2.3),colour=ss$colours[18],alpha=.1,na.rm=TRUE)+
    annotate("text",x=min(z_score)-2,y=line_6,label="GRADE",hjust=0,vjust=1,size=size)+
    annotate("text",x=c(-2,-1,0,1,2),y=rep(line_6,5),label=c("E","D","C","B","A"),hjust=.5,vjust=.5,size=size)+
    geom_hline(yintercept=line_6+.02,alpha=.5,size=.1)+
    
    annotate("text",x=min(z_score)-2,y=line_7,label="IQ",hjust=0,vjust=1,size=size)+
    annotate("text",x=z_score,y=rep(line_7,length(z_score)),label=z_score*15+100,hjust=.5,vjust=1,size=size)+
    annotate("segment",x=z_score,xend=z_score,y=rep(line_7+.02,length(z_score)),yend=rep(line_7+.01,length(z_score)),alpha=.5)+
    geom_hline(yintercept=line_7+.02,alpha=.5,size=.1)+
    
    coord_fixed(ratio=6)
  
  return(normal_plot)
}
