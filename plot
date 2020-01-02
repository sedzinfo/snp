library(psycholatefunctions)

z_stanine<-c(-4,-1.75,-1.75,-1.25,-1.25,-0.75,-0.75,-0.25,-0.25,0.25,0.25,0.75,0.75,1.25,1.25,1.75,1.75,4)
stanine<-c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9)
z_sten<-c(-4,-2,-1.5,-1.5,-1,-1,-0.5,-0.5,0,0,0.5,0.5,1,1,1.5,1.5,2,4)
sten<-c(1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10)

ss<-data.frame(stanine,z_stanine)
ssn<-data.frame(sten,z_sten)

colours<-rainbow(nrow(ss))
standard_normal_distribution<-function(x)
  return((1/sqrt(2*pi))*exp(-1/2*x^2))
res<-snd<--4:4
size<-11
snd<-data.frame(y=standard_normal_distribution(seq(-4,4,.1)),x=seq(-4,4,.1))
normal_plot<-ggplot(snd,aes(x=x,y=y))+
  geom_line(size=2,alpha=.5)+
  theme_bw(base_size=40)+
  labs(x="",y="")+
  theme(legend.position="none",
        panel.grid.minor=element_blank(),
        panel.grid=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank())

normal_plot<-normal_plot+
  geom_segment(aes(x=-4,y=0,xend=-4,yend=snd[which(snd$x==-4),"y"],alpha=.5),alpha=.5)+
  geom_segment(aes(x=-3,y=0,xend=-3,yend=snd[which(snd$x==-3),"y"],alpha=.5),alpha=.5)+
  geom_segment(aes(x=-2,y=0,xend=-2,yend=snd[which(snd$x==-2),"y"],alpha=.5),alpha=.5)+
  geom_segment(aes(x=-1,y=0,xend=-1,yend=snd[which(snd$x==-1),"y"],alpha=.5),alpha=.5)+
  geom_segment(aes(x=0,y=0,xend=0,yend=snd[which(snd$x==-0),"y"],alpha=.5),alpha=.5)+
  geom_segment(aes(x=1,y=0,xend=1,yend=snd[which(snd$x==1),"y"],alpha=.5),alpha=.5)+
  geom_segment(aes(x=2,y=0,xend=2,yend=snd[which(snd$x==2),"y"],alpha=.5),alpha=.5)+
  geom_segment(aes(x=3,y=0,xend=3,yend=snd[which(snd$x==3),"y"],alpha=.5),alpha=.5)+
  geom_segment(aes(x=4,y=0,xend=4,yend=snd[which(snd$x==4),"y"],alpha=.5),alpha=.5)+
  
  annotate("text",x=c(-3:4)-.5,y=rep(0.01,8),label=c("0.13%","2.14%","13.59%","34.13%","34.13%","13.59%","2.14%","0.13%"),size=size)+
  
  annotate("text",x=min(res)-1,y=-.05,label="z - σ",hjust=0,vjust=0,size=size)+
  annotate("text",x=res,y=rep(-.05,length(res)),label=res,vjust=0,size=size)+
  annotate("segment",x=res,xend=res,y=rep(-.055,length(res)),yend=rep(-.052,length(res)),colour="black",size=1)+
  
  annotate("text",x=min(res)-1,y=-.10,label="T",hjust=0,vjust=0,size=size)+
  annotate("text",x=res,y=rep(-.10,length(res)),label=(res*10)+50,vjust=0,size=size)+
  annotate("segment",x=res,xend=res,y=rep(-.105,length(res)),yend=rep(-.102,length(res)),colour="black",size=1)+
  
  annotate("text",x=min(res)-1,y=-.15,label="STEN",hjust=0,vjust=0,size=size)+
  annotate("text",x=c(-3,-1.5-.25,-1-.25,-.5-.25,0-.25,0+.25,.5+.25,1+.25,1.5+.25,3),y=rep(-.15,10),label=c(1,2,3,4,5,6,7,8,9,10),vjust=0,size=size)+

  annotate("text",x=min(res)-1,y=-.20,label="STANINE",hjust=0,vjust=0,size=size)+
  annotate("text",x=c(-3,-1.75+.25,-1.25+.25,-0.75+.25,-0.25+.25,0.25-.25,0.75-.25,1.25-.25,1.75-.25,3),y=rep(-.20,10),label=c(1,2,3,4,5,5,6,7,8,9),vjust=0,size=size)+
  
  annotate("text",x=min(res)-1,y=-.25,label="PERCENTILE",hjust=0,vjust=0,size=size)+
  annotate("text",x=c(-2.326,-1.645,-1.282,-0.675,0,0.675,1.282,1.645,2.326),y=rep(-.25,length(c(-2.326,-1.645,-1.282,-0.675,0,0.675,1.282,1.645,2.326))),label=c(1,5,10,25,50,75,90,95,99),vjust=0,size=size)+
  annotate("segment",x=c(-2.326,-1.645,-1.282,-0.675,0,0.675,1.282,1.645,2.326),xend=c(-2.326,-1.645,-1.282,-0.675,0,0.675,1.282,1.645,2.326),y=rep(-.255,length(c(-2.326,-1.645,-1.282,-0.675,0,0.675,1.282,1.645,2.326))),yend=rep(-.252,length(c(-2.326,-1.645,-1.282,-0.675,0,0.675,1.282,1.645,2.326))),colour="black",size=1)+
  
  geom_hline(yintercept=-.002,alpha=.5)+
  geom_hline(yintercept=-.055,alpha=.5)+
  geom_hline(yintercept=-.105,alpha=.5)+
  geom_hline(yintercept=-.155,alpha=.5)+
  geom_hline(yintercept=-.205,alpha=.5)+
  geom_hline(yintercept=-.255,alpha=.5)

for(i in 1:nrow(ss))
  normal_plot<-normal_plot+geom_segment(x=ss$z_stanine[i],y=-.18,xend=ss$z_stanine[i+1],yend=-.18,size=22,colour=colours[i],alpha=.01)
for(i in 1:nrow(ssn))
  normal_plot<-normal_plot+geom_segment(x=ssn$z_sten[i],y=-.13,xend=ssn$z_sten[i+1],yend=-.13,size=22,colour=colours[i],alpha=.01)

normal_plot
