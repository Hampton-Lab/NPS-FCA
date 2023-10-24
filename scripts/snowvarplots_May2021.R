

# plot SWE interannual variability, and ice out date
iceout_SWE<-bigjoin.stats %>% filter(variable=="ProfTemp_top2m") %>% as.data.frame()
iceout_SWE$year_label<-as.character(substr(iceout_SWE$year,3,4))
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snodas = dense_rank((SWE_May_snodas))/max(dense_rank((SWE_May_snodas)))) %>% as.data.frame()
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snotel = dense_rank((SWE_May_snotel))/max(dense_rank((SWE_May_snotel)),na.rm=TRUE)) %>% as.data.frame()
# subset to a manageable number of lakes
iceout_SWE<-subset(iceout_SWE,iceout_SWE$short_code %in% c("15","AL","BL","PA","SI","ER","LE","LB","CR","HE","HO","LC"))


orig<-ggplot(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel,color=park_code,label=year_label))+
  geom_point(size=4,alpha=0.5)+
  ylab("Cumulative Distribution")+xlab("May SWE (cm) and corresponding ice-out date (day of year)")+
  geom_text(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=3)+
  geom_point(data=iceout_SWE,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~substr(park_site,1,15),nrow=3)+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnotel_scatterplot_orig.png",
       plot = orig, width = 8, height = 5, units = "in")

iceout_SWE<-bigjoin.stats %>% filter(variable=="ProfTemp_top2m") %>% as.data.frame()
iceout_SWE$year_label<-as.character(substr(iceout_SWE$year,3,4))
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snodas = dense_rank((SWE_May_snodas))/max(dense_rank((SWE_May_snodas)))) %>% as.data.frame()
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snotel = dense_rank((SWE_May_snotel))/max(dense_rank((SWE_May_snotel)),na.rm=TRUE)) %>% as.data.frame()

all<-ggplot(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel,color=park_code,label=year_label))+
  geom_point(size=4,alpha=0.5)+
  ylab("Cumulative Distribution")+xlab("May SWE (cm) and corresponding ice-out date (day of year)")+
  geom_text(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=3)+
  geom_point(data=iceout_SWE,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~substr(park_site,1,15),nrow=3)+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnotel_plot_everylake.png",
       plot = all, width = 12, height = 5, units = "in")


iceout_SWE<-bigjoin.stats %>% filter(variable=="ProfTemp_top2m") %>% as.data.frame()
iceout_SWE$year_label<-as.character(substr(iceout_SWE$year,3,4))
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snodas = dense_rank((SWE_May_snodas))/max(dense_rank((SWE_May_snodas)))) %>% as.data.frame()
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snotel = dense_rank((SWE_May_snotel))/max(dense_rank((SWE_May_snotel)),na.rm=TRUE)) %>% as.data.frame()
# subset to a manageable number of lakes
iceout_SWE<-subset(iceout_SWE,iceout_SWE$short_code %in% c("15","AL","BL","PA","SI","ER","LE","LB","CR","GL","SU","LC"))


bill1<-ggplot(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel,color=park_code,label=year_label))+
  geom_point(size=4,alpha=0.5)+
  ylab("Cumulative Distribution")+xlab("May SWE (cm) and corresponding ice-out date (day of year)")+
  geom_text(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=3)+
  geom_point(data=iceout_SWE,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~substr(park_site,1,15),nrow=3)+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnotel_plot_revised.png",
       plot = bill1, width = 8, height = 5, units = "in")

bill2<-ggplot(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel,color=park_code,label=year_label))+
  geom_point(size=4,alpha=0.3)+
  ylab("Cumulative Distribution")+xlab("May SWE (cm) and corresponding ice-out date (day of year)")+
  geom_text(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=3)+
  geom_point(data=iceout_SWE,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25)+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~substr(park_site,1,15),nrow=3)+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnotel_plot_revised2.png",
       plot = bill2, width = 8, height = 5, units = "in")


bill3<-ggplot(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel,color=park_code,label=year_label))+
  geom_point(size=3,alpha=0.3)+
  ylab("Cumulative Distribution")+xlab("May SWE (cm) and corresponding ice-out date (day of year)")+
  geom_text(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=2)+
  geom_point(data=iceout_SWE,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2,color="dark gray")+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~substr(park_site,1,15),nrow=3)+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnotel_plot_revised3.png",
       plot = bill3, width = 8, height = 5, units = "in")

bill4<-ggplot(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel,color=park_code,label=year_label))+
  geom_point(size=3,alpha=0.3)+
  ylab("Cumulative Distribution")+xlab("May SWE (cm) and corresponding ice-out date (day of year)")+
  geom_text_repel(direction="x",box.padding = 0,label.padding=0,point.padding=0,label.r=0,min.segment.length=0,data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=2)+
  geom_point(data=iceout_SWE,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2,color="dark gray")+
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~substr(park_site,1,15),nrow=3)+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnotel_plot_revised4.png",
       plot = bill4, width = 8, height = 5, units = "in")

iceout_SWE<-bigjoin.stats %>% filter(variable=="ProfTemp_top2m") %>% as.data.frame()
iceout_SWE$year_label<-as.character(substr(iceout_SWE$year,3,4))
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snodas = dense_rank((SWE_May_snodas))/max(dense_rank((SWE_May_snodas)))) %>% as.data.frame()
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snotel = dense_rank((SWE_May_snotel))/max(dense_rank((SWE_May_snotel)),na.rm=TRUE)) %>% as.data.frame()
# subset to a manageable number of lakes
iceout_SWE<-subset(iceout_SWE,iceout_SWE$short_code %in% c("15","AL","BL","PA","SI","ER","LE","LB","CR","GL","SU","LC"))
iceout_SWE_no16CRLCSU<-iceout_SWE[-which(iceout_SWE$year_label=="16" & iceout_SWE$short_code %in% c("CR","SU","LC")),]
iceout_SWE_only16CRLCSU<-iceout_SWE[which(iceout_SWE$year_label=="16" & iceout_SWE$short_code %in% c("CR","SU","LC")),]

bill5<-ggplot(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel,color=park_code,label=year_label))+
  ylab("Cumulative Distribution")+xlab("May SWE (cm) and corresponding ice-out date (day of year)")+
  geom_point(size=3,alpha=0.3)+
  geom_point(data=iceout_SWE_no16CRLCSU,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2,color="dark gray")+
  geom_text(data=iceout_SWE_no16CRLCSU,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=2)+
 
  geom_point(data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2,color="dark gray")+
  geom_text_repel(box.padding = 0.4,point.padding=0.5,min.segment.length=0,
                  data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=2)+
  
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~substr(park_site,1,15),nrow=3)+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnotel_plot_revised5.png",
       plot = bill5, width = 8, height = 5, units = "in")

bill6<-ggplot(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel,color=park_code,label=year_label))+
  ylab("Cumulative Distribution")+xlab("May SWE (cm) and corresponding ice-out date (day of year)")+
  geom_point(size=4,alpha=0.3)+
  geom_point(data=iceout_SWE_no16CRLCSU,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25,color="dark gray")+
  geom_text(data=iceout_SWE_no16CRLCSU,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=3)+
  
  geom_point(data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25,color="dark gray")+
  geom_text_repel(box.padding = 0.4,point.padding=0.5,min.segment.length=0,
                  data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=3)+
  
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~substr(park_site,1,15),nrow=3)+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnotel_plot_revised6.png",
       plot = bill6, width = 8, height = 5, units = "in")


bill7<-ggplot(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel,color=park_code,label=year_label))+
  ylab("Cumulative Distribution")+xlab("May SWE (cm) and corresponding ice-out date (day of year)")+
  geom_point(size=4,alpha=0.3)+
  geom_point(data=iceout_SWE_no16CRLCSU,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25,color="dark gray")+
  geom_text(data=iceout_SWE_no16CRLCSU,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=2.5)+
  
  geom_point(data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25,color="dark gray")+
#  geom_text_repel(box.padding = 0.4,point.padding=0.5,min.segment.length=0,
#                  data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=2.5)+
  geom_text(data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel+0.07,x=SWE_May_snotel-35),color="black",size=2.5)+
#  geom_segment(size=0.5,data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel+0.07,x=SWE_May_snotel-35,
#                                                yend=SWE_rank_snotel,xend=SWE_May_snotel),color="black",size=2.5)+
  geom_segment(size=0.5,data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel+0.07*.7,x=SWE_May_snotel-35*.7,
                                                         yend=SWE_rank_snotel+.07*.3,xend=SWE_May_snotel-35*.3),color="black",size=2.5)+
  
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~substr(park_site,1,15),nrow=3)+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnotel_plot_revised7.png",
       plot = bill7, width = 8, height = 5, units = "in")

iceout_SWE<-bigjoin.stats %>% filter(variable=="ProfTemp_top2m") %>% as.data.frame()
iceout_SWE$year_label<-as.character(substr(iceout_SWE$year,3,4))
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snodas = dense_rank((SWE_May_snodas))/max(dense_rank((SWE_May_snodas)))) %>% as.data.frame()
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snotel = dense_rank((SWE_May_snotel))/max(dense_rank((SWE_May_snotel)),na.rm=TRUE)) %>% as.data.frame()
# subset to a manageable number of lakes
iceout_SWE<-subset(iceout_SWE,iceout_SWE$short_code %in% c("15","AL","BL","PA","SI","ER","LE","LB","CR","GL","SU","LC"))
iceout_SWE_no16CRLCSU<-iceout_SWE[-which(iceout_SWE$year_label=="16" & iceout_SWE$short_code %in% c("CR","SU","LC")),]
iceout_SWE_only16CRLCSU<-iceout_SWE[which(iceout_SWE$year_label=="16" & iceout_SWE$short_code %in% c("CR","SU","LC")),]


bill8<-ggplot(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel,color=park_code,label=year_label))+
  ylab("Cumulative Distribution")+xlab("May SWE (cm) and corresponding ice-out date (day of year)")+
  geom_point(size=3.5,alpha=0.3)+
  geom_point(data=iceout_SWE_no16CRLCSU,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25,color="dark gray")+
  geom_text(data=iceout_SWE_no16CRLCSU,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=2.5)+
  
  geom_point(data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25,color="dark gray")+
  geom_text(data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel+0.08,x=SWE_May_snotel-37),color="black",size=2.5)+
  geom_segment(size=0.5,data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel+0.08*.7,x=SWE_May_snotel-37*.7,
                                                         yend=SWE_rank_snotel+.08*.3,xend=SWE_May_snotel-37*.3),color="black",size=2.5)+
  
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~substr(park_site,1,24),nrow=3)+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnotel_plot_revised8.png",
       plot = bill8, width = 8, height = 5, units = "in")


iceout_SWE<-bigjoin.stats %>% filter(variable=="ProfTemp_top2m") %>% as.data.frame()
iceout_SWE$year_label<-as.character(substr(iceout_SWE$year,3,4))
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snodas = dense_rank((SWE_May_snodas))/max(dense_rank((SWE_May_snodas)))) %>% as.data.frame()
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snotel = dense_rank((SWE_May_snotel))/max(dense_rank((SWE_May_snotel)),na.rm=TRUE)) %>% as.data.frame()
# subset to a manageable number of lakes
iceout_SWE<-subset(iceout_SWE,iceout_SWE$short_code %in% c("15","AL","19","DW","SI","ER","LE","LB","CR","GL","SU","LC"))
iceout_SWE_no16CRLCSU<-iceout_SWE[-which(iceout_SWE$year_label=="16" & iceout_SWE$short_code %in% c("CR","SU","LC")),]
iceout_SWE_only16CRLCSU<-iceout_SWE[which(iceout_SWE$year_label=="16" & iceout_SWE$short_code %in% c("CR","SU","LC")),]


bill9<-ggplot(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel,color=park_code,label=year_label))+
  ylab("Cumulative Distribution")+xlab("May SWE (cm) and corresponding ice-out date (day of year)")+
  geom_point(size=3.5,alpha=0.3)+
  geom_point(data=iceout_SWE_no16CRLCSU,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25,color="dark gray")+
  geom_text(data=iceout_SWE_no16CRLCSU,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=2.5)+
  
  geom_point(data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25,color="dark gray")+
  geom_text(data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel+0.08,x=SWE_May_snotel-37),color="black",size=2.5)+
  geom_segment(size=0.5,data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel+0.08*.7,x=SWE_May_snotel-37*.7,
                                                         yend=SWE_rank_snotel+.08*.3,xend=SWE_May_snotel-37*.3),color="black",size=2.5)+
  
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~substr(park_site,1,24),nrow=3)+#facet_wrap(~park_siteshort,nrow=3)+ 
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnotel_plot_revised9.png",
       plot = bill9, width = 8, height = 5, units = "in")

#######

iceout_SWE<-bigjoin.stats %>% filter(variable=="ProfTemp_top2m") %>% as.data.frame()
iceout_SWE$year_label<-as.character(substr(iceout_SWE$year,3,4))
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snodas = dense_rank((SWE_May_snodas))/max(dense_rank((SWE_May_snodas)))) %>% as.data.frame()
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snotel = dense_rank((SWE_May_snotel))/max(dense_rank((SWE_May_snotel)),na.rm=TRUE)) %>% as.data.frame()
# subset to a manageable number of lakes
iceout_SWE<-subset(iceout_SWE,iceout_SWE$short_code %in% c("15","AL","BL","PA","SI","ER","LE","LB","CR","GL","SU","LC"))
iceout_SWE_no16CRLCSU<-iceout_SWE[-which(iceout_SWE$year_label=="16" & iceout_SWE$short_code %in% c("CR","SU","LC")),]
iceout_SWE_only16CRLCSU<-iceout_SWE[which(iceout_SWE$year_label=="16" & iceout_SWE$short_code %in% c("CR","SU","LC")),]


bill10<-ggplot(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel,color=park_code,label=year_label))+
  ylab("Cumulative Distribution")+xlab("May SWE (cm) and corresponding ice-out date (day of year)")+
  geom_point(size=3.5,alpha=0.3)+
  geom_point(data=iceout_SWE_no16CRLCSU,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25,color="dark gray")+
  geom_text(data=iceout_SWE_no16CRLCSU,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=2.5)+
  
  geom_point(data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25,color="dark gray")+
  geom_text(data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel+0.08,x=SWE_May_snotel-37),color="black",size=2.5)+
  geom_segment(size=0.5,data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel+0.08*.7,x=SWE_May_snotel-37*.7,
                                                         yend=SWE_rank_snotel+.08*.3,xend=SWE_May_snotel-37*.3),color="black",size=2.5)+
  
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~substr(park_site,1,24),nrow=3)+#facet_wrap(~park_siteshort,nrow=3)+ 
  theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnotel_plot_revised10.png",
       plot = bill10, width = 8, height = 5, units = "in")


iceout_SWE<-bigjoin.stats %>% filter(variable=="ProfTemp_top2m") %>% as.data.frame()
iceout_SWE$year_label<-as.character(substr(iceout_SWE$year,3,4))
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snodas = dense_rank((SWE_May_snodas))/max(dense_rank((SWE_May_snodas)))) %>% as.data.frame()
iceout_SWE<-iceout_SWE %>% group_by(short_code) %>% mutate(SWE_rank_snotel = dense_rank((SWE_May_snotel))/max(dense_rank((SWE_May_snotel)),na.rm=TRUE)) %>% as.data.frame()
# subset to a manageable number of lakes
iceout_SWE<-subset(iceout_SWE,iceout_SWE$short_code %in% c("15","AL","19","DW","SI","ER","LE","LB","CR","GL","SU","LC"))
iceout_SWE_no16CRLCSU<-iceout_SWE[-which(iceout_SWE$year_label=="16" & iceout_SWE$short_code %in% c("CR","SU","LC")),]
iceout_SWE_only16CRLCSU<-iceout_SWE[which(iceout_SWE$year_label=="16" & iceout_SWE$short_code %in% c("CR","SU","LC")),]


bill11<-ggplot(data=iceout_SWE,aes(y=SWE_rank_snotel,x=SWE_May_snotel,color=park_code,label=year_label))+
  ylab("Cumulative Distribution")+xlab("May SWE (cm) and corresponding ice-out date (day of year)")+
  geom_point(size=3.5,alpha=0.3)+
  geom_point(data=iceout_SWE_no16CRLCSU,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25,color="dark gray")+
  geom_text(data=iceout_SWE_no16CRLCSU,aes(y=SWE_rank_snotel,x=SWE_May_snotel),color="black",size=2.5)+
  
  geom_point(data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel,x=ice_out_doy,color=park_code),shape=0,size=2.25,color="dark gray")+
  geom_text(data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel+0.08,x=SWE_May_snotel-37),color="black",size=2.5)+
  geom_segment(size=0.5,data=iceout_SWE_only16CRLCSU,aes(y=SWE_rank_snotel+0.08*.7,x=SWE_May_snotel-37*.7,
                                                         yend=SWE_rank_snotel+.08*.3,xend=SWE_May_snotel-37*.3),color="black",size=2.5)+
  
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x=element_text(),axis.title.x = element_text(vjust=-0.5))+facet_wrap(~substr(park_site,1,24),nrow=3)+#facet_wrap(~park_siteshort,nrow=3)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_colour_viridis_d(end = 0.85)

ggsave(filename = "../figures/analysispowers/iceout_SWEsnotel_plot_revised11.png",
       plot = bill11, width = 8, height = 5, units = "in")

