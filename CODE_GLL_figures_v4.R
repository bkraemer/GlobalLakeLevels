#load relevant packages
library(ggplot2)
library(ncdf4)
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)
library(forecast)
library(zoo)
library(FactoMineR)
library(rnn)
library(neuralnet)
library(Kendall)
library(NeuralNetTools)
library(data.table)
library(lubridate)
library(ggrepel)
library(dismo)
library(gbm)
library(gridExtra)
library(stringr)
library(rgdal)
library(ggalt)
library(ggthemes)
library(grid)
library(trend)
library(zyp)
library(rkt)
library(cowplot)
library(scales)
library(ggpubr)
library(splitstackshape)

#set the working directory
setwd("D:/GlobalLakeLevels/envisat_lakes.smooth.txt")
setwd("D:/GlobalLakeLevels/lakes.TPJOJ.2.smooth_v2")
setwd("D:/GlobalLakeLevels")
setwd("U:/B.Kraemer/GlobalLakeLevels_IGB")

#transformations
asinh_trans<-function(){
  trans_new(name='asinh',transform=function(x) asinh(x),
            inverse=function(x) sinh(x))
}

#FIGURE A: plot of the p values of lake level trends before and after accoutning for background climate variability.####
TPJO_trends<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_trends_v4.csv")
TPJO_trends[slope_raw>1]$slope<-NA
names(TPJO_trends)
head(TPJO_trends)
TPJO_trends_l<-melt(TPJO_trends,id=c("lake","lakeID"))
names(TPJO_trends_l)
head(TPJO_trends_l)
summary(TPJO_trends_l)
TPJO_trends_l

a<-ggplot()+
  geom_segment(data=TPJO_trends,
               aes(y=slope_p_raw,
                   x=rank(slope_p_raw),
                   yend=slope_p,
                   xend=rank(slope_p_raw),
                   colour=-sqrt(slope_p_raw)+sqrt(slope_p)))+
  scale_colour_gradientn(colors=c("black","black","black","darkgrey","darkgrey","darkgrey"),
                         breaks=c(-.3,0,.3),
                         limits=c(-1,1),
                         values=c(0,.4999999999999,.5000000000001,1))+
  guides(color=FALSE)+
  geom_point(data=TPJO_trends,
             aes(y=slope_p_raw,
                 x=rank(slope_p_raw)),
             colour="#00AFBB")+
  #geom_point(data=TPJO_trends,
  #           aes(y=slope_p,
  #               x=rank(slope_p_raw)),
  #           colour="#00AFBB")+
  geom_point(data=TPJO_trends,
             aes(y=slope_p,
                 x=rank(slope_p_raw)),
             colour="#FC4E07",
             size=.9)+
  scale_y_sqrt(breaks=c(0.01,0.05,0.1,0.5),
               limits=c(0,0.63),
               expand = c(0, 0))+
  scale_x_continuous(limits=c(0,118),
                     expand = c(0, 0))+
  theme_bw()+
  labs(y="Water level trend p-value",x="Rank of water level trend p-value")

b<-ggplot()+
  geom_density(data=TPJO_trends_l[variable=="slope_p_raw"|variable=="slope_p"],
               aes(value,fill=variable,colour=NA),
               adjust = .7,
               alpha=.7)+
  scale_x_sqrt(breaks=c(0.01,0.05,0.1,0.5),
               limits=c(0,0.63),
               expand = c(0, 0))+
  scale_colour_manual(values=c("black","black"))+
  scale_fill_manual(values=c("#00AFBB","#FC4E07"),
                    name = NULL, 
                    labels = c("raw p-value","p-value after 
removing 
background 
climate effects"))+
  theme_void()+
  coord_flip()+
  guides(colour=FALSE)

print(ggarrange(a,b, 
          ncol = 2, nrow = 1,  align = "hv"),
      common.legend=TRUE,
      legend="bottom")


#FIGURE B: plot of the lake level trends before and after accoutning for background climate variability.####
TPJO_trends<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_trends_v4.csv")
TPJO_trends[slope_raw>1]$slope<-NA
names(TPJO_trends)
head(TPJO_trends)
TPJO_trends_l<-melt(TPJO_trends,id=c("lake","lakeID"))
names(TPJO_trends_l)
head(TPJO_trends_l)
summary(TPJO_trends_l)
summary(na.omit(TPJO_trends_l[variable=="slope_raw"|variable=="slope"]))

a<-ggplot()+
  geom_segment(data=TPJO_trends,
               aes(y=100*slope_raw,
                   x=rank(slope_raw),
                   yend=100*slope,
                   xend=rank(slope_raw),
                   colour=slope-slope_raw))+
  scale_colour_gradientn(colors=c("black","black","black","darkgrey","darkgrey","darkgrey"),
                         breaks=c(-.5,0,.5),
                         limits=c(-.5,.5),
                         values=c(0,.4999999999999,.5000000000001,1),
                         labels=NULL)+
  geom_point(data=TPJO_trends,
             aes(y=100*slope_raw,
                 x=rank(slope_raw)),
             colour="#00AFBB")+
  #geom_point(data=TPJO_trends,
  #           aes(y=100*slope,
  #               x=rank(slope_raw)))+
  geom_point(data=TPJO_trends,
             aes(y=100*slope,
                 x=rank(slope_raw)),
             colour="#FC4E07")+
  scale_y_continuous(trans='asinh',
                     limits=c(-100,500),
                     expand = c(0, 0),
                     breaks=c(-50,-5,-.5,0,.5,5,50))+
  #scale_y_sqrt(breaks=c(0.01,0.05,0.1,.5))+
  scale_x_continuous(limits=c(0,118),
                     expand = c(0, 0))+
  theme_bw()+
  guides(color=FALSE)+
  labs(y="Water level trend (cm year^-1)",x="Rank of water level trend")

b<-ggplot()+
  geom_density(data=na.omit(TPJO_trends_l[variable=="slope_raw"|variable=="slope"]),
               aes(100*value,fill=variable,colour=NA),
               adjust = 1,
               alpha=.7)+
  scale_x_continuous(trans='asinh',
                     limits=c(-100,500),
                     breaks=c(-50,-5,-.5,0,.5,5,50),
                     #limits=c(-50,50),
                     expand = c(0, 0))+
  scale_colour_manual(values=c("black","black"))+
  scale_fill_manual(values=c("#00AFBB","#FC4E07"),
                    name = NULL, 
                    labels = c("raw slope","slope after removing 
background climate effects"))+
  theme_void()+
  coord_flip()+
  guides(colour=FALSE)

print(ggarrange(a,b, 
                ncol = 2, nrow = 1,  align = "hv"),
      common.legend=TRUE,
      legend="bottom")

#FIGURE C: Correaltions between PCs and temperature and telleconnections across space and time.####
#plot the contributions across space

PCA_cor<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/PCA_cor_v4.csv", header=TRUE)
head(PCA_cor)
summary(PCA_cor)
PCA_cor<-cSplit(PCA_cor,"lonlat",sep="lat")
PCA_cor$lonlat_1<-gsub("lon","",PCA_cor$lonlat_1)
PCA_cor$lonlat_1<-as.numeric(PCA_cor$lonlat_1)
PCA_cor<-PCA_cor[,c(6,7,1,2,3)]
names(PCA_cor)<-c("lon","lat","PCcor","PCcor_p","var")

lakeinfo<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/DATA_lakeinfo_v1.csv")
summary(lakeinfo)
lakeinfo$lake<-as.factor(lakeinfo$lake)
lakeinfo$lakeID<-as.factor(lakeinfo$lakeID)


TPJO_bestBRT_relimp<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_bestBRT_modeldata_relimp_v4.csv",stringsAsFactors = TRUE)
summary(TPJO_bestBRT_relimp)
TPJO_bestBRT_relimp$var<-as.factor(TPJO_bestBRT_relimp$var)
TPJO_bestBRT_relimp$lake<-as.factor(TPJO_bestBRT_relimp$lake)
TPJO_bestBRT_relimp<-TPJO_bestBRT_relimp[,count:=.N,by=lake]
#names(TPJO_bestBRT_relimp)<-c("dim","rel.inf","lakeID","n.trees","lr","count")
TPJO_bestBRT_relimp<-droplevels(TPJO_bestBRT_relimp)

TPJO_trends<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_trends_v4.csv")
summary(TPJO_trends)

TPJO_bestBRT_relimp<-TPJO_bestBRT_relimp[,count:=.N,by=lake]
TPJO_bestBRT_relimp[,rel.inf_scaled:=rel.inf*count/100]

relimp<-expand.grid(lake=unique(TPJO_bestBRT_relimp$lake),var=unique(TPJO_bestBRT_relimp$var))
relimp_exp<-as.data.table(merge(relimp,TPJO_bestBRT_relimp,all.x=TRUE,by=c("lake","var")))
relimp_exp[is.na(rel.inf)=="TRUE"]$rel.inf<-0
relimp_exp[is.na(rel.inf_scaled)=="TRUE"]$rel.inf_scaled<-0
summary(relimp_exp)
relimp_exp<-as.data.table(relimp_exp)

relimp_mean<-relimp_exp[,.(rel.inf_mean=mean(rel.inf)),by=var]
relimp.scaled_mean<-relimp_exp[,.(rel.inf_scaled_mean=mean(rel.inf_scaled)),by=var]
relimp.scaled_mean<-relimp.scaled_mean[order(rel.inf_scaled_mean)]

relimp_exp_winfo<-merge(relimp_exp,lakeinfo,all.x=TRUE)
#relimp_exp_winfo$rel.inf_scaled<-relimp_exp_winfo$rel.inf*relimp_exp_winfo$count/100
#relimp_exp_winfo[is.na(rel.inf_scaled)=="TRUE"]$rel.inf_scaled<-0

summary(relimp_exp_winfo)
unique(relimp_exp_winfo$var)
summary(PCA_cor[,c(1:10,300:322)])
summary(PCA_cor_l)

# read shapefile
wmap <- readOGR(dsn="U:/B.Kraemer/GlobalLakeLevels_IGB", layer="ne_110m_land")
# convert to dataframe
wmap_df <- fortify(wmap)


mapWorld<- borders("world", colour="gray55", fill=NA)
world <- map_data("world",interior = F, add = T)

PCA_cor_sub<-as.data.table(PCA_cor)[
                           var=="PC2"|
                           #var=="PC3"|
                           var=="PC4"|
                           var=="PC5"]#|
                           #var=="PC6"|
                           #var=="PC7"|
                           #var=="PC8"|
                           #var=="PC11"]
relimp_exp_winfo_sub<-as.data.table(relimp_exp_winfo)[
  var=="PC2"|
    #var=="PC3"|
    var=="PC4"|
    var=="PC5"]#|
    #var=="PC6"|
    #var=="PC7"|
    #var=="PC8"|
    #var=="PC11"]

relimp_exp_winfo_sub<-droplevels(relimp_exp_winfo_sub)
relimp_exp_winfo_sub$var<-as.factor(relimp_exp_winfo_sub$var)
levels(relimp_exp_winfo_sub$var)<-c("PC[ENSO]",
                                    "PC[NPO]",
                                    "PC[NAO]")

PCA_cor_sub<-droplevels(PCA_cor_sub)
PCA_cor_sub$var<-as.factor(PCA_cor_sub$var)
levels(PCA_cor_sub$var)<-c("PC[ENSO]",
                           "PC[NPO]",
                           "PC[NAO]")

summary(relimp_exp_winfo_sub)
relimp_exp_winfo_sub<-as.data.table(relimp_exp_winfo_sub)
summary(PCA_cor_sub)

ggplot()+
  geom_raster(data=PCA_cor_sub,aes(x=lon,y=lat,fill=PCcor,alpha=(10^(3*(1-PCcor_p)))/1000),interpolate=TRUE)+
  scale_alpha_continuous(trans="exp")+
  #geom_path(data=world,aes(x=long,y=lat,group=group),colour="gray55")+
  geom_path(data=wmap_df,aes(x=long,y=lat,group=group),colour="gray55")+
  scale_fill_distiller(palette = "Spectral",
                       values=c(0,.4,.5,.6,1)
                       #,limits=c(min(relimp_wInfo$rel_imp_perPCA),max(relimp_wInfo$rel_imp_perPCA)))+
                       ,limits=c(-.91,0.91))+
  scale_y_continuous(limits=c(-66,86),
                     expand=c(0,0))+
  scale_x_continuous(limits=c(-180,180),
                     expand=c(0,0))+
  geom_point(data=relimp_exp_winfo_sub[rel.inf_scaled>0],
             aes(x=lon,
                 y=lat,
                 size=rel.inf_scaled,
                 alpha=.4))+
  scale_radius(#limits=c(0.8,2),
    range=c(0.2,6),
    #breaks=c(1,1.2,1.4,1.6,1.8,2),
    trans="sqrt")+
  geom_point(data=relimp_exp_winfo_sub[rel.inf_scaled==0],
             aes(x=lon,
                 y=lat),shape=1,size=1)+
  facet_wrap(~var,ncol=1,labeller = label_parsed)+
  theme_bw()+
  theme(text = element_text(size=14))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(size="Relative 
Influence",
       fill="Correlation between 
surface temperature 
and the PC",
       x="Longitude",
       y="Latitude",
       alpha=NULL,
       shape=NULL)+
  guides(alpha=FALSE)



####FIGURE D: Time series of PCs compared to time series of indexes.####
#subsetted TPJO data with outliers removed
PCA_brt<-fread("file:///U:/B.Kraemer/GlobalLakeLevels_IGB/PCA_scores_detrend_v2.csv",stringsAsFactors = TRUE)
head(PCA_brt[,c(1:10,300:323)])
PCA_brt_l<-PCA_brt%>%
  gather(PC,value,2:327)
PCA_brt_l_sub<-droplevels(as.data.table(PCA_brt_l)[
  PC=="PC2"|
    #PC=="PC3"|
    PC=="PC4"|
    PC=="PC5"#|
    #PC=="PC6"|
    #PC=="PC7"|
    #PC=="PC8"|
    #PC=="PC11"
    ])
PCA_brt_l_sub$date<-date_decimal(PCA_brt_l_sub$decyear)
PCA_brt_l_sub$month<-as.numeric(month(PCA_brt_l_sub$date))
PCA_brt_l_sub<-PCA_brt_l_sub[order(PCA_brt_l_sub$date),]

PCA_brt_l_sub<-droplevels(PCA_brt_l_sub)
PCA_brt_l_sub$PC<-as.factor(PCA_brt_l_sub$PC)
levels(PCA_brt_l_sub$PC)<-c("PC[ENSO]",
                            "PC[NPO]",
                            "PC[NAO]")
PCA_brt_l_sub[which(PCA_brt_l_sub$PC=="PC[NAO]"),]$value<-
 (-PCA_brt_l_sub[which(PCA_brt_l_sub$PC=="PC[NAO]"),]$value)

PCA_brt_l_sub<-PCA_brt_l_sub%>%
  group_by(PC)%>%
  mutate(median=median(value,na.rm=TRUE),
         mad=mad(value,na.rm=TRUE))
PCA_brt_l_sub$value<-(PCA_brt_l_sub$value-PCA_brt_l_sub$median)/PCA_brt_l_sub$mad

#indeces
index<-fread("DATA_Indeces_v3.csv",header=TRUE)
index<-index[,1:14]
index_l<-index %>%
  gather(month,value,3:14)
names(index_l)<-c("index","year","month","value")
index_l$day<-15
index_l$date<-as.Date(with(index_l, paste(year, month, day,sep="-")), "%Y-%b-%d")
index_l<-index_l[order(index_l$date),]
index_l$PC<-NA

index_l[which(index_l$index=="NP"),]$PC<-"PC[NPO]"
index_l[which(index_l$index=="MEI"),]$PC<-"PC[ENSO]"
#index_l[which(index_l$index=="Solar"),]$PC<-"PC[Solar]"
index_l[which(index_l$index=="NAO"),]$PC<-"PC[NAO]"

#index_l[which(index_l$index=="MEI"),]$index<-"ENSO"
#index_l[which(index_l$index=="AtlTri"),]$index<-"AOT"

index_l_sub<-droplevels(as.data.table(index_l)[is.na(PC)==FALSE])
index_l_sub<-index_l_sub%>%
  group_by(PC)%>%
  mutate(median=median(value,na.rm=TRUE),
            mad=mad(value,na.rm=TRUE))
index_l_sub$value<-(index_l_sub$value-index_l_sub$median)/index_l_sub$mad
index_l_sub<-index_l_sub[order(index_l_sub$date),]

head(index_l_sub)
summary(PCA_brt_l_sub)

ggplot()+
  geom_point(data=PCA_brt_l_sub,aes(y=value,x=as.Date(date),colour="PC"),alpha=.3)+
  geom_smooth(data=PCA_brt_l_sub,aes(y=value,x=as.Date(date),colour="PC"),span=.1,se=FALSE,method="loess")+
  geom_point(data=index_l_sub,aes(y=value,x=as.Date(date),colour="Index"),alpha=.3)+
  geom_smooth(data=index_l_sub,aes(y=value,x=as.Date(date),colour="Index"),span=.1,se=FALSE,method="loess")+
  scale_x_date(limits=as.Date(c("1992-08-01","2019-01-01")),expand=c(0,0))+
  #scale_y_continuous()+
  #scale_color_manual()+
  theme_bw()+
  theme(text = element_text(size=14))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
  #      axis.title.x=element_blank(),
  #      axis.text.x=element_blank(),
        legend.title=element_blank())+
  labs(x="Year",y="Value z-score")+
  #guides()+
  facet_wrap(~PC,ncol=1,labeller=label_parsed,scales="free_y")

#FIGURE E: trend map for lake levels and proportion ov variability attributable to background climate####
TPJO_trends<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_bestBRT_output1_trends_v5.csv")
lakeinfo<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/DATA_lakeinfo_v1.csv")
TPJO_trends_winfo<-as.data.table(merge(TPJO_trends,lakeinfo,all.x=TRUE))

relimp_exp_PCs<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/relimp_exp_PCs_v1.csv")
TPJO_trends_winfo<-as.data.table(merge(TPJO_trends_winfo,relimp_exp_PCs,all.x=TRUE))

TPJO_bestBRT_modeldata<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_bestBRT_modeldata_v3.csv",stringsAsFactors = TRUE)
TPJO_trends_winfo<-as.data.table(merge(TPJO_trends_winfo,TPJO_bestBRT_modeldata,all.x=TRUE))

names(TPJO_trends_winfo)
summary(TPJO_trends_winfo)

TPJO_trends_winfo[slope_raw>1]$slope_raw<-1
#TPJO_trends_winfo[slope_level_anom_anMAD>0.1]$slope_level_anom_anMAD<-0.1
#TPJO_trends_winfo[slope_level_anom_anMAD<(-0.1)]$slope_level_anom_anMAD<-(-0.1)
#TPJO_trends_winfo[slope_level_anom_anVAR>.1]$slope_level_anom_anVAR<-.1
#TPJO_trends_winfo[slope_level_anom_anVAR<(-0.1)]$slope_level_anom_anVAR<-(-0.1)

mapWorld<- borders("world", colour="black", fill="black")

a<-ggplot()+
  #geom_path(data=world,aes(x=long,y=lat,group=group),colour="gray55")+
  mapWorld+
  scale_y_continuous(limits=c(-60,86),
                     expand=c(0,0))+
  scale_x_continuous(#limits=c(-180,180),
    expand=c(0,0))+
  geom_point(data=TPJO_trends_winfo,
             aes(x=lon,
                 y=lat,
                 colour=slope_raw,
                 size=(slope_p_raw)))+
  scale_size(trans="sqrt", 
             range=c(4, 1), breaks=c(0.01,0.05,0.1,0.5,1))+
  scale_colour_distiller(palette = "Spectral",
                         values=c(0,.48,.5,.52,1)
                         #,limits=c(min(relimp_wInfo$rel_imp_perPCA),max(relimp_wInfo$rel_imp_perPCA)))+
                         ,limits=c(-1,1))+
  theme_bw()+
  theme(text = element_text(size=12))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        legend.position="bottom",
        legend.box = "vertical",
        #legend.spacing.y=0.5,
        axis.text.x=element_blank(),
        #axis.title.y =element_blank(),
        plot.title = element_text(margin = margin(t = 2, b = -14)))+
  labs(size="p-value",
       x="Longitude",
       y="",
       colour="Slope (m/year)")#+
  #guides(size=FALSE)

b<-ggplot()+
  #geom_path(data=world,aes(x=long,y=lat,group=group),colour="gray55")+
  mapWorld+
  scale_y_continuous(limits=c(-60,86),
                     expand=c(0,0))+
  scale_x_continuous(#limits=c(-180,180),
    expand=c(0,0))+
  geom_point(data=TPJO_trends_winfo,
             aes(x=lon,
                 y=lat,
                 colour=rel.inf_sum,
                 size=test_correlation.mean
                 ))+
  scale_radius(trans="log", 
             range=c(1,4),
             breaks=c(0.6,0.7,0.8,0.9,1))+
  scale_colour_distiller(palette="Greens",
                         #values=c(0,.45,.5,.55,1)
                         #,limits=c(min(relimp_wInfo$rel_imp_perPCA),max(relimp_wInfo$rel_imp_perPCA)))+
                         limits=c(0,100),
                         direction=1)+
  theme_bw()+
  theme(text = element_text(size=12))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        #axis.title.x=element_blank(),
        legend.position="bottom",
        legend.box = "vertical",
        #axis.text.x=element_blank(),
        #axis.title.y =element_blank(),
        plot.title = element_text(margin = margin(t = 2, b = -14)))+
  labs(size="Cross validation correlation",
       x="Longitude",
       y="Latitude",
       colour="Proportion explained by PCs")#+
  #guides(size=FALSE)

#c<-ggplot()+
#  #geom_path(data=world,aes(x=long,y=lat,group=group),colour="gray55")+
#  mapWorld+
#  scale_y_continuous(limits=c(-60,86),
#                     expand=c(0,0))+
#  scale_x_continuous(#limits=c(-180,180),
#    expand=c(0,0))+
#  geom_point(data=TPJO_trends_winfo,
#             aes(x=lon,
#                 y=lat,
#                 colour=slope_level_anom_anVAR,
#                 size=(p_slope_level_anom_anVAR)))+
#  scale_size(trans="sqrt", 
#             range=c(4, 1), breaks=c(0.01,0.05,0.1,0.5,1))+
#  scale_colour_distiller(palette = "Spectral",
#                         values=c(0,.45,.5,.55,1)
#                         #,limits=c(min(relimp_wInfo$rel_imp_perPCA),max(relimp_wInfo$rel_imp_perPCA)))+
#                         ,limits=c(-.1,0.1)
#  )+
#  theme_bw()+
#  theme(text = element_text(size=12))+
#  theme(panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(), 
#        axis.line = element_line(colour = "black"),
#        axis.title.x=element_blank(),
#        legend.position="bottom",
#        legend.box = "vertical",
#        axis.text.x=element_blank(),
#        #axis.title.y =element_blank(),
#        plot.title = element_text(margin = margin(t = 2, b = -14)))+
#  labs(size="p-value",
#       x="Longitude",
#       y="",
#       colour="Variance Slope (m/year)
#       ")#+
#  #guides(size=FALSE)

plot_grid(a,b,ncol=1, 
          align="v",
          axis=c("lr","lr"),
          labels="AUTO")  

#FIGURE F: lake tanganyika apparent trend####

#Plot lake tanganyika time series with subset showing strong false trend
TPJO<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_dif_anom_v1.csv",stringsAsFactors = TRUE)
TPJOtang<-TPJO[which(TPJO$lakeID=="lake0315"),]
summary(TPJOtang)
head(TPJOtang)

#plot prep
mainplot <- ggplot(data=TPJOtang[which(TPJOtang$date>2005),], aes(x=date,y=level))+
  geom_line(colour = I("black"),size = 0.8)+ 
  geom_point() +
  geom_smooth(method="lm",se=FALSE)+
  labs(x = "Year",y = "Lake Level (m)",size=2)+
  scale_x_continuous(breaks=c(2005,2010,2015),limits=c(2005,2019))+
  scale_y_continuous(breaks=c(-.5,0,.5,1))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size=16))
subplot <- ggplot(data=TPJOtang, aes(x=date,y=level))+
  geom_rect(fill="lightblue",
            alpha=0.5, 
            mapping=aes_string(x="date", y="level"), 
            xmin=2005,
            xmax=2019,
            ymin=min(TPJOtang$level),
            ymax=max(TPJOtang$level))+
  geom_line(colour = I("black"),size = 0.8)+ 
  geom_smooth(method="lm",se = FALSE, data=subset(TPJOtang,date>2005))+
  scale_x_continuous(breaks=c(1995,2000,2005,2010,2015))+
  scale_y_continuous(breaks=c(-.5,0,.5,1))+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank())+
  annotate("text", 
           label = "Full data: 
1992-2018", 
           x = 2008, 
           y = 1.4, 
           size = 5, 
           colour = "black")

vp <- viewport(width = 0.36, 
               height = 0.36, 
               x = 0.78,
               y = 0.3)
               #y = 0.5)
###print the plot
print(mainplot)
print(subplot, vp = vp)

#FIGURE G: The two PCs (9 and 14) which were relatively importnat in the model but not strongly correlated to any of the NOAA indeces.####



#FIGURE L: comparison of model fit with lake characteristics####
TPJO_trends_winfo<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_trends_winfo_v1.csv",header=TRUE)
names(TPJO_trends_winfo)

ggplot()+
  geom_point(data=TPJO_trends_winfo,aes(y=correlation,x=abs(lat)))+
  #geom_smooth(data=TPJO_trends_winfo,aes(y=correlation,x=abs(lat)),span=10,color="black")+
  geom_abline(slope=(senP(y=TPJO_trends_winfo$correlation,x=abs(TPJO_trends_winfo$lat),reps=100)$slope),
              intercept=(senP(y=TPJO_trends_winfo$correlation,x=abs(TPJO_trends_winfo$lat),reps=100)$intercept))+
  theme_bw()+
  labs(x="Latitude (°N/S)",y="Model fit correlation")

#FIGURE M: comparison of trends before bc with lake characteristics####
TPJO_trends_winfo<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_trends_winfo_v1.csv",header=TRUE)

ggplot()+
  geom_point(data=TPJO_trends_winfo,
             aes(y=abs(slope_level_anom_anmean),
                 x=AREA_SKM))+
  #geom_smooth(data=TPJO_trends_winfo,
  #            aes(y=abs(slope_level_anom_anmean),
  #                x=AREA_SKM),
  #            span=10,
  #            color="black")+
  geom_abline(slope=(senP(y=log(abs(TPJO_trends_winfo$slope_level_anom_anmean)),
                          x=log(TPJO_trends_winfo$AREA_SKM),
                          reps=100)$slope),
              intercept=(senP(y=log(abs(TPJO_trends_winfo$slope_level_anom_anmean)),
                              x=log(TPJO_trends_winfo$AREA_SKM),
                              reps=100)$intercept))+
  scale_x_continuous(trans="log",breaks=c(100,1000,10000,100000))+
  scale_y_continuous(trans="log",breaks=c(0.001,0.01,0.1,1))+
  theme_bw()+
  labs(x="Surface area (km^2)",y="Absolute value of lake level trend (m/year)")


#FIGURE N: Lake levels plots for one of each of the lakes from categories above###
TPJO_bestBRT_output1<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_bestBRT_output1_v2.csv",header=TRUE)
names(TPJO_bestBRT_output1)
head(TPJO_bestBRT_output1)

TPJO_bestBRT_output<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_bestBRT_output_b_v1.csv",header=TRUE)
TPJO_bestBRT_output$decyear<-TPJO_bestBRT_output$year+(((TPJO_bestBRT_output$month-1)*(365.25/12)+(365.25/24))/365.25)
TPJO_bestBRT_output$date<-date_decimal(TPJO_bestBRT_output$decyear)
#TPJO_bestBRT_output$lakeID<-TPJO_bestBRT_output$lake


lake<-"lake0068"
a<-ggplot()+
  geom_point(data=TPJO_bestBRT_output[lakeID==lake],
             aes(decyear,residual),
             colour="red",
             shape=1#,
             #span=.1#,
             #se=FALSE
  )+
  geom_abline(slope=(senP(y=TPJO_bestBRT_output1[lakeID==lake]$resid_anmean,
                          x=(TPJO_bestBRT_output1[lakeID==lake]$year),reps=100)$slope),
              intercept=(senP(y=TPJO_bestBRT_output1[lakeID==lake]$resid_anmean,
                              x=(TPJO_bestBRT_output1[lakeID==lake]$year),reps=100)$intercept),
              color="red")+
  geom_point(data=TPJO_bestBRT_output[lakeID==lake],
             aes(decyear,level_anom),colour="black",shape=1)+
  geom_abline(slope=(senP(y=TPJO_bestBRT_output1[lakeID==lake]$level_anom_anmean,
                          x=(TPJO_bestBRT_output1[lakeID==lake]$year),reps=100)$slope),
              intercept=(senP(y=TPJO_bestBRT_output1[lakeID==lake]$level_anom_anmean,
                              x=(TPJO_bestBRT_output1[lakeID==lake]$year),reps=100)$intercept),
              color="black")+
  #facet_wrap(~lake,scales="free_y",ncol=3)+
  theme_bw()+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size=16))+
  labs(x="Year",y="Water level anomaly (m)",title="Lake Chad")+
  annotate("text", 
           label = "Model residuals (slope=-0.56 cm/year)", 
           x = 2005, 
           y = 0.8, 
           size = 5, 
           colour = "red")+
  annotate("text", 
           label = "Water level (slope=-0.027 cm/year)", 
           x = 2005, 
           y = 0.7, 
           size = 5, 
           colour = "black")

lake<-"lake0093"
b<-ggplot()+
  geom_point(data=TPJO_bestBRT_output[lakeID==lake],
             aes(decyear,residual),
             colour="red",
             shape=1#,
             #span=.1#,
             #se=FALSE
  )+
  geom_abline(slope=(senP(y=TPJO_bestBRT_output1[lakeID==lake]$resid_anmean,
                          x=(TPJO_bestBRT_output1[lakeID==lake]$year),reps=100)$slope),
              intercept=(senP(y=TPJO_bestBRT_output1[lakeID==lake]$resid_anmean,
                              x=(TPJO_bestBRT_output1[lakeID==lake]$year),reps=100)$intercept),
              color="red")+
  geom_point(data=TPJO_bestBRT_output[lakeID==lake],
             aes(decyear,level_anom),colour="black",shape=1)+
  geom_abline(slope=(senP(y=TPJO_bestBRT_output1[lakeID==lake]$level_anom_anmean,
                          x=(TPJO_bestBRT_output1[lakeID==lake]$year),reps=100)$slope),
              intercept=(senP(y=TPJO_bestBRT_output1[lakeID==lake]$level_anom_anmean,
                              x=(TPJO_bestBRT_output1[lakeID==lake]$year),reps=100)$intercept),
              color="black")+
  #facet_wrap(~lake,scales="free_y",ncol=3)+
  theme_bw()+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size=16))+
  labs(x="Year",y="Water level anomaly (m)",title="Lake Turkana")+
  annotate("text", 
           label = "Model residuals (slope=2.1 cm/year)", 
           x = 2008, 
           y = -2, 
           size = 5, 
           colour = "red")+
  annotate("text", 
           label = "Water level (slope=9.2 cm/year)", 
           x = 2008, 
           y = -2.5, 
           size = 5, 
           colour = "black")
  

plot_grid(a,b,ncol=1, 
          align="v",
          axis=c("lr","lr"),
          labels="AUTO") 

#FIGURE O: Lake levels plots for lakes that had improvements to significance of trend and worsening significance
TPJO_bestBRT_output1<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_bestBRT_output1_v2.csv",header=TRUE)
names(TPJO_bestBRT_output1)
head(TPJO_bestBRT_output1)

TPJO_bestBRT_output<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_bestBRT_output_b_v1.csv",header=TRUE)
TPJO_bestBRT_output$decyear<-TPJO_bestBRT_output$year+(((TPJO_bestBRT_output$month-1)*(365.25/12)+(365.25/24))/365.25)
TPJO_bestBRT_output$date<-date_decimal(TPJO_bestBRT_output$decyear)
#TPJO_bestBRT_output$lakeID<-TPJO_bestBRT_output$lake


lake<-"lake0223"
a<-ggplot()+
  geom_point(data=TPJO_bestBRT_output[lakeID==lake],
             aes(decyear,residual),
             colour="red",
             shape=1#,
             #span=.1#,
             #se=FALSE
  )+
  geom_abline(slope=(senP(y=TPJO_bestBRT_output1[lakeID==lake]$resid_anmean,
                          x=(TPJO_bestBRT_output1[lakeID==lake]$year),reps=100)$slope),
              intercept=(senP(y=TPJO_bestBRT_output1[lakeID==lake]$resid_anmean,
                              x=(TPJO_bestBRT_output1[lakeID==lake]$year),reps=100)$intercept),
              color="red")+
  geom_point(data=TPJO_bestBRT_output[lakeID==lake],
             aes(decyear,level_anom),colour="black",shape=1)+
  geom_abline(slope=(senP(y=TPJO_bestBRT_output1[lakeID==lake]$level_anom_anmean,
                          x=(TPJO_bestBRT_output1[lakeID==lake]$year),reps=100)$slope),
              intercept=(senP(y=TPJO_bestBRT_output1[lakeID==lake]$level_anom_anmean,
                              x=(TPJO_bestBRT_output1[lakeID==lake]$year),reps=100)$intercept),
              color="black")+
  #facet_wrap(~lake,scales="free_y",ncol=3)+
  theme_bw()+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size=16))+
  labs(x="Year",y="Water level anomaly (m)",title="Rybinkskoye")+
  annotate("text", 
           label = "Model residuals (p=0.01)", 
           x = 1999, 
           y = 2, 
           size = 5, 
           colour = "red")+
  annotate("text", 
           label = "Water level (p=0.10)", 
           x = 1999, 
           y = 1.7, 
           size = 5, 
           colour = "black")

lake<-"lake0434"
b<-ggplot()+
  geom_point(data=TPJO_bestBRT_output[lakeID==lake],
             aes(decyear,residual),
             colour="red",
             shape=1#,
             #span=.1#,
             #se=FALSE
  )+
  geom_abline(slope=(senP(y=TPJO_bestBRT_output1[lakeID==lake]$resid_anmean,
                          x=(TPJO_bestBRT_output1[lakeID==lake]$year),reps=100)$slope),
              intercept=(senP(y=TPJO_bestBRT_output1[lakeID==lake]$resid_anmean,
                              x=(TPJO_bestBRT_output1[lakeID==lake]$year),reps=100)$intercept),
              color="red")+
  geom_point(data=TPJO_bestBRT_output[lakeID==lake],
             aes(decyear,level_anom),colour="black",shape=1)+
  geom_abline(slope=(senP(y=TPJO_bestBRT_output1[lakeID==lake]$level_anom_anmean,
                          x=(TPJO_bestBRT_output1[lakeID==lake]$year),reps=100)$slope),
              intercept=(senP(y=TPJO_bestBRT_output1[lakeID==lake]$level_anom_anmean,
                              x=(TPJO_bestBRT_output1[lakeID==lake]$year),reps=100)$intercept),
              color="black")+
  #facet_wrap(~lake,scales="free_y",ncol=3)+
  theme_bw()+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size=16))+
  labs(x="Year",y="Water level anomaly (m)",title="Vilyuyskoye")+
  annotate("text", 
           label = "Model residuals (p=0.18)", 
           x = 1999, 
           y = -2.6, 
           size = 5, 
           colour = "red")+
  annotate("text", 
           label = "Water level (p=0.06)", 
           x = 1999, 
           y = -3.1, 
           size = 5, 
           colour = "black")


plot_grid(a,b,ncol=1, 
          align="v",
          axis=c("lr","lr"),
          labels="AUTO") 

####Exploratory FIGURE: Relationship betwene lake levels and various PCs of high importance####
partdep<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_bestBRT_partdep_v4.csv",stringsAsFactors = TRUE)
partdep_new<-partdep[,.(response.value.median=median(response.value),response.value.mad=mad(response.value)),lake]
partdep<-merge(partdep,partdep_new,all.x=TRUE)
partdep$response.value.z<-(partdep$response.value-partdep$response.value.median)/partdep$response.value.mad
summary(partdep)
head(partdep)

lakeinfo<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/DATA_lakeinfo_v1.csv")
partdep<-merge(partdep,lakeinfo,all.x=TRUE)
partdep<-as.data.table(partdep)
partdep$lakename<-as.factor(partdep$lakename)
partdep<-partdep[order(predictor.name,lakename)]
partdep$lake<-as.factor(partdep$lake)
summary(partdep)

#LAKENAMES<-unique(droplevels((partdep[predictor.name=="PC2"]$lake)))
#LAKENAMES<-order(LAKENAMES)
#lake.i<-"Vanern"

pdf("lakelevels_PC2_trunc.pdf",onefile=TRUE)
for(lake.i in LAKENAMES){
  print(ggplot()+ 
          geom_smooth(data=partdep[predictor.name=="PC2"&lake==lake.i],
                      aes(y=response.value,
                          x=PC2),
                      span=.3)+
          theme_bw()+
          scale_x_continuous(limits=c(-40,40))+
          labs(title=lake.i)
  )
} 
dev.off()


LAKENAMES<-unique(droplevels((partdep[predictor.name=="PC15"]$lakename)))
#LAKENAMES<-order(LAKENAMES)

pdf("lakelevels_PC15.pdf",onefile=TRUE)
for(lake.i in LAKENAMES){
  print(ggplot()+ 
          geom_smooth(data=partdep[predictor.name=="PC15"&lakename==lake.i],
                      aes(y=response.value,
                          x=PC15),
                      span=.3)+
          theme_bw()+
          #scale_x_continuous(limits=c(-40,40))+
          labs(title=lake.i)
  )
} 
dev.off()

mapWorld<- borders("world", colour="black", fill="black")

ggplot()+
  mapWorld+
  scale_y_continuous(limits=c(-60,86),
                     expand=c(0,0))+
  scale_x_continuous(#limits=c(-180,180),
    expand=c(0,0))+
  geom_point(data=partdep[predictor.name=="PC15"],
             aes(x=lon,
                 y=lat),colour="blue")+
  geom_text(data=partdep[predictor.name=="PC15"],
                  aes(label=lakename,
                      x=lon,
                      y=lat),
                  colour="red",
                  size=3)
  
  
  geom_text_repel(data=partdep[predictor.name=="PC2"],
            aes(label=lakename,
                x=lon,
                y=lat),
            colour="red",
            size=3,
            max.iter=100)

  ggplot()+ 
    geom_smooth(data=partdep[predictor.name=="PC2"&lat>35&lon<(-30)],
                aes(y=response.value.z,
                    x=predictor.value,
                    group=lakename),
                span=.3)+
    theme_bw()
  
####FIGURE P: Effects of PCs on lake levels in specific lakes
  partdep<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_bestBRT_partdep_v4.csv",stringsAsFactors = TRUE)
  partdep_new<-partdep[,.(response.value.median=median(response.value),response.value.mad=mad(response.value)),lake]
  partdep<-merge(partdep,partdep_new,all.x=TRUE)
  partdep$response.value.z<-(partdep$response.value-partdep$response.value.median)/partdep$response.value.mad
  summary(partdep)
  
  lakeinfo<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/DATA_lakeinfo_v1.csv")
  partdep<-merge(partdep,lakeinfo,all.x=TRUE)
  partdep<-as.data.table(partdep,stringsAsFactors=T)
  partdep$lakename<-as.factor(partdep$lakename)
  partdep<-partdep[order(predictor.name,lakename)]
  summary(partdep)
  
  a<-ggplot()+
  geom_smooth(data=partdep[is.na(groupingENSO)==FALSE&predictor.name=="PC2"],
              aes(y=response.value.z,
                  x=PC2,
                  group=lakename),
              span=.5,
              se=FALSE,
              colour="#d8b365")+
  facet_wrap(~groupingENSO,scales = "free_y",ncol=2)+
  labs(x=bquote(~PC[ENSO]),
       y="Lake level z-score")+
  theme_bw()

b<-ggplot()+
  geom_smooth(data=partdep[is.na(groupingNAO)==FALSE&predictor.name=="PC5"],
              aes(y=response.value.z,
                  x=PC5,
                  group=lakename),
              span=.5,
              se=FALSE,
              colour="#5ab4ac")+
  labs(x=bquote(~PC[NAO]),
       y="Lake level z-score")+
  facet_wrap(~groupingNAO,scales = "free_y",ncol=2)+
  theme_bw()

c<-ggplot()+
  geom_smooth(data=partdep[is.na(groupingNPO)==FALSE&predictor.name=="PC4"],
              aes(y=response.value.z,
                  x=PC4,
                  group=lakename),
              span=.5,
              se=FALSE,
              colour="firebrick4")+
  labs(x=bquote(~PC[NPO]),
       y="Lake level z-score")+
  facet_wrap(~groupingNPO,scales = "free_y",ncol=2)+
  theme_bw()

plot_grid(a,b,ncol=1#, 
          #align="v",
          #axis=c("lr","lr"),
          #labels=FALSE
          ) 

#FIGURE Q: Correaltions between PCs and temperature and telleconnections across space and time.####
#plot the contributions across space for a PC which is not correlated to a NOAA index

PCA_cor<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/PCA_cor_v4.csv", header=TRUE)
head(PCA_cor)
summary(PCA_cor)
PCA_cor<-cSplit(PCA_cor,"lonlat",sep="lat")
PCA_cor$lonlat_1<-gsub("lon","",PCA_cor$lonlat_1)
PCA_cor$lonlat_1<-as.numeric(PCA_cor$lonlat_1)
PCA_cor<-PCA_cor[,c(6,7,1,2,3)]
names(PCA_cor)<-c("lon","lat","PCcor","PCcor_p","var")

lakeinfo<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/DATA_lakeinfo_v1.csv")
summary(lakeinfo)
lakeinfo$lake<-as.factor(lakeinfo$lake)
lakeinfo$lakeID<-as.factor(lakeinfo$lakeID)


TPJO_bestBRT_relimp<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_bestBRT_modeldata_relimp_v3.csv",stringsAsFactors = TRUE)
summary(TPJO_bestBRT_relimp)
TPJO_bestBRT_relimp$var<-as.factor(TPJO_bestBRT_relimp$var)
TPJO_bestBRT_relimp$lake<-as.factor(TPJO_bestBRT_relimp$lake)
TPJO_bestBRT_relimp<-TPJO_bestBRT_relimp[,count:=.N,by=lake]
#names(TPJO_bestBRT_relimp)<-c("dim","rel.inf","lakeID","n.trees","lr","count")
TPJO_bestBRT_relimp<-droplevels(TPJO_bestBRT_relimp)

TPJO_trends<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_trends_v3.csv")
summary(TPJO_trends)

TPJO_bestBRT_relimp<-TPJO_bestBRT_relimp[,count:=.N,by=lake]
TPJO_bestBRT_relimp[,rel.inf_scaled:=rel.inf*count/100]

relimp<-expand.grid(lake=unique(TPJO_bestBRT_relimp$lake),var=unique(TPJO_bestBRT_relimp$var))
relimp_exp<-as.data.table(merge(relimp,TPJO_bestBRT_relimp,all.x=TRUE,by=c("lake","var")))
relimp_exp[is.na(rel.inf)=="TRUE"]$rel.inf<-0
relimp_exp[is.na(rel.inf_scaled)=="TRUE"]$rel.inf_scaled<-0
summary(relimp_exp)
relimp_exp<-as.data.table(relimp_exp)

relimp_mean<-relimp_exp[,.(rel.inf_mean=mean(rel.inf)),by=var]
relimp.scaled_mean<-relimp_exp[,.(rel.inf_scaled_mean=mean(rel.inf_scaled)),by=var]
relimp.scaled_mean<-relimp.scaled_mean[order(rel.inf_scaled_mean)]

relimp_exp_winfo<-merge(relimp_exp,lakeinfo,all.x=TRUE)
#relimp_exp_winfo$rel.inf_scaled<-relimp_exp_winfo$rel.inf*relimp_exp_winfo$count/100
#relimp_exp_winfo[is.na(rel.inf_scaled)=="TRUE"]$rel.inf_scaled<-0

summary(relimp_exp_winfo)
unique(relimp_exp_winfo$var)
#summary(PCA_cor[,c(1:10,300:322)])
#summary(PCA_cor_l)

# read shapefile
wmap <- readOGR(dsn="U:/B.Kraemer/GlobalLakeLevels_IGB", layer="ne_110m_land")
# convert to dataframe
wmap_df <- fortify(wmap)

mapWorld<- borders("world", colour="gray55", fill=NA)
world <- map_data("world")

PCA_cor_sub<-as.data.table(PCA_cor)[
  #var=="PC2"|
    #var=="PC3"|
    #var=="PC4"|
    var=="PC15"]#|
#var=="PC6"|
#var=="PC7"|
#var=="PC8"|
#var=="PC11"]

relimp_exp_winfo_sub<-as.data.table(relimp_exp_winfo)[
  #var=="PC2"|
    #var=="PC3"|
    #var=="PC4"|
    var=="PC15"]#|
#var=="PC6"|
#var=="PC7"|
#var=="PC8"|
#var=="PC11"]

relimp_exp_winfo_sub<-droplevels(relimp_exp_winfo_sub)
relimp_exp_winfo_sub$var<-as.factor(relimp_exp_winfo_sub$var)
levels(relimp_exp_winfo_sub$var)<-c("PC[15]")

PCA_cor_sub<-droplevels(PCA_cor_sub)
PCA_cor_sub$var<-as.factor(PCA_cor_sub$var)
levels(PCA_cor_sub$var)<-c("PC[15]")

summary(relimp_exp_winfo_sub)
relimp_exp_winfo_sub<-as.data.table(relimp_exp_winfo_sub)
summary(PCA_cor_sub)

a<-ggplot()+
  geom_raster(data=PCA_cor_sub,aes(x=lon,y=lat,fill=PCcor,alpha=(10^(3*(1-PCcor_p)))/1000),interpolate=TRUE)+
  scale_alpha_continuous(trans="exp")+
  geom_path(data=wmap_df,aes(x=long,y=lat,group=group),colour="gray55")+
  scale_fill_distiller(palette = "Spectral",
                       values=c(0,.4,.5,.6,1)
                       #,limits=c(min(relimp_wInfo$rel_imp_perPCA),max(relimp_wInfo$rel_imp_perPCA)))+
                       ,limits=c(-.91,0.91))+
  scale_y_continuous(limits=c(-66,86),
                     expand=c(0,0))+
  scale_x_continuous(limits=c(-180,180),
                     expand=c(0,0))+
  geom_point(data=relimp_exp_winfo_sub[rel.inf_scaled>0],
             aes(x=lon,
                 y=lat,
                 size=rel.inf_scaled,
                 alpha=.4))+
  scale_radius(#limits=c(0.8,2),
    range=c(2,6),
    #breaks=c(1,1.2,1.4,1.6,1.8,2),
    trans="sqrt")+
  geom_point(data=relimp_exp_winfo_sub[rel.inf_scaled==0],
             aes(x=lon,
                 y=lat),shape=1,size=1)+
  #facet_wrap(~var,ncol=1,labeller = label_parsed)+
  theme_bw()+
  theme(text = element_text(size=14))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position="bottom",
        legend.box = "vertical")+
  labs(size="Relative Influence",
       fill=expression(paste("Correlation between surface temperature and ",PC[15],sep="")),
       x="Longitude",
       y="Latitude",
       alpha=NULL,
       shape=NULL)+
  guides(alpha=FALSE)


PCA_brt<-fread("file:///U:/B.Kraemer/GlobalLakeLevels_IGB/PCA_scores_detrend_v1.csv",stringsAsFactors = TRUE)
head(PCA_brt[,c(1:10,300:323)])
PCA_brt_l<-PCA_brt%>%
  gather(PC,value,2:327)
PCA_brt_l_sub<-droplevels(as.data.table(PCA_brt_l)[
  #PC=="PC2"|
    #PC=="PC3"|
    #PC=="PC4"|
    PC=="PC15"#|
  #PC=="PC6"|
  #PC=="PC7"|
  #PC=="PC8"|
  #PC=="PC11"
  ])
PCA_brt_l_sub$date<-date_decimal(PCA_brt_l_sub$decyear)
PCA_brt_l_sub$month<-as.numeric(month(PCA_brt_l_sub$date))
PCA_brt_l_sub<-PCA_brt_l_sub[order(PCA_brt_l_sub$date),]

PCA_brt_l_sub<-droplevels(PCA_brt_l_sub)
PCA_brt_l_sub$PC<-as.factor(PCA_brt_l_sub$PC)
levels(PCA_brt_l_sub$PC)<-c("PC[15]")
#PCA_brt_l_sub[which(PCA_brt_l_sub$PC=="PC[NAO]"),]$value<-
#  (-PCA_brt_l_sub[which(PCA_brt_l_sub$PC=="PC[NAO]"),]$value)

#PCA_brt_l_sub<-PCA_brt_l_sub%>%
#  group_by(PC)%>%
#  mutate(median=median(value,na.rm=TRUE),
#         mad=mad(value,na.rm=TRUE))
#PCA_brt_l_sub$value<-(PCA_brt_l_sub$value-PCA_brt_l_sub$median)/PCA_brt_l_sub$mad

b<-ggplot()+
  geom_point(data=PCA_brt_l_sub,aes(y=value,x=as.Date(date)),alpha=.3)+
  geom_smooth(data=PCA_brt_l_sub,aes(y=value,x=as.Date(date)),span=.1,se=FALSE,method="loess",colour="black")+
  #geom_point(data=index_l_sub,aes(y=value,x=as.Date(date),colour="Index"),alpha=.3)+
  #geom_smooth(data=index_l_sub,aes(y=value,x=as.Date(date),colour="Index"),span=.1,se=FALSE,method="loess")+
  scale_x_date(limits=as.Date(c("1992-08-01","2019-01-01")),expand=c(0,0))+
  #scale_y_continuous()+
  #scale_color_manual()+
  theme_bw()+
  theme(text = element_text(size=14))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        #      axis.title.x=element_blank(),
        #      axis.text.x=element_blank(),
        legend.title=element_blank())+
  labs(x="Year",y=bquote(~PC[15]))#+
  #guides()+
  #facet_wrap(~PC,ncol=1,labeller=label_parsed,scales="free_y")


partdep<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_bestBRT_partdep_v3.csv",stringsAsFactors = TRUE)
partdep_new<-partdep[,.(response.value.median=median(response.value),response.value.mad=mad(response.value)),lake]
partdep<-merge(partdep,partdep_new,all.x=TRUE)
partdep$response.value.z<-(partdep$response.value-partdep$response.value.median)/partdep$response.value.mad
summary(partdep)

lakeinfo<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/DATA_lakeinfo_v1.csv")
partdep<-merge(partdep,lakeinfo,all.x=TRUE)
partdep<-as.data.table(partdep)
partdep$lakename<-as.factor(partdep$lakename)
partdep<-partdep[order(predictor.name,lakename)]


c<-ggplot()+
  geom_smooth(data=partdep[is.na(groupingPC15)==FALSE&predictor.name=="PC15"],
              aes(y=response.value.z,
                  x=predictor.value/10,
                  group=lakename),
              span=.5,
              se=FALSE,
              colour="#339900")+
  facet_wrap(~groupingPC15,scales = "free_y",ncol=3)+
  labs(x=bquote(~PC[15]),
       y="Lake level z-score")+
  theme_bw()

print(plot_grid(a,b,c,ncol=1,rel_heights=c(3,1,1)))

####FIG R: Time series of a lake with masked trend and a lake with false trend  ####
TPJO_interp_bymonth<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_interp_bymonth_v3.csv",header=TRUE,stringsAsFactors = TRUE)
TPJO_interp_bymonth$level<-TPJO_interp_bymonth$level-
  TPJO_interp_bymonth[,levelmean:=mean(level),.(lakeID)]$levelmean
levels(TPJO_interp_bymonth$lakeID)
levels(TPJO_interp_bymonth$lakeID)[c(79,80)]<-c("Lake Mweru","Kainji Lake")

partdep<-fread("U:/B.Kraemer/GlobalLakeLevels_IGB/TPJO_bestBRT_partdep_v4.csv",stringsAsFactors = TRUE)
partdep_new<-partdep[,.(response.value.median=median(response.value),response.value.mad=mad(response.value)),lake]
partdep<-merge(partdep,partdep_new,all.x=TRUE)
partdep$response.value.z<-(partdep$response.value-partdep$response.value.median)/partdep$response.value.mad
partdep$lakeID<-partdep$lake
partdep[,response.value.centered:=response.value-mean(response.value),.(lakeID)]
summary(partdep)
levels(partdep$lakeID)
levels(partdep$lakeID)[c(79,80)]<-c("Lake Mweru","Kainji Lake")


ggplot()+
  #geom_point(data=TPJO_interp_bymonth[lakeID=="lake0423"|
  #                                      lakeID=="lake0416"],
  #           aes(y=level,x=decyear))+
  geom_line(data=TPJO_interp_bymonth[lakeID=="Lake Mweru"|
                                       lakeID=="Kainji Lake"],aes(y=level,x=decyear))+
  geom_line(data=partdep[lakeID=="Lake Mweru"|
                                       lakeID=="Kainji Lake"][predictor.name=="decyear"],
            aes(y=response.value.centered,x=predictor.value),colour="red")+
  facet_wrap(~lakeID,scales="free_y")+
  theme_bw()+
  labs(y="Water level (m)",
       x="Year")





