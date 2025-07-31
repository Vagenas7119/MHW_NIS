###Decoding the spread of non-indigenous fishes in the Mediterranean Sea

###For inquiries contact to @Vagenas G. 2024 (g.vagenas@hcmr.gr) | (georgvagenas@gmail.com)

#Required_Libraries

#install.packages("xlsx")
library(xlsx)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("doBy")
library(doBy)
#install.packages("powerjoin")
library(powerjoin)
#install.packages("dplyr")
library(dplyr)
#install.packages("reshape")
require(reshape)
#install.packages("ggjoy")
require(ggjoy)

#Set_working directory
setwd("C:/Users/geo_v/Documents/GitHub/MHW_NIS")

#Data_Input [A sample .csv file with 5 columns: "ID","Species","country_id","first_area_sighting","msfd_id"]
#Note: First Area Sighting is denoted as the date when the species was sighted
DATA<-read.csv("Analysis_Zenetos_Movement_2024.csv",header=TRUE,sep=";")

str(DATA)

DATA$country_id<-as.factor(DATA$country_id)

#Modification of Variables
DATA$Species<-as.factor(DATA$Species)
DATA$country_id<-as.factor(DATA$country_id)
DATA$msfd_id<-as.factor(DATA$msfd_id)
DATA$first_area_sighting<-as.numeric(DATA$first_area_sighting)

#removes NAs
DATA<-orderBy(~ID+first_area_sighting, data=DATA)%>%filter(!DATA$first_area_sighting=="NA")


#Calculate each Momentum Steps for all Species on a EEZ based layer

#t0 #First_Sight_Point
t_zero<-c(1,rep(0,length(DATA$ID)-1))
for(i in 2:length(DATA$ID)){
  if(DATA$ID[i]==DATA$ID[i-1])
  {t_zero[i]=0}
  else {t_zero[i]=1}
}
DATA<-data.frame(DATA,t_zero)

#t1
t_one<-c(0,rep(0,(length(DATA$ID)-1)))
for(i in 2:length(DATA$ID)){
  if((DATA$t_zero[i]<DATA$t_zero[i-1])&(DATA$ID[i]==DATA$ID[i-1]))
  {t_one[i]=1}
  else {t_one[i]=0}
}
DATA<-data.frame(DATA,t_one)

#t2
t_two<-c(0,rep(0,(length(DATA$ID)-1)))
for(i in 2:length(DATA$ID)){
  if((DATA$t_one[i]<DATA$t_one[i-1])&(DATA$ID[i]==DATA$ID[i-1]))
  {t_two[i]=1}
  else {t_two[i]=0}
}
DATA<-data.frame(DATA,t_two)

#t3
t_three<-c(0,rep(0,(length(DATA$ID)-1)))
for(i in 2:length(DATA$ID)){
  if((DATA$t_two[i]<DATA$t_two[i-1])&(DATA$ID[i]==DATA$ID[i-1]))
  {t_three[i]=1}
  else {t_three[i]=0}
}
DATA<-data.frame(DATA,t_three)

#t4
t_four<-c(0,rep(0,(length(DATA$ID)-1)))
for(i in 2:length(DATA$ID)){
  if((DATA$t_three[i]<DATA$t_three[i-1])&(DATA$ID[i]==DATA$ID[i-1]))
  {t_four[i]=1}
  else {t_four[i]=0}
}
DATA<-data.frame(DATA,t_four)

#t5
t_five<-c(0,rep(0,(length(DATA$ID)-1)))
for(i in 2:length(DATA$ID)){
  if((DATA$t_four[i]<DATA$t_four[i-1])&(DATA$ID[i]==DATA$ID[i-1]))
  {t_five[i]=1}
  else {t_five[i]=0}
}
DATA<-data.frame(DATA,t_five)

#t6
t_six<-c(0,rep(0,(length(DATA$ID)-1)))
for(i in 2:length(DATA$ID)){
  if((DATA$t_five[i]<DATA$t_five[i-1])&(DATA$ID[i]==DATA$ID[i-1]))
  {t_six[i]=1}
  else {t_six[i]=0}
}
DATA<-data.frame(DATA,t_six)

#t7
t_seven<-c(0,rep(0,(length(DATA$ID)-1)))
for(i in 2:length(DATA$ID)){
  if((DATA$t_six[i]<DATA$t_six[i-1])&(DATA$ID[i]==DATA$ID[i-1]))
  {t_seven[i]=1}
  else {t_seven[i]=0}
}
DATA<-data.frame(DATA,t_seven)

#t8
t_eight<-c(0,rep(0,(length(DATA$ID)-1)))
for(i in 2:length(DATA$ID)){
  if((DATA$t_seven[i]<DATA$t_seven[i-1])&(DATA$ID[i]==DATA$ID[i-1]))
  {t_eight[i]=1}
  else {t_eight[i]=0}
}
DATA<-data.frame(DATA,t_eight)

#t9
t_nine<-c(0,rep(0,(length(DATA$ID)-1)))
for(i in 2:length(DATA$ID)){
  if((DATA$t_eight[i]<DATA$t_eight[i-1])&(DATA$ID[i]==DATA$ID[i-1]))
  {t_nine[i]=1}
  else {t_nine[i]=0}
}
DATA<-data.frame(DATA,t_nine)

#t10
t_ten<-c(0,rep(0,(length(DATA$ID)-1)))
for(i in 2:length(DATA$ID)){
  if((DATA$t_nine[i]<DATA$t_nine[i-1])&(DATA$ID[i]==DATA$ID[i-1]))
  {t_ten[i]=1}
  else {t_ten[i]=0}
}
DATA<-data.frame(DATA,t_ten)

#Herein, if needed - apply the source of Supplementary Material from last Section#

###POST_ANALYSIS####

#Movement_Analysis#t0

#Create Countries and step momentum

DATA_momentum<-dplyr::select(DATA,!"ID" & !"Species" & !"first_area_sighting")

DATA_momentum_country<-data.frame(DATA_momentum %>% 
                                    group_by(country_id) %>% 
                                    summarise(
                                      t_zero= sum(t_zero),t_one=sum(t_one),t_two=sum(t_two),t_three=sum(t_three),t_four=sum(t_four),t_five=sum(t_five),t_six=sum(t_six),t_seven=sum(t_seven),t_eight=sum(t_eight),t_nine=sum(t_nine),t_ten=sum(t_ten)))


DATA_momentum_msfd<-data.frame(DATA_momentum %>% 
                                 group_by(msfd_id) %>% 
                                 summarise(
                                   t_zero= sum(t_zero),t_one=sum(t_one),t_two=sum(t_two),t_three=sum(t_three),t_four=sum(t_four),t_five=sum(t_five),t_six=sum(t_six),t_seven=sum(t_seven),t_eight=sum(t_eight),t_nine=sum(t_nine),t_ten=sum(t_ten)))

#HEATMAP_Countries

#Remove countries with zero records to improve the visualized output

for (i in 1:nrow(DATA_momentum_country)) {
  if(sum(DATA_momentum_country[i,-1])==0){
    DATA_momentum_country<-DATA_momentum_country[-i,]
  }else{DATA_momentum_country[i,]<-DATA_momentum_country[i,]}
}

DATA_momentum_country_fixed<-DATA_momentum_country

samp2 <- DATA_momentum_country_fixed[,-1]
rownames(samp2) <- DATA_momentum_country_fixed[,1]
DATA_momentum_country_m<-as.matrix(samp2)
#either with melt
heatmap_melt_c<-melt(DATA_momentum_country_m)

#reorder from east to west (counter-clockwise)
heatmap_melt_c$X1<-factor(heatmap_melt_c$X1,levels=c("EG","PS","IL","LB","SY","TR","CY","GR","AL","ME","HR","LY","IT","MT","TN","DZ","ES"))

#save as .tiff
tiff("deco_c.tiff", units="in", width=11, height=6, res=300)

#NEW HEATMAP
ggp_c <- ggplot(heatmap_melt_c, aes(X1, X2,fill=value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 20))+
  scale_fill_gradient2(low="white",mid="snow1",high="black")+
  guides(fill = guide_colourbar(title = "NIS"))+
  geom_text(aes(label = value), color = "white", size = 6)+ 
  scale_y_discrete(labels=c(expression(t[0]),
                            expression(t[1]),
                            expression(t[2]),
                            expression(t[3]),
                            expression(t[4]),
                            expression(t[5]),
                            expression(t[6]),
                            expression(t[7]),
                            expression(t[8]),
                            expression(t[9]),
                            expression(t[10])))+ labs(x="Countries",y="Introductions (Momentum)")
ggp_c<-ggp_c+theme_light()+theme(axis.text = element_text(size=12,face="bold"),axis.title.y = element_text(size=15,face="bold"),axis.title.x = element_text(size=15,face="bold"))
ggp_c

dev.off()

#save as .tiff height 600 width 900

#HEATMAP_MSFD

DATA_momentum_msfd_fixed<-DATA_momentum_msfd
samp2 <- DATA_momentum_msfd_fixed[,-1]
rownames(samp2) <- DATA_momentum_msfd_fixed[,1]
DATA_momentum_msfd_m<-as.matrix(samp2)
#heatmap(DATA_momentum_msfd_m, scale = "row",Rowv = NA, Colv = NA)

#melt
heatmap_melt_msfd<-melt(DATA_momentum_msfd_m)
#reorder from east o west
heatmap_melt_msfd$X1<-factor(heatmap_melt_msfd$X1,levels=c("EMED","CMED","ADRIA","WMED"))

#save as .tiff
tiff("deco_msfd.tiff", units="in", width=11, height=6, res=300)

#NEW HEATMAP
ggp_msfd <- ggplot(heatmap_melt_msfd, aes(X1, X2,fill=value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 20))+
  scale_fill_gradient2(low="white",mid="snow1",high="black")+
  guides(fill = guide_colourbar(title = "NIS"))+
  geom_text(aes(label = value), color = "white", size = 6)+ 
  scale_y_discrete(labels=c(expression(t[0]),
                            expression(t[1]),
                            expression(t[2]),
                            expression(t[3]),
                            expression(t[4]),
                            expression(t[5]),
                            expression(t[6]),
                            expression(t[7]),
                            expression(t[8]),
                            expression(t[9]),
                            expression(t[10])))+ labs(x="MSFD",y="Introductions (Momentum)")
ggp_msfd<-ggp_msfd+theme_light()+theme(axis.text = element_text(size=10,face="bold"),axis.title.y = element_text(size=15,face="bold"),axis.title.x = element_text(size=10,face="bold"))
ggp_msfd

dev.off()

##Temporal analysis

#Recall Dataset
DATA<-read.csv("Analysis_Zenetos_Movement.csv",header=TRUE,sep=";")
#Modification of Variables
DATA$Species<-as.factor(DATA$Species)
DATA$country_id<-as.factor(DATA$country_id)
DATA$msfd_id<-as.factor(DATA$msfd_id)
DATA$first_area_sighting<-as.numeric(DATA$first_area_sighting)

#removes NAs
DATA<-orderBy(~ID+first_area_sighting, data=DATA)%>%filter(!DATA$first_area_sighting=="NA")
#if( (a[x,y]>1.0)&(a[x,y]<2.0)

#Calculate each Momentum Steps for all Species in Time [Temporal]

t_zero_year<-rep(0,length(t_zero))
for(i in 1:length(t_zero)){
  if(t_zero[i]==1)
  {t_zero_year[i]=DATA$first_area_sighting[i]}
  else {t_zero_year[i]==0}
}
DATA_year<-data.frame(DATA,t_zero_year)

#t1
t_one_year<-rep(0,length(t_one))
for(i in 1:length(t_one)){
  if(t_one[i]==1)
  {t_one_year[i]=DATA$first_area_sighting[i]}
  else {t_one_year[i]==0}
}
DATA_year<-data.frame(DATA_year,t_one_year)

#t2
t_two_year<-rep(0,length(t_two))
for(i in 1:length(t_two)){
  if(t_two[i]==1)
  {t_two_year[i]=DATA$first_area_sighting[i]}
  else {t_two_year[i]==0}
}
DATA_year<-data.frame(DATA_year,t_two_year)

#t3
t_three_year<-rep(0,length(t_three))
for(i in 1:length(t_three)){
  if(t_three[i]==1)
  {t_three_year[i]=DATA$first_area_sighting[i]}
  else {t_three_year[i]==0}
}
DATA_year<-data.frame(DATA_year,t_three_year)

#t4
t_four_year<-rep(0,length(t_four))
for(i in 1:length(t_four)){
  if(t_four[i]==1)
  {t_four_year[i]=DATA$first_area_sighting[i]}
  else {t_four_year[i]==0}
}
DATA_year<-data.frame(DATA_year,t_four_year)

#t5
t_five_year<-rep(0,length(t_five))
for(i in 1:length(t_five)){
  if(t_five[i]==1)
  {t_five_year[i]=DATA$first_area_sighting[i]}
  else {t_five_year[i]==0}
}
DATA_year<-data.frame(DATA_year,t_five_year)

#t6
t_six_year<-rep(0,length(t_six))
for(i in 1:length(t_six)){
  if(t_six[i]==1)
  {t_six_year[i]=DATA$first_area_sighting[i]}
  else {t_six_year[i]==0}
}
DATA_year<-data.frame(DATA_year,t_six_year)

#t7
t_seven_year<-rep(0,length(t_seven))
for(i in 1:length(t_seven)){
  if(t_seven[i]==1)
  {t_seven_year[i]=DATA$first_area_sighting[i]}
  else {t_seven_year[i]==0}
}
DATA_year<-data.frame(DATA_year,t_seven_year)

#t8
t_eight_year<-rep(0,length(t_eight))
for(i in 1:length(t_eight)){
  if(t_eight[i]==1)
  {t_eight_year[i]=DATA$first_area_sighting[i]}
  else {t_eight_year[i]==0}
}
DATA_year<-data.frame(DATA_year,t_eight_year)

#t9
t_nine_year<-rep(0,length(t_nine))
for(i in 1:length(t_nine)){
  if(t_nine[i]==1)
  {t_nine_year[i]=DATA$first_area_sighting[i]}
  else {t_nine_year[i]==0}
}
DATA_year<-data.frame(DATA_year,t_nine_year)

#t10
t_ten_year<-rep(0,length(t_ten))
for(i in 1:length(t_ten)){
  if(t_ten[i]==1)
  {t_ten_year[i]=DATA$first_area_sighting[i]}
  else {t_ten_year[i]==0}
}
DATA_year<-data.frame(DATA_year,t_ten_year)

#Select certain variables
DATA_momentum_year<-dplyr::select(DATA_year,!"Species" & !"first_area_sighting" & !"msfd_id")
DATA_momentum_year_delta<-DATA_momentum_year
DATA_momentum_year<-replace(DATA_momentum_year, DATA_momentum_year==0, NA)

#Calculate the time step between each momentum (Delta difference)

for(x in 1:(nrow(DATA_momentum_year-1))){
  for(y in 3:(ncol(DATA_momentum_year)-1)){
    if(DATA_momentum_year$ID[x+1]==DATA_momentum_year$ID[x]){
      DATA_momentum_year_delta[x,y]=DATA_momentum_year[x+1,y+1]-DATA_momentum_year[x,y]}
    else if (DATA_momentum_year$ID[x+1]>DATA_momentum_year$ID[x]){DATA_momentum_year_delta[x,y]=c(NA)}
  }}

#remove last column and last row since the difference -delta- is calculted
last_row<-length(DATA_momentum_year_delta$ID)
last_col<-ncol(DATA_momentum_year_delta)
DATA_momentum_year_delta<-DATA_momentum_year_delta[-last_row,-last_col]

summary(DATA_momentum_year_delta$t_zero_year,na.rm=TRUE)
summary(DATA_momentum_year_delta$t_one_year,na.rm=TRUE)
summary(DATA_momentum_year_delta$t_two_year,na.rm=TRUE)
summary(DATA_momentum_year_delta$t_three_year,na.rm=TRUE)
summary(DATA_momentum_year_delta$t_four_year,na.rm=TRUE)
summary(DATA_momentum_year_delta$t_five_year,na.rm=TRUE)
summary(DATA_momentum_year_delta$t_six_year,na.rm=TRUE)
summary(DATA_momentum_year_delta$t_seven_year,na.rm=TRUE)
summary(DATA_momentum_year_delta$t_eight_year,na.rm=TRUE)
summary(DATA_momentum_year_delta$t_nine_year,na.rm=TRUE)

#MELT DATASET
DATA_momentum_year_delta<-dplyr::select(DATA_momentum_year_delta,!"ID")
DATA_momentum_year_delta_melted<-melt(DATA_momentum_year_delta,na.rm = TRUE)


#set the subscript of Delta margins
step1<-expression(Δt^(t[1]-t[0]))
step2<-expression(Δt^(t[2]-t[1]))
step3<-expression(Δt^(t[3]-t[2]))
step4<-expression(Δt^(t[4]-t[3]))
step5<-expression(Δt^(t[5]-t[4]))
step6<-expression(Δt^(t[6]-t[5]))
step7<-expression(Δt^(t[7]-t[6]))
step8<-expression(Δt^(t[8]-t[7]))
step9<-expression(Δt^(t[9]-t[8]))
step10<-expression(Δt^(t[10]-t[9]))

#ecdf all
tiff("deco_ecdf_i.tiff", units="in", width=9, height=9, res=300)

#Cumulative density function of all cases #Tail probs #https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
CumDenDelta<-ggplot(DATA_momentum_year_delta_melted, aes(x=value, y =variable, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,quantile_lines = TRUE, quantiles = 2,col="red",size=0.7) +
  scale_fill_viridis_c(name = "Probability", direction = -1,option="F")+scale_x_continuous(breaks=seq(0,50,5),limits=c(0,50))+theme_minimal()

my_labels <- c(step1,step2,step3,step4,step5,step6,step7,step8,step9,step10)
CumDenDelta<-CumDenDelta+scale_y_discrete(labels=my_labels)+ labs(y="Delta (Δ) of succesive momenta",x="Years")
CumDenDelta<-CumDenDelta+theme(axis.title = element_text(size = 15),axis.text=element_text(size=12))
CumDenDelta #Cumulative distribution to all

dev.off()


#MELT
DATA_momentum_year_delta_melted<-melt(DATA_momentum_year_delta,na.rm = TRUE)
str(DATA_momentum_year_delta_melted)

#Select the countries whereas the proportion equal or higher of 50% of the invasive species is present
#recall the dataset retrieved from heatmap
#head(heatmap_melt_c)
cast_c<-cast(heatmap_melt_c,X1~X2)

#get the proportions
cast_prop<-cast_c
dom_c<-rep(0,10)
for(i in 2:ncol(cast_c)){
  cast_prop[,i]<-cast_prop[,i]/sum(cast_prop[,i])
  dom_c[i]<-which.max(cast_prop[,i])
}
#cast_prop

rownames(cast_prop)<-cast_prop[,1]

#get the cases with proportion larger than 0.5

dom_countries<-data.frame(matrix(NA,ncol=10,nrow=5))
#dom_countries<-rep(0,11)
for(i in 3:ncol(cast_prop)){
  cast_prop<-cast_prop[order(-cast_prop[,i]),]
  if(cast_prop[1,i]>0.5){
    dom_countries[1,i-2]=c(rownames(cast_prop[1,]))
  }else if(cast_prop[1,i]+cast_prop[2,i]>0.5){
    dom_countries[1:2,i-2]=c(rownames(cast_prop[1,],),rownames(cast_prop[2,],))
  }else if(cast_prop[1,i]+cast_prop[2,i]+cast_prop[3,i]>0.5){
    dom_countries[1:3,i-2]=c(rownames(cast_prop[1,],),rownames(cast_prop[2,],),rownames(cast_prop[3,]))
  }else{dom_countries[1:4,i-2]=c(rownames(cast_prop[1,],),rownames(cast_prop[2,],),rownames(cast_prop[3,]),rownames(cast_prop[4,]))
  }
}

#dom_countries

#arrange countries
dom_countries1<-c(as.vector(na.omit(dom_countries[,1])))
dom_countries2<-c(as.vector(na.omit(dom_countries[,2])))
dom_countries3<-c(as.vector(na.omit(dom_countries[,3])))
dom_countries4<-c(as.vector(na.omit(dom_countries[,4])))
dom_countries5<-c(as.vector(na.omit(dom_countries[,5])))
dom_countries6<-c(as.vector(na.omit(dom_countries[,6])))
dom_countries7<-c(as.vector(na.omit(dom_countries[,7])))
dom_countries8<-c(as.vector(na.omit(dom_countries[,8])))
dom_countries9<-c(as.vector(na.omit(dom_countries[,9])))
dom_countries10<-c(as.vector(na.omit(dom_countries[,10])))

#herein we bind the dominant countries as cases estimated above along with the respective time step
melt_t0_1<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries1 & variable=="t_zero_year")
melt_t1_2<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries2 & variable=="t_one_year")
melt_t2_3<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries3 & variable=="t_two_year")
melt_t3_4<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries4 & variable=="t_three_year")
melt_t4_5<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries5 & variable=="t_four_year")
melt_t5_6<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries6 & variable=="t_five_year")
melt_t6_7<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries7 & variable=="t_six_year")
melt_t7_8<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries8 & variable=="t_seven_year")
melt_t8_9<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries9 & variable=="t_eight_year")
melt_t9_10<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries10 & variable=="t_nine_year")

#bind the dataset

dominant_melt_delta<-rbind(melt_t0_1,melt_t1_2,melt_t2_3,melt_t3_4,melt_t4_5,melt_t5_6,melt_t6_7,melt_t7_8,melt_t8_9,melt_t9_10)

#run density function for dominant 


#ecdf dom
tiff("deco_ecdf_ii.tiff", units="in", width=9, height=9, res=300)

#Cumulative density function #Tail probs #https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html

CumDenDelta_dom<-ggplot(dominant_melt_delta, aes(x=value, y =variable, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,quantile_lines = TRUE, quantiles = 2,col="red",size=0.7) +
  scale_fill_viridis_c(name = "Probability", direction = -1,option="F")+scale_x_continuous(breaks=seq(0,50,5),limits=c(0,50))+theme_minimal()
my_labels <- c(step1,step2,step3,step4,step5,step6,step7,step8,step9,step10)
CumDenDelta_dom<-CumDenDelta_dom+scale_y_discrete(labels=my_labels)+ labs(y="Delta (Δ) of succesive momenta | Dominant hotspots (>50%)",x="Years")
CumDenDelta_dom<-CumDenDelta_dom+theme(axis.title = element_text(size = 15),axis.text=element_text(size=12))
CumDenDelta_dom

dev.off()

#ANALYZE THE MEDIAN PER TIME STEP

median_delta<-c(0,median(DATA_momentum_year_delta$t_zero_year,na.rm=TRUE),
                median(DATA_momentum_year_delta$t_one_year,na.rm=TRUE),
                median(DATA_momentum_year_delta$t_two_year,na.rm=TRUE),
                median(DATA_momentum_year_delta$t_three_year,na.rm=TRUE),
                median(DATA_momentum_year_delta$t_four_year,na.rm=TRUE),
                median(DATA_momentum_year_delta$t_five_year,na.rm=TRUE),
                median(DATA_momentum_year_delta$t_six_year,na.rm=TRUE),
                median(DATA_momentum_year_delta$t_seven_year,na.rm=TRUE),
                median(DATA_momentum_year_delta$t_eight_year,na.rm=TRUE),
                median(DATA_momentum_year_delta$t_nine_year,na.rm=TRUE))
step_delta<-c(seq(0,10,1))

Delta_median_process<-data.frame(median_delta,step_delta)
#Delta_median_process
Delta_median_process_cum<-Delta_median_process

for(i in 1:11){
  if(i==1 | i==2){
    Delta_median_process_cum[i,1]=Delta_median_process[i,1]
  }else(Delta_median_process_cum[i,1]=Delta_median_process_cum[i,1]+Delta_median_process_cum[i-1,1])
}

Delta_median_process_cum$step_delta<-as.factor(Delta_median_process_cum$step_delta)

#combination
tiff("deco_combination.tiff", units="in", width=11, height=9, res=300)

CumDenDelta_process<-ggplot(
  Delta_median_process_cum,
  aes(x=median_delta, y=step_delta,group = 1))+geom_point(size=3)+geom_smooth()+theme_light()+ labs(y="Delta (Δt) of momentums | Dominant hotspot (Cumulative; >50%)",x="Years")+theme(axis.title = element_text(size = 15),axis.text=element_text(size=12))
CumDenDelta_process

my_labels <- c(step0,step1,step2,step3,step4,step5,step6,step7,step8,step9,step10)
CumDenDelta_process<-CumDenDelta_process+scale_x_continuous(breaks=seq(0,30,2),limits=c(0,30))+scale_y_discrete(labels=my_labels)
CumDenDelta_process<-CumDenDelta_process
CumDenDelta_process

#Get the cumulative timeframe for all momenta
#CALCULATE THE DOM FOR ALL INVASIONS NOT JUST THE DOMINANT HOTSPOTS, BOTH CONVERGED

DATA_momentum_year_delta_melted<-melt(DATA_momentum_year_delta,na.rm = TRUE)

melt_t0_1<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries1 & variable=="t_zero_year")
melt_t1_2<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries2 & variable=="t_one_year")
melt_t2_3<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries3 & variable=="t_two_year")
melt_t3_4<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries4 & variable=="t_three_year")
melt_t4_5<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries5 & variable=="t_four_year")
melt_t5_6<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries6 & variable=="t_five_year")
melt_t6_7<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries7 & variable=="t_six_year")
melt_t7_8<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries8 & variable=="t_seven_year")
melt_t8_9<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries9 & variable=="t_eight_year")
melt_t9_10<-filter(DATA_momentum_year_delta_melted,country_id %in% dom_countries10 & variable=="t_nine_year")

#bind_em_all

melt_delta_dom<-rbind(melt_t0_1,melt_t1_2,melt_t2_3,melt_t3_4,melt_t4_5,melt_t5_6,melt_t6_7,melt_t7_8,melt_t8_9,melt_t9_10)
#melt_delta_dom


median_delta_dom<-c(0,median(melt_t0_1$value,variable=="t_one_year",na.rm=TRUE),
                    median(melt_t1_2$value,variable=="t_two_year",na.rm=TRUE),
                    median(melt_t2_3$value,variable=="t_three_year",na.rm=TRUE),
                    median(melt_t3_4$value,variable=="t_four_year",na.rm=TRUE),
                    median(melt_t4_5$value,variable=="t_five_year",na.rm=TRUE),
                    median(melt_t5_6$value,variable=="t_six_year",na.rm=TRUE),
                    median(melt_t6_7$value,variable=="t_seven_year",na.rm=TRUE),
                    median(melt_t7_8$value,variable=="t_eight_year",na.rm=TRUE),
                    median(melt_t8_9$value,variable=="t_nine_year",na.rm=TRUE),
                    median(melt_t9_10$value,variable=="t_ten_year",na.rm=TRUE))

#median_delta_dom

step_delta_dom<-c(seq(0,10,1))

Delta_median_process_dom<-data.frame(median_delta_dom,step_delta_dom)
#Delta_median_process_dom
Delta_median_process_cum_dom<-Delta_median_process_dom

for(i in 1:11){
  if(i==1 | i==2){
    Delta_median_process_cum_dom[i,1]=Delta_median_process_dom[i,1]
  }else(Delta_median_process_cum_dom[i,1]=Delta_median_process_cum_dom[i,1]+Delta_median_process_cum_dom[i-1,1])
}

#Delta_median_process_cum_dom
Delta_median_process_cum_dom$step_delta_dom<-as.factor(Delta_median_process_cum_dom$step_delta_dom)

#Delta_median_process_cum_dom

#Apply the exact same variable tag for rbind

colnames(Delta_median_process_cum_dom)[1] ="median_delta"
colnames(Delta_median_process_cum_dom)[2] ="step_delta"

#Converge the dataset

conv_deltas<-rbind(Delta_median_process_cum,Delta_median_process_cum_dom)
index_col<-c(rep("non_dom",11),rep("dom",11))

conv_deltas_ind<-data.frame(conv_deltas,index_col)

conv_deltas_ind$step_delta<-as.factor(conv_deltas_ind$step_delta)

conv_deltas_ind$index_col<-as.factor(conv_deltas_ind$index_col)

#str(conv_deltas_ind)

#+ geom_line(aes(group = Subject), colour = "blue")

#set the text forms
step0<-c("Entry Point")
step1<-expression(Δt^(t[1]-t[0]))
step2<-expression(Δt^(t[2]-t[1]))
step3<-expression(Δt^(t[3]-t[2]))
step4<-expression(Δt^(t[4]-t[3]))
step5<-expression(Δt^(t[5]-t[4]))
step6<-expression(Δt^(t[6]-t[5]))
step7<-expression(Δt^(t[7]-t[6]))
step8<-expression(Δt^(t[8]-t[7]))
step9<-expression(Δt^(t[9]-t[8]))
step10<-expression(Δt^(t[10]-t[9]))

CumDenDelta_process<-ggplot(
  conv_deltas_ind,
  aes(x=median_delta, y=step_delta,col=index_col))+geom_point(aes(group=index_col),size=3)+geom_line(aes(group=index_col),size=1)+theme_light()

CumDenDelta_process
CumDenDelta_process<-CumDenDelta_process+ labs(y="Cumulative median of Delta (Δ) of succesive momenta",x="Years")
CumDenDelta_process
CumDenDelta_process<-CumDenDelta_process+theme(axis.title = element_text(size = 15),axis.text=element_text(size=15),legend.title.align=0.5)
CumDenDelta_process

my_labels <- c(step0,step1,step2,step3,step4,step5,step6,step7,step8,step9,step10)
CumDenDelta_process<-CumDenDelta_process+scale_x_continuous(breaks=seq(0,30,2),limits=c(0,30))+scale_y_discrete(labels=my_labels)
CumDenDelta_process<-CumDenDelta_process+scale_colour_discrete(name="Scenario",labels=c('MED NIS (>50%)', 'MED NIS (100%)'))
CumDenDelta_process
#width 850

dev.off()

#Convergence
minus<-(Delta_median_process_cum$median_delta-Delta_median_process_cum_dom$median_delta)/Delta_median_process_cum$median_delta
minus

conv_deltas_ind

#Average values of scenario (i) & (ii)
non_dom<-as.vector(Delta_median_process[,1])
mean(non_dom)
dom<-as.vector(Delta_median_process_dom[,1])
mean(dom)

DATA_dio<-read.csv("C:/Users/geo_v/Desktop/rSDMs/MHW_NIS/Dataset_Zenetos_2025_Triple_CSVF.csv",header=TRUE,sep=",",dec=".")

str(DATA_dio)

# #As additional files
# #Supplementary material_1
# 
# #Cumulative invasion process
# DATAt0<-filter(DATA,DATA$t_zero==1)
# DATAt0s<-orderBy(~first_area_sighting, data=DATAt0)
# #cumulative data table
# Years<-seq(1882,2022,1)
# n<-rep(0,length(Years))
# Cum_Year<-data.frame(Years,n)
# Countd<-dplyr::count(DATAt0s,DATAt0s$first_area_sighting)
# str(Countd)
# names(Countd)[names(Countd) == "DATAt0s$first_area_sighting"] <- "Years"
# 
# Cumulative_dataset<-merge(Cum_Year,Countd,by="Years",all=T)
# 
# Cumulative_dataset[is.na(Cumulative_dataset)] = 0
# 
# sum<-rowSums(cbind(Cumulative_dataset[,2],Cumulative_dataset[,3]))
# 
# Cumulative_dataset<-data.frame(Cumulative_dataset[,1],sum)
# plot(Cumulative_dataset)
# 
# names(Cumulative_dataset)[names(Cumulative_dataset) == "Cumulative_dataset...1."] <- "Years"
# names(Cumulative_dataset)[names(Cumulative_dataset) == "sum"] <- "Obs"
# 
# CumFreq<-cumsum(Cumulative_dataset$Obs)
# 
# Cumulative_dataset<-data.frame(Cumulative_dataset,CumFreq)
# head(Cumulative_dataset)
# Cumulative_dataset<-Cumulative_dataset[,-2]
# #plot(Cumulative_dataset)
# 
#
# require(MASS)
# require(fitdistrplus)
# model<-glm(log(CumFreq)~log(Years),family=gaussian,data=Cumulative_dataset)
# summary(model)
# coef(model)
# model$call
# stepAIC(model)
# 
# #define new observation
# newdata = data.frame(Years=2022)
# 
# #use model to predict value of am
# predict(model, newdata, type="response")
# 
# #install.packages("rcompanion")
# library(rcompanion)
# nagelkerke(model)
# 
# #power model
# powercum<-lm(log(Cumulative_dataset[,2])~log(Cumulative_dataset[,1]))
# summary(powercum)
# expcum<-lm(log(Cumulative_dataset[,2])~Cumulative_dataset[,1])
# summary(expcum)
# 
# #Predict
# 
# #powercum slightly betta
# coefficients(powercum)
# #loga=intercept--> a=e^(intercept) /  logTL=b
# a=exp(coefficients(powercum)[1])
# b=coefficients(powercum)[2]
# 
# funCumFreq<-function(x){a*x^b}
# 
# predict.lm(powercum,newdata)
# 
# Plot_one<- ggplot(Cumulative_dataset, aes(y=CumFreq,x=Years))
# Plot_one<- Plot_one + geom_point(alpha=0.5,size=2)+ labs(x="Year",y="Introductions (Cumulative frequency)")
# Plot_one
# Plot_one<- Plot_one + stat_function(fun=funCumFreq,xlim=c(1880,2020),col="black",alpha=5,size=0.2)
# Plot_one
# Plot_one<- Plot_one + scale_x_continuous(breaks=seq(1880,2022,20),limits=c(1880,2022))+scale_y_continuous(breaks=seq(0,150,10),limits=c(0,150))
# Plot_one<- Plot_one + theme_light()+theme(axis.text = element_text(size=12,face="bold"),axis.title.y = element_text(size=15,face="bold"),axis.title.x = element_text(size=15,face="bold"))
# Plot_one
# 
# 
# #Supplementary_material_2
# 
# #COUNTRY
# 
# str(DATA)
# DATAt0<-filter(DATA,DATA$t_zero==1)
# str(DATAt0s)
# Count_country<-dplyr::count(DATAt0s,DATAt0s$country_id)
# head(Count_country)
# #Count_country<-orderBy(~n,data=Count_country)
# Plot_two<-ggplot(Count_country,aes(x=Count_country[,1],y=Count_country[,2]))+geom_bar(aes(x = reorder(Count_country[,1], -Count_country[,2]), y = Count_country[,2]), stat = "identity")
# Plot_two<-Plot_two+labs(y="Introductions",x="Country")
# Plot_two<-Plot_two+theme_light()+theme(axis.text = element_text(size=12,face="bold"),axis.title.y = element_text(size=15,face="bold"),axis.title.x = element_text(size=15,face="bold"))
# Plot_two<-Plot_two+scale_y_continuous(breaks=seq(0,70,10),limits=c(0,70))
# Plot_two
# 
# #Supplementary_material_3
# 
# #MSFD_ID
# 
# str(DATA)
# DATAt0<-filter(DATA,DATA$t_zero==1)
# str(DATAt0s)
# Count_msfd<-dplyr::count(DATAt0s,DATAt0s$msfd_id)
# head(Count_msfd)
# #Count_country<-orderBy(~n,data=Count_country)
# Plot_three<-ggplot(Count_msfd,aes(x=Count_msfd[,1],y=Count_msfd[,2]))+geom_bar(aes(x = reorder(Count_msfd[,1], -Count_msfd[,2]), y = Count_msfd[,2]), stat = "identity")
# Plot_three<-Plot_three+labs(y="Introductions",x="MSFD region")
# Plot_three<-Plot_three+theme_light()+theme(axis.text = element_text(size=12,face="bold"),axis.title.y = element_text(size=15,face="bold"),axis.title.x = element_text(size=15,face="bold"))
# Plot_three<-Plot_three+scale_y_continuous(breaks=seq(0,140,20),limits=c(0,140))
# Plot_three

#
# #set the text forms
# step0<-c("Entry Point")
# step1<-expression(Δt^(t[1]))
# step2<-expression(Δt^(t[2]))
# step3<-expression(Δt^(t[3]))
# step4<-expression(Δt^(t[4]))
# step5<-expression(Δt^(t[5]))
# step6<-expression(Δt^(t[6]))
# step7<-expression(Δt^(t[7]))
# step8<-expression(Δt^(t[8]))
# step9<-expression(Δt^(t[9]))
# step10<-expression(Δt^(t[10]))












#NEW SET 2025 analysis

# Required Libraries
library(terra)        # For spatial operations
library(tidyverse)    # For data manipulation
library(doBy)         # For ordering and summaries
library(powerjoin)    # For advanced joins
library(ggplot2)      # For visualization
#install.packages("ggridges")
library(ggridges)     # For density plots

# Set Working Directory
setwd("C:/Users/geo_v/Desktop/rSDMs/MHW_NIS/")

str(DATA)

# Read and Preprocess Data
DATA <- read.csv("Dataset_Zenetos_2025_Triple_CSVF.csv", header = TRUE, sep = ",", dec = ".") %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    year = as.integer(year)
  ) %>%
  filter(!is.na(Latitude), !is.na(Longitude))  # Remove invalid coordinates

# Country Classification System
country_classification <- tribble(
  ~CounTurkeyy,            ~country_id, ~msfd_id,
  "Greece",             "GR",       "EMED",
  "Turkey",             "TR",       "EMED",
  "Syria",              "SY",       "EMED",
  "Cyprus",             "CY",       "EMED",
  "Lebanon",            "LB",       "EMED",
  "Israel",             "IL",       "EMED",
  "Palestine Authority","PS",       "EMED",
  "Egypt",              "EG",       "EMED",
  "Libya",              "LY",       "EMED",  # Primary assignment
  "Albania",            "AL",       "ADRIA",
  "Montenegro",         "ME",       "ADRIA",
  "Croatia",            "HR",       "ADRIA",
  "Tunisia",            "TN",       "CMED",
  "Italy",              "IT",       "CMED",  # Primary assignment
  "Malta",              "MT",       "CMED",
  "Spain",              "ES",       "WMED",
  "Algeria",            "DZ",       "WMED",
  "France",             "FR",       "WMED",
  "Morocco",            "MA",       "WMED"
)

# Add country_id and msfd_id
DATA <- DATA %>%
  left_join(country_classification, by = "CounTurkeyy") %>%
  filter(!is.na(country_id))  # Remove unclassified countries

# Coordinate Validation using Mediterranean Bounding Box
med_bbox <- c(xmin = -6, xmax = 36, ymin = 30, ymax = 46)  # Mediterranean approx
valid_coords <- DATA %>%
  filter(
    between(Longitude, med_bbox["xmin"], med_bbox["xmax"]),
    between(Latitude, med_bbox["ymin"], med_bbox["ymax"])
  )

# Spatial Object Creation
spat_data <- vect(
  valid_coords,
  geom = c("Longitude", "Latitude"),
  crs = "EPSG:4326"
)

# Optional: Visualize points
plot(spat_data, col = "red", pch = 16)

extent<-ext(spat_data)

plot(extent)

# Data Aggregation (First Sighting per Species-Country) with proper ordering
agg_data <- valid_coords %>%
  group_by(species, country_id,Latitude,Longitude) %>%
  summarise(
    first_area_sighting = min(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(first_area_sighting) %>%  # Arrange by year first
  mutate(ID = as.numeric(factor(species))) %>%  # Create species ID
  arrange(species, first_area_sighting)  # Then arrange by species and year

# Verify the results
head(agg_data,20)

#works

# Momentum Calculation Function
calculate_momentum <- function(data) {
  data <- data %>%
    arrange(ID, first_area_sighting,Latitude,Longitude) %>%
    group_by(ID) %>%
    mutate(
      t_step = row_number() - 1,  # t0, t1, t2...
      t_flag = ifelse(t_step < 11, 1, 0)  # Limit to t0-t50
    ) %>%
    filter(t_flag == 1) %>%
    ungroup()
  return(data)
}


# Apply Momentum Calculation
momentum_data <- calculate_momentum(agg_data)


# Define east-to-west ordering for countries
country_order <- c("EG", "PS", "IL", "LB", "SY", "TR", "CY", "GR", 
                   "AL", "ME", "HR", "LY", "IT", "MT", "TN", "DZ", "ES")

# Define east-to-west ordering for MSFD regions
msfd_order <- c("EMED","CMED","ADRIA","WMED")


generate_heatmap <- function(data, group_var) {
  group_sym <- sym(group_var)
  
  heat_data <- data %>%
    count(!!group_sym, t_step) %>%
    complete(!!group_sym, t_step = 0:10, fill = list(n = 0)) %>%
    filter(!is.na(!!group_sym))
  
  # Apply ordering based on group_var
  if (group_var == "country_id") {
    heat_data <- heat_data %>%
      mutate(!!group_sym := factor(!!group_sym, levels = country_order))
  } else if (group_var == "msfd_id") {
    heat_data <- heat_data %>%
      mutate(!!group_sym := factor(!!group_sym, levels = msfd_order))
  }
  
  ggplot(heat_data, aes(x = !!group_sym, y = t_step, fill = n)) +
    geom_tile(color = "white", lwd = 0.5) +
    scale_fill_viridis_c(option = "inferno") +
    scale_y_continuous(
      breaks = 0:10,
      labels = c("t₀", paste0("t", 1:10))
    ) +
    labs(
      x = ifelse(group_var == "country_id", "Countries", "MSFD Regions"),
      y = "Introduction Momentum",
      fill = "Count"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# First, join the MSFD information to your momentum data
momentum_data <- momentum_data %>%
  left_join(country_classification %>% select(country_id, msfd_id), 
            by = "country_id") %>%
  filter(!is.na(msfd_id))  # Remove any rows without MSFD classification

# Now generate the MSFD heatmap
msfd_heatmap <- generate_heatmap(momentum_data, "msfd_id") +
  geom_tile(
    color = "white",
    lwd = 1.5,
    linetype = 1
  ) +
  coord_fixed() +
  scale_fill_gradient2(
    low = "white",
    mid = "snow1",
    high = "black"
  ) +
  scale_y_continuous(
    breaks = 0:10,
    labels = c(expression(t[0]),
               expression(t[1]),
               expression(t[2]),
               expression(t[3]),
               expression(t[4]),
               expression(t[5]),
               expression(t[6]),
               expression(t[7]),
               expression(t[8]),
               expression(t[9]),
               expression(t[10]))
  ) +
  geom_text(aes(label = n), color = "white", size = 6) +
  guides(
    fill = guide_colourbar(
      title = "NIS",
      barwidth = 1,
      barheight = 20
    )
  ) +
  labs(
    x = "MSFD Regions",
    y = "Introductions (Momentum)"
  ) +
  theme_light() +
  theme(
    axis.text = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Display the plot
msfd_heatmap

#Country
country_heatmap <- generate_heatmap(momentum_data, "country_id") +
  geom_tile(
    color = "white",
    lwd = 1.5,
    linetype = 1
  ) +
  coord_fixed() +
  scale_fill_gradient2(
    low = "white",
    mid = "snow1",
    high = "black"
  ) +
  scale_y_continuous(
    breaks = 0:10,
    labels = c(expression(t[0]),
               expression(t[1]),
               expression(t[2]),
               expression(t[3]),
               expression(t[4]),
               expression(t[5]),
               expression(t[6]),
               expression(t[7]),
               expression(t[8]),
               expression(t[9]),
               expression(t[10]))
  ) +
  geom_text(aes(label = n), color = "white", size = 6) +
  guides(
    fill = guide_colourbar(
      title = "NIS",
      barwidth = 1,
      barheight = 20
    )
  ) +
  labs(
    x = "MSFD Regions",
    y = "Introductions (Momentum)"
  ) +
  theme_light() +
  theme(
    axis.text = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#display
country_heatmap

# Generate and save heatmaps with east-to-west ordering
ggsave("country_heatmap.jpg", 
       country_heatmap,
       width = 10, height = 6)

ggsave("msfd_heatmap.jpg", 
       msfd_heatmap,
       width = 8, height = 6)


# Temporal Analysis (Parallel Processing)
library(future.apply)
plan(multisession)  # Enable parallel processing


# Create the temporal_analysis data frame from your momentum_data_geo
temporal_analysis <- as.data.frame(momentum_data) %>%
  group_by(ID) %>%  # Group by species ID
  arrange(t_step) %>%  # Order by time step
  mutate(
    delta_t = first_area_sighting - lag(first_area_sighting)  # Calculate time differences
  ) %>%
  filter(!is.na(delta_t)) %>%  # Remove NA values from first time step
  ungroup()


# ECDF Plot
ecdf_plot <- ggplot(temporal_analysis, aes(x = delta_t, y = factor(t_step), fill = after_stat(ecdf))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "ECDF", option = "plasma") +
  scale_y_discrete(
    labels = c(expression(Delta*"t"["0-1"]),
               lapply(1:9, function(i) bquote(Delta*"t"[.(i)])))
  ) +
  labs(x = "Time Difference (Years)", y = "Introduction Step") +
  theme_ridges()+scale_x_continuous(limit=c(0,40))


#modified as in Vagenas 2024

# ECDF Plot
ecdf_plot <- ggplot(temporal_analysis, aes(x = delta_t, y = factor(t_step), fill = after_stat(ecdf))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "ECDF", option = "plasma") +
  scale_y_discrete(
    labels = c(expression(Delta*"t"["0-1"]),
               lapply(1:9, function(i) bquote(Delta*"t"[.(i)])))
  ) +
  labs(x = "Time Difference (Years)", y = "Introduction Step") +
  theme_ridges()+scale_x_continuous(limit=c(0,50))

ecdf_plot

# ECDF Plot with matching technical specs
ecdf_plot <- ggplot(temporal_analysis, aes(x = delta_t, y = factor(t_step), 
                                           fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantile_lines = TRUE,
    quantiles = 2,
    color = "red",
    linewidth = 0.7
  ) +
  scale_fill_viridis_c(
    name = "Probability",
    direction = -1,
    option = "F"
  ) +
  scale_x_continuous(
    breaks = seq(0, 50, 5),
    limits = c(0, 50)
  ) +
  scale_y_discrete(
    labels = c(expression(Delta*tau[1]-tau[0]),
               expression(Delta*tau[2]-tau[1]),
               expression(Delta*tau[3]-tau[2]),
               expression(Delta*tau[4]-tau[3]),
               expression(Delta*tau[5]-tau[4]),
               expression(Delta*tau[6]-tau[5]),
               expression(Delta*tau[7]-tau[6]),
               expression(Delta*tau[8]-tau[7]),
               expression(Delta*tau[9]-tau[8]),
               expression(Delta*tau[10]-tau[9]))
  ) +
  labs(
    x = "Years",
    y = "Delta (Δ) of successive momenta"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(vjust = 0.5) # Ensures proper vertical alignment
  )

# Display the plot
ecdf_plot

ggsave("ecdf_analysis.jpg", ecdf_plot, width = 9, height = 7)

# Cumulative Spread Analysis
cumulative_spread_median <- temporal_analysis %>%
  group_by(t_step) %>%
  summarise(median_delta = median(delta_t, na.rm = TRUE)) %>%
  mutate(cumulative_time = cumsum(coalesce(median_delta, 0))) 

ggplot(cumulative_spread_median, aes(x = t_step, y = cumulative_time)) +
  geom_line(color = "steelblue", size = 1.5) +
  geom_point(size = 3) +
  labs(title = "Cumulative Spread Timeline (Median)",
       x = "Introduction Step", 
       y = "Cumulative Years") +
  theme_bw()

cumulative_spread_mean <- temporal_analysis %>%
  group_by(t_step) %>%
  summarise(mean_delta = mean(delta_t, na.rm = TRUE)) %>%
  mutate(cumulative_time = cumsum(coalesce(mean_delta, 0))) 

ggplot(cumulative_spread_mean, aes(x = t_step, y = cumulative_time)) +
  geom_line(color = "steelblue", size = 1.5) +
  geom_point(size = 3) +
  labs(title = "Cumulative Spread Timeline (Average)",
       x = "Introduction Step", 
       y = "Cumulative Years") +
  theme_bw()


#ADD THE SPATIAL ASPECT


library(terra)
MED<-vect("C:/Users/geo_v/Desktop/rSDMs/MHW_NIS/MED/iho/iho.shp")
MED

# 1. First ensure all polygons are valid
MED_valid <- makeValid(MED)

# 2. Create a common ID for all features
MED_valid$merge_id <- 1

# 3. Aggregate with dissolve (this is the key step)
MED_merged <- aggregate(MED_valid, by = "merge_id", dissolve = TRUE)

# 4. Force merge with minimal buffer if lines remain
#    (using 0.00001 instead of 0 for better stability)
MED_final <- buffer(MED_merged, width = 0.00001)

# 5. Verify the result
plot(MED_final, col = "lightblue", main = "Fully Merged Mediterranean")

library(ncdf4)
library(terra)

# Open the NetCDF file #einai ena aplo raster giati mas dini meses times
nc_file_temp <- rast("Project/layers/thetao_baseline_2000_2019_depthsurf_4e3e_1426_a71d_U1751549382872.nc")  # Replace with your file path
plot(nc_file_temp,ext=extent)

# 4. Add points to the plot
plot(spat_data, add = TRUE, col = "red", pch = 16, cex = 1.2)

nc_file_temp<-mask(nc_file_temp,MED_final)

# Momentum Calculation Function
calculate_momentum <- function(data) {
  data <- data %>%
    arrange(ID, first_area_sighting) %>%
    group_by(ID) %>%
    mutate(
      t_step = row_number() - 1,  # t0, t1, t2...
      t_flag = ifelse(t_step < 10, 1, 0)  # Limit to t0-t50
    ) %>%
    filter(t_flag == 1) %>%
    ungroup()
  return(data)
}


# Apply Momentum Calculation
momentum_data_geo<-  vect(momentum_data,
geom = c("Longitude", "Latitude"),
crs = "EPSG:4326")

momentum_data_geo

#Vamos

library(terra)

# 1. Prepare empty list to store velocity rasters for each species
velocity_rasters <- list()

# 2. Get unique species IDs
unique_ids <- unique(momentum_data_geo$ID)

# Create a lookup table of ID to species name
id_to_name <- data.frame(
  ID = momentum_data_geo$ID,
  species = momentum_data_geo$species
) %>% distinct()

# 3. Process each species separately
for(species_id in unique_ids) {
  # Subset data for current species
  species_data <- momentum_data_geo[momentum_data_geo$ID == species_id, ]
  
  # Order by t_step
  species_data <- species_data[order(species_data$t_step), ]
  
  # Skip if only one observation
  if(nrow(species_data) < 2) next
  
  # Get coordinates
  coords <- crds(species_data)
  
  # Calculate distances between consecutive points (in km)
  # Using Haversine distance for better accuracy
  dists <- sqrt((diff(coords[,1]) * 111.32 * cos(coords[,2] * pi/180))^2 + 
                  (diff(coords[,2]) * 111.32)^2)
  
  # Improved calculation with proper handling of duplicate years
  time_diffs <- diff(species_data$first_area_sighting)
  
  # First identify which steps to keep (where time_diff != 0)
  keep <- which(time_diffs != 0)
  
  # Then calculate only for valid steps
  velocities <- numeric(length(time_diffs)) # Initialize empty vector
  velocities[] <- NA # Default to NA
  
  # Only calculate where time_diff != 0
  if(length(keep) > 0) {
    velocities[keep] <- dists[keep]/time_diffs[keep]
  }
  
  # Alternative approach using case-wise calculation:
  # velocities <- mapply(function(d, t) ifelse(t == 0, NA, d/t), 
  #                     dists, time_diffs)
  
  # Create output SpatVector with velocity values
  result_vect <- species_data[-1, ] # Remove first point (no velocity for it)
  result_vect$velocity <- velocities
  
  #plot(species_data,add=TRUE)
  
  # Rasterize velocities for this species
  vel_raster <- rasterize(result_vect, nc_file_temp, field = "velocity")
  
  # Get species name for this ID
  species_name <- id_to_name$species[id_to_name$ID == species_id]
  
  # Create a clean filename (replace spaces with underscores)
  clean_name <- gsub(" ", "_", species_name)
  
  # Store with combined ID-name identifier
  velocity_rasters[[paste0("ID_", species_id, "_", clean_name)]] <- vel_raster}

library(mapview)
#mapview(velocity_rasters[[18]])

# For one raster layer (e.g., element 25 in your list)
non_na_count <- global(!is.na(velocity_rasters[[18]]), "sum", na.rm = TRUE)
print(paste("Number of cells with values:", non_na_count))

# 4. Create a raster stack of all species velocities
velocity_stack <- rast(velocity_rasters)

#SUM VELOCITIES
composite_sum <- sum(velocity_stack, na.rm = TRUE)
names(composite_sum) <- "total_velocity_sum"
composite_sum
#mapview(composite_sum)

#MEAN VELOCITIES
composite_mean <- mean(velocity_stack, na.rm = TRUE)
names(composite_mean) <- "mean_velocity"
composite_mean
#mapview(composite_mean)

#MAX VELOCITIES
composite_max <- max(velocity_stack, na.rm = TRUE)
names(composite_max) <- "max_velocity"
composite_max

# For one raster layer (e.g., element 25 in your list)
non_na_count <- global(!is.na(composite_mean), "sum", na.rm = TRUE)
print(paste("Number of cells with values:", non_na_count))


# Tabulate min/max/mean for each layer
stats<-data.frame(
  Layer = names(velocity_stack),
  Min = global(velocity_stack, "min", na.rm=TRUE)[,1],
  Mean = global(velocity_stack, "mean", na.rm=TRUE)[,1],
  Max = global(velocity_stack, "max", na.rm=TRUE)[,1]
)

summary(stats$Max)
summary(stats$Min)
summary(stats$Mean)

##########

plot(composite_mean,ext=extent)

#vamos aver max -- Inverse Distance Weighting (IDW)

library(terra)
library(gstat)

# 1. Prepare your data (example with known points)
known_pts <- as.points(composite_mean, na.rm = TRUE)
known_df <- as.data.frame(known_pts, geom = "XY")

# 2. Fit a gstat model (IDW example)
gmodel <- gstat(
  formula = mean_velocity~1,  # Using mean value
  locations = ~x+y, 
  data = known_df, 
  nmax = 10  # Use 10 nearest neighbors
)

# 3. Create prediction grid (only over marine areas)
marine_mask <- composite_mean  # Copy of original
values(marine_mask) <- !is.na(values(marine_mask))  # Convert to 1/NA mask

# 4. Perform interpolation WITH masking
filled <- interpolate(
  object = marine_mask,  # Use mask as template
  model = gmodel,
  xyNames = c("x", "y"),
  fun = predict
) %>% 
  mask(nc_file_temp)  # Restrict to marine areas

plot(filled,ext=extent)

# 5. Combine with original (marine areas only)
final_result <- cover(composite_mean, filled)

# Verify results
print(paste("Original NAs:", global(is.na(composite_mean), "sum")[1,1]))
print(paste("Remaining NAs:", global(is.na(final_result), "sum")[1,1]))

# Visual comparison

plot(c(composite_mean, filled),ext=extent, main = c("Original", "Interpolated"))


#mapview(filled)

#### UNCERTAINTY #####

# Calculate uncertainty based on point density (inverse distance to nearest points)
pts <- as.points(composite_mean, na.rm = TRUE)  # Your original points

# Create distance-to-nearest-point raster
dist_raster <- distance(marine_mask, pts) %>% 
  mask(marine_mask)

# 2. Create normalized uncertainty (0-1 range)
uncertainty <- 1/(dist_raster + 1)  # +1 avoids division by zero
uncertainty <- uncertainty / global(uncertainty, max, na.rm = TRUE)[1,1]

plot(uncertainty,ext=extent)
plot(dist_raster,ext=extent)
#


#Scale

# 1. First ensure no NA values in distance raster
dist_raster <- mask(dist_raster, marine_mask)

dist_raster_mask<-mask(dist_raster,filled)

plot(dist_raster_mask,ext=extent)


# 2. Scale distances to 0-1 range (1 = farthest/most uncertain)
scaled_uncertainty <- dist_raster_mask / global(dist_raster_mask, max, na.rm = TRUE)[1,1]

scaled_uncertainty

plot(scaled_uncertainty,ext=extent)


plot(c(composite_max, filled,scaled_uncertainty), main = c("Original", "Interpolated","Uncertainty"),ext=extent)


plot(filled$var1.pred,main=c("Interpolated"),ext=extent)
plot(momentum_data_geo,add=TRUE)


#PLOT EM
library(terra)
library(ggplot2)

# 1. Prepare the data
# Convert velocity raster to data frame
vel_df <- as.data.frame(filled$var1.pred, xy = TRUE, na.rm = TRUE)
names(vel_df) <- c("longitude", "latitude", "velocity")

# Convert points to data frame
pts_df <- as.data.frame(geom(momentum_data_geo)[, c("x", "y")])
names(pts_df) <- c("longitude", "latitude")

# 2. Create the plot
ggplot() +
  # Velocity raster
  geom_raster(data = vel_df, 
              aes(x = longitude, y = latitude, fill = velocity)) +
  
  # Observation points
  geom_point(data = pts_df, 
             aes(x = longitude, y = latitude),
             color = "red",fill="red", size = 0.5, shape = 21) +  # Red X markers
  
  # Color scale
  scale_fill_viridis_c(option = "plasma", 
                       name = "Velocity\n(km/year)",
                       na.value = NA) +
  
  # Titles and theme
  ggtitle("Interpolated Spread of NIS based on momentum theory 
    \nbased on IDW (Observation Points, N=14487)") +
  coord_equal() +  # Maintain aspect ratio
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center title


#PLOT UNCERTAINTY

#PLOT EM
library(terra)
library(ggplot2)

# 1. Prepare the data
# Convert velocity raster to data frame
vel_df <- as.data.frame(scaled_uncertainty$lyr1, xy = TRUE, na.rm = TRUE)
names(vel_df) <- c("longitude", "latitude", "Uncertainty")

# Convert points to data frame
pts_df <- as.data.frame(geom(momentum_data_geo)[, c("x", "y")])
names(pts_df) <- c("longitude", "latitude")

# 2. Create the plot
ggplot() +
  # Velocity raster
  geom_raster(data = vel_df, 
              aes(x = longitude, y = latitude, fill = Uncertainty)) +
  
  # Observation points
  geom_point(data = pts_df, 
             aes(x = longitude, y = latitude),
             color = "red",fill="red", size = 0.5, shape = 21,stroke = 0.5) +  # Red X markers
  
  # Color scale
  scale_fill_viridis_c(option = "plasma", 
                       name = "Uncertainty",
                       na.value = NA) +
  
  # Titles and theme
  ggtitle("Uncertainty based on Inverted Distance (Observation Points, N=14487)") +
  coord_equal() +  # Maintain aspect ratio
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center title


#I wanna standardize the values of the interpolated raster based on the uncertainty

#how to do that? by multiplying :: high uncertainty will give same values, while low will give really high
vel_df_inter <- as.data.frame(filled$var1.pred, xy = TRUE, na.rm = TRUE)
vel_df_unc <- as.data.frame(scaled_uncertainty$lyr1, xy = TRUE, na.rm = TRUE)

stand_veloc<-vel_df_inter$var1.pred*(abs(vel_df_unc$lyr1-1))
str(stand_veloc)


stand_veloc_geom<-data.frame(stand_veloc,vel_df$longitude,vel_df$latitude)
str(stand_veloc_geom)

#standardize it
library(scales)
stand_veloc_geom$stand_veloc_01 <- rescale(stand_veloc_geom$stand_veloc, to = c(0, 1))

# 2. Create the plot
ggplot() +
  # Velocity raster
  geom_raster(data = stand_veloc_geom, 
              aes(x = vel_df.longitude, y = vel_df.latitude, fill = stand_veloc_01)) +
  
  # Observation points
  geom_point(data = pts_df, 
             aes(x = longitude, y = latitude),
             color = "red",fill="red", size = 0.5, shape = 21,stroke = 0.5) +  # Red X markers
  
  # Color scale
  scale_fill_viridis_c(option = "plasma", 
                       name = "Velocity (scaled)",
                       na.value = NA) +
  
  # Titles and theme
  ggtitle("Standardized Spread of NIS based on Uncertainty (Observation Points, N=14487)") +
  coord_equal() +  # Maintain aspect ratio
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+  labs(x = "longitude",
                                                       y = "latitude") # Center title

