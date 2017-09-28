###very powerful visualization!
library("ggplot2")
library("dplyr")
library("magrittr")
data=read.csv("data/data.csv",stringsAsFactors = F)
index=is.na(data$shot_made_flag)
test=data[index,]
train=data[!index,]

##explore the data
res.remain.min=table(train$minutes_remaining,train$shot_made_flag)
res.remain.min=cbind(res.remain.min,res.remain.min[,2]/(res.remain.min[,1]+res.remain.min[,2]))
ggplot(data.frame(res.remain.min),aes(x=factor(0:11),y=V3))+geom_bar(stat = "identity")

res.distance=table(train$shot_distance,train$shot_made_flag)
res.distance=cbind(res.distance,res.distance[2]/(res.distance[,1]+res.distance[,2]),rownames(res.distance))
ggplot(data.frame(res.distance),aes(x=factor(0:11),y=V3))+geom_bar(stat = "identity")


train$shot_made_flag=factor(train$shot_made_flag,levels = c("1","0"))

##a plot to see accuracy by feature
pplot=function(feat){
    feat=substitute(feat)
    ggplot(data = train,aes_q(x=feat))+
        geom_bar(aes(fill=shot_made_flag),stat = "count",position = "fill")+
        scale_fill_brewer(palette = "Set1",direction = -1)+
        ggtitle(paste("accuracy by",feat))
}
##a plot to see position by feature
courtplot=function(feat){
    feat=substitute(feat)
    train%>%
        ggplot(aes(x=lon,y=lat))+
        geom_point(aes_q(color=feat),alpha=0.7,size=3)+
        ylim(c(33.7,34.0883))+
        scale_color_brewer(palette = "Set1")+
        theme_void()+
        ggtitle(paste(feat))
}

courtplot(combined_shot_type)

