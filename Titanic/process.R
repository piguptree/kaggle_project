library("dplyr")
library("mice")
library("ggplot2")
library("ggthemes")
library("scales")
library("randomForest")
train = read.csv("Titanic//train.csv")
test = read.csv("Titanic//test.csv")
full = bind_rows(train,test)

####开始增删变量
##第一个变量title
full$title = gsub("(.*, )|(\\..*)","",full$Name)##转义字符要两个\\
## 合并Title,这里有问题！！！已解决(错误在于字符里有空格)
rare_title <-c('Dona',"Lady","the Countess","Capt","Col","Don","Dr","Major","Rev","Sir","Jonkheer")
full$title[full$title=='Mlle']<-'Miss'
full$title[full$title=="Ms"] <-"Miss"
full$title[full$title=="Mme"]<-"Mrs"
full$title[full$title %in% rare_title]<-"Rare title"
table(full$Sex,full$title)

##第二个变量Surname
full$Surname <- sapply(full$Name, function(x)strsplit(x,split = "[,.]")[[1]][1])
full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste(full$Surname,full$Fsize,sep="_")
ggplot(full[1:891,],aes(x=Fsize, fill=factor(Survived)))+
    geom_bar(stat = 'count', position = 'dodge')+
    scale_x_continuous(breaks = c(1:11))+
    labs(x='Family Size')+
    theme_few()
full$FsizeD[full$Fsize==1]="single"
full$FsizeD[full$Fsize %in% 2:4]="small"
full$FsizeD[full$Fsize>4]="large"
mosaicplot(table(full$FsizeD,full$Survived),main="family size by survival",shade=T)

##第三类变量 deck客舱层数
full$Deck <- factor(sapply(full$Cabin,function(x)strsplit(x,NULL)[[1]][1]))
##观察deck的影响
full_b=full[1:891,]
full_a=full_b[!(is.na(full_b$Deck)),]
ggplot(full_a,aes(x=Deck, fill=factor(Survived)))+
    geom_bar(stat = 'count', position = 'dodge')+
    theme_few()


#####开始填补
#is.na(full$Embarked)
#sum(is.na(full$Embarked))
##第一个填补，embarked
grep("\\w",full$Embarked,invert = T)###通过正则表达式判断空字符串！！
embark_fare <- full[full$PassengerId!=62&full$PassengerId!=830,]
#####一个新的管道思想！
embark_fare <- full %>% filter(PassengerId!=62&PassengerId!=830)
###此处filter会出现重名，但会先引用dplyr中的，因为这个包靠前
ggplot(embark_fare,aes(x=Embarked,y=Fare,fill=factor(Pclass)))+
    geom_boxplot()+
    geom_hline(aes(yintercept = 80),linetype="dashed",color="red",lwd=2)+
    scale_y_continuous(labels = dollar_format())+
    theme_few()
##观察62和830的Pclass和Fare
full$Embarked[c(62,830)]="C"

##第二个填补fare
##通过which函数查找下标,补全fare
which(is.na(full$Fare))
full[1044,]
id1044 <- full %>% filter(Embarked=="S" & Pclass=='3')
ggplot(id1044,aes(x=Fare))+geom_density(fill="#99d6ff",alpha=.6)+
    geom_vline(aes(xintercept = median(Fare,na.rm = T)),colour="red",linetype="dashed")+
    scale_x_continuous(labels = dollar_format())+
    theme_few()
full$Fare[1044]=median(id1044$Fare,na.rm = T)
full[1044,]
###第三个年龄填补 这一段不太懂
sum(is.na(full$Age))
factor_vars <- c('PassengerId','Pclass','Sex','Embarked','title','Surname','Family','Fsize')
full[factor_vars]=lapply(full[factor_vars],function(x)as.factor(x))
#set.seed(129)
mice_mod <- mice(full[,!names(full) %in% 
        c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')],method = 'rf')
mice_output <- complete(mice_mod)

par(mfrow=c(1,2))
hist(full$Age,freq = F,main = "origin data",col="darkgreen",ylim = c(0,0.04))
hist(mice_output$Age,freq = F,main = "mice data",col="lightgreen",ylim = c(0,0.04))
full$Age <- mice_output$Age
sum(is.na(full$Age))


###细分年龄
ggplot(full[1:891,],aes(x=Age,fill=factor(Survived)))+
    geom_histogram()+
    facet_grid(.~Sex)+
    theme_few()
full$child[ full$Age<18] <- 'child'
full$child[full$Age>=18] <- 'adult'
table(full$child,full$Survived)
full$Mother <-'notmother'
full$Mother[full$Sex=='female'&full$Parch>0&full$Age>18&full$title!='Miss']<-'mother'
table(full$Mother,full$Survived)
full$child<- factor(full$child)
full$Mother<- factor(full$Mother)
md.pattern(full)




###模型预测
train=full[1:891,]
test=full[892:1309,]
#set.seed(754)
rf_model <- randomForest(factor(Survived)~Pclass+Sex+Age+SibSp+
        Sex*Parch+Fare+Embarked+title+Fsize,data=train)##+child+Mother
par(mfrow=c(1,1))
plot(rf_model,ylim=c(0,0.36))
legend('topright',colnames(rf_model$err.rate),col=1:3,fill=1:3)


###变量重要性,最后的ggplot没画
importance <- importance(rf_model)
importance
var_impo <- data.frame(variables=row.names(importance)
    ,importance=round(importance[,1],2))
##rankimport <- var_impo  %>% mutate(paste0('#',dence_rank(desc(importance))))


###预测
prediction=predict(rf_model,test)
solution <- data.frame(PassengerId=test$PassengerId,Survived=prediction)
write.csv(solution,file='rf_mod_solution.csv',row.names = F)
