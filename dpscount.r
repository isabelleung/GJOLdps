setwd("E:/OneDrive/墨悲丝/dps统计");
胖瘦 <-read.table(file="number1.csv", header=TRUE, sep=",");
铁牛 <-read.table(file="number2.csv", header=TRUE, sep=",");
温雪名 <-read.table(file="number3.csv", header=TRUE, sep=",");
names=unique(胖瘦$name);

#正态服从检验
library(nortest)
lillie_result=data.frame(pvalue_1=NA,pvalue_2=NA,pvalue_3=NA);
for(i in names)
{
  dps=胖瘦[which(胖瘦$name==i),"dps"]
  dps=na.omit(dps);
  if(length(dps)<5)next;
  lillie_result[i,"pvalue_1"]=lillie.test(dps)["p.value"];
}
for(i in names)
{
  dps=铁牛[which(铁牛$name==i),"dps"]
  dps=na.omit(dps);
  if(length(dps)<5)next;
  lillie_result[i,"pvalue_2"]=lillie.test(dps)["p.value"];
}
for(i in names)
{
  dps=温雪名[which(温雪名$name==i),"dps"]
  dps=na.omit(dps);
  if(length(dps)<5)next;
  lillie_result[i,"pvalue_3"]=lillie.test(dps)["p.value"];
}
lillie_result

#作图
library(ggplot2)
library(ggpubr)
color_group=colorRampPalette(c("orangered","royalblue","pink","firebrick","gold","mediumslateblue","lightskyblue","turquoise","limegreen","darkorchid"))(10);
p1 <-  ggplot(胖瘦, aes(x=num, y=dps))+
  geom_line(aes(colour=name, group=name), size=1.2) +
  geom_point(aes(colour=name), size=2)+
    scale_color_manual(values=color_group)+
  theme(panel.grid.minor = element_line(linetype = "dotted", color="red"))+
   labs(title = "~2020.09.21-阳极丹上dps统计图",
      subtitle = "胖子和瘦子",
      x = "队伍序号",
      y = "dps")

p1_box<-ggplot(胖瘦,aes(x=name, y=dps,fill=name))+
  geom_boxplot(alpha=0.7)+
  scale_fill_manual(values=color_group)+
  theme(panel.grid.minor = element_line(linetype = "dotted", color="red"))
ggarrange(p1, p1_box, ncol = 1, nrow = 2)

p2 <-  ggplot(铁牛, aes(x=num, y=dps))+
  geom_line(aes(colour=name, group=name), size=1.2) +
  geom_point(aes(colour=name), size=2)+
    scale_color_manual(values=color_group)+
  theme(panel.grid.minor = element_line(linetype = "dotted", color="red"))+
     labs(title = "~2020.09.21-阳极丹上dps统计图",
      subtitle = "铁牛",
      x = "队伍序号",
      y = "dps")

p2_box<-ggplot(铁牛,aes(x=name, y=dps,fill=name))+
  geom_boxplot(alpha=0.7)+
  scale_fill_manual(values=color_group)+
  theme(panel.grid.minor = element_line(linetype = "dotted", color="red"))

ggarrange(p2, p2_box, ncol = 1, nrow = 2)

p3 <-  ggplot(温雪名, aes(x=num, y=dps))+
  geom_line(aes(colour=name, group=name), size=1.2) +
  geom_point(aes(colour=name), size=2)+
    scale_color_manual(values=color_group)+
  theme(panel.grid.minor = element_line(linetype = "dotted", color="red"))+
     labs(title = "~2020.09.21-阳极丹上dps统计图",
      subtitle = "温雪名",
      x = "队伍序号",
      y = "dps")

p3_box<-ggplot(温雪名,aes(x=name, y=dps,fill=name))+
  geom_boxplot(alpha=0.7)+
  scale_fill_manual(values=color_group)+
  theme(panel.grid.minor = element_line(linetype = "dotted", color="red"))

ggarrange(p3, p3_box, ncol = 1, nrow = 2)

#假设检验
#kruskal-wallis
library(spdep)
library(pgirmess)
kruskal_test_set=list(温雪名[which(温雪名$name=="斩风"),"dps"],温雪名[which(温雪名$name=="龙城"),"dps"],温雪名[which(温雪名$name=="飞星"),"dps"],温雪名[which(温雪名$name=="神虹"),"dps"],温雪名[which(温雪名$name=="朝弦"),"dps"],温雪名[which(温雪名$name=="妙法"),"dps"],温雪名[which(温雪名$name=="狐狸"),"dps"],温雪名[which(温雪名$name=="男刀"),"dps"],温雪名[which(温雪名$name=="咒隐"),"dps"],温雪名[which(温雪名$name=="司命"),"dps"]);
kruskal.test(kruskal_test_set)
categ<-factor(rep(1:10,c(7,8,10,9,15,9,12,6,10,8)));
kruskal_full_set=as.vector(unlist(kruskal_test_set[1]))
for(i in 2:10)
{
  kruskal_full_set=append(kruskal_full_set,as.vector(unlist(kruskal_test_set[i])));
}
kruskalmc(kruskal_full_set,categ,probs=0.05)

#两样本t test
t_test_result_boss3=data.frame(name1=NA,name2=NA);
t_test_result_boss2=data.frame(name1=NA,name2=NA);
t_test_result_boss1=data.frame(name1=NA,name2=NA);
for(i in names)
{
  for(j in names)
  {
    if(i==j)next;
    test_data=rbind(温雪名[which(温雪名$name==i),],温雪名[which(温雪名$name==j),]);
    list=t.test(formula=dps~name,data=test_data,paired=FALSE);
    if(list$p.value<0.05)
    {t_test_result_boss3=cbind(t_test_result_boss3,rbind(i,j));}
  }
}
for(i in names)
{
  for(j in names)
  {
    if(i==j)next;
    test_data=rbind(铁牛[which(铁牛$name==i),],铁牛[which(铁牛$name==j),]);
    list=t.test(formula=dps~name,data=test_data,paired=FALSE);
    if(list$p.value<0.05)
    {t_test_result_boss2=cbind(t_test_result_boss2,rbind(i,j));}
  }
}
for(i in names)
{
  for(j in names)
  {
    if(i==j)next;
    test_data=rbind(胖瘦[which(胖瘦$name==i),],胖瘦[which(胖瘦$name==j),]);
    list=t.test(formula=dps~name,data=test_data,paired=FALSE);
    if(list$p.value<0.05)
    {t_test_result_boss1=cbind(t_test_result_boss1,rbind(i,j));}
  }
}

write.table(t_test_result_boss1,"t_test_result_boss1.csv",row.names=FALSE,col.names=TRUE,sep=",");
write.table(t_test_result_boss2,"t_test_result_boss2.csv",row.names=FALSE,col.names=TRUE,sep=",");
write.table(t_test_result_boss3,"t_test_result_boss3.csv",row.names=FALSE,col.names=TRUE,sep=",")
