library(data.table)
library(igraph)
load('totalResults.RData')

###########以下用于输出nodes和edges信息
lapply(resu,function(x)
{
x$nodes->nn
x$edges->ee
data.table('URL'=x$summ$URL,nn)->nn
data.table('URL'=x$summ$URL,ee)->ee
list(nn,ee)
})->ne

rbindlist(lapply(ne,`[[`,1))->nns
rbindlist(lapply(ne,`[[`,2))->ees

fwrite(nns,file='nodes.csv',sep=",",quote=T)
fwrite(ees,file='edges.csv',sep=",",quote=T)
#####################


#####################以下用于输出table.csv （summary info）
quantile(info$Tweet,na.rm=T,prob=c(0.5,0.9))
info[,tweetG:=fcase(Tweet<=6,'lowly',
                    Tweet>6 & Tweet<=40,'medium',
					Tweet>40,'highly',
					default=NA)]

info2<-info[,.(PY=paste(sort(unique(PY)),collapse=", "),
        Discipline=paste(sort(unique(Discipline)),collapse=", "),
		tweetG=paste(sort(unique(tweetG)),collapse=", "),
		Group=paste(sort(unique(Group)),collapse=", ")),by=.(URL)]
	
	
tt<-lapply(resu,`[[`,'summ')
tt<-rbindlist(tt)
info2[tt,on=.(URL)]->ok

fwrite(ok,file='table.csv',sep=",",quote=T)


#############################以下用于作图
library(ggplot2)
library(tidyr)

##################plot 4.1
ok[,.(Max.DFS,Group)]->dat41
dat41<-separate_rows(dat41,Group,sep=",\\s+")
setDT(dat41)
dat41[,Max.DFS:=ordered(Max.DFS,0:7)]

dat41[,table(Max.DFS)/.N,Group]->dat41
dat41[,DFS:=rep(0:7,4)]->dat41


plot41<-ggplot(dat41,aes(y=V1,x=DFS,colour=Group,group=Group))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(y='ratio')

ggsave(plot41,file='plot41.png',width=6.2,height=5.4)


##################plot 4.2
ok[,.(Group,N.edges,Mean.DFS)]->dat42
dat42<-separate_rows(dat42,Group,sep=",\\s+")

plot42<-ggplot(dat42,aes(x=N.edges,y=Mean.DFS,colour=Group))+
   geom_point(size=1)+
   theme_bw()+
   scale_x_continuous(expand=expansion(0))+
   geom_smooth(se=F)

ggsave(plot42,file='plot42.png',width=14.2,height=5.4)


################plot3
todo<-'Mean.DFS'  ###可以换成其他指标；以下基本不需要调整。



#######Discipline
c(todo,'Discipline','Group')->colss
ok[,..colss]->dat3
dat3<-separate_rows(dat3,Group,sep=",\\s+")
dat3<-separate_rows(dat3,Discipline,sep=",\\s+")


plot31<-ggplot(dat3,aes_string(x=todo,fill='Group'))+
    theme_bw()+
    geom_histogram(bins=50)+
    facet_wrap(vars(Discipline))

ggsave(paste0(todo,"_Discipline.png"),plot31,width=7.8,height=5.2)	

#######PY
c(todo,'PY','Group')->colss
ok[,..colss]->dat3
dat3<-separate_rows(dat3,Group,sep=",\\s+")
dat3<-dat3[dat3$PY!='',]

plot32<-ggplot(dat3,aes_string(x=todo,fill='Group'))+
    theme_bw()+
    geom_histogram(bins=50)+
    facet_wrap(vars(PY))
	
ggsave(paste0(todo,"_PY.png"),plot32,width=7.8,height=5.2)	

#######tweetG
c(todo,'tweetG','Group')->colss
ok[,..colss]->dat3
dat3<-separate_rows(dat3,Group,sep=",\\s+")
dat3<-dat3[dat3$tweetG!='',]

plot33<-ggplot(dat3,aes_string(x=todo,fill='Group'))+
    theme_bw()+
    geom_histogram(bins=50)+
    facet_wrap(vars(tweetG))
	
ggsave(paste0(todo,"_tweetG.png"),plot33,width=7,height=3.2)	
