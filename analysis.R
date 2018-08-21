
library(dplyr)
library(ggplot2)

#setwd(paste0(Sys.getenv('CS_HOME'),'/Reviews/'))
source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

res<- as.tbl(read.csv('exploration/20180820_115310_DIRECTSAMPLING.csv'))

g=ggplot(res,aes(x=initialDensity,y=livingProp,group=withSex,color=withSex))
g+geom_point(pch='.')+geom_smooth()
# -> smoothed plot not appropriate


sres = res %>% group_by(initialDensity,withSex) %>% summarize(
  livingPropSd = sd(livingProp),livingProp = mean(livingProp)
)

g=ggplot(sres,aes(x=initialDensity,y=livingProp,colour=withSex,group=withSex))
g+geom_point()+geom_line()+geom_errorbar(aes(ymin=livingProp-livingPropSd,ymax=livingProp+livingPropSd))+
  xlab('Initial density')+ylab(expression('Living proportion at '*t[f]*'=10000'))+
  scale_color_discrete(name='With sex')+stdtheme
ggsave(file='res/livingProp.png',width=30,height=20,units='cm')


# variations (end-of-life-still states)
sres = res %>% group_by(initialDensity,withSex) %>% summarize(
  livingVarSd = sd(livingVar),livingVar = mean(livingVar)
)

g=ggplot(sres,aes(x=initialDensity,y=livingVar,colour=withSex,group=withSex))
g+geom_point()+geom_line()+geom_errorbar(aes(ymin=livingVar-livingVarSd,ymax=livingVar+livingVarSd))+
  xlab('Initial density')+ylab(expression('Living variation at '*t[f]*'=10000'))+
  scale_color_discrete(name='With sex')+stdtheme
ggsave(file='res/livingVar.png',width=30,height=20,units='cm')






# by color

sres = res %>% group_by(initialDensity,withSex) %>% summarize(
  livingPropBlueSd = sd(livingPropBlue),livingPropBlue = mean(livingPropBlue),
  livingPropGreenSd = sd(livingPropGreen),livingPropGreen = mean(livingPropGreen),
  livingPropRedSd = sd(livingPropRed),livingPropRed = mean(livingPropRed)
)

g=ggplot(sres[sres$withSex=='true',],aes(x=initialDensity))
g+geom_point(aes(y=livingPropBlue),col='blue')+geom_errorbar(aes(ymin=livingPropBlue-livingPropBlueSd,ymax=livingPropBlue+livingPropBlueSd),col='blue')+
  geom_point(aes(y=livingPropRed),col='red')+geom_errorbar(aes(ymin=livingPropRed-livingPropRedSd,ymax=livingPropRed+livingPropRedSd),col='red')+
  geom_point(aes(y=livingPropGreen),col='green')+geom_errorbar(aes(ymin=livingPropGreen-livingPropGreenSd,ymax=livingPropGreen+livingPropGreenSd),col='green')
ggsave(file='res/livingPropColor_withSex.png',width=30,height=20,units='cm')

g=ggplot(sres[sres$withSex=='false',],aes(x=initialDensity))
g+geom_point(aes(y=livingPropBlue),col='blue')+geom_errorbar(aes(ymin=livingPropBlue-livingPropBlueSd,ymax=livingPropBlue+livingPropBlueSd),col='blue')+
  geom_point(aes(y=livingPropRed),col='red')+geom_errorbar(aes(ymin=livingPropRed-livingPropRedSd,ymax=livingPropRed+livingPropRedSd),col='red')+
  geom_point(aes(y=livingPropGreen),col='green')+geom_errorbar(aes(ymin=livingPropGreen-livingPropGreenSd,ymax=livingPropGreen+livingPropGreenSd),col='green')
ggsave(file='res/livingPropColor_withoutSex.png',width=30,height=20,units='cm')









