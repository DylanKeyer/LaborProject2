setwd("C:/Users/dylan_000/Downloads")
library(xlsx)
library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)

eitc <- read.xlsx('EITC.xls',1)
View(eitc)

eitc %>% group_by(state) %>% summarize(mean(urate))

eitc %>% group_by(children) %>% summarize(mean(finc))

attach(eitc)

View(eitc[earn>300000,])

for(i in 1:nrow(eitc)){
  if(eitc[i,'earn'] > 0){
    eitc[i,'earncon'] <- eitc[i,'earn']
  }
}

eitc$ANYKIDS <- eitc$children
eitc$ANYKIDS[eitc$ANYKIDS > 0] <- 1
View(eitc)

eitc$POST93 <- eitc$year
eitc$POST93[eitc$POST93 > 1993] <- 1
eitc$POST93[eitc$POST93 %in% seq(2,1993)] <- 0

eitc$emp <- NA
eitc$emp[eitc$earn>0] <- 1
eitc$emp[eitc$earn==0] <- 0

withkids <- eitc %>% filter(ANYKIDS==1) %>% group_by(year) %>% summarize(mean(emp))
withoutkids <- eitc %>% filter(ANYKIDS==0) %>% group_by(year) %>% summarize(mean(emp))
names(withkids) <- c('year','avg_emp')
names(withoutkids) <- c('year','avg_emp')

p1 <- ggplot(withkids) + geom_bar(aes(year,avg_emp,fill=factor(year)),stat='identity') +
  xlab('Year') + ylab('Percentage of Women Employed') + theme(legend.position='Null') +
  ggtitle('One or More Children') + coord_cartesian(ylim = c(0, 1))

p2 <- ggplot(withoutkids) + geom_bar(aes(year,avg_emp,fill=factor(year)),stat='identity') +
  xlab('Year') + ylab('Percentage of Women Employed') + theme(legend.position='Null') +
  ggtitle('No Children') + coord_cartesian(ylim = c(0, 1))

grid.arrange(p1,p2)

withkids1 <- eitc %>% filter(ANYKIDS==1) %>% group_by(year) %>% summarize(mean(earn))
withoutkids1 <- eitc %>% filter(ANYKIDS==0) %>% group_by(year) %>% summarize(mean(earn))
names(withkids1) <- c('year','avg_earn')
names(withoutkids1) <- c('year','avg_earn')

p3 <- ggplot(withkids1) + geom_bar(aes(year,avg_earn,fill=factor(year)),stat='identity') +
  xlab('Year') + ylab('Average Annual Earnings') + theme(legend.position='Null') +
  ggtitle('One or More Children') 

p4 <- ggplot(withoutkids1) + geom_bar(aes(year,avg_earn,fill=factor(year)),stat='identity') +
  xlab('Year') + ylab('Average Annual Earnings') + theme(legend.position='Null') +
  ggtitle('No Children')

grid.arrange(p3,p4)

#both lines below accomplish the same task.
eitc %>% group_by(POST93,ANYKIDS) %>% summarize(mean(emp))
aggregate(eitc$emp,by=list(time=eitc$POST93,kids=eitc$ANYKIDS),FUN=mean,na.rm=TRUE)
