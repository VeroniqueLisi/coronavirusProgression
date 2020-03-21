#data source:
#https://www.journaldemontreal.com/2020/03/15/un-outil-de-visualisation-et-de-recherche-des-cas-de-la-covid-19
#https://ourworldindata.org/coronavirus-source-data
# https://www.cdph.ca.gov/Programs/OPA/Pages/New-Release-2020.aspx ## Californai data

library(ggplot2)
library(reshape2)
data = read.csv("Coronavirus+Qc.csv")
dataSub = data[,c('date','Quebec','Canada','United.States',
                  'British.Columbia','California')]
dataSub[which(is.na(dataSub),arr.ind = T)] = 0
dataSub[,'day'] = as.numeric(rownames(dataSub))
dataSub[,'date'] = as.Date(dataSub[,'date'], format = "%m/%d/%y")
meltedData = melt(dataSub,id.vars = c("day","date"))

palette = c("Quebec" = "#2b04d9",'Canada' = '#d90432','United.States'='#0492d9',
            'British.Columbia'='#8004d9', 'California'='#32a852')


p1 = ggplot(meltedData,aes(x = date,y=value,group = variable,color = variable))+
  geom_line()+geom_smooth() +
  scale_color_manual(values=palette)+
  geom_point()+scale_y_continuous(trans='log10')+
  ylab("Nb of cases")+xlab('Progression (days)')
p1

png("Coronavirus-log.png")
print(p1)
dev.off()

dQc = dataSub[which(dataSub[,2] > 0),2]
names(dQc) = 1:length(dQc)
dCa = dataSub[which(dataSub[,3] > 0),3]
names(dCa) = 1:length(dCa)
dUS = dataSub[which(dataSub[,4] > 0),4]
names(dUS) = 1:length(dUS)
dBC = dataSub[which(dataSub[,5] > 0),5]
names(dUS) = 1:length(dBC)
dCal = dataSub[which(dataSub[,6] > 0),6]
names(dCal) = 1:length(dCal)

dataPrime = merge(merge(merge(merge(dQc,dCa,by.x = 0,by.y = 0,all=T),
       dUS,by.x = 1,by.y = 0,all=T),
       dBC,by.x = 1,by.y = 0,all=T),
       dCal,by.x = 1,by.y = 0,all=T)
dataPrime[,1] = as.numeric(dataPrime[,1])
colnames(dataPrime) = c("Days.since.case.1",'Quebec',
                        'Canada','United.States','British.Columbia',
                        'California')
dataPrime = dataPrime[order(dataPrime[,1]),]
meltedDataPrime = melt(dataPrime,id.vars = c("Days.since.case.1"))

png("Coronavirus-Day0.png")
print(ggplot(meltedDataPrime,
       aes_string(x = "Days.since.case.1",y="value",group = "variable",
                             color = "variable"))+
  geom_smooth() +
  geom_line()+scale_color_manual(values=palette)+
  geom_point()+scale_y_continuous(trans='log10')+
  ylab("Nb of cases")+xlab('Days since case 1'))
dev.off()


