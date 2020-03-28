#data source:
#https://www.journaldemontreal.com/2020/03/15/un-outil-de-visualisation-et-de-recherche-des-cas-de-la-covid-19
##https://www.canada.ca/fr/sante-publique/services/maladies/2019-nouveau-coronavirus.html
#https://ourworldindata.org/coronavirus-source-data
# https://www.cdph.ca.gov/Programs/OPA/Pages/New-Release-2020.aspx ## Californai data

library(ggplot2)
library(reshape2)
data = read.csv("Coronavirus+Qc.csv")
data[,'date'] = as.Date(data[,'date'], format = "%m/%d/%y")
data[,'day'] = as.numeric(rownames(data))

dataSub = data[,c('date','day','Quebec','Canada','United.States',
                  'British.Columbia','California')]
meltedData = melt(dataSub,id.vars = c("day","date"))

palette = c("Quebec" = "#2b04d9",'Canada' = '#d90432','United.States'='#0492d9',
            'British.Columbia'='#8004d9', 'California'='#32a852',
            'United.Kingdom' = '#f29807','Australia'='#7c26ad',
            'France'='#26ad7b','Netherlands'='#2655ad')


p1 = ggplot(meltedData,aes(x = date,y=value,group = variable,color = variable))+
  geom_line()+geom_smooth() +
  scale_color_manual(values=palette)+
  geom_point()+scale_y_continuous(trans='log10')+
  ylab("Nb of cases")+xlab('Progression (days)')
p1
png("Coronavirus-NorthAmericaCentered.png",width = 800,height = 800)
print(p1)
dev.off()


####################
### Adding trendline to some data of interest
####################
meltedData2 = meltedData[which(meltedData[,'variable'] %in% 
                                 c("Quebec","British.Columbia","California")),]

p2 = ggplot(meltedData2,aes(x = date,y=value,group = variable,color = variable))+
  geom_line()+geom_smooth() +
  scale_color_manual(values=palette)+
  geom_point()+scale_y_continuous(trans='log10')+
  ylab("Nb of cases")+xlab('Progression (days)')

p2
dataSubPrime = dataSub
dataSubPrime[,c("California",'Quebec','British.Columbia',"Canada")] = 
  log10(dataSub[,c('California','Quebec','British.Columbia',"Canada")])
data.lm = list()
data.lm[['Quebec']] = lm(Quebec ~ date, data = dataSubPrime[51:56,])
data.lm[['California']] = lm(California ~ date, data = dataSubPrime[51:56,])
data.lm[['British.Columbia']] = lm(British.Columbia ~ date, data = dataSubPrime[51:56,])
data.lm[['Canada']] = lm(Canada ~ date, data = dataSubPrime[51:56,])

p2 = 
  p2 + 
  geom_abline(slope = data.lm[['Quebec']]$coefficients[2], 
              intercept = data.lm[['Quebec']]$coefficients[1],
              color = palette['Quebec'],lty=2,lwd=2)+ 
  geom_abline(slope = data.lm[['California']]$coefficients[2], 
              intercept = data.lm[['California']]$coefficients[1],
              color = palette['California'],lty=2,lwd=2)+ 
  geom_abline(slope = data.lm[['British.Columbia']]$coefficients[2], 
              intercept = data.lm[['British.Columbia']]$coefficients[1],
              color = palette['British.Columbia'],lty=2,lwd=2)
  
png("Coronavirus-logTrend.png",width = 1000,height = 1000)
print(p2)
dev.off()

###############################
### plotting data aligned with first case
###############################

dQc = data[which(data[,'Quebec'] > 0),'Quebec']
names(dQc) = 1:length(dQc)
dCa = data[which(data[,'Canada'] > 0),'Canada']
names(dCa) = 1:length(dCa)
dUS = data[which(data[,'United.States'] > 0),'United.States']
names(dUS) = 1:length(dUS)
dBC = data[which(data[,'British.Columbia'] > 0),'British.Columbia']
names(dUS) = 1:length(dBC)
dCal = data[which(data[,'California'] > 0),'California']
names(dCal) = 1:length(dCal)
dUK = data[which(data[,'United.Kingdom'] > 0),'United.Kingdom']
names(dUK) = 1:length(dUK)

dataPrime = merge(merge(merge(merge(merge(dQc,dCa,by.x = 0,by.y = 0,all=T),
       dUS,by.x = 1,by.y = 0,all=T),
       dBC,by.x = 1,by.y = 0,all=T),
       dCal,by.x = 1,by.y = 0,all=T),
       dUK,by.x = 1,by.y = 0,all=T)
dataPrime[,1] = as.numeric(dataPrime[,1])
colnames(dataPrime) = c("Days.since.case.1",'Quebec',
                        'Canada','United.States','British.Columbia',
                        'California','United.Kingdom')
dataPrime = dataPrime[order(dataPrime[,1]),]
meltedDataPrime = melt(dataPrime,id.vars = c("Days.since.case.1"))

p3 = ggplot(meltedDataPrime,
            aes_string(x = "Days.since.case.1",y="value",group = "variable",
                       color = "variable"))+
  geom_smooth() +
  geom_line()+scale_color_manual(values=palette)+
  geom_point()+scale_y_continuous(trans='log10')+
  ylab("Nb of cases")+xlab('Days since case 1')

p3
png("Coronavirus-Day0.png")
print(p3)
dev.off()

##################################################
### Plotting only countries of interest
##################################################
dataSub3 = data[,c('date','day','France','Canada','United.States',
                  'Australia','United.Kingdom','Netherlands')]
meltedData3 = melt(dataSub3,id.vars = c("day","date"))

p4 = ggplot(meltedData3,aes(x = date,y=value,group = variable,color = variable))+
  geom_line()+geom_smooth() +
  scale_color_manual(values=palette)+
  geom_point()+scale_y_continuous(trans='log10')+
  ylab("Nb of cases")+xlab('Progression (days)')
p4theme_bw()

png("Coronavirus-CountryCentered.png",width = 700,height = 700)
print(p4)
dev.off()

#####################
### Highlighting the day when distancing measures came into effect
#####################
distancingMeasureDay = c("Quebec" = as.Date('3/15/20', format = "%m/%d/%y"),
                         "California" = as.Date('3/17/20', format = "%m/%d/%y"))
disMD = data.frame()
for (i in (1:length(distancingMeasureDay))){
  distMD[i,] = meltedData[which(meltedData[,'date'] == distancingMeasureDay[i] &
                                  meltedData[,'variable'] == names(distancingMeasureDay[i])),]
}

png("Coronavirus-NorthAmericaCentered-SDday.png")
p1+geom_point(data = distMD, 
              aes(x = date, y = value, fill = variable),color = 'black',size = 4,pch=21)+
  scale_fill_manual(values=palette)
dev.off()
