library(midasr)
library(zoo)
library(forecast)
library(ggplot2)
library(MLmetrics)
library(Metrics)
################################
ru1 <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data3/Russia.csv',sep = ",")
al1 <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data3/Germany.csv',sep = ",")
uk1 <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data3/Ukraine.csv',sep = ",")
bi1 <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data3/United Kingdom.csv',sep = ",")
fr1 <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data3/France.csv',sep = ",")
ir1 <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data3/Iran.csv',sep = ",")
ho1 <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data3/Netherlands.csv',sep = ",")

ru2 <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data2/Russia.csv',sep = ",")
al2 <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data2/Germany.csv',sep = ",")
uk2 <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data2/Ukraine.csv',sep = ",")
bi2 <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data2/United Kingdom.csv',sep = ",")
fr2 <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data2/France.csv',sep = ",")
ir2 <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data2/Iran.csv',sep = ",")
ho2 <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data2/Netherlands.csv',sep = ",")



ruk = mean(head(ru2,4)[,3]) / mean(tail(ru1,4)[,3])
alk = mean(head(al2,4)[,3]) / mean(tail(al1,4)[,3]) 
ukk = mean(head(uk2,4)[,3]) / mean(tail(uk1,4)[,3]) 
bik = mean(head(bi2,4)[,3]) / mean(tail(bi1,4)[,3]) 
frk = mean(head(fr2,4)[,3]) / mean(tail(fr1,4)[,3]) 
irk = mean(head(ir2,4)[,3]) / mean(tail(ir1,4)[,3]) 
hok = mean(head(ho2,4)[,3]) / mean(tail(ho1,4)[,3]) 



expense <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data3/expense.csv',sep = ",")
expenseps <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data3/expenseps.csv',sep = ",")
names(expense) = c("Date","Value")
names(expenseps) = c("Date","Value")


tourist1 <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data3/tourist.csv',sep = ",")
tourist2 <- read.csv(file = 'C:/Users/HP/Google Drive/tez/data2/tourist.csv',sep = ",")
names(tourist2) = c("date","value")
#country_list <- c(2080236,1079564,914256,807334,285086,257696,250956)
country_list <- c(2080236,1079564,914256,807334,285086,257696,250956)
weights <- country_list / sum(country_list)

exlude_list = c("2011-10-30",
                "2012-01-29","2012-04-29","2012-07-29","2012-09-30","2012-12-30",
                "2013-03-31","2013-06-30","2013-09-29","2013-12-29",
                "2014-03-30","2014-06-29","2014-08-31","2014-11-30",
                "2015-03-29","2015-05-31","2015-08-30","2015-11-29",
                "2016-01-31","2016-05-29","2016-07-31","2016-10-30",
                "2017-01-29","2017-04-30","2017-07-30","2017-10-29","2017-12-31",
                "2018-04-29","2018-07-29","2018-09-30","2018-12-30",
                "2019-03-31","2019-06-30","2019-09-29","2019-12-29",
                "2020-03-29","2020-05-31","2020-08-30","2020-11-29",
                "2021-01-03","2021-05-30","2021-08-29")



al1[,4] <- as.numeric(replace(al1[,4],al1[,4]=="<1",0))
al1[,5] <- as.numeric(replace(al1[,5],al1[,5]=="<1",0))
al1$"finalindex" <- rowSums(al1[,2:5])
bi1[,4] <- as.numeric(replace(bi1[,4],bi1[,4]=="<1",0))
bi1[,5] <- as.numeric(replace(bi1[,5],bi1[,5]=="<1",0))
bi1$"finalindex" <- rowSums(bi1[,2:5])
ho1[,4] <- as.numeric(replace(ho1[,4],ho1[,4]=="<1",0))
ho1[,5] <- as.numeric(replace(ho1[,5],ho1[,5]=="<1",0))
ho1$"finalindex" <- rowSums(ho1[,2:5])
fr1[,2] <- as.numeric(replace(fr1[,2],fr1[,2]=="<1",0))
fr1[,3] <- as.numeric(replace(fr1[,3],fr1[,3]=="<1",0))
fr1[,4] <- as.numeric(replace(fr1[,4],fr1[,4]=="<1",0))
fr1[,5] <- as.numeric(replace(fr1[,5],fr1[,5]=="<1",0))
fr1$"finalindex" <- rowSums(fr1[,2:5])
ir1[,4] <- as.numeric(replace(ir1[,4],ir1[,4]=="<1",0))
ir1[,5] <- as.numeric(replace(ir1[,5],ir1[,5]=="<1",0))
ir1$"finalindex" <- rowSums(ir1[,2:5])
ru1[,4] <- as.numeric(replace(ru1[,4],ru1[,4]=="<1",0))
ru1[,5] <- as.numeric(replace(ru1[,5],ru1[,5]=="<1",0))
ru1$"finalindex" <- rowSums(ru1[,2:5])
uk1[,4] <- as.numeric(replace(uk1[,4],uk1[,4]=="<1",0))
uk1[,5] <- as.numeric(replace(uk1[,5],uk1[,5]=="<1",0))
uk1$"finalindex" <- rowSums(uk1[,2:5])

al2[,4] <- as.numeric(replace(al2[,4],al2[,4]=="<1",0))
al2[,5] <- as.numeric(replace(al2[,5],al2[,5]=="<1",0))
al2$"finalindex" <- rowSums(al2[,2:5])
bi2[,4] <- as.numeric(replace(bi2[,4],bi2[,4]=="<1",0))
bi2[,5] <- as.numeric(replace(bi2[,5],bi2[,5]=="<1",0))
bi2$"finalindex" <- rowSums(bi2[,2:5])
ho2[,4] <- as.numeric(replace(ho2[,4],ho2[,4]=="<1",0))
ho2[,5] <- as.numeric(replace(ho2[,5],ho2[,5]=="<1",0))
ho2$"finalindex" <- rowSums(ho2[,2:5])
fr2[,2] <- as.numeric(replace(fr2[,2],fr2[,2]=="<1",0))
fr2[,3] <- as.numeric(replace(fr2[,3],fr2[,3]=="<1",0))
fr2[,4] <- as.numeric(replace(fr2[,4],fr2[,4]=="<1",0))
fr2[,5] <- as.numeric(replace(fr2[,5],fr2[,5]=="<1",0))
fr2$"finalindex" <- rowSums(fr2[,2:5])
ir2[,4] <- as.numeric(replace(ir2[,4],ir2[,4]=="<1",0))
ir2[,5] <- as.numeric(replace(ir2[,5],ir2[,5]=="<1",0))
ir2$"finalindex" <- rowSums(ir2[,2:5])
ru2[,4] <- as.numeric(replace(ru2[,4],ru2[,4]=="<1",0))
ru2[,5] <- as.numeric(replace(ru2[,5],ru2[,5]=="<1",0))
ru2$"finalindex" <- rowSums(ru2[,2:5])
uk2[,4] <- as.numeric(replace(uk2[,4],uk2[,4]=="<1",0))
uk2[,5] <- as.numeric(replace(uk2[,5],uk2[,5]=="<1",0))
uk2$"finalindex" <- rowSums(uk2[,2:5])

ru = data.frame(Hafta = c(head(ru1$Hafta, -4),ru2$Hafta),finalindex = c(head(ru1$finalindex, -4) * ruk,ru2$finalindex))
al = data.frame(Hafta = c(head(al1$Hafta, -4),al2$Hafta),finalindex = c(head(al1$finalindex, -4) * alk,al2$finalindex))
uk = data.frame(Hafta = c(head(uk1$Hafta, -4),uk2$Hafta),finalindex = c(head(uk1$finalindex, -4) * ruk,uk2$finalindex))
bi = data.frame(Hafta = c(head(bi1$Hafta, -4),bi2$Hafta),finalindex = c(head(bi1$finalindex, -4) * bik,bi2$finalindex))
fr = data.frame(Hafta = c(head(fr1$Hafta, -4),fr2$Hafta),finalindex = c(head(fr1$finalindex, -4) * frk,fr2$finalindex))
ir = data.frame(Hafta = c(head(ir1$Hafta, -4),ir2$Hafta),finalindex = c(head(ir1$finalindex, -4) * irk,ir2$finalindex))
ho = data.frame(Hafta = c(head(ho1$Hafta, -4),ho2$Hafta),finalindex = c(head(ho1$finalindex, -4) * hok,ho2$finalindex))

tourist = head(data.frame(Date = c(head(tourist1$date, -1),tourist2$date),Value = c(head(tourist1$value, -1),tourist2$value)),-1)
expense$Value <- as.numeric(expenseps$Value) * as.numeric(tourist$Value) / 1000

al <- subset(al, !(Hafta %in% exlude_list))
bi <- subset(bi, !(Hafta %in% exlude_list))
ho <- subset(ho, !(Hafta %in% exlude_list))
ir <- subset(ir, !(Hafta %in% exlude_list))
ru <- subset(ru, !(Hafta %in% exlude_list))
uk <- subset(uk, !(Hafta %in% exlude_list))
fr <- subset(fr, !(Hafta %in% exlude_list))



weighted_inputs <-  weights[1]*ru$"finalindex" + weights[2]*al$"finalindex" + weights[3]*uk$"finalindex" +
  weights[4]*bi$"finalindex" + weights[5]*fr$"finalindex" + weights[6]*ir$"finalindex" + weights[7]*ho$"finalindex"



weighted_inputs_2 <-  (ru$"finalindex" + al$"finalindex" + uk$"finalindex" + bi$"finalindex" + fr$"finalindex" +
                         ir$"finalindex" + ho$"finalindex")/ 7


final_data = data.frame(ru[,1],weighted_inputs)
final_noist = data.frame(ru[,1],weighted_inputs_2)
final_c = data.frame(ru[,1],ho$finalindex)

#for (z in 1:10) {
#  asd = as.numeric(rownames(final_data[diff(final_data$weighted_inputs) > 20,])) - 1
#  final_data[c(FALSE,diff(final_data$weighted_inputs) > 20),"weighted_inputs"] = final_data[c(asd),"weighted_inputs"]
#  rownames(final_data) <- 1:nrow(final_data)}
#for (z in 1:10) {
#  asd = as.numeric(rownames(final_noist[diff(final_noist$weighted_inputs) > 20,])) - 1
#  final_noist[c(FALSE,diff(final_noist$weighted_inputs) > 20),"weighted_inputs_2"] = final_noist[c(asd),"weighted_inputs_2"]
#  rownames(final_noist) <- 1:nrow(final_noist)}
#for (z in 1:10) {
#  asd = as.numeric(rownames(final_c[diff(final_c[,2]) > 20,])) - 1
#  final_c[c(FALSE,diff(final_c[,2]) > 20),"weighted_inputs_2"] = final_c[c(asd),2]
#  rownames(final_c) <- 1:nrow(final_c)}


fin_da = read.zoo(final_data, format = "%Y-%m-%d")
fin_no = read.zoo(final_noist, format = "%Y-%m-%d")
fin_c = read.zoo(final_c, format = "%Y-%m-%d")
fin_to = read.zoo(tourist, format = "%d.%m.%Y")

asd = as.numeric(rownames(final_data[diff(final_data$weighted_inputs) > 20,])) - 1
final_data[c(FALSE,diff(final_data$weighted_inputs) > 20),"weighted_inputs"] = final_data[c(asd),"weighted_inputs"]

fin_no[diff(fin_da) > 20]

x1 <- diff(log(fin_da),48)
x2 <- diff(log(fin_no),48)
x3 <- diff(log(fin_c),48)
y <- diff(log(fin_to),12)

x33 = tail(rollmean(x1,3),-2)
y33 = tail(y,-1)


xx <- window(x2, start = as.Date("2010-12-01"), end =as.Date("2023-08-30"))
yy <- window(y, start = as.Date("2010-12-01"), end =as.Date("2023-08-30"))


#xx <- window(x2, start = as.Date("2010-01-01"), end =as.Date("2019-12-30"))
#yy <- window(y, start = as.Date("2010-01-01"), end =as.Date("2019-12-30"))

#xx <- window(x2, start = as.Date("2018-12-01"), end =as.Date("2023-08-30"))
#yy <- window(y, start = as.Date("2018-12-01"), end =as.Date("2023-08-30"))

length(yy)
i = 95

xxx <- as.numeric(xx)[1:(i*4)]
length(xxx)
yyy <- as.numeric(yy)[1:i]
length(yyy)*4

#xxx <- as.numeric(xx)
#length(xxx)
#yyy <- as.numeric(yy)
#length(yyy)*4

beta0 <- midas_r(yyy ~ mls(yyy, c(1,2,3,12), 1)  + mls(xxx, 0:3,4,nbeta), start = list(xxx = c(1, 1, 1)), 
                 Ofunction = "optim", method = "Nelder-Mead")
summary(beta0)

nealmo <- midas_r(yyy ~ mls(yyy, c(1,2,3,12), 1) + mls(xxx, 0:3, 4, nealmon), start = list(xxx = c(1, 1)), 
                  Ofunction = "optim", method = "Nelder-Mead")
summary(nealmo)


################################
plot(x1)
plot(x2)
plot(y)
################################

################################
xxx <- as.numeric(xx)
yyy <- as.numeric(yy)
fulldata <- list(xxx = xxx, yyy = yyy)
#insample <- 1:round(length(yyy)*3/4)
insample = 1:i
outsample <- (1:length(fulldata$yyy))[-insample]
#outsample = 96:107
avgf <- average_forecast(list(beta0), data = fulldata, insample = insample, outsample = outsample)
sqrt(avgf$accuracy$individual$MSE.in.sample)
avgf$accuracy$individual

fitARIMA <- arima(yyy[insample], order=c(1,0,1),method="ML")
library(lmtest)
coeftest(fitARIMA)
fcar = forecast(fitARIMA,h=length(outsample))
summary(fcar)

################################
pol = c()
for (ol in 1:length(outsample)) {
  fitARIMA <- arima(yyy[1:(i+ol-1)], order=c(1,0,1),method="ML")
  fkar = forecast(fitARIMA,h=1)$mean
  pol = append(pol,fkar)
  
}
fcar = pol
fore = forecast(beta0,fulldata)
fore2 = forecast(nealmo,fulldata)
#fore$mean = polx
#fore2$mean = polx2

rmse(yyy[outsample],tail(fore$mean,length(outsample)))
mse(yyy[outsample],tail(fore$mean,length(outsample)))
mape(yyy[outsample],tail(fore$mean,length(outsample)))
mase(yyy[outsample],tail(fore$mean,length(outsample)))

rmse(yyy[outsample],tail(fore2$mean,length(outsample)))
mse(yyy[outsample],tail(fore2$mean,length(outsample)))
mape(yyy[outsample],tail(fore2$mean,length(outsample)))
mase(yyy[outsample],tail(fore2$mean,length(outsample)))

rmse(yyy[outsample],pol)
mse(yyy[outsample],pol)
mape(yyy[outsample],pol)
mase(yyy[outsample],pol)

mainy = data.frame(Date = as.Date(index(yy)[round(i/1.2):length(yyy)]),Value = yyy[round(i/1.2):length(yyy)])
mainx = data.frame(Date = as.Date(index(yy)[round(i/1.2):length(yyy)]),Value = c(yyy[round(i/1.2):i],tail(fore$mean,length(outsample))))
mainx2 = data.frame(Date = as.Date(index(yy)[round(i/1.2):length(yyy)]),Value = c(yyy[round(i/1.2):i],tail(fore2$mean,length(outsample))))
mainarim = data.frame(Date = as.Date(index(yy)[round(i/1.2):length(yyy)]),Value = c(yyy[round(i/1.2):i],fcar))


pasd <- ggplot() +
  geom_line(mainx, mapping = aes(x=Date, y=Value, color = "Almon MIDAS model")) + 
  geom_point(mainx, mapping = aes(x=Date, y=Value, color = "Almon MIDAS model")) +
  geom_line(mainx2, mapping = aes(x=Date, y=Value, color = "Beta MIDAS model")) + 
  geom_point(mainx2, mapping = aes(x=Date, y=Value, color = "Beta MIDAS model")) +
  geom_line(mainarim, mapping = aes(x=Date, y=Value, color = "ARIMA model")) + 
  geom_point(mainarim, mapping = aes(x=Date, y=Value, color = "ARIMA model")) +
  geom_line(mainy, mapping = aes(x=Date, y=Value, color = "Tourist Arrivals")) +
  geom_point(mainy, mapping = aes(x=Date, y=Value, color = "Tourist Arrivals")) +
  ylab("YoY Logarithm of Tourist Arrivals") +
  xlab("") +
  scale_color_manual(name = " ", 
                     values = c("Almon MIDAS model" = "red", 
                                "Beta MIDAS model" = "blue",
                                "ARIMA model" = "darkgreen",
                                "Tourist Arrivals" = "black")) +
  geom_vline(xintercept = as.numeric(as.Date("2020-8-01")), linetype=1) +
  theme_bw() +
  theme(legend.position="bottom")
pasd


#fcar = forecast(fitARIMA,h=12)

dm.test(sqrt((tail(fore2$mean,length(outsample)) - yyy[outsample])^2),sqrt((pol - yyy[outsample])^2))


dm.test(sqrt((tail(fore$mean,length(outsample)) - yyy[outsample])^2),sqrt((pol - yyy[outsample])^2))




################################[49:60]
names(tourist) = c("Date","Value")
test = tourist$Value
expensezz = expense$Value/1000
testdate = as.Date(tourist$Date, format = "%d.%m.%Y")
#(tail(fore$mean,length(outsample))/100) * test[49:60] + test[49:60]
#(fcar$mean/100) * test[49:60] + test[49:60]

mainyy = data.frame(Date = testdate[79:119],Value = (expensezz[79:119]))
mainyyyy = data.frame(Date = testdate[79:119],Value = (c(expensezz[79:107],exp(log(test[96:107]) + yyy[96:length(yyy)]))))

                      
mainxx = data.frame(Date = testdate[79:119],Value = (c(expensezz[79:107],exp(log(test[96:107]) + tail(fore$mean,length(outsample)))*0.834/1000)))
mainxx2 = data.frame(Date = testdate[79:119],Value = (c(expensezz[79:107],exp(log(test[96:107]) + tail(fore2$mean,length(outsample)))*0.834/1000)))
mainarimm = data.frame(Date = testdate[79:119],Value = (c(expensezz[79:107],exp(log(test[96:107]) + fcar)*0.834/1000)))

exp(diffinv(c(yyy[1:36],tail(fore$mean,length(outsample))),12)*100)


plot(mainxx$Value,type="l")
lines(mainarimm$Value,type="l")
lines(mainyy$Value,type="l")

pasd2 <- ggplot() +
  geom_line(mainxx, mapping = aes(x=Date, y=Value, color = "Almon MIDAS model")) + 
  geom_point(mainxx, mapping = aes(x=Date, y=Value, color = "Almon MIDAS model")) +
  geom_line(mainxx2, mapping = aes(x=Date, y=Value, color = "Beta MIDAS model")) + 
  geom_point(mainxx2, mapping = aes(x=Date, y=Value, color = "Beta MIDAS model")) +
  geom_line(mainarimm, mapping = aes(x=Date, y=Value, color = "ARIMA model")) + 
  geom_point(mainarimm, mapping = aes(x=Date, y=Value, color = "ARIMA model")) +
  geom_line(mainyy, mapping = aes(x=Date, y=Value, color = "Tourist Arrivals")) +
  geom_point(mainyy, mapping = aes(x=Date, y=Value, color = "Tourist Arrivals")) +
  ylab("Tourism income (in thousands)") +
  xlab("") +
  scale_color_manual(name = " ", 
                     values = c("Almon MIDAS model" = "red", 
                                "Beta MIDAS model" = "blue",
                                "ARIMA model" = "purple",
                                "Tourist Arrivals" = "black")) +
  geom_vline(xintercept = as.numeric(as.Date("2020-8-01")), linetype=1) +
  theme_bw() +
  theme(legend.position="bottom")
pasd2













#sum(mainxx$Value - mainyy$Value)
#sum(mainarimm$Value - mainyy$Value)
################################
#asdasd = dm.test(residuals(fitARIMA)[25:length(residuals(fitARIMA))],residuals(beta0))


#fitARIMA <- arima(yyy[insample], order=c(1,0,1),method="ML")
#fffx = forecast(fitARIMA,h=24)$mean
#asdasd2 = dm.test(fffx,residuals(beta0))

#fitARIMA <- arima(yyy[insample], order=c(1,0,1),method="ML")
#fff = forecast(fitARIMA,h=24)$mean


#kek2 = sqrt((mainx$Value - mainy$Value)^2)[13:24]
#kek3 = sqrt((mainarim$Value - mainy$Value)^2)[13:24]
#kek4 = sqrt((mainx2$Value - mainy$Value)^2)[13:24]
#kek5 = sqrt((fff - mainy$Value)^2)[13:24]

#asdasd3 = dm.test(kek5,kek2)
#asdasd3
