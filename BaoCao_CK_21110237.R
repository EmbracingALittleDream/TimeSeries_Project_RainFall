#Tai thu vien
library(astsa)
library(forecast)
library(fUnitRoots)

#Tai du lieu
rainFallRaw <- read.csv("C:/Users/Admin/Downloads/Rainfall_data.csv")

#Thong ke mo ta
dim(rainFallRaw) #Tim so quan trac va tim so bien 
str(rainFallRaw) #Xac dinh ten bien va kieu du lieu cua bien

#Lam sach du lieu

subRainFall <- rainFallRaw[, c(1,7)] 
subRainFall #Lay tap hop con cua du lieu

boxplot(subRainFall$Precipitation) #Quan sat ngoai lai
sum(is.na(subRainFall$Precipitation)) #Tinh tong du lieu khuyet

class(subRainFall) #Kiem tra loai du lieu

rainFall.ts <- ts(subRainFall$Precipitation, start = 2000, frequency = 12)
rainFall.ts #Chuyen du lieu ve kieu du lieu timeseries

lRainFall <- log(rainFall.ts)
lRainFall #Lay loganepe cho timeseries

lRainFall[lRainFall == -Inf] <- 0 
lRainFall #Thay nhung gia tri khong xac dinh bang gia tri 0 nhu ban dau

#Truc quan hoa du lieu

par(mfrow = c(2,1)) #Dat lai cach bieu dien do thi

plot.ts(rainFall.ts,
        main = "Rain Fall",
        xlab = "Nam",
        ylab = "Luong Mua",
        axes = FALSE)
axis(2); axis(1, at = seq(2000, 2020, by = 1)); box()
abline(v = 2000:2020, lty = 2, col = gray(0.7))

plot.ts(lRainFall,
        main = " Log Rain Fall",
        xlab = "Nam",
        ylab = "Luong Mua",
        axes = FALSE)
axis(2); axis(1, at = seq(2000, 2020, by = 1)); box()
abline(v = 2000:2020, lty = 2, col = gray(0.7))

DlRainFall <- diff(lRainFall, 12)
DlRainFall #Lay sai phan theo mua
#Ghi chu: lay sai phan theo mua de lam mat tinh mua vu(sesonal) 

par(mfrow = c(1,1))

#Quan sat du lieu co dung va mat tinh mua vu hay chua
plot(DlRainFall, axes = FALSE )
axis(2); axis(1, at = seq(2000, 2020, by = 1)); box()
abline(v = 2000:2020, lty = 2, col = gray(0.7))

#Tim mo hinh phu hop voi tap du lieu

acf2(DlRainFall) #Xac dinh bac cua mo hinh
#Ghi chu: bac cua mo hinh la p,q, P,Q

#SARIMA(1,0,1)x(1,1,1)_{12}
mod1 <- sarima(lRainFall, 1,0,1, 1,1,1, 12) #Mo hinh cho?` 1
#Nhan xet: uoc luong sar1 khong co y nghia thong ke

#SARIMA(1,0,1)x(0,1,1)_{12}
mod2 <- sarima(lRainFall, 1,0,1, 0,1,1, 12) #Mo hinh cho?` 2
#Nhan xet: cac he so cua mo hinh phu hop voi bo du lieu

#Du bao luong mua 12 thang cua nam 2021 tai Mumbai
predMod <- sarima.for(lRainFall, 12, 1,0,1, 0,1,1, 12)

shapiro.test(resid(mod2$fit)) #Kiem dinh gia dinh phan phoi chuan

pred.Value <- predMod$pred
pred.Value #Gia tri du bao luong mua 12 thang cua nam 2021 

se.Value <- predMod$se
se.Value #Sai so du bao luong mua 12 thang cua nam 2021

predRainFall <- exp(pred.Value); predRainFall #Gia tri du bao thuc te 
seData <- exp(se.Value)

U1 <- exp(pred.Value + (2 * se.Value))
L1 <- exp(pred.Value - (2 * se.Value))
cbind(L1, U1)

plot(rainFall.ts, 
     type = 'l',
     xlim = c(2000, 2022),
     ylim = c(0, 1600),
     xlab = "Year",
     ylab = "Luong mua",
     axes = FALSE)
axis(2); axis(1, at = seq(2000, 2022, by = 1)); box()
abline(v = 2000:2022, lty = 2, col = gray(0.7))
lines(predRainFall, type = "o", col = "darkgreen", lwd = 2)
Months <- c("J","F","M","A","M","J","J","A","S","O","N","D")
points(predRainFall, pch = Months, cex = 1, font = 4, col = 1:4)
legend("topleft",
       legend = c("RainFall","RainFall Forecast"),
       col=c("black","darkgreen"),
       lty = 1, lwd = 2, pch = 1, box.lwd = 0.5 )
xx1 <- c(time(U1), rev(time(U1)))
yy1 <- c(L1, rev(U1))
polygon(xx1 ,yy1 ,border = 8, col= gray(.5, alpha = .2))

