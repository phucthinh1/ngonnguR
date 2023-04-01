
#thiết lập thư mục làm việc

#7.1.1 
setwd("D:/Dataset")


BFCases <-read.table(file ="BirdFluCases.txt",header = TRUE)
names(BFCases)
#liệt kê các tên trog trường dữ liệu trong dataset
str(BFCases)
Cases <- rowSums(BFCases[,2:16])
names(Cases) <- BFCases[,1]
Cases
par(mfrow = c(2,2),mar = c(3,3,2,1))
pie(Cases,main = "Ordinary pie chart") #A
pie(Cases,col = gray(seq(0.4,1.0,length =6)),
    clockwise = TRUE,main = "grey colour")#B
pie(Cases,col = rainbow(6),clockwise = TRUE,
    main = "rainbow colours")#c
#install.packages("plotrix")
library(plotrix)
pie3D(Cases, labels= names(Cases),explode=0.1,
      main = "3D pie chart", labelcex = 0.6)#D

#biểu diễn biểu đồ dữ liệu cúm gia cầm
BFDeaths <- read.table(file = "Birdfludeaths.txt",
                       header = TRUE)
Deaths <- rowSums(BFDeaths[,2:16])
names(Deaths) <-BFDeaths[,1]
Deaths


par(mfrow=c(2,2),mar=c(3,3,2,1))
barplot(Cases,main = "Bird flu cases")
Counts <- cbind(Cases,Deaths)
barplot(Counts)
barplot(t(Counts),col=rainbow(c(2)))
barplot(t(Counts),beside=TRUE)
Counts
t(Counts)




#7.2.2 Biểu đồ thanh hiển thị giá trị trung bình với độ lệch chuẩn
setwd("D:/Dataset")
Benthic <- read.table(file="RIKZ2.txt",
                      header = TRUE)
Bent.M <- tapply(Benthic$Richness,
                 INDEX = Benthic$Beach,FUN = mean)
Bent.sd <- tapply(Benthic$Richness,
                  INDEX = Benthic$Beach,FUN = sd)
MSD <- cbind(Bent.M,Bent.sd)
MSD
barplot(Bent.M)
barplot(Bent.M,xlab = "Beach",ylim = c(0,20),
        ylab = "Richness",col = rainbow(9))
bp <- barplot(Bent.M,xlab= "Beach",ylim = c(0,20),
              ylab = "Richness",col =rainbow(9))
arrows(bp,Bent.M,bp,Bent.M+Bent.sd,lwd=1.5,
       angle = 90,length=0.1)
box()
bp
Benth.le<- tapply(Benthic$Richness,
                  INDEX = Benthic$Beach,FUN = length)
Bent.se <- Bent.sd / sqrt(Benth.le)
stripchart(Benthic$Richness ~ Benthic$Beach,
           vert =TRUE,pch=1,method="jitter",
           jit = 0.05,xlab="Beach",ylab="Richness")
points(1:9,Bent.M,pch=16,cex=1.5)
arrows(1:9,Bent.M,
       1:9,Bent.M+Bent.se,lwd=1.5,
       angle = 90,length = 0,1)
arrows(1:9,Bent.M,
       1:9,Bent.M-Bent.se,lwd=1.5,
       angle = 90,length=0.1)

#7.3.1 hộp dữ liệu biểu diễu dữ liệu cú
setwd("D:/Dataset")
Owls <- read.table(file = "Owls.txt",header = TRUE)
boxplot(Owls$NegPerChick)
boxplot
par(mfrow = c(2,2),mar = c(3,3,2,1))
boxplot(NegPerChick ~ SexParent,data = Owls)
boxplot(NegPerChick ~ FoodTreatment,data = Owls)
boxplot(NegPerChick ~ Sexparent * FoodTreatment,
        data = Owls)
boxplot(NegPerChick ~ SexParent * FoodTreatment,
        name = c("F/Dep","M/Dep","F/Sat","M/Sat"),
        data = Owls)
boxplot(NegPerChick ~ Nest, data = Owls)


par(mar = c(2,2,3,3))
boxplot(NegPerChick ~ Nest,data = Owls,
        axes = FALSE,ylim = c(-3,8.5))
axis(2,at=c(0,2,4,6,8))
text(x=1:27,y=-2,labels=levels(Owls$Nest),
     cex=0.75,srt=65)

#7.3.2 hộp hiển thị dữ liệu sinh vật dưới đáy
setwd("D:/Dataset")
Benthic<- read.table(file = "RIKZ2.txt",
                     header=TRUE)
Benthic.n <- tapply(Benthic$Richness,Benthic$Beach,
                    FUN = length)
Benthic.n
boxplot(Richness ~ Beach,data = Benthic,
        col="grey",xlab = "Beach",ylab = "Richness")
BP.info <- boxplot(Richness ~Beach,data = Benthic,
                   col="brown",xlab = "Beach",
                   ylab = "Richness")
BP.midp<- BP.info$stats[2,]+
  (BP.info$stats[4, ]-BP.info$stats[2,])/2
text(1:9,BP.midp,Benthic.n,col = "white",font = 2)

#7.4 biều đồ chấm cleveland

setwd("D:/Dataset")
Deer <- read.table("Deer.txt",header = TRUE)
dotchart(Deer$LCT,xlab = "Length (cm)",
         ylab = "Observation number")
dotchart(Deer$LCT,groups = factor(Deer$Sex))
Isna<-  is.na(Deer$Sex)
dotchart(Deer$LCT[!Isna],
         groups = factor(Deer$Sex[!Isna]),
         xlab = "Length (cm)",
         ylab = "Observation number grouped by sex")

#7.4.1 thêm tính trung bình vào biều đồ chấm cleveland
setwd("D:/Dataset")
Benthic<-read.table(file = "RIKZ2.txt",
                    header = TRUE)
Benthic$Beach <- factor(Benthic$Beach)
par(mfrow= c(1,2))
dotchart(Benthic$Richness,groups = Benthic$Beach,
         xlab="Richness",ylab = "Beach")
Bent.M<-tapply(Benthic$Richness,Benthic$Beach,
               FUN = mean)
dotchart(Benthic$Richness,groups = Benthic$Beach,col="blue",
         gdata=Bent.M,gpch = 10,xlab = "Richness",
         ylab = "Beach")
legend("bottomright",c("values","mean"),
       pch = c(1,19),bg = "white")
#7.5.1 xem lại hàm đã dùng trong đồ thị plot
methods(plot)


#7.5.2 hàm đồ thị cụ thể và nhiều lựa chọn

setwd("D:/Dataset")
Benthic <- read.table(file = "RIKZ2.txt",
                      header = TRUE)
Benthic$Beach <- factor(Benthic$Beach)
plot(Benthic$Richness ~ Benthic$Beach)
plot(y=Benthic$Richness,x = Benthic$NAP,
     xlab = "Mean high tide (m)",
     ylab = "Species richness",main = "Benthic data")
M0 <- lm(Richness ~NAP,data = Benthic)
abline(M0)
#mô tả bảng đồ thị với thêm trục x và trục y
plot(y= Benthic$Richness,x = Benthic$NAP,
     xlab = "Mean high tide (m)",
     ylab = "Species richness",
     xlim = c(-3,3),ylim = c(0,20))
plot(y= Benthic$Richness,x= Benthic$NAP,
     type = "n",axes = FALSE,
     xlab = "Mean high tide",
     ylab = "Species richness")
points(y = Benthic$Richness, x = Benthic$NAP)
plot(y = Benthic$Richness ,x= Benthic$NAP,
     type = "n",axes = FALSE,xlab = "Mean high tide",
     ylab = "Species richness",
     xlim = c(-1.75,2),ylim = c(0,20))
points(y= Benthic$Richness,x = Benthic$NAP)
axis(2,at= c(0,10,20),tcl = 1)
axis(1,at= c(-1.75,0,2),
     labels = c("Sea", "Water line","Dunes"))

#7.5.5 ghi chú, đọc đồ thị

legend("bottomright", c("value", "mean"),
       pch = c(1,19),bg = "white")

setwd("D:/Dataset")
Birds <- read.table(file = "loyn.txt",header = TRUE)
Birds$LOGAREA <- log10(Birds$AREA)
plot(x= Birds$LOGAREA,y = Birds$ABUND,
     xlab ="Log transformed AREA",
     ylab ="Bird abundance")
M0 <- lm(ABUND ~ LOGAREA + GRAZE,data = Birds)
summary(M0)
LAR <- seq(from = -1,to = 3,by =1)
LAR
ABUND1 <- 15.7 +7.2 *LAR
ABUND2 <- 16.1 +7.2 *LAR
ABUND3 <- 15.5 + 7.2 *LAR
ABUND4 <- 14.1 +7.2 *LAR
ABUND5 <- 3.8 +7.2 *LAR
lines(LAR, ABUND1,lty=1,lwd =1,col=1)
lines(LAR, ABUND2,lty=2,lwd =2,col=2)
lines(LAR, ABUND3,lty=3,lwd =3,col=3)
lines(LAR, ABUND4,lty=4,lwd =4,col=4)
lines(LAR, ABUND5,lty=5,lwd =5,col=5)

legend.txt <- c("Graze 1", "Graze 2","Graze 3", "Graze 4","Graze 5")
legend("topleft",legend= legend.txt,
       col = c(1,2,3,4,5),
       lty = c(1,2,3,4,5),
       lwd = c(1,2,3,4,5),
       bty = "o", cex = 1)


#7.5.6 xác định điểm

plot(y= Benthic$Richness , x= Benthic$NAP,col='blue',
     xlab = "Mean high tide (m)",
     ylab = "Species richness")#,main = "Benthic data")
indentify(y= Benthic$Richness, x = Benthic$NAP)
title("bird abundance", cex.main =2,
      family = "serif", font.main = 1)
#7.5.8 thay đổi kiểu và kích thước
title("Bird abundance", cex.main = 2,
      family = "serif", font.main = 1)

#7.5.8 thêm kí tự đặc biệt
setwd("D:/Dataset")
Whales <- read.table(file="TeethNitrogen.txt",
                     header = TRUE)
N.Moby <- Whales$X15N[Whales$Tooth == "Moby"]
Age.Moby <- Whales$Age[Whales$Tooth == "Moby"]
plot(x= Age.Moby, y = N.Moby,xlab = "Age",
     ylab = expression(paste(delta^{15},"N")))

#7.6 ghép nối
setwd("D:/Dataset")
Benthic <- read.table(file = "RIKZ2.txt",
                      header = TRUE)
pairs(Benthic[,2:9])

#7.6.1 hàm điều khiển
pairs(Benthic[,2:9], diag.panel = panel.hist,
      upper.panel = panel.smooth,
      lower.panel = panel.cor)


#7.7 coplot
setwd("D:/Dataset")
Benthic <- read.table(file = "RIKZ2.txt",
                      header = TRUE)
coplot(Richness ~ NAP | grainsize ,pch = 19,
       data= Benthic)

panel.lm = function(x,y,...){
  tmp <- lm(y~x,na.action = na.omit)
  abline(tmp)
  points(x,y,...)}
coplot(Richness ~ NAP | Beach, pch = 19,
       panel = panel.lm,data = Benthic)

#7.7.2 bảng với 2 điều kiện biến

setwd("D:/Dataset")
pHEire <- read.table(file = "SDI2003.txt",
                     header = TRUE)
pHEire$LOGAlt <- log10(pHEire$Altitude)
pHEire$Forested <- factor(pHEire$Forested)
coplot(pH ~ SDI | LOGAlt * Forested,
       panel = panel.lm,data = pHEire, number = 3,col="red")

#7.7.3 tô màu lên biểu đồ chấm 

pHEire$Temp2 <- cut(pHEire$Temperature,breaks = 2)
pHEire$Temp2.num <- as.numeric(pHEire$Temp2)
cut(pHEire$Temperature,breaks = 2)
coplot(pH ~ SDI | LOGAlt * Forested,
       panel = panel.lm,data= pHEire,
       number=3,cex = 1.5,pch =19,
       col= gray(pHEire$Temp2.num/3))

#7.8 kết hợp các loại ô
#tạo ma trận hafng cột
MyLayOut <- matrix(c(2,0,1,3), nrow = 2,ncol = 2,
                   byrow = TRUE)
MyLayOut
nf <- layout(mat = MyLayOut, widths = c(3, 1),
             heights = c(1, 3), respect = TRUE)
layout.show(nf)

xrange <- c(min(Benthic$NAP),max(Benthic$NAP))
yrange <- c(min(Benthic$Richness),max(Benthic$Richness))
#First graph
par(mar = c(4, 4, 2, 2))
plot(Benthic$NAP, Benthic$Richness, xlim = xrange,
     ylim = yrange, xlab = "NAP", ylab = "Richness")
#Second graph
par(mar = c(0, 3, 1, 1))
boxplot(Benthic$NAP, horizontal = TRUE, axes = FALSE,
        frame.plot = FALSE, ylim = xrange, space = 0)
#Third graph
par(mar = c(3, 0, 1, 1))
boxplot(Benthic$Richness, axes = FALSE,
        ylim = yrange, space = 0, horiz = TRUE)