# Bai tap thuc hanh so 3
# Lop:21CTT5C
# Ho ten: Tran Thi Kim Trinh /MSSV: 21120580

setwd("D:/RWorks")
###Cau 1
data = read.csv("Profit.csv", header = T)
names(data)
attach(data)
#Cau a
#KTC 95% cho sai khac trung binh doanh thu ca 2 quan:
t.test(Dist.1,Dist.3, alternative = "two.side", var.equal = F)

##Ta duoc ket qua sau khi chay, KTC 95% can tim la:
 ## 7.457951 41.407851

#Cau b
#H0: p1 = p2
#H1: p1 > p2

n = c(length(Dist.1), length(Dist.3))
y1 = length(Dist.1[Dist.1 > 600])
y2 = length(Dist.3[Dist.3 > 600])

prop.test(c(y1,y2), n, alternative = "greater")
cat('p-value = ', prop.test(c(y1,y2), n, alternative = "greater")$p.value)

#Ket qua cho ta p-value = 0.1661 > alpha = 0.05
# => chua du co so bac bo gia thuyet H0 voi muc y nghia 5%

#Ket luan: voi muc y nghia 5%, ti le cac ngay co doanh so cao o quan 1 bang quan 3

#Cau c
#H0: px = py
#H1: px > py
prop.test.geq = function(x,y,alpha){
  n1 = length(x)
  n2 = length(y)
  y1 = length(x[x > 600])
  y2 = length(y[y > 600])
  p1.hat = y1/n1
  p2.hat = y2/n2
  p.hat = (y1+y2)/(n1+n2)
  
  z0 = (p1.hat - p2.hat)/sqrt(p.hat*(1-p.hat)*(1/n1 + 1/n2))

  #P{Z>=Z0} = 1 - P{Z <= Z0} = 1 - pnorm(z0)
  p.val = 1 - pnorm(z0);
  cat('p-value = ', p.val, '\n')
  if (p.val < alpha)
  {
    cat('Bac bo H0')
  }
  else
    cat('Khong bac bo H0')
}

#Ap dung cho cau b
prop.test.geq(Dist.1, Dist.3, 0.05)

###Cau 2
#Cau a
#su thay doi cua nhiet do khong khi theo do cao
x = c(600,1000,1250,1600,1800,2100,2500,2900)
y = c(56,54,56,50,47,49,47,45)

plot(x,y) #đo thi phan tan
abline(lm(y~x))

#Cau b
#Hoi quy
lm(y~x)

#Ta thay Intercept <=> B0 = 59.290662,
#B1 = -0.005115
#=> y = 59.290662 - 0.005115x
#Kinh nghiem noi giam 9.8C/km <=> 49.64F/3280,8 ft
# <=> khi do cao tang 3280.8ft thi nhiet do giam 49.64F
#Kiem tra:y1 = 59.290662 - 0.005115(x + 3280.8)
#           = 59.290662 - 0.005115x - 16.781292
# => chi giam 16.78F => kinh nghiem sai
