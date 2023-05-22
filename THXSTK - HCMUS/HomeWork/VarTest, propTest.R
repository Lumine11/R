setwd('D:/XSTK')
cholesterol = read.table("cholesterol.txt", header = T)
attach(cholesterol)
cholesterol

var.test(Before, After)
# Kiểm định phương sai
alpha = 0.05
v.equal = ifelse(var.test(Before, After)$p.value < alpha, FALSE, TRUE);
v.equal

# Kiểm định t.test
t.test(Before, After, alternative =  "greater", val.equal = v.equal, 
 conf.level = 0.95,paired = TRUE)

test.leq.oneside = function(x,y,mu0, alpha){
  d = x - y
  d.bar = mean(d)
  s = sd(d)
  n = length(d)
  t0 = d.bar/s*sqrt(n)
  
  p.value = pt(t0, n-1)
  cat('p.value = ', p.value)
  cat("\n");
  cat("alpha = ", alpha)
  cat("\n");
  if (p.value < alpha)
  cat("Bac bo H0")
  else 
    cat("K du co so bac bo H0")
}


############################ BÀI 7
rm(list = ls())
y = c(15,8)
n = c(300,300)
prop.test(y,n, alternative = "two.sided", conf.level =  0.975)


#####250,400,550,317,425,289,389,55p############ 
p = c(300,250,400,550,317,389,425,289,389,559)
nb = c(3,3,4,5,4,3,6,3,4,5)

plot(nb,p)



