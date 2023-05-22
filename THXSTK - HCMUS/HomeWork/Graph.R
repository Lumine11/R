#Vẽ một biểu đồ cột của hàm xác suất của phân phối siêu bội 
#với N = 100, M = 25 và cỡ mẫu, n = 15.
#H(100,35,15)
x = 0:15
m = 25
n = 100 - m
k = 15
barplot(dhyper(x,m,n,k))

#Bài 2
#Nếu X có phân phối như trên, đầu tiên tính P(5 ≤ X ≤ 12) bằng cách lấy tổng các xác suất
#được cho bởi hàm xác suất,và sau đó bằng cách sử dụng hàm phân phối tích lũy.
sum(dhyper(5:12,m,n,k))
phyper(12,m,n,k) - phyper(4,m,n,k)

##bài 3
#a) Sử dụng lệnh curve(dexp(x,0.6),0,10) để vẽ hàm mật độ xác suất của phân phối mũ
#với tham số λ = 0.6.

curve(dexp(x,0.6),0,10)

#b) Đối với đồ thị nhận được bạn vẽ thêm hàm mật độ xác suất của phân phối mũ với tham số
#λ = 0.3 (đảm bảo bạn thêm add=T trong lệnh curve).

curve(dexp(x,0.3),0,10, add =T)

#c) Sử dụng hàm phân phối tích lũy để tính diện tích bên dưới của hai hàm mật độ.

no = uniroot(function(x) dexp(x,0.6) - dexp(x,0.3) , c(0,10))$root
res = integrate(function(x) dexp(x,0.3),0,no)$value + integrate(function(x) dexp(x,0.6),no,10)$value
res

#bài 4
#Vẽ hàm xác suất của biến X ~ P(1) với x thuộc 0 tới 8.
x = 0:8
plot(x,dpois(x,1), type= "h")

#bài 5: Vẽ đồ thị hàm mật độ xác suất của biến 2 X ~  (3) với x[0,10] .
curve(dchisq(x,3), from = 0, to = 10)

#Chia cửa sổ đồ thị thành hai phần trên và dưới.

#- Trong phần dưới, vẽ đồ thị của hàm xác suất của biến X ~ P(4) với x{0,,50} với cùng
#lựa chọn: ylim=c(0,0.25).
#(Điều này minh họa kết quả là khi n đủ lớn và np đủ nhỏ ta có thể xấp xỉ phân phối nhị thực
# B(n, p) bằng luật Poisson P(np)

#- Trong phần trên, vẽ đồ thị của hàm xác suất của biến X ~ B(50,0.08) lấy
#ylim=c(0,0.25).
x=0:50
plot(x,dbinom(x,50,0.08),type = "h", ylim = c(0,0.25))
