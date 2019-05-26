#!/usr/bin/env Rscript
##################################
# Author : cndaqiang             #
# Update : 2019-05-23            #
# Build  : 2019-05-11            #
# What   : 画波函数fft 3d图      #
##################################
#头文件
#函数库rplot dos.dat 13 x y title 

library(scatterplot3d)
png()
#读入数据
datafile="fxy.dat"
mydata=read.table(datafile)
N=sqrt(length(mydata[,1]))
x0=mydata[1,1]
x1=mydata[2,1]
dx=x1-x0
x=c(0:(N-1))*dx+x0
y=x
z=matrix(mydata[,3],byrow=F,nrow=N)
#contour(x,y,z,main="fxy",nlevels=0.001)
#image(x, y, z)
filled.contour(x,y,z,col = rainbow(10),nlevels=(10),xla="x",ylab="y",main="f(x,y)") #,col=topo.colors(10))

#fk
datafile="fk.dat"
mydata=read.table(datafile)
N=sqrt(length(mydata[,1]))
x0=mydata[1,1]
x1=mydata[2,1]
dx=x1-x0
x=c(0:(N-1))*dx+x0
y=x
z=matrix(mydata[,3],byrow=F,nrow=N)
#contour(x,y,z,main="fkxy")
filled.contour(x,y,z,col = rainbow(10),nlevels=(10),xla="px",ylab="py",main="f(x,y)->FFT->f(px,py)")
persp(x,y,z,xla="px",ylab="py",zlab="f(px,py)",main="f(x,y)->FFT->f(px,py)",ticktype="detailed",theta=-45, phi=20)
#theta是x,y平面绕z轴转, phi是偏离z轴，角度制
#image(x, y, z)


#
datafile="ffxy.dat"
mydata=read.table(datafile)
N=sqrt(length(mydata[,1]))
x0=mydata[1,1]
x1=mydata[2,1]
dx=x1-x0
x=c(0:(N-1))*dx+x0
y=x
z=matrix(mydata[,3],byrow=F,nrow=N)
#contour(x,y,z,main="ffxy")
filled.contour(x,y,z,col = rainbow(10),nlevels=(10),xla="x",ylab="y",main="f(px,py)->IFFT->f(x,y)")
dev.off()