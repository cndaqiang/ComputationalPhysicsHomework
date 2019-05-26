#!/usr/bin/env Rscript
##################################
# Author : cndaqiang             #
# Update : 2019-05-23            #
# Build  : 2019-05-11            #
# What   : 画波函数fft图         #
##################################
#头文件
#函数库rplot dos.dat 13 x y title 
datafile="result.dat"

#读入数据
mydata=read.table(datafile,head=T)
png()
plot(mydata[,1],mydata[,2],type="l",main="f(x)", xlab = "x",  ylab = "abs(f(x))")
plot(mydata[,3],mydata[,4],main="f(x)->FFT->f(p)", xlab = "p",  ylab = "abs(f(p))")

#----下面为加text
minfft=max(mydata[,4])/10
peak=mydata[,4] > minfft
text(mydata[peak,3],mydata[peak,4],format(mydata[peak,3],digits = 1),cex=1,pos=1,col="red")


plot(mydata[,5],mydata[,6],type="l",main="f(p)->IFFT->f(x)", xlab = "x",  ylab = "abs(f(x))")

dev.off()