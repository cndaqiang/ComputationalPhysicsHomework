#!/usr/bin/env Rscript
##################################
# Author : cndaqiang             #
# Update : 2019-05-23            #
# Build  : 2019-05-11            #
# What   : 画波函数fft 3D图      #
##################################
#头文件
#函数库rplot dos.dat 13 x y title 

library(rgl)
#读入数据
datafile="fk.dat"
mydata=read.table(datafile)


plot3d(mydata[,1],mydata[,2],mydata[,3])
rgl.snapshot("fk.png")


surface3d(mydata[,1],mydata[,2],mydata[,3])



disp,mpg,col="red",size=5)


N=sqrt(length(mydata[,1]))
x0=mydata[1,1]
xe=mydata[N,1]
z=matrix(mydata[,3],byrow=T,nrow=N)