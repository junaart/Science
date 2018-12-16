amount<-c(100000, 500000, 1000000, 5000000)
capacity<-c(1000000, 10000000, 100000000, 500000000)
d<-read.table("file.txt",header = FALSE, sep=";", dec = ".")


time_cs_capacity_1<-matrix(0,nrow=4,ncol=30)
time_stable_capacity_1<-matrix(0,nrow=4,ncol=30)
time_nonstable_capacity_1<-matrix(0,nrow=4,ncol=30)

counter<-1

for (i in 1:4)
{
  for (j in 1:30)
  {
    time_cs_capacity_1[i,j]<-d[counter,1];
    counter <- counter +1;
    time_stable_capacity_1[i,j]<-d[counter,1];
    counter <- counter +1;
    time_nonstable_capacity_1[i,j]<-d[counter,1];
    counter <- counter +1;
  }
}



time_cs_capacity_2<-matrix(0,nrow=4,ncol=30)
time_stable_capacity_2<-matrix(0,nrow=4,ncol=30)
time_nonstable_capacity_2<-matrix(0,nrow=4,ncol=30)


for (i in 1:4)
{
  for (j in 1:30)
  {
    time_cs_capacity_2[i,j]<-d[counter,1];
    counter <- counter +1;
    time_stable_capacity_2[i,j]<-d[counter,1];
    counter <- counter +1;
    time_nonstable_capacity_2[i,j]<-d[counter,1];
    counter <- counter +1;
  }
}

time_cs_capacity_3<-matrix(0,nrow=4,ncol=30)
time_stable_capacity_3<-matrix(0,nrow=4,ncol=30)
time_nonstable_capacity_3<-matrix(0,nrow=4,ncol=30)


for (i in 1:4)
{
  for (j in 1:30)
  {
    time_cs_capacity_3[i,j]<-d[counter,1];
    counter <- counter +1;
    time_stable_capacity_3[i,j]<-d[counter,1];
    counter <- counter +1;
    time_nonstable_capacity_3[i,j]<-d[counter,1];
    counter <- counter +1;
  }
}

time_cs_capacity_4<-matrix(0,nrow=4,ncol=30)
time_stable_capacity_4<-matrix(0,nrow=4,ncol=30)
time_nonstable_capacity_4<-matrix(0,nrow=4,ncol=30)


for (i in 1:4)
{
  for (j in 1:30)
  {
    time_cs_capacity_4[i,j]<-d[counter,1];
    counter <- counter +1;
    time_stable_capacity_4[i,j]<-d[counter,1];
    counter <- counter +1;
    time_nonstable_capacity_4[i,j]<-d[counter,1];
    counter <- counter +1;
  }
}

plot(1:30, time_nonstable_capacity_1[4,], col="black", panel.first=grid(), pch = 23, ylim=c(0,10),xlab="array number",ylab="real time (sec)", 
     main = paste("Time dependence for different sorting algorithms \n", "Capacity = 10^6, Amount = 5*10^6"))
points(1:30, time_stable_capacity_1[4,], col="black", pch = 21)
points(1:30, time_cs_capacity_1[4,], col="black", pch = 22)
legend(0, 10, legend=c("Cs-sort", "Merge-sort", "Heap-sort"), col=c("black", "black", "black"),  lty=1:1,pch=c(22,21,23))


plot(1:30, time_nonstable_capacity_2[4,], col="black", panel.first=grid(), pch = 23, ylim=c(0,12),xlab="array number",ylab="real time (sec)", 
     main = paste("Time dependence for different sorting algorithms \n", "Capacity = 10^7, Amount = 5*10^6"))
points(1:30, time_stable_capacity_2[4,], col="black", pch = 21)
points(1:30, time_cs_capacity_2[4,], col="black", pch = 22)
legend(0, 12, legend=c("Cs-sort", "Merge-sort", "Heap-sort"), col=c("black", "black", "black"),  lty=1:1,pch=c(22,21,23))


plot(1:30, time_nonstable_capacity_3[4,], col="black", panel.first=grid(), pch = 23, ylim=c(0,12),xlab="array number",ylab="real time (sec)", 
     main = paste("Time dependence for different sorting algorithms \n", "Capacity = 10^8, Amount = 5*10^6"))
points(1:30, time_stable_capacity_3[4,], col="black", pch = 21)
points(1:30, time_cs_capacity_3[4,], col="black", pch = 22)
legend(0, 12, legend=c("Cs-sort", "Merge-sort", "Heap-sort"), col=c("black", "black", "black"),  lty=1:1,pch=c(22,21,23))

plot(1:30, time_nonstable_capacity_4[4,], col="black", panel.first=grid(), pch = 23, ylim=c(0,12),xlab="array number",ylab="real time (sec)", 
     main = paste("Time dependence for different sorting algorithms \n", "Capacity = 5*10^8, Amount = 5*10^6"))
points(1:30, time_stable_capacity_4[4,], col="black", pch = 21)
points(1:30, time_cs_capacity_4[4,], col="black", pch = 22)
legend(0, 12, legend=c("Cs-sort", "Merge-sort", "Heap-sort"), col=c("black", "black", "black"),  lty=1:1,pch=c(22,21,23))



mean_cs<-c(mean(time_cs_capacity_1[1,]),mean(time_cs_capacity_1[2,]),mean(time_cs_capacity_1[3,]),mean(time_cs_capacity_1[4,]))
plot(amount, mean_cs, type = "b", col="black", pch = 22, ylim=c(0,8), panel.first=grid(),xlab="amount numbers",ylab="real time (sec)", 
     main = paste("Average time for sorting algorithms \n", "Capacity 10^6"))
mean_stable<-c(mean(time_stable_capacity_1[1,]),mean(time_stable_capacity_1[2,]),mean(time_stable_capacity_1[3,]),mean(time_stable_capacity_1[4,]))
lines(amount,mean_stable,type="b", pch=21, col="black")
mean_nonstable<-c(mean(time_nonstable_capacity_1[1,]),mean(time_nonstable_capacity_1[2,]),mean(time_nonstable_capacity_1[3,]),mean(time_nonstable_capacity_1[4,]))
lines(amount,mean_nonstable,type="b", pch=23, col="black")
legend(0, 8, legend=c("Cs-sort", "Merge-sort", "Heap-sort"), col=c("black", "black", "black"), pch=c(22,21,23))

mean_cs<-c(mean(time_cs_capacity_2[1,]),mean(time_cs_capacity_2[2,]),mean(time_cs_capacity_2[3,]),mean(time_cs_capacity_2[4,]))
plot(amount, mean_cs, type = "b", col="black", pch = 22, ylim=c(0,8), panel.first=grid(),xlab="amount numbers",ylab="real time (sec)", 
     main = paste("Average time for sorting algorithms \n", "Capacity 10^7"))
mean_stable<-c(mean(time_stable_capacity_2[1,]),mean(time_stable_capacity_2[2,]),mean(time_stable_capacity_2[3,]),mean(time_stable_capacity_2[4,]))
lines(amount,mean_stable,type="b", pch=21, col="black")
mean_nonstable<-c(mean(time_nonstable_capacity_2[1,]),mean(time_nonstable_capacity_2[2,]),mean(time_nonstable_capacity_2[3,]),mean(time_nonstable_capacity_2[4,]))
lines(amount,mean_nonstable,type="b", pch=23, col="black")
legend(0, 8, legend=c("Cs-sort", "Merge-sort", "Heap-sort"), col=c("black", "black", "black"),pch=c(22,21,23))


mean_cs<-c(mean(time_cs_capacity_3[1,]),mean(time_cs_capacity_3[2,]),mean(time_cs_capacity_3[3,]),mean(time_cs_capacity_3[4,]))
plot(amount, mean_cs, type = "b", col="black", pch = 22, ylim=c(0,8), panel.first=grid(),xlab="amount numbers",ylab="real time (sec)", 
     main = paste("Average time for sorting algorithms \n", "Capacity 10^8"))
mean_stable<-c(mean(time_stable_capacity_3[1,]),mean(time_stable_capacity_3[2,]),mean(time_stable_capacity_3[3,]),mean(time_stable_capacity_3[4,]))
lines(amount,mean_stable,type="b", pch=21, col="black")
mean_nonstable<-c(mean(time_nonstable_capacity_3[1,]),mean(time_nonstable_capacity_3[2,]),mean(time_nonstable_capacity_3[3,]),mean(time_nonstable_capacity_3[4,]))
lines(amount,mean_nonstable,type="b", pch=23, col="black")
legend(0, 8, legend=c("Cs-sort", "Merge-sort", "Heap-sort"), col=c("black", "black", "black"),pch=c(22,21,23))

mean_cs<-c(mean(time_cs_capacity_4[1,]),mean(time_cs_capacity_4[2,]),mean(time_cs_capacity_4[3,]),mean(time_cs_capacity_4[4,]))
plot(amount, mean_cs, type = "b", col="black", pch = 22, ylim=c(0,8),panel.first=grid(),xlab="amount numbers",ylab="real time (sec)", 
     main = paste("Average time for sorting algorithms \n", "Capacity 5*10^8"))
mean_stable<-c(mean(time_stable_capacity_4[1,]),mean(time_stable_capacity_4[2,]),mean(time_stable_capacity_4[3,]),mean(time_stable_capacity_4[4,]))
lines(amount,mean_stable,type="b", pch=21, col="black")
mean_nonstable<-c(mean(time_nonstable_capacity_4[1,]),mean(time_nonstable_capacity_4[2,]),mean(time_nonstable_capacity_4[3,]),mean(time_nonstable_capacity_4[4,]))
lines(amount,mean_nonstable,type="b", pch=23, col="black")
legend(0, 8, legend=c("Cs-sort", "Merge-sort", "Heap-sort"), col=c("black", "black", "black"),  pch=c(22,21,23))



memory_stable<-c(0.76, 3.8, 7.6, 38, 152)

plot(c(1e+05, 5e+05, 1e+06, 5e+06, 20e+06), memory_stable, type = "b", col="black", pch = 21, ylim=c(0,200), panel.first=grid(),xlab="amount numbers",ylab="Memory (Mb)", 
     main = paste("Memory consumption for merge-sort algorithm"))
