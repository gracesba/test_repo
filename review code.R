library(MASS)
attach(shoes)
shoes
boxplot(shoes)
#two-sample t-test with unequal variance#
t.test(A,B)
#test equal variance#
var.test(A,B)
#two sample t-test with equal variance#
t.test(A,B,var.eq=T)
#wilcoxon rank sum test#
wilcox.test(A,B)
#How W is calculated#
rank(c(A,B));c(A,B)
sum(rank(c(A,B))[1:10])-sum(1:10)
rank(c(A,B))[1:10]
1:10
sum(1:10)
#Pearson's sample correlation#
cor.test(A,B,method='pearson')
#Spearman's rank correlation#
cor.test(A,B,method='spearman')
#Paired t-test#
t.test(A,B,pair=T)
#Wilcoxon signed rank test#
wilcox.test(A,B,pair=T)
#to see how V is calculated#
d<-A-B;d
rank(abs(d))
abs(d)
d>0
sum(rank(abs(d))[d>0])
#a simple simulation comparint t and Wilcoxon tests#
n<-10
nsim<-1000
d<-seq(0,2,len=10)
pt<-pw<-matrix(NA,10,nsim)
for(j in 1:10){
  for(i in 1:nsim){
    y<-rnorm(n,mean=d[j],sd=1)
    pt[j,i]<-t.test(y)$p.value
    pw[j,i]<-wilcox.test(y)$p.value
  }
}
powert<-apply(pt<0.05,1,mean)
powerw<-apply(pw<0.05,1,mean)
rbind(powert,powerw)
#permutation test for shoes#
d<-A-B;abs(d)
n<-length(d)
d.perm<-matrix(abs(d),n,1000)
d.perm<-d.perm*sign(runif(1000*n)-.5)
d.bar<-apply(d.perm,2,mean)
mean(d.bar<mean(d))
#bootstrap confidence intervals for shoes#
d<-A-B;
d
boot.smpl<-matrix(d,length(d),1000)
boot.smpl1<-apply(boot.smpl,2,sample,r=T)
boot.md<-apply(boot.smpl,2,median)
var(boot.md)

#Birth hour Slide:95#
rep(1,24)

