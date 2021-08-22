#Load data
pwd<-read.csv(".\\passwords.csv", header = TRUE)
wsite<-read.csv(".\\wsites.csv", header = TRUE)

wsite

wsite$REMOTE_IPS

#A plot showing the distribution of values for REMOTE_IPS
plot(density(wsite$REMOTE_IPS), main="Distribution plot of Remote_IPS")
polygon(density(wsite$REMOTE_IPS), col="red", border="blue")

#Boxplot of URL_LENGTH by Type of website. 
boxplot(split(wsite$URL_LENGTH,wsite$Type),main='Boxplotof URL_LENGTH by Type of website')

#A scatterplot of number of app packets.
with(wsite,plot(APP_PACKETS))

mode(wsite$Type)

wsite$Type = factor(wsite$Type)


mode(wsite$Type)

levels(wsite$Type)

# There are 2 levels. The first step is to create a vector of color values:
mycolors = c('red','blue')

with(wsite,plot(APP_PACKETS,APP_BYTES,col=mycolors[Type]))
with(wsite,legend('topleft',legend=levels(Type),col=mycolors,pch=1,title='Type'))
title('scatterplot of the number of app packets vs the app bytes according
to type ')

library("plyr")
count_u<-count(wsite, 'CHARSET')
count_u


label<-count_u$CHARSET


slices<-count_u$freq
pct <- round(slices/sum(slices)*100)
lbls <- paste(label, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
   main="Pie Chart of Countries")

slices<-count_u$freq
lbls <- paste(label, slices) # add count to labels
pie(slices,label=lbls, col=rainbow(length(lbls)),
   main="Pie Chart of Countries")

counts <- table(wsite$Type,wsite$CHARSET)
barplot(counts, main="CHARSET Distribution",
   xlab="Number of Occurence",col=c("darkblue","red"),
  legend = rownames(counts))



count_wh<-count(wsite, 'WHOIS_COUNTRY')
count_wh <- transform(count_wh, cumFreq = cumsum(freq))
count_wh

# arguments:  qhpvt(dataFrame, rows, columns, calculations, ...)
# qhpvt(bhmtrains, "TOC", "TrainCategory", "n()") # TOC = Train Operating Company 
qhpvt(wsite,"Type","CHARSET","n()")

summary(wsite['SERVER'])


summary(wsite['CONTENT_LENGTH'])

t.test(pwd$LengthAfter,conf.level = 0.99)


x<-pwd$LengthAfter
y<-pwd$LengthBefore
t.test(x, y, paired = TRUE, alternative = "greater",conf=0.05)

wilcox.test(x , y, paired = TRUE,alternative="two.sided")

wilcox.test(x , y, paired = TRUE,alternative="greater")

#check for normality
shapiro.test(pwd$LengthAfter)

shapiro.test(pwd$LengthBefore)

# Test for equality of variance
bartlett.test(LengthAfter ~ Department, data=pwd)

anova1<-aov(LengthAfter ~ Department, data=pwd,conf=0.05)
summary(anova1)

TukeyHSD(x=anova1, conf.level=0.95)



power.t.test(delta=7,sd=15,power=.85,sig.level=0.01)

power.t.test(delta=7,sd=15,power=.9,sig.level=0.01)

power.t.test(delta=7,sd=15,power=.95,sig.level=0.01)

power.t.test(delta=10,sd=15,power=.85,sig.level=0.01)

power.t.test(delta=10,sd=15,power=.9,sig.level=0.01)

power.t.test(delta=10,sd=15,power=.95,sig.level=0.01)

power.t.test(delta=13,sd=15,power=.85,sig.level=0.01)

power.t.test(delta=13,sd=15,power=.9,sig.level=0.01)

power.t.test(delta=13,sd=15,power=.95,sig.level=0.01)

set.seed(123)
seq_n<-seq(1, 150, 1)

sample_n<-sample(seq_n, size = 20)
print(sort(sample_n))
