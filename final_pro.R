data1 <-read.csv('cities_r2.csv',sep=',',h =TRUE)
x <- data.frame(data1$state_name, data1$population_total)
pop <- aggregate(x$data1.population_total, by=list(state_name=x$data1.state_name), FUN=sum)
pop
x <- data.frame(data1$state_name, data1$literates_total)
lit <- aggregate(x$data1.literates_total, by=list(state_name=x$data1.state_name), FUN=sum)
lit
pop$literates_tot <- lit$x
pop
pop$id <- 1:29
qplot(pop$id, pop$liter, data = pop, color = pop$state_name, size = pop$liter,xlab = "state code",ylab="literates total",main = "StateName VS Total Literates" ,alpha = I(0.7))


pop1 <- pop[order(pop$literates_tot, pop$state_name, decreasing = TRUE),]
pop1
pop2 <- head(pop1, n = 10)
pop2
qplot(pop2$id, pop2$literates_tot, data = pop2, color = pop2$state_name, size = pop2$literates_tot,xlab = "state code",ylab="literacy total",main = "Top 10 Literated States",alpha = I(0.7))
options(scipen= pop2$literates_tot)


pop3 <- tail(pop1, n = 10)
qplot(pop3$id, pop3$liter, data = pop3, color = pop3$state_name, size = pop3$ef_lit_tot,xlab = "state code",ylab="literacy total",main = "Least 10 Literated States",alpha = I(0.7))



x <- data.frame(data1$state_name, data1$literates_male)
eff <- aggregate(x$data1.literates_male, by=list(state_name=x$data1.state_name), FUN=sum)
eff
x <- data.frame(data1$state_name, data1$literates_female)
eff1 <- aggregate(x$data1.literates_female, by=list(state_name=x$data1.state_name), FUN=sum)
eff1
eff$female <- eff1$x
eff
eff2<-data.frame(eff$state_name,"male",eff$x) 
colnames(eff2)<-c("state","gender","value")
eff3<-data.frame(eff$state_name,"female",eff$female)
colnames(eff3)<-c("state","gender","value")
View(eff2)
eff2<-rbind(eff2,eff3)
merge.data.frame(eff2,eff3, by="state")
mydata <- matrix(nrow=2,ncol=28, eff1$x,eff1$female)
mydata <- matrix(nrow=2,ncol=28, rbind(sample(1:29, replace=T)))
max(eff2$value)
#max(eff2)
#barplot(mydata, xlim=c(0,25),legend.text = c("male", "female"), names.arg = data$state_name,las = 2, cex.names = 0.55,col=c("blue","green"),axisnames = T, main="Stack barplot")
p<-ggplot(data=eff2,aes(y=value,x=state, fill=factor(gender)))+ggtitle("Total Literates\n(Bar Indicates Female And Male)")+geom_bar(stat = "identity", colours="black")+theme_bw()+coord_flip(ylim=c(1,21000000))
p


x <- data.frame(data1$state_name, data1$effective_literacy_rate_total)
pop4 <- aggregate(x$data1.effective_literacy_rate_total, by=list(state_name=x$data1.state_name), FUN=sum)
pop4
barplot(pop4$x,main ="StateName VS Effective Literacy Rate", ylab = "Effective Literacy Rate", names.arg = pop$state_name ,  col = c(rainbow(length(pop$state_name))),las = 3,cex.names = 0.57)

x <- data.frame(data1$state_name, data1$effective_literacy_rate_male)
pop5 <- aggregate(x$data1.effective_literacy_rate_male, by=list(state_name=x$data1.state_name), FUN=sum)
pop5
barplot(pop5$x,main ="StateName VS Male Effective Literacy Rate", ylab = "Effective Literacy Rate", names.arg = pop5$state_name ,  col = c(rainbow(length(pop$state_name))),las = 3,cex.names = 0.57)

x <- data.frame(data1$state_name, data1$effective_literacy_rate_female)
pop6 <- aggregate(x$data1.effective_literacy_rate_female, by=list(state_name=x$data1.state_name), FUN=sum)
pop6
barplot(pop6$x,main ="StateName VS Female Effective Literacy Rate", ylab = "Effective Literacy Rate", names.arg = pop6$state_name ,  col = c(rainbow(length(pop6$state_name))),las = 3,cex.names = 0.57)


ml2 <- pop4[order(pop4$x, pop4$state, decreasing = TRUE),]
ml2$id < 1:29
hed <- head(ml2,n = 10)
radius <- sqrt(hed$x/pi)
symbols(hed$state_name, hed$x, circles=radius, inches=0.30, fg="white", bg=c(rainbow(hed$state_name)), xlab="State Name", ylab="Literates Total")
text(hed$state_name, hed$x, hed$state_name, cex=0.5)


ml1 <- ml[order(ml$total_gd, ml$state, decreasing = TRUE),]
ml2 <- head(ml1,n = 5)
pie3D(x,labels= ml2$state,labelcex = 1.0,explode = 0.1,main = "Pie Chart of States\n(With Maximum  Graduates)" )


ml$total_gd <- tl$x
x <- ml$x
y <- ml$total_gd
r <- lm(y~x)
a <- data.frame(x = 1600000)
result <- predict(r,a)
print(result)
png(file = "linear_regression.png")
dev.off()
plot(x,y,abline(lm(y~x)),ylim = rev(range(y)),main = "Total Graduates Prediction Using Male Graduates", xlab = "male graduates", ylab = "total graduates",col = ml$state, lwd = 5, cex = 0.5)
