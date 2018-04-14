# Load Library, Define max value
library(ggplot2)
max = 10000         # Define the maximum number to crunch

# Create data frame
df<-data.frame("num"=NA,"iter"=NA,"maxval"=NA)

# Iterate and solve
for(m in 1:(round(max/1000,-3))){
  
  # Start up buffer frame
  buffer<-data.frame("num"=((m-1)*1000+1):(m*1000),"iter"=NA,"maxval"=NA)
  
  # Iterate from 1:1000
  for(n in 1:1000){
    val<-buffer$num[n]
    buffer$iter[n]<-0
    buffer$maxval[n]<-buffer$num[n]
    
    while(val!=1){
      ifelse(val%%2==0,
             val<-val/2,
             val<-3*val+1)
      buffer$iter[n]<-buffer$iter[n]+1
      if(val>buffer$maxval[n]){buffer$maxval[n]<-val}
    }
    print(paste(m*1000+n," of ",max," complete: ",signif((m*1000+n)/max*100,4),"%",sep=""))
  }
  df<-rbind(df,buffer)}

df<-subset(df,!is.na(df$num))

# Plot results
ggplot(df,aes(num,iter))+
  geom_point(size=.01,fill="steelblue")+
  scale_x_log10(breaks=10^(0:6),labels=format(10^(0:6),scientific=F),
                minor_breaks=c(seq(1,10,1),seq(10,100,10),seq(100,1000,100),seq(1000,10000,1000),
                               seq(10000,100000,10000), seq(100000,1000000,100000)))+
  labs(title="Collatz Conjecture",
       x="Starting Value",
       y="Number of Iterations to reach 1",
       caption="created by /u/zonination")+
  theme_bw()
ggsave("collatz.png", height=6, width=10, dpi=120, type="cairo-png")
