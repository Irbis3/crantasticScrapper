library(quantmod)

STK = get(getSymbols("2330.tw"))

head(STK)
tail(STK,10)
chartSeries(STK)

STK.week = to.weekly(STK)

profit = setNames(numeric(nrow(STK.week)), rownames(STK.week))

for (i in 1:nrow(STK.week)) {	
	profit[i]=STK.week[i,4]-STK.week[i,1]
}
# col controls color, lwd controls the width of the line
plot(cumsum(profit),type="l",col="red",lwd=2)

paste("Profit:",sum(profit))
paste("WinRate:",length(profit[profit>0])*100/length(profit[profit!=0]),"%")
paste("Profit Factor:",sum(profit[profit>0])/-sum(profit[profit<0]))
