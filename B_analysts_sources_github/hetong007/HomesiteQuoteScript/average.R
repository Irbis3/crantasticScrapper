require(readr)
xgb.res = read_csv('xgb1.csv')
mx.res = read_csv('mxnet1.csv')


mix.res = xgb.res[,2]*0.95+mx.res[,2]*0.05
submission <- data.frame(QuoteNumber=id[teind], QuoteConversion_Flag=mix.res)
write_csv(submission, "mix1.csv")