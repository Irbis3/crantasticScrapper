# Code placed in this file fill be executed every time the
# lesson is started. Any variables created here will show up in
# the user's working directory and thus be accessible to them
# throughout the lesson.


set.seed(456)
#rt1 <- rnorm(100,600,100)
#rt2 <- rnorm(120,540,80)
#rt <- c(rt1,rt2)
#cond <- rep(c(1,2),times=c(100,120))
#sex <- ifelse(runif(220,1,2)>1.5,1,2)
#mood <- round(rnorm(220,3.5,0.7),2)

df <- data.frame(rt=c(rnorm(100,600,100),rnorm(120,540,80)),
                 cond=rep(c(1,2),times=c(100,120)),
                 sex=ifelse(runif(220,1,2)>1.5,1,2),
                 mood=round(rnorm(220,3.5,0.7),2))
df$mood <- as.character(df$mood)



#insert missing values
#miss <- sample(nrow(df),size=8)
df[sample(nrow(df),size=8),"rt"] <- NA

