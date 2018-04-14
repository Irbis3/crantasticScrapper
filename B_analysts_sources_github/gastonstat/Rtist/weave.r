##################################################################################
# Title:      Weave
# Copyright:  Copyright (c) 2012, under the Simplified BSD License
# Author:     Gaston Sanchez
# url:        www.gastonsanchez.com/projects/rtist
##################################################################################

# Don't use my working directory!
setwd("/Users/gaston/Documents/Rtist_Plots/Images")

# ============================================================================
# Weave
# ============================================================================
# generate x-y values
theta = seq(-2*pi, 2*pi, length=300)
x = tan(theta)
y = x + sin(theta) 

png("weave.png", width=700, height=400)
# set graphical parameters
op = par(bg="black", mar=rep(0.2, 4))
# plot
plot(x, y, type="n", xlim=c(-10,10), ylim=c(-2,2))
for (i in seq(-2*pi,2*pi,length=50))
{  
  lines(i*x, y, col=hsv(runif(1,.7,.8),0.6,0.8,runif(1,.2)), lwd=3*runif(1))
}
# signature
legend("bottomright", legend="© Gaston Sanchez", bty="n", text.col="gray80")
# reset par
par(op)
dev.off()