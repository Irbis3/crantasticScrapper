# Stability Selection
require('c060')
stablevars = stabpath(y, x, steps = 200, family = 'binomial', mc.cores = 4)

png('stablevars.png', width = 800, height = 600)
plot(stablevars)
dev.off()

stabsel(stablevars, error = 0.25, type = 'pfer', pi_thr = 0.51)

names(acc_normal_1)[7028]
names(acc_normal_1)[7739]
