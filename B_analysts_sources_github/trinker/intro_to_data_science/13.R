ggplot() +
    geom_jitter(data = carnegie, aes(x = ROOMS, y = SATV25), width = 50, height = 5) +
    geom_smooth(data = carnegie, aes(x = ROOMS, y = SATV25))


