########################################################
#Section
########################################################

t <- merge(ct2010, subset(pop, Year == 2010), by = "id", all.x = TRUE)
head(t)
t[which(is.na(t$MunName.y)),]
t$diff <- t$Population.x - t$Population.y
t[which(abs(t$diff) > 50000),]

ggplot(t, aes(Population.x, diff)) +
  geom_point() +
  geom_smooth()

t$group <- cut(t$Population.x, c(seq(from = 1, to = 2000000, by = 10000)))
ggplot(ddply(t, .(group), function(df) sum(df$diff, na.rm = TRUE) /
      sum(df$Population.x, na.rm = TRUE)),
       aes(group, V1, group = 1)) +
       geom_point() +
  geom_smooth()

im <- read.csv("data/im.csv")
head(im)
im$id <- as.numeric(gsub(" ", "0", str_c(format(im$Ent, width = 2), format(im$Mun, width = 3))))
t <- merge(im[, c("id", "Indice.de.intensidad.migratoria",
                  "Grado.de.intensidad.migratoria")], t, by = "id",
           all.y = TRUE)

ggplot(t, aes(Grado.de.intensidad.migratoria, diff)) +
  geom_jitter() +
  geom_boxplot() +
  coord_cartesian(ylim = c(-10000, 10000))




idm <- read.csv("data/IDM2005.csv.bz2", fileEncoding = "windows-1252")
idm$id <- as.numeric(gsub(" ", "0", str_c(format(idm$Clave.de.la.entidad, width = 2), format(idm$Clave.del.municipio, width = 3))))
idm <- idm[, c("id", "Índice.de.marginación", "Grado.de.marginación")]
head(idm)
t <- merge(t, idm, by = "id")

ggplot(t, aes(Grado.de.marginación, diff))+
  geom_jitter(aes(size = Population.x)) +
  geom_boxplot(fill = "transparent", color = "red") +
  coord_cartesian(ylim = c(-10000, 10000))


ggplot(t, aes(-Índice.de.marginación, Indice.de.intensidad.migratoria))+
  geom_jitter(aes(size = Population.x), alpha = .5) +
  geom_smooth()


hom <- read.csv("data/executions.csv")
hom$id <- as.numeric(hom$code)
t <- merge(t, subset(hom, Year == 2010), by = "id")
#t$rate <- t$TotalHom / t$Population.x * 10^5
head(t)

ggplot(t, aes(rate, diff)) +
  geom_point(aes(size = Population.x)) +
  geom_smooth() +
  coord_cartesian(ylim = c(-10000, 10000), xlim = c(0, 230))

ggplot(t, aes(Grado.de.intensidad.migratoria, rate)) +
  geom_point(aes(size = Population.x)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 250))

fit <- lm(diff ~ Grado.de.intensidad.migratoria * Grado.de.marginación * Population.x * rate.y, data = t)
summary(fit)
plot(fit)
