

meds <- ddply(subset(unam, 
                     accepted == "A"),
      .(major),
      summarise,
      score = median(score, na.rm = TRUE))
meds[order(-meds$score),]

top.majors <-  c("ARTES VISUALES",
                                 "MEDICO CIRUJANO",
                                 "INGENIERIA MECATRONICA",
                                 "RELACIONES INTERNACIONALES")
exam.dates <- c("Jun 2011", "Feb 2012", "Jun 2012",
                     "Feb 2013", "Jun 2013")
p <- ggplot(subset(unam, major %in% top.majors &
             accepted == "A"),
       aes(date, score, group = major, color = major)) +
  stat_summary(fun.y = median, geom = "line") +
  scale_x_date(breaks = sort(unique(unam$date)),
                     labels = exam.dates) +
  ylab("median exam score")
addSave(p, "top-majors.svg")






meds <- ddply(subset(unam, 
             accepted == "A"),
      .(major, area, date),
      summarise,
      score = min(score, na.rm = TRUE))
meds <- ddply(meds, .(major, area), function (df) {
  df$score <- c(NA, diff(df$score))
  df})
p <- ggplot(na.omit(meds), aes(date,score, group = major)) +
  geom_line(alpha = .6) +
  facet_wrap(~area)+
  scale_x_date(breaks = sort(unique(na.omit(meds)$date)),
                     labels = c("Feb 2012", "Jun 2012",
                     "Feb 2013", "Jun 2013"))  
addSave(p, "change-in-trend.svg")
  

## ggplot(subset(unam, area == "Ciencias Físico-Matemáticas y las Ingenierías" &
##               accepted == "A"),
##        aes(reorder(faculty, score, median), score)) +
##   geom_jitter(alpha = .5) +
##   geom_boxplot(fill = "transparent", color = "red") +
##   ##stat_summary(fun.data = median_cl_boot, color = "red",
##   ##             geom = "crossbar")+
##   coord_flip() 


shapes <- list(c(15:20,0:10), c(0:1, 15, 2:10),
               c(15:18,0:10), c(15:18,20,0:10))
location.colors <- c("CU" = "#1F78B4",
             "ESCUELA NACIONAL DE ARTES PLASTICAS" = "#8DD3C7",
             "ESCUELA NACIONAL DE ENFERMERIA Y OBSTETRICIA" = "#BEBADA",
             "ESCUELA NACIONAL DE MUSICA" = "#FB8072",
             "FES ACATLAN" = "#80B1D3",
             "FES ARAGON" = "#FDB462", 
             "FES CUAUTITLAN" = "#B3DE69",
             "FES IZTACALA" = "#FCCDE5",
             "FES ZARAGOZA" = "#FFFFB3")
bio <- location.colors[c("CU",
                         "ESCUELA NACIONAL DE ENFERMERIA Y OBSTETRICIA",
                         "FES CUAUTITLAN",
                         "FES IZTACALA",
                         "FES ZARAGOZA"
                  )]
humanities <- location.colors[c("CU",
                                "ESCUELA NACIONAL DE ARTES PLASTICAS",
                                "ESCUELA NACIONAL DE MUSICA",             
                                "FES ACATLAN",
                                "FES ARAGON",
                                "FES CUAUTITLAN"
                  )]
stem <- location.colors[c("CU",
                          "FES ACATLAN",
                          "FES ARAGON",
                          "FES CUAUTITLAN",
                          "FES ZARAGOZA"
                  )]
ss <- location.colors[c("CU",
                          "FES ACATLAN",
                          "FES ARAGON",
                          "FES CUAUTITLAN"
                  )]
palettes <- list(bio, humanities,
                 stem, ss)
i <- 1
for(major in levels(as.factor(unam$area))){
  p <- plotMajors(unam, major, shapes[[i]], palettes[[i]])
  addSave(p, str_c(major, "-majors.svg"), 14, 8)
  i <- i + 1
}


for(area in levels(as.factor(unam$area))) {
  p <- plotFaculties(unam, area)
  addSave(p, str_c(area, "-faculty.svg"))
}


p <- ggplot(unam,
            aes(score, group = accepted,
                fill = accepted), color = "transparent") +
  geom_histogram(binwidth = 5, alpha= .5, position = "identity") +
  facet_wrap(~ area) +
  scale_fill_manual("exam\nresult",
                    breaks = c("A", "R"), values = c("blue", "red"),
                    labels = c("accepted", "rejected")) +
  labs(title = "Rejected and accepted students, by area of study (Jun 2011‐Jun 2013)")
addSave(p, "rejected-area.svg")


# histogram from the wasted talent page
hist <- ddply(na.omit(unam), 
                     .(score, area, accepted), summarise, total = length(score))
hist$accepted <- str_replace(hist$accepted, "A", "Granted Admission")
hist$accepted <- str_replace(hist$accepted, "R", "Denied Admission")
names(hist) <- c("score", "area", "status", "total")
r1 <- rPlot(y = "total", x = "bin(score,1)" , color = "status", data = hist, type = "bar",
            tooltip="#! function(item){return 'Score: ' + item.score +'<brn>' + 'Total Students: ' + item.total + '<brn>' + 'Status: ' + item.status } !#",
)
r1$facet(var = "area", type = "wrap",rows = 2)
r1$coord( type = "cartesian")
r1$guides(
          y = list(title = "number of students",
                   min = 0,
                   max = max(hist$total) + 1000),
          x = list(title = "score", renderGrid = FALSE,
                   min = 0,
                   max = max(hist$score, na.rm = TRUE)))

r1$set(width = 800, height = 600)
r1$save(file.path("..", "html", "histogram.html"), cdn = TRUE)

# If the UNAM allowed applicants to list two or more majors in order preference, 
# scores would improve quite a bit (one standard deviation is 17 points), 
# even more so among the lowest performing students. To create the synthetic 
# score I assumed the same number of students would be admitted but ordered by 
# top score for each area. This is not an entirely realistic assumption because 
# not everyone would be willing to list more than one major or attend 
# FES Zaragoza instead of FES Cuautitlan, but it does show that the UNAM is wasting talent.
ideal <- ddply(subset(unam, accepted == "A"), 
      .(area, accepted), summarise, median = median(score),
      students = length(score))
ideal$synthetic <- NA

ddply(subset(unam, accepted == "A"), .(area), summarise, sd(score, na.rm = TRUE))
naunam <- na.omit(unam[order(-unam$score), ])
for(i in 1:4)
  ideal$synthetic[i] <- median(
    subset(naunam, area == ideal$area[i])$score[1:ideal$students[i]]
  )
ideal <- melt(ideal, id = c("area", "accepted", "students"))
ideal$variable <- str_replace(ideal$variable, "median", "real median score")
ideal$variable <- str_replace(ideal$variable, "synthetic", "synthetic score")

n1 <- nPlot(value ~ area, group = 'variable', data = ideal, 
      type = 'multiBarChart')
n1$set(width = 900, height = 400)
n1$save(file.path("..", "html", "improvement.html"), cdn = TRUE)

# Percentage of students admitted to each major from June 2011 to June 2013. 
# The black line corresponds to a quadratic model.
df <- ddply(unam, .(major, faculty, area), summarise,
            yield = length(accepted[accepted == "A"]) / length(accepted),
            median = min(score[accepted == "A"]))
## for(field in levels(as.factor(df$area))) {
##   p <- ggplot(subset(df, area == field), aes(yield, reorder(major, yield, max))) +
##     geom_point(aes(shape = faculty)) +
##     scale_x_continuous(labels = percent)
##   addSave(p, str_c(field, "-yield.png"), 14, 7)
## }
df$location <- isCU(df$faculty, TRUE)
df$area <- str_replace(df$area, "Physical Sciences, Mathematics and Engineering", "STEM")
p <- ggplot(df, aes(yield, median)) +
  geom_point(aes(color = location), size = 5) +
  geom_smooth(method = lm, formula = y ~ poly(x, 2), aes(group = location)) +
  facet_wrap(~area)+
  scale_x_continuous(labels = percent)
## g4.svg <- gridToSVG(file.path("..", "graphs", "plot1.svg"))

r1 <- rPlot(median ~ yield | area,
            data = df, type = "point", color = "location",
                tooltip="#! function(item){return item.major +'<brn>' + 'Location: ' + item.faculty + '<brn>' + 'Minimum score: ' + item.median + '<brn>' + 'Percent accepted: ' + Math.round(item.yield*1000)/10 + '%'} !#",
                title = "Enlace", size =  list(const = 3.5))
r1$facet(var = 'area', type = 'wrap', cols = 2)
r1$guides(x = list(title = "percent admitted",
                min = min(df$yield)-.03,
                max = max(df$yield)+.03),
              y = list(title = "minimum score for admittance",
                min = min(df$median)-5,
                max = max(df$median)+15))
df <- df[order(df$yield),]
df.loess <- ddply(df, .(area, location), summarise,
                  median = predict(lm(median ~ I(poly(yield,2)))),
                  yield = yield)
## df.loess <- df.loess[,c("loess", "yield", "location", "area")]
## names(df.loess)[1] <- "median"
r1$layer(data = subset(df.loess, location == "CU"), type = 'line', 
             color = list(const = 'black'), copy_layer = TRUE, tooltip = NULL,
             size = list(const = 2))
r1$layer(data = subset(df.loess, location != "CU"), type = 'line', 
             color = list(const = 'black'), copy_layer = TRUE, tooltip = NULL,
             size = list(const = 2))
r1$addParams(title = "")
r1$set(width = 500, height = 600)
r1$save(file.path("..", "html", "min-vs-admitted.html"), cdn = TRUE)


# table of percent admited by campus
df <- unam
df$cu <- isCU(unam$faculty, TRUE)
kable(ddply(df, .(date, cu), summarise,
      percentage = round(length(date[accepted == "A"]) / length(date), 3),
      apply = length(date),
      took_test = length(na.omit(score)),
      admit = length(date[accepted == "A"])), format = "html")

p <- ggplot(ddply(df, .(date, cu), summarise,
             sum = length(date[accepted == "A"]) / length(date)),
       aes(date, sum, group = cu, color = cu)) +
  geom_line() +
  geom_point(size = 4) +
  scale_x_date(breaks = sort(unique(unam$date)),
               labels = exam.dates) +
  scale_y_continuous(labels = percent, limits = c(0,.15)) +
  labs(title = "Percentage admitted to the UNAM") +
  ylab("percent admitted")
addSave(p, "percent-admit.svg",
       width = 8, height = 5)


p <- ggplot(ddply(df, .(date, cu), summarise,
             sum =  median(score[accepted == "A"], na.rm = TRUE)),
       aes(date, sum, group = cu, color = cu)) +
  geom_line() +
  geom_point(size = 4) +
  scale_x_date(breaks = sort(unique(unam$date)),
               labels = exam.dates) +
  scale_y_continuous(labels = comma) +
  labs(title = "Median scores of those admitted to the UNAM") +
  ylab("median score")
addSave(p, "median-admit.svg")


demand <- ddply(unam, .(major, cu), summarise, sum = length(date))
demand[order(demand$sum),]
p <- ggplot(demand, aes(sum, reorder(major, sum, max), color = cu)) +
  geom_point(size = 4) +
  scale_color_manual("location", values = location.colors) +
  scale_x_continuous(label = comma) +
  xlab("number of applicants") +
  ylab("major") +
  labs(title = "Number of applicants at the UNAM (Jun 2011‐Jun 2013)")
addSave(p, "demand.svg",
        width = 12, height = 15)

## ing.mecatro <- subset(unam, major %in% c( "INGENIERIA MECATRONICA",
##                                           "INGENIERIA ELECTRICA Y ELECTRONICA",
##                                           "INGENIERIA MECANICA"))
## ing.mecatro <- ing.mecatro[str_detect(ing.mecatro$faculty, "FACULTAD"),]
## ggplot(ddply(ing.mecatro, .(date), summarise,
##              sum = length(date[accepted == "A"]) / length(date)),
##        aes(date, sum)) +
##   geom_line()+
##   scale_x_date(breaks = sort(unique(unam$date)),
##                labels = exam.dates) 

nrow(subset(unam, date == "2013-06-01"))


sd(unam[unam$accepted == "A" &
        unam$major == "RELACIONES INTERNACIONALES","score"])
max(unam[unam$accepted == "R","score"], na.rm = TRUE)


nrow(unam[unam$accepted == "R" & unam$score > 80, ])
nrow(unam[unam$accepted == "A" & unam$score > 80, ])

# Mechatronics engineering is similar to both mechanical and electronic engineering. 
# As can be seen in the chart some of the students who were rejected from 
# mechatronics scored higher than those who were accepted to mechanical and 
# electronic engineering. Since the UNAM only allows each student to list one major 
# as their choice, those rejected were excluded from the UNAM. The data is for the 
# exam of Feb. 2013
wasted <- subset(subset(unam, date == "2013-02-01"), (major == "INGENIERIA MECATRONICA") |
       (major == "INGENIERIA ELECTRICA Y ELECTRONICA" &
              accepted == "A") |
       (major == "INGENIERIA MECANICA" &
              accepted == "A") )
wasted <- wasted[str_detect(wasted$faculty, "FACULTAD"), ]
minimum <- min(wasted[wasted$accepted == "A" &
                      wasted$major == "INGENIERIA MECANICA","score"],
               na.rm = TRUE)
nrow(wasted[wasted$accepted == "R" & wasted$score > minimum  ,])
nrow(wasted[wasted$accepted == "A" ,])

p <- ggplot(wasted, aes(major, score, color = accepted)) +
  geom_jitter(alpha = .6) +
  ##stat_summary(fun.data = median_cl_boot, color = "red",
  ##             geom = "crossbar") +
  coord_flip() +
  scale_color_manual("exam\nresult", values = c("blue", "red"),
                     labels = c("accepted", "rejected")) +
  labs(title = "Accepted or Rejected Students")
addSave(p, "mecatronica.svg")


wasted$jitter <- as.numeric( as.factor(wasted$major )) - 1
wasted$jitter <- wasted$jitter + round(runif(n = nrow(wasted), min = -0.3, max = 0.3),2)

wasted$accepted <- str_replace(wasted$accepted, "A", "Granted Admission")
wasted$accepted <- str_replace(wasted$accepted, "R", "Denied Admission")
wasted$date = NULL
wasted <- na.omit(wasted)
wasted = wasted[order(wasted$accepted),]
rwasted <- rPlot(
  y = "jitter",
  x = "score",
  data = wasted,
  color = "accepted",
  type = 'point',
  size = list( const = 3),
  tooltip="#! function(item){return item.accepted +'<brn>' + 'Score:' + item.score } !#",
  
)
rwasted$guides(
  color = list(
    numticks = 3,
    title = "status",
    opacity=.5
  ),
  y =  list(
    numticks = 3,
    labels = unique(wasted$major),
    renderGrid = FALSE,
    title = "major",
    min = -.5,
    max = 2.8
  ),
  x = list (title = "score", max=126, min = 0)
)
rwasted$save(file.path("..", "html", "waste-mecatronica.html"), cdn = TRUE)

# 
# d1 <- dPlot(
#   y = "score",
#   x = "major",
#   groups = c("major","accepted", "score"),
#   data = wasted[,c("major","score", "accepted")],
#   type = "bubble"
# )
# d1$yAxis( type = "addMeasureAxis" )
# d1$xAxis( type = "addCategoryAxis" )
# d1$save(file.path("..", "html", "waste-mecatronica.html"), cdn = TRUE)

# Chart of percent admitted to the best majors
yield <- ddply(unam, .(major, accepted, date), summarise,
      total = length(accepted))
yield <- ddply(yield, .(major, date), summarise, yield = total[1]/ total[2])

tail(yield[order(-yield$yield), ], 20)
head(yield[order(-yield$yield), ])

p <- ggplot(subset(yield, major %in% top.majors),
       aes(date, yield, group = major, color = major)) +
  geom_line() +
  ylab("acceptance rate") +
  scale_y_continuous(labels = percent) +
  scale_x_date(breaks = sort(unique(yield$date)),
                     labels = exam.dates,
               limits = c(as.Date("2011-06-01"), as.Date("2014-08-01")))
p <- direct.label(p, "last.bumpup")
addSave(p, "percent-admitted-top.png")


# Starting salary and test scores
allareas.a <- subset(unam, accepted == "A")
allareas.cu.a <- allareas.a[grep("FACULTAD",allareas.a$faculty),]
avscore <- function(str, df){
    mean(df[grep(str, df$major),]$score)
}
majors <- c("MEDICO", "INGENIERIA EN COMPUTACION",
            "DERECHO","MECATRONICA", "ARQUITECTO","INGENIERIA CIVIL",
            "INGENIERIA INDUSTRIAL","PSICOLOGIA","RELACIONES INT",
            "ADMINISTRACION","COMUNICACION","CONTADURIA" )
scores <- sapply(majors, avscore, allareas.cu.a)
salaries <- c(13364,12636,10969,10902,10870,10821,9747,9708,9704,9567,9372,8151)
ss <- data.frame(scores, log.salaries = log(salaries))
p <- ggplot(ss, aes(scores, log.salaries, label = rownames(ss))) +
    geom_point() +
    xlab("Average number of correct answers") +
    #scale_y_log10() +
  ylab("log starting salary") +
    geom_smooth(method = lm) +
    geom_text(hjust=-0.05, angle = -50, size = 4) +
    coord_cartesian(xlim = c(80, 115)) +
    labs(title = "Starting Salary vs Entrance Exam Score") +
    theme_bw()
addSave(p, "score_vs_salary.png",
        text = "Data Source: Suplemento Universitarios Reforma 2008 / Dirección General de Administración Escolar - UNAM")


# The admission exam to the UNAM is quite competitive and only a small percentage 
# of those who apply get in. This chart compares the number of students 
# that applied to study at the UNAM with those that were granted admission. 
# The data corresponds to all who applied to take the test from June 2011 to June 2013.
df <- ddply(subset(unam, accepted == "A"), .(major, cu), summarise,
            median = median(score, na.rm = TRUE))
df <- df[order(-df$median),]

df$major <- str_c(df$major, " - ", df$cu)

# There are four types of admission exam, each based on the area of study of the major 
# the students chose. The data corresponds to all who applied to take the test from 
# June 2011 to June 2013.
df <- ddply(unam, .(area), summarise,
            applied = length(accepted),
            zadmitted = length(accepted[accepted == "A"]))
df <- melt(df)
names(df) <- c("area", "status", "students")

r1 <- rPlot(students ~ status | area, data = df, type = "line")
r1$facet(var = 'area', type = 'wrap', cols = 2)
r1$guides(x = list(title = "status",
                   labels = "#! function(value){
  color_mapping = {zadmitted: 'admitted', applied: 'applied'}
  return color_mapping[value];                  
} !#"),
          y = list(title = "number of students",
                   min = 0,
                   max = 170000))
r1$layer(data = df, type = 'point', 
         color = list(const = 'black'), copy_layer = TRUE, tooltip = NULL,
         size = list(const = 2))
r1$set(width = 500, height = 600)
r1$save(file.path("..", "html", "area-area-sm.html"), cdn = TRUE)


df <- ddply(unam, .(major, cu), summarise,
            applied = length(accepted),
            zadmitted = length(accepted[accepted == "A"]))
df$temp <- str_replace(df$cu, "FES", "F")
df$temp <- abbreviate(df$temp, minlength = 3, strict = TRUE)
df$m <- str_c(abbreviate(df$major, 15), "-", df$temp)
df$temp <- NULL
df <- melt(df, id = c("m", "cu", "major"))
names(df) <- c("m", "cu", "major", "status", "students")
df <- df[order(-df$students),]
#df$m <- str_sub(df$m, 1, 15) 

r1 <- rPlot(students ~ status | m, data = df, type = "line")
r1$facet( var = list( var = "m", levels = unique(df$m) ),
         type = 'wrap', cols = 4)
r1$guides(color=list(labels = c("a", "b"),
                     numticks = 2),
          x = list(title = "status", labels = c("admitted", "applied")),
          y = list(title = "number of students", renderGrid = FALSE,
                   numticks = 2,
                   min = 0,
                   max = max(df$students)+4000))
r1$layer(data = df, type = 'point', 
         color = list(const = 'black'), copy_layer = TRUE, 
         tooltip="#! function(item){return item.status +'<brn>' + 'Students: ' + item.students + '<brn>' + 'Major: ' + item.major + '<brn>' + 'Campus: ' + item.cu } !#",
         size = list(const = 2))
r1$set(width = 800, height = 1600)
r1$save(file.path("..", "html", "major-majorsm.html"), cdn = TRUE)

