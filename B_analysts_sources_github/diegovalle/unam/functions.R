

addSource <- function(plot, text = "Data Source: Dirección General de Administración Escolar - UNAM") {
  plot <- arrangeGrob(plot, 
                      sub = textGrob(text,
                                     x = 0, hjust = -0.1, vjust=0.1,
                                     gp = gpar(fontface = "italic", fontsize = 9,
                                               col = "gray50")))
  return(plot)
}

saveChart <- function(p, filename, width = 9.60, height = 6.00) {
  ggsave(filename = filename, plot = p, dpi = 100,
         width = width, height = height)
}


median_cl_boot <- function(x, conf.int = 0.95, B = 1000, na.rm = TRUE, reps = FALSE) {
  if (na.rm)
    x <- x[!is.na(x)]
  n <- length(x)
  xm <- median(x)
  if (n < 2)
    return(data.frame(y = xm, ymin = NA, ymax = NA))
  resamples <- lapply(1:B, function(i) sample(x, replace=T))
  r.median <- sapply(resamples, median)
  quant <- quantile(unlist(r.median),
                    c((1 - conf.int)/2, (1 + conf.int)/2))
  names(quant) <- NULL
  Median <- median(x)
  data.frame(y = Median,
             ymin = quant[1],
             ymax = quant[2])
}



plotMajors <- function(unam, area_sub, shapes = c(15:18,0:10), palette) {
  df <- subset(unam, area == area_sub &
                 accepted == "A")
  df <- ddply(df, .(major, faculty), transform,
              median = median(score))
  df <- ddply(df, .(major), transform, median = max(median))
  df$faculty <- with(df, reorder(faculty, -score, mean))
  df$major <- with(df, reorder(major, median))
  
  ggplot(df,
         aes(major, score, color = cu, 
             group = faculty, shape = faculty)) +
    geom_jitter(alpha = .8,) +
    stat_summary(fun.data = median_cl_boot, alpha=1,
                 color = "red", geom = "linerange") +
    stat_summary(fun.data = median_cl_boot, color = "black",
                 geom = "point", show_guide = FALSE) +
    coord_flip() +
    guides(color = guide_legend("individual scores,\nby campus",
                                override.aes = list(alpha = 1)),
           shape = guide_legend("median score (95% CI),\nby faculty/campus",
                                override.aes = list(alpha = 1))) +
    scale_shape_manual(values= shapes) +
    scale_colour_manual(values = palette) +
    scale_fill_manual(values = palette) +
    labs(title =str_c("Admission Scores - ", area_sub, " (Jun 2011-Jun 2013)")) +
    ylab("exam score")
}

plotFaculties<- function(unam, area_sub) {
  df <- subset(unam, area == area_sub &
                 accepted == "A")
  ggplot(df, aes(reorder(faculty, score, median), score)) +
    geom_jitter(alpha = .3) +
    geom_boxplot(fill = "transparent", color = "red") +
    ##stat_summary(fun.data = median_cl_boot, color = "red",
    ##             geom = "crossbar", show_guide = FALSE) +
    coord_flip() +
    labs(title = str_c("Admission Scores - ", area_sub, " (Jun 2011-Jun 2013)")) +
    ylab("exam score") +
    xlab("faculty")
}

addSave <- function(p, filename, width = 9, height = 6.00,
                    text = "Data Source: Dirección General de Administración Escolar - UNAM") {
  saveChart(addSource(p, text),
            file.path("..", "graphs", filename),
            width, height)
}

isCU <- function(vec, asbool = FALSE) {
  ifelse(str_detect(vec, "FACULTAD") |
           vec == "ESCUELA NACIONAL DE TRABAJO SOCIAL",
         "CU",
         if(asbool)
           "Not CU"
         else
           vec)
}