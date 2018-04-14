

##Load and clean the unam admission scores database
unam <- read.csv("../clean-data/unam-admission.csv",
                 stringsAsFactors = FALSE)
## salaries <- read.csv("../data/salaries.csv", skip = 1)
unam$faculty <- str_replace(unam$faculty, "\\([0-9]*\\)  ", "")
unam$major <- str_replace(unam$major, "\\([0-9]*\\)  ", "")
unam$date <- as.Date(as.yearmon(as.character(unam$date), "%b %Y"))
unam$score <- as.numeric(unam$score)
unam$cu <- isCU(unam$faculty)
ddply(unam, .(date), summarise, sum = length(date))





unam <- subset(unam,
               !faculty %in%
                 c("U.  MULT.  DE DOCEN.  E INV. DE LA FAC. DE C., QRO.",
                   "UMDI, JURIQUILLA, QRO.",
                   "ESCUELA NACIONAL DE ESTUDIOS SUPERIORES UNIDAD LEON",
                   "ESCUELA NACIONAL DE ESTUDIOS SUP. UNIDAD LEON",
                   "ESCUELA NACIONAL DE ARTES PLASTICAS, PLANTEL TAXCO",
                   "ESCUELA NACIONAL DE ESTUDIOS SUP. UNIDAD MORELIA",
                   "CENTRO PENINSULAR EN HUMANIDADES Y CIENCIAS SOCIALES",
                   "PLANTEL TAXCO, GRO"))
unam$area <- car::recode(unam$area,
                         '"Biológicas, Químicas y de la Salud"="Biological Sciences and Health";
                    "Ciencias Físico-Matemáticas y las Ingenierías"="Physical Sciences, Mathematics and Engineering";
                    "Ciencias Sociales"="Social Sciences";
                    "Humanidades y Artes"="Humanities and Arts"')
unam <- subset(unam, date != as.Date("2014-02-01"))
