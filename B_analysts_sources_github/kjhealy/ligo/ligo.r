###--------------------------------------------------
### Rough network plots of the LIGO paper
###--------------------------------------------------

library(igraph)
library(stringr)
library(tidyr)
library(dplyr)
library(networkD3)


## leg.col <- function (colours, borders = NULL)
## {
##     labels <- names(colours)
##     n <- length(colours)
##     ncol <- ceiling(sqrt(n))
##     nrow <- ceiling(n/ncol)
##     colours <- c(colours, rep(NA, nrow * ncol - length(colours)))
##     colours <- matrix(colours, ncol = ncol, byrow = TRUE)
##     old <- par(pty = "s", mar = c(0, 0, 0, 0))
##     on.exit(par(old))
##     size <- max(dim(colours))
##     plot(c(0, size), c(0, -size), xlab = "", ylab = "",
##         axes = FALSE)
##     rect(col(colours) - 1, -row(colours) + 1, col(colours), -row(colours),
##         col = colours, border = borders)
##         text(col(colours) - 0.5, -row(colours) + 0.5, labels)
## }


###--------------------------------------------------
### Data
###--------------------------------------------------

## Author names and numbered affiliations
name.dat <- read.csv("data/author-affils.csv")

### Departments and Countries
affil.dat <- read.csv("data/affil-clean.csv")
affil.dat$Country <- str_trim(affil.dat$Country)
affil.dat$Shortname <- str_trim(affil.dat$Shortname)

### Long-form Authors and Depts
data <- gather(name.dat, Affiliation, School, Aff1:Aff3, na.rm = TRUE)
ind <- match(data$School, affil.dat$id)
data$School.Name <- affil.dat$Shortname[ind]
data$Country <- affil.dat$Country[ind]


## Authors + Schools
data.bp <- data.frame(data[,c("Name", "School.Name")])
colnames(data.bp) <- c("Name", "School")
data.bp.tab <- table(data.bp)
data.bp.mat <- as.matrix(data.bp.tab)

person.bp.net <- data.bp.mat %*% t(data.bp.mat)
group.bp.net <- t(data.bp.mat) %*% data.bp.mat

diag(group.bp.net) <- NA
diag(person.bp.net) <- NA

person.bp.g <- graph.adjacency(person.bp.net,mode="undirected",
                            weighted=NULL, diag=FALSE)


group.bp.g <- graph.adjacency(group.bp.net, weighted=TRUE,
                           mode="undirected", diag=FALSE)



## Authors + Countries
data.co <- data.frame(data[,c("Name", "Country")])
data.co.tab <- table(data.co)
data.co.mat <- as.matrix(data.co.tab)

person.co.net <- data.co.mat %*% t(data.co.mat)
group.co.net <- t(data.co.mat) %*% data.co.mat

diag(group.co.net) <- NA
diag(person.co.net) <- NA

person.co.g <- graph.adjacency(person.co.net,mode="undirected",
                            weighted=NULL, diag=FALSE)
group.co.g <- graph.adjacency(group.co.net, weighted=TRUE,
                           mode="undirected", diag=FALSE)


## Authors
data.uni <- data.frame(data[,c("Name", "School.Name")])
data.uni.tab <- table(data.uni)
data.uni.mat <- as.matrix(data.uni.tab)

person.uni.net <- data.uni.mat %*% t(data.uni.mat)
group.uni.net <- t(data.uni.mat) %*% data.uni.mat

person.uni.g <- graph.adjacency(person.uni.net,mode="undirected",
                                weighted=NULL, diag=FALSE)
group.uni.g <- graph.adjacency(group.uni.net, weighted=TRUE,
                           mode="undirected", diag=FALSE)



###--------------------------------------------------
### Plots
###--------------------------------------------------

la <- layout.kamada.kawai(group.co.g)
e.wt <- get.edge.attribute(group.co.g, "weight")

pdf(file="figures/group-co-view.pdf", width=10, height=10)
plot(group.co.g, layout=la, vertex.size=5,edge.width=e.wt,
     vertex.label=V(group.co.g)$name, vertex.label.dist=0.4)
dev.off()


pdf(file="figures/person-co-view.pdf", width=22, height=17,pointsize=8)
la <- layout.kamada.kawai(person.co.g)
e.wt <- get.edge.attribute(person.co.g, "weight")
plot(simplify(person.co.g, remove.loops=TRUE), layout=layout.kamada.kawai, vertex.size=3,edge.width=0.1,
     vertex.label=V(person.co.g)$name)
dev.off()




la <- layout.kamada.kawai(group.bp.g)
e.wt <- get.edge.attribute(group.bp.g, "weight")


pdf(file="figures/group-bp-view.pdf", width=10, height=10)
plot(group.bp.g, layout=la, vertex.size=5,edge.width=e.wt,
     vertex.label=V(group.bp.g)$name, vertex.label.dist=0.4)
dev.off()


la <- layout.kamada.kawai(group.uni.g)
e.wt <- get.edge.attribute(group.uni.g, "weight")


pdf(file="figures/group-uni-view.pdf", width=10, height=10)
plot(group.uni.g, layout=la, vertex.size=2,edge.width=sqrt(e.wt),
     vertex.label=V(group.uni.g)$name, vertex.label.cex=0.6)
dev.off()


pdf(file="figures/group-uni-view-fr.pdf", width=10, height=10)
plot(group.uni.g, layout=layout.fruchterman.reingold, vertex.size=2,edge.width=sqrt(e.wt),
     vertex.label=V(group.uni.g)$name, vertex.label.cex=0.6)
dev.off()



### Sorting out the vertex color is quite annoying
### The following is way too messy, and also doesn't
### work at the moment

library(RColorBrewer)

col.d <- brewer.pal(11, name="Paired")

color.table <- data %>% group_by(Country) %>% tally() %>% filter(n<10) %>% arrange(desc(n)) %>% select(Country)
color.table$Color <- col.d

ind <- match(data.co$Country, color.table$Country)
data.co$Color <- color.table$Color[ind]
ind <- is.na(data.co$Color)
data.co$Color[ind] <- "#FFFFFF"

color.vec <- c(color.table$Color, "#FFFFFF")

names(color.vec) <- c(color.table$Country, "Other")

V(person.bp.g)$Country <- as.character(data.co$Country[match(V(person.bp.g)$name, data.co$Name)])
V(person.bp.g)$color <- as.character(data.co$Color[match(V(person.bp.g)$name, data.co$Name)])

person.bp.g <- simplify(person.bp.g)

ordered.vertices <-get.data.frame(person.bp.g, what="vertices")

person.bp.g<-simplify(person.bp.g)

V(person.bp.g)$color <- data.co$Color[match(ordered.vertices$name, data.co$Name)]

leg.col(color.vec)


pdf(file="figures/person-bp-view.pdf", width=22, height=17,pointsize=8)
e.wt <- get.edge.attribute(person.bp.g, "weight")

plot(person.bp.g, layout=layout.kamada.kawai,
     vertex.size=2,edge.width=0.1, vertex.label.cex=0.35)

title("Author Affiliation Network for 'Astrophysical Implications Of The Binary Black Hole Merger GW150914'")

dev.off()

### Gaaah, broken
color.table <- data %>% group_by(Country) %>% tally() %>% filter(n>10) %>% arrange(desc(n))
color.table$Color <- col.d
color.table <- rbind(color.table, (c("Other", 0, "#FFFFFF")))
color.table$x <- 1
color.table$y <- 1

o <- order(color.table$n, decreasing = TRUE)

p <- ggplot(color.table, aes(x=x, y=y, color=reorder(Country, n, order = TRUE)))

p + geom_point() + scale_color_manual(values = color.table$Color[o])


pdf(file="figures/person-bp-view-fr.pdf", width=10, height=12,pointsize=12)
e.wt <- get.edge.attribute(person.bp.g, "weight")

plot(person.bp.g, layout=layout.fruchterman.reingold,
     vertex.size=2,edge.width=0.1, vertex.label.cex=0.12)

dev.off()




person.nodes <- get.data.frame(person.bp.g, what="vertices")
person.links <- data.frame(get.edgelist(person.bp.g), stringsAsFactors = FALSE)
colnames(person.links) <- c("from", "to")
person.links$value <- 1

name.chars <- as.character(data.co$Name)

data.co$id <- c(1:nrow(data.co))

ind <- match(person.links$from, name.chars)
person.links$from.n <- data.co$id[ind]

ind <- match(person.links$to, name.chars)
person.links$to.n <- data.co$id[ind]

group.table <- data %>% group_by(Country) %>% tally() %>% filter(n>10) %>% arrange(desc(n)) %>% select(Country)
group.table$group <- 1:length(group.table$Country)

ind <- match(person.nodes$Country, group.table$Country)
person.nodes$group <- group.table$group[ind]
ind <- is.na(person.nodes$group)
person.nodes$group[ind] <- 12





library(networkD3)

data(MisLinks)
data(MisNodes)

forceNetwork(Links = person.links, Nodes = person.nodes, Source = "from.n",
             Target = "to.n", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8, zoom = TRUE)


forceNetwork(Links = person.links, Nodes = person.nodes, Source = "to.n",
             Target = "from.n", Value = "value", NodeID = "name",
             Group = "Country", opacity = 1, legend = TRUE)





colnames(person.vert) <-

V(group.g)$Country <- as.factor(data.co$Country[match(V(group.g)$name, data.co$Country)])

V(group.g)$color=V(group.g)$Country





person.g <- graph.adjacency(person.net,mode="undirected",
                            weighted=NULL, diag=FALSE)


group.g <- graph.adjacency(group.net, weighted=TRUE,
                           mode="undirected", diag=FALSE)




### Fancier, with triplets
library('Matrix')

A <- spMatrix(nrow=length(unique(data.bp$Name)),
        ncol=length(unique(data.bp$School)),
        i = as.numeric(factor(data.bp$Name)),
        j = as.numeric(factor(data.bp$School)),
        x = rep(1, length(as.numeric(data.bp$Name))) )

row.names(A) <- levels(factor(data.bp$Name))
colnames(A) <- levels(factor(data.bp$School))

Acol <- tcrossprod(t(A))

Arow <- tcrossprod(A)

## Countries
A <- spMatrix(nrow=length(unique(data.co$Name)),
        ncol=length(unique(data.co$Country)),
        i = as.numeric(factor(data.co$Name)),
        j = as.numeric(factor(data.co$Country)),
        x = rep(1, length(as.numeric(data.co$Name))) )

row.names(A) <- levels(factor(data.co$Name))
colnames(A) <- levels(factor(data.co$Country))

Acol <- tcrossprod(t(A))

Arow <- tcrossprod(A)

data.col <- graph.adjacency(Acol, mode=c("undirected"))
la <- layout.fruchterman.reingold(data.col)

plot(simplify(data.col, remove.loops = TRUE), layout=la,
     vertex.size=5, vertex.label.dist=0.35, vertex.label.cex=0.65)

data.row <- graph.adjacency(Arow, mode=c("undirected"))

V(data.row)$Country <- as.factor(data.co$Country[match(V(data.row)$name, data.co$Name)])

V(data.row)$color=V(data.row)$Country


la <- layout.fruchterman.reingold(data.row)


pdf(file="figures/test.pdf", height=5, width=5)
plot(simplify(data.row, remove.loops = TRUE), layout=la, vertex.label=NA, vertex.size=4)
dev.off()
