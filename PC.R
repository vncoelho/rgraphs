parcoord(state.x77[, c(7, 4, 6, 2, 5, 3)])

rm(list=ls())
#data<-read.table("ResultsEvalPinball05",header=T)
#data$EVAL<-as.factor(data$EVAL)
data<-read.table("data",header=T)



library(MASS)

ir <- rbind(iris3[,,1], iris3[,,2], iris3[,,3])
parcoord(log(ir)[, c(1, 2, 3, 4)], col = 1 + (0:149)%/%50)
parcoord(data, col=rainbow(length(data[,1])), var.label=TRUE)

data<-read.table("data2",header=T)
datascaled <- as.data.frame(lapply(data, ggplot2:::rescale01))
datascaled$model <- rownames(data)
datamelted <- reshape2::melt(datascaled)

pdf("parallelCoordMOHFMStockMarket.pdf",width=15,height=10) # comment to open plot in R
parcoord(data[c("MAPEINV", "RMSE", "MAPE", "MAPEINV", "WMAPE", "SMAPE", "MMAPE")], col=rainbow(length(data[,1])), var.label=TRUE)
dev.off() # comment this if you commented the pdf command.


coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

pdf("parallelCoordMOHFMStockMarket.pdf",width=15,height=10) # comment to open plot in R
ggplot(datamelted, aes(x = variable, y = value)) +
  geom_path(aes(group = model, color = model),  size = 1) +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") + 
  scale_colour_discrete("Model 1dddsdsa", labels=c(),breaks=c())+
  coord_polar()
dev.off() # comment this if you commented the pdf command.


ggplot(datamelted, aes(x = variable, y = value)) +
  geom_polygon(aes(group = model, color = model), fill = NA, size = 2, show.legend = FALSE) +
  geom_line(aes(group = model, color = model), size = 2) +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = guide_legend(ncol=2)) +
  coord_radar()



mtcarsscaled <- as.data.frame(lapply(mtcars, ggplot2:::rescale01))
mtcarsscaled$model <- rownames(mtcars)
mtcarsmelted <- reshape2::melt(mtcarsscaled)
library(ggplot2)
ggplot(mtcarsmelted, aes(x = variable, y = value)) +
  geom_path(aes(group = model, color = model),  size = 2) +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = guide_legend(ncol=2)) +
  coord_polar()

coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

ggplot(mtcarsmelted, aes(x = variable, y = value)) +
  geom_polygon(aes(group = model, color = model), fill = NA, size = 2, show.legend = FALSE) +
  geom_line(aes(group = model, color = model), size = 2) +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = guide_legend(ncol=2)) +
  coord_radar()




ir <- rbind(iris3[,,1], iris3[,,2], iris3[,,3])
parcoord(log(ir)[, c(3, 4, 2, 1)], col = 1 + (0:149)%/%50)


cardeaths <- Seatbelts[,"DriversKilled"]
cardeaths & lt;- data.frame(data[,1], data[,5], data[,6],data[,8])
colnames(cardeaths) &lt;- c("DriversKilled", "DistanceDriven", "PriceofGas", 
                            "SeatbeltLaw")

cardeaths &lt;- data.frame(Seatbelts[,1], Seatbelts[,5], Seatbelts[,6],Seatbelts[,8])

colnames(cardeaths) &lt;- 
  
  c("DriversKilled", "DistanceDriven", "PriceofGas", "SeatbeltLaw")

library(reshape2)
library(parcoord)
library(ggparallel)

ggparallel(list("gear", "cyl"), data=mtcars, method="hammock", ratio=0.25)


data(mtcars)
ggparallel(list("gear", "cyl"), data=data)


ggparallel(list("gear", "cyl"), data=mtcars, method="hammock", ratio=0.25)
require(RColorBrewer)
require(ggplot2)
cols <- c(brewer.pal(4, "Reds")[-1], brewer.pal(4, "Blues")[-1])
ggparallel(list("gear", "cyl"), ratio=0.2, data=mtcars,
           method="hammock", text.angle=0) +
  scale_fill_manual(values=cols) + scale_colour_manual(values=cols) +
  theme_bw()

data<-read.table("data",header=T)
is.numeric(data$error1)
data$error1<-as.numeric(data$error1)
data$error2<-as.numeric(data$error2)
data$error3<-as.numeric(data$error3)
data$error4<-as.numeric(data$error4)
data$error5<-as.numeric(data$error5)

ggparallel(names(data), order=0, data) 

+
  scale_fill_brewer(palette="Paired", guide="none") +
  scale_colour_brewer(palette="Paired", guide="none")


ggparallel(names(titanic)[c(1,4,2,3)], order=0, titanic) +
  scale_fill_brewer(palette="Paired", guide="none") +
  scale_colour_brewer(palette="Paired", guide="none")

ggparallel(list("gear", "cyl"), data=mtcars, method="adj.angle",
           ratio=2)

data <- data

data(genes)
cols <- c(rep("grey80", 24), brewer.pal("YlOrRd", n = 9))
genes$chrom <- factor(genes$chrom, levels=c(paste("chr", 1:22, sep=""), "chrX", "chrY"))
ggparallel(list("path", "chrom"), text.offset=c(0.03, 0,-0.03),
           data = genes,  width=0.1, order=c(1,0), text.angle=0,
           color="white",
           factorlevels =  c(sapply(unique(genes$chrom), as.character),
                             unique(genes$path))) +
  scale_fill_manual(values = cols, guide="none") +
  scale_colour_manual(values = cols, guide="none") +
  coord_flip()
