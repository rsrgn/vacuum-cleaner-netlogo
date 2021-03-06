
#----Cleaning memory----
rm(list = ls())


#----Loading libraries----

library(tidyverse) #to ploting with ggplot2
library(reshape) #to using function melt
library(gridExtra) #to arrange multiple plots frome ggplot2

#----Setting Working Directory----

#Working directory on my computer
setwd("C:/Users/serou/Documents/cours/MEDAS/BANOS - netlogo")

#----Loading data----

df  <- read.csv2(file="./Aspirateurs_V2-2 experiment-table-2.csv", sep = "," , dec = ".")


#----data prep

df <- df %>% 
  dplyr::rename( CleanRate = count.nodes...count.patches.with..obstacle...0.) %>% 
  dplyr::select(-X.step.) %>% 
  dplyr::filter(CleanRate > 0.1)
  

df$plotorder <- sample(x=1:nrow(df) , nrow(df) )



#----Data stats----

summary(df)

colnames(df)

#----Plot----

#plot1
p <- ggplot(df, aes(x=X.run.number., y=CleanRate,
                    shape = moving.function,
                    color = number_of_obstacles))
p <- p + geom_point()
p <- p + facet_grid(  number_of_obstacles ~ moving.function)
p


#plot2
p2 <- ggplot(df, aes(x=plotorder, y=CleanRate,
                    color = CleanRate))
p2 <- p2 + geom_point() 
p2 <- p2 + facet_grid(  number_of_obstacles ~ moving.function)
p2


#plot3
p3 <- ggplot(df, aes(x=plotorder, y=CleanRate,
                    color = moving.function))
p3 <- p3 + geom_boxplot()
p3 <- p3 + facet_grid(  number_of_obstacles ~ moving.function)
p3

#----multiplot-----

p4 <- ggplot(df[df$moving.function==unique(df$moving.function)[1],], aes(x=plotorder, y=CleanRate,
                     color = CleanRate))
p4 <- p4 + geom_point() + theme(legend.position = "none")
p4 <- p4 + facet_grid(  number_of_obstacles ~ moving.function)
p4 <- p4 + theme(axis.text.x=element_blank(),
                 axis.ticks.x=element_blank(),
                 axis.title.x=element_blank(),
                 axis.text.y=element_blank(), 
                 axis.ticks.y=element_blank(),
                 axis.title.y=element_blank(),
                 strip.text.y=element_blank())

p5 <- ggplot(df[df$moving.function==unique(df$moving.function)[2],], aes(x=plotorder, y=CleanRate,
                                                                         color = CleanRate))
p5 <- p5 + geom_point() + theme(legend.position = "none")
p5 <- p5 + facet_grid(  number_of_obstacles ~ moving.function)
p5 <- p5 + theme(axis.text.x=element_blank(),
                 axis.ticks.x=element_blank(),
                 axis.title.x=element_blank(),
                 axis.text.y=element_blank(), 
                 axis.ticks.y=element_blank(),
                 axis.title.y=element_blank(),
                 strip.text.y=element_blank())

p5b <- ggplot(df[df$moving.function==unique(df$moving.function)[3],], aes(x=plotorder, y=CleanRate,
                                                                         color = CleanRate))
p5b <- p5b + geom_point() 
p5b <- p5b + facet_grid(  number_of_obstacles ~ moving.function)
p5b <- p5b + theme(axis.text.x=element_blank(),
                 axis.ticks.x=element_blank(),
                 axis.title.x=element_blank(),
                 axis.text.y=element_blank(), 
                 axis.ticks.y=element_blank(),
                 axis.title.y=element_blank())


p6 <- ggplot(df[df$moving.function==unique(df$moving.function)[1],], aes(x=plotorder, y=CleanRate))
p6 <- p6 + geom_boxplot() + theme(legend.position = "none")
p6 <- p6 + facet_grid(  number_of_obstacles ~ moving.function)
p6 <- p6 + theme(axis.text.x=element_blank(),
                 axis.ticks.x=element_blank(),
                 axis.title.x=element_blank(),
                 strip.text.y=element_blank())

p7 <- ggplot(df[df$moving.function==unique(df$moving.function)[2],], aes(x=plotorder, y=CleanRate))
p7 <- p7 + geom_boxplot() + theme(legend.position = "none")
p7 <- p7 + facet_grid(  number_of_obstacles ~ moving.function)
p7 <- p7 + theme(axis.text.x=element_blank(), 
                 axis.ticks.x=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 strip.text.y=element_blank())

p7b <- ggplot(df[df$moving.function==unique(df$moving.function)[3],], aes(x=plotorder, y=CleanRate))
p7b <- p7b + geom_boxplot() + theme(legend.position = "none")
p7b <- p7b + facet_grid(  number_of_obstacles ~ moving.function)
p7b <- p7b + theme(axis.text.x=element_blank(), 
                 axis.ticks.x=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 strip.text.y=element_blank())

grid.arrange(p6, p4, p7, p5, p7b, p5b, nrow = 1 , widths = c(1, 2, 1, 2, 1, 3))
