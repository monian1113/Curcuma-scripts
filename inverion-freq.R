library(ggplot2)

library(tidyverse)
library(rstatix)
library(ggpubr)
library(Rmisc)

mydata2 <- read.table("inverion-freq.txt", header = T) 
df <- mydata2
p1= ggpaired(df, x="group", y="less", color = "group", line.color = "gray",xlab = 'g1-L and g2-L',
             line.size = 0.2, point.size = 1,palette = "npg")+ stat_compare_means(method = "t.test",paired = TRUE)


p2= ggpaired(df, x="group", y="more", color = "group", line.color = "gray", xlab = 'g1 and g2',
             line.size = 0.2, point.size = 1,palette = "npg")+ stat_compare_means(method = "t.test",paired = TRUE)
box<-ggarrange(p1, p2,  ncol = 2,common.legend = T)

ggsave(box,filename = "inverion-freq.pdf",width = 3.2,height = 4)
ggsave(box,filename = "inverion-freq.png",width = 3.2,height = 4)