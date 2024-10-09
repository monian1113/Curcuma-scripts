library(ggplot2)
a<-read.table("result.xls",header=T)
b <- melt(a, id=c("chr","pos"),variable.name="variable",value.name="value")
p1 <- ggplot(subset(b,chr=="1"), aes(x=pos, y=value)) +
  geom_rect(aes(xmin = 19167948, xmax = 19763037,ymin = -0.71, ymax = 1),fill="#F9E5E5", alpha = 1) +
  geom_rect(aes(xmin = 21820124, xmax = 21873148,ymin = -0.71, ymax = 1),fill="#F9E5E5", alpha = 1)+ 
  geom_point(aes(color=variable), alpha=0.7, size=0.5)+
  scale_color_manual(values = c("#23649e","#d4483d"))+
  facet_grid(chr~.,scales="free")+theme_bw() +
  scale_x_continuous(labels = scales::comma, breaks = seq(0,200000000, 20000000))+
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_text(size=6),axis.text.y=element_text(size=6))
pdf(file = "result.pdf",height=8.5,width=6.4)
p1
dev.off()