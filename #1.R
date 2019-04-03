data <- read.table("C:/대학원/2019-1/1.전공/1.다변량분석/CD (updated) of JW/table1.txt")
colnames(data) <- c("x1","x2","x3","x4","x5")
plot(data$x2, data$x3)


library(ggplot2)
library(gridExtra)
pMain <- ggplot(data, aes(x = x2, y = x3)) +
  geom_point()
pTop <- ggplot(data, aes(x = x2)) +
  geom_histogram()
pRight <- ggplot(data, aes(x = x3)) +
  geom_histogram() + coord_flip()
pEmpty <- ggplot(data, aes(x = x2, y = x3)) +
  geom_blank() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        line = element_blank(),
        panel.background = element_blank())

grid.arrange(pTop, pEmpty, pMain, pRight,
             ncol = 2, nrow = 2, widths = c(3, 1), heights = c(1, 3))

mean(data$x1); mean(data$x2); mean(data$x3); mean(data$x4); mean(data$x5)
cov(data)
cor(data)
