library(openxlsx)
library(FactoMineR)
library(factoextra)
library(basicPlotteR) # To overlap labels
library(shape) # To arrows plot

dfr <- read.xlsx("ForPCA.xlsx", sheet = "data")

(summary(dfr))

df <- dfr[,c("Textura", "Sabor", "Apariencia")]
rownames(df) <- dfr$INSTN

res.pca <- PCA(df, scale.unit = TRUE, graph = TRUE)

#print(res.pca)

barplot(res.pca$eig[,2], main = "Percentage of variance", ylab = "(%)", ylim = c(0, 80))

### PCA PLOT ###

var = res.pca$var$coord   # "coord. for the variables"
ind = res.pca$ind$coord   # "coord. for the individuals"

var <- as.data.frame(var)
ind <- as.data.frame(ind)

write.csv(ind, "ind_pca.csv")
write.csv(var, "var_pca.csv")

png(file.path("figure_pca.png"), width = 800, height = 800, units = "px", pointsize = 12)
# add extra space to right margin of plot within frame
par(cex.axis = 1.8, mar = c(5.5, 5.5, 3.5, 3.5), family = "serif")

# plotting the pca
plot(ind$Dim.1, ind$Dim.2, axes = FALSE, pch = 20, col = "blue", cex = 2.,
     xlim = c(-3.7, 3.7), ylim = c(-3.7, 3.7), xlab = "", ylab = "")
box()

# adding the x-axis and y-axis
abline(h = 0, v = 0, lty = 2, col = "gray30")

# adding the axis
axis(side = 1, -3:3, xlim = c(-3.7, 3.7), family = "serif", tck = 0.02)
axis(side = 2, -3:3, ylim = c(-3.7, 3.7), family = "serif", tck = 0.02, las = 1)

# addin minor ticks
rug(x = seq(-3.5, 3.5, by = 1), ticksize = 0.01, side = 1)
rug(x = seq(-3.5, 3.5, by = 1), ticksize = 0.01, side = 2)

# adding the axis label
mtext(side = 1, expression("CP 1 (75.1%)"), family = "serif", line = 3.8, cex = 2.4)
mtext(side = 2, expression("CP 2 (17.9%)"), family = "serif", line = 3.3, cex = 2.4)

# adding the accessions' name
addTextLabels(ind$Dim.1, ind$Dim.2, labels = rownames(ind), 
              col.label = "blue", cex.label = 1.1, col.line = "gray80",
              lwd = 1, lty = 2)

# missing plot to add the arrows for the variables
par(new = TRUE)
plot(x = NULL, y = NULL, axes = FALSE, 
     xlim = c(-1., 1.), ylim = c(-1., 1.), xlab = "", ylab = "")

Arrows(0, 0, var[1, 1], var[1, 2], arr.type = "simple", arr.length = 0.5, col = "red", lwd = 2)
Arrows(0, 0, var[2, 1], var[2, 2], arr.type = "simple", arr.length = 0.5, col = "red", lwd = 2)
Arrows(0, 0, var[3, 1], var[3, 2], arr.type = "simple", arr.length = 0.5, col = "red", lwd = 2)

# add the axis for the variables
axis(side = 3, seq(-1, 1, by = 0.2), xlim = c(-1., 1.), tck = 0.02, family = "serif", col = "red", col.axis = "red")
axis(side = 4, seq(-1, 1, by = 0.2), ylim = c(-1., 1.), tck = 0.02, family = "serif", col = "red", col.axis = "red", las = 1)

# add minor ticks for the variables 
rug(x = seq(-0.9, 0.9, by = 0.2), ticksize = 0.01, side = 3, col = "red")
rug(x = seq(-0.9, 0.9, by = 0.2), ticksize = 0.01, side = 4, col = "red")

# add name of variables
text(0.94,  0.62, bquote("Sabor"), family = "serif", font = 2, col = "red", lwd = 1.5, pos = 2, cex = 2.4)
text(1.05, -0.10, bquote("Textura"), family = "serif", font = 2, col = "red", lwd = 1.5, pos = 2, cex = 2.4)
text(0.95, -0.58, bquote("Apariencia"), family = "serif", font = 2, col = "red", lwd = 1.5, pos = 2, cex = 2.4)

dev.off()