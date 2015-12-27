#Plot barcharts template
makebarplot <- function(x,y,title,xlabel,ylabel,las,col){
    barplot(y, names = x, xlab = xlabel, ylab = ylabel,
            main = title,las = las,cex.lab=1, cex.names=0.8, col = col)
}
