##A script to create a plot that is a schematic describing forecast reconciliation.

library(latex2exp)
library(tikzDevice)
library(scatterplot3d)
library(tidyverse)
options(tikzLatexPackages=c(getOption("tikzLatexPackages"),"\\usepackage{amsfonts}","\\usepackage{bm}"))
# options(tikzLatexPackages=c(getOption("tikzLatexPackages"),"\\usepackage{amsfonts}","\\usepackage{bm}"),
#         tikzFooter = "\\caption{a caption}")

## 3D hierarchy
set.seed(1989)

X <- matrix(runif(40),20,2) %>% as.data.frame()
X[,3] <- X[,1] + X[,2]

colnames(X) <- c(expression('$y_A$'), 
                 expression('$y_B$'), 
                 expression('$y_{Tot}$'))
# dr <- getwd()
# file <- file.path(dr, '3D_hierarchy.tex')
tikz('3D_hierarchy.tex',standAlone = TRUE)

s3d <- scatterplot3d(X, color = "red",
                     angle=55, pch = 16, grid=TRUE, box=FALSE,cex.lab = 2)


# Add regression plane
s3d$plane3d(0,1,1, draw_lines = T, draw_polygon = T)

#
#s1pro<-s3d$xyz.convert(1,1,0)
s1 <- s3d$xyz.convert(1,0,1)
s2 <- s3d$xyz.convert(0,1,1)
arrows(0,0,s1$x,s1$y,lwd = 3)
arrows(0,0,s2$x,s2$y,lwd = 3)
text(s1$x,s1$y, TeX('$\\textbf{s}_1$}'),col = 1, adj = c(-.1, -.1),cex = 2)
text(s2$x,s2$y, TeX('$\\textbf{s}_2$}'),col = 1, adj = c(-.1, -.1),cex = 2)
text(3, 3, "{\\Huge $\\mathfrak{s}$}",col = 1, adj = c(-.1, -.1))
dev.off()

tools::texi2dvi('3D_hierarchy.tex',pdf=T)

##Point forecast reconciliation.

tikz('pointforerec_schematic.tex',height=5)
plot.new()
plot.window(xlim = c(-0.25,4),ylim = c(-0.5,5))
lines(c(0,0),c(-1,5))
lines(c(-1,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,3.5,1.75,lwd=3)
text(3.5,1.75,"{\\Large ${\\bm S}$}",pos=4)
text(2,1,"{\\huge $\\mathfrak{s}$}",pos=1,offset = 1.3)
#lines(c(-0.25,1),c(-1,4),lwd=2)
arrows(0,0,1,4,lwd=3)
text(1,4,"{\\Large ${\\bm R}$}",pos=3)

lines(c(0,2),c(-3,5),lty=2)
points(1.5,3,pch=20,cex=2,col='blue')
text(1.5,3,"{\\huge $\\color{blue}{\\hat{\\bm{y}}}$}",pos = 3,offset = 1.5)

text(1.25,2,"{\\huge ${\\color{blue} s\\circ g}$}",pos=4)

points(6/7,3/7,pch=20,cex=2,col='red')
text(6/7,3/7,"{\\huge $\\color{red}{\\tilde{\\bm{y}}}$}",pos=1,offset = 1.5)

arrows(1.5,3,6/7,3/7,col='blue')

dev.off()


##OLS reconciliation

tikz('orth_pointforerec_schematic.tex',width=4)
plot.new()
plot.window(xlim = c(-0.25,4),ylim = c(-0.5,4),asp=1)
lines(c(0,0),c(-1,3))
lines(c(-1,3),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,3.5,1.75,lwd=3)
text(3.5,1.75,"{\\large $\\mathfrak{s}$}",pos=4)

yhat<-c(1.5,3)

points(1.5,3,pch=20,cex=2,col='blue')
text(1.5,3,"{\\large $\\color{blue}{\\hat{\\bm{y}}}$}",pos = 3,offset = 1.5)

SP<-matrix(c(0.8,0.4,0.4,0.2),2,2)

ytilde<-SP%*%yhat

points(ytilde[1],ytilde[2],pch=20,cex=2,col='red')
text(ytilde[1],ytilde[2],"{\\large $\\color{red}{\\tilde{\\bm{y}}}$}",pos=1,offset = 1.5)

#lines(c(0,2),c(-3,5),lty=2)
arrows(yhat[1],yhat[2],ytilde[1],ytilde[2],col='blue')
y <- c(6/7,3/7)
#lines(c(yhat[1],yhat[2]),c(y[1],y[2]), lty = 2)
arrows(yhat[1],yhat[2],y[1],y[2], lty = 2)

#text(1.25,2,"{\\large ${\\color{blue} S\\circ P}$}",pos=4)

points(y[1],y[2],pch=20,cex=2,col='black')
text(y[1],y[2],"{\\large $\\color{black}{{\\bm{y}}}$}",pos=1,offset = 1.5)

dev.off()


##--How MinT outperforms--##

set.seed(1989)

# In sample direction

tikz('MinT_justif/Error_dir.tex',height=5, standAlone = TRUE)
plot.new()
plot.window(xlim = c(-0.25,4),ylim = c(-0.5,5))
lines(c(0,0),c(-1,5))
lines(c(-1,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,3.5,1.75,lwd=3)
text(3.5,1.75,"{\\huge $\\mathfrak{s}$}",pos=4)

rmat<-matrix(c(1,4,-0.5,0.25),2,2,byrow = FALSE)
e<-t(rmat%*%matrix(rnorm(200,0,0.3),2,100))
points(e[,1],e[,2],pch=19,col='orange')

arrows(0,0,1,4,lwd=3)
text(1,4,"{\\Large ${\\bm R}$}",pos=3)

# text(0.6,0.3,"{\\huge $\\color{black}{\\bm{y}}$}",pos = 1,offset = 1)
dev.off()

tools::texi2dvi('MinT_justif/Error_dir.tex',pdf=T)

# True value/Target

tikz('MinT_justif/Target.tex',height=5, standAlone = TRUE)
plot.new()
plot.window(xlim = c(-0.25,4),ylim = c(-0.5,5))
lines(c(0,0),c(-1,5))
lines(c(-1,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,3.5,1.75,lwd=3)
text(3.5,1.75,"{\\huge $\\mathfrak{s}$}",pos=4)

arrows(0,0,1,4,lwd=3)
text(1,4,"{\\Large ${\\bm R}$}",pos=3)

points(0.6,0.3,pch=20,cex=2,col='black')
text(0.6,0.3,"{\\huge $\\color{black}{\\bm{y}}$}",pos = 1,offset = 1)
dev.off()

tools::texi2dvi('MinT_justif/Target.tex',pdf=T)

# Possible estimates

tikz('MinT_justif/SampleEstimates.tex',height=5, standAlone = TRUE)
plot.new()
plot.window(xlim = c(-0.25,4),ylim = c(-0.5,5))
lines(c(0,0),c(-1,5))
lines(c(-1,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,3.5,1.75,lwd=3)
text(3.5,1.75,"{\\huge $\\mathfrak{s}$}",pos=4)

rmat<-matrix(c(1,4,-0.5,0.25),2,2,byrow = FALSE)
e<-t(rmat%*%matrix(rnorm(200,0,0.3),2,100))
e_sample <- e + matrix(rep(c(0.6,0.3), 100), nrow = 100, ncol = 2, byrow = T)
points(e_sample[,1],e_sample[,2],pch=19,col='gray')

arrows(0,0,1,4,lwd=3)
text(1,4,"{\\Large ${\\bm R}$}",pos=3)

points(1.5,3,pch=20,cex=2,col='blue')
text(1.5,3,"{\\huge $\\color{blue}{\\hat{\\bm{y}}}$}",pos = 3,offset = 1.5)

points(0.6,0.3,pch=20,cex=2,col='black')
text(0.6,0.3,"{\\huge $\\color{black}{\\bm{y}}$}",pos = 1,offset = 1)
dev.off()

tools::texi2dvi('MinT_justif/SampleEstimates.tex',pdf=T)


#Oblique projection

tikz('MinT_justif/ObliqueProjection.tex',height=5, standAlone = TRUE)
plot.new()
plot.window(xlim = c(-0.25,4),ylim = c(-0.5,5))
lines(c(0,0),c(-1,5))
lines(c(-1,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,3.5,1.75,lwd=3)
text(3.5,1.75,"{\\huge $\\mathfrak{s}$}",pos=4)

rmat<-matrix(c(1,4,-0.5,0.25),2,2,byrow = FALSE)
e<-t(rmat%*%matrix(rnorm(200,0,0.3),2,100))
e_sample <- e + matrix(rep(c(0.6,0.3), 100), nrow = 100, ncol = 2, byrow = T)
points(e_sample[,1],e_sample[,2],pch=19,col='gray')

arrows(0,0,1,4,lwd=3)
text(1,4,"{\\Large ${\\bm R}$}",pos=3)

lines(c(0,2),c(-3,5),lty=2)
points(1.5,3,pch=20,cex=2,col='blue')
text(1.5,3,"{\\huge $\\color{blue}{\\hat{\\bm{y}}}$}",pos = 3,offset = 1.5)

#text(1.25,2,"{\\huge ${\\color{blue} s\\circ g}$}",pos=4)

points(6/7,3/7,pch=20,cex=2,col='red')
text(6/7,3/7,"{\\huge $\\color{red}{\\tilde{\\bm{y}}}$}",pos=1,offset = 1.5)

arrows(1.5,3,6/7,3/7,col='blue')
points(0.6,0.3,pch=20,cex=2,col='black')
text(0.6,0.3,"{\\huge $\\color{black}{\\bm{y}}$}",pos = 1,offset = 1)
dev.off()

tools::texi2dvi('MinT_justif/ObliqueProjection.tex',pdf=T)




###3D Scatter plot

set.seed(1989)

# X <- matrix(runif(2),1,2) %>% as.data.frame()
X <- matrix(c(0.2,0.2),1,2) %>% as.data.frame()
X[,3] <- X[,1] + X[,2]
colnames(X) <- c(expression('$X$'), 
                 expression('$Y$'), 
                 expression('$Z$'))

tikz('Schem_3D.tex',standAlone = FALSE, onefile = FALSE)

s3d <- scatterplot3d(X, color = "red", xlim = c(0,0.5), ylim = c(0,0.5), zlim = c(0,0.5),
                     angle=55, pch = 20, grid=TRUE, box=FALSE,cex.lab = 2, cex.symbols = 2)


# Add regression plane
s3d$plane3d(0,0.5,0.5, draw_lines = T, draw_polygon = T)

#
#s1pro<-s3d$xyz.convert(1,1,0)
# s1 <- s3d$xyz.convert(1,0,1)
p1 <- s3d$xyz.convert(0.5,0.4,0.5)
arrows(0,0,p1$x,p1$y,lwd = 3)

# drawing a point in line
p <- c(0.4, 0, (p1$y/p1$x)*0.4) ##p1$y/p1$x is the slope of line
s3d$points3d(p[1], p[2], p[3], pch = 20, col = "blue", cex = 2)

#Projection from the red point to blue point
p2 <- s3d$xyz.convert(X[1,1], X[1,2], X[1,3])
p3 <- s3d$xyz.convert(p[1], p[2], p[3])
arrows(p2$x,p2$y,p3$x,p3$y,lwd = 3, col = "blue")

# arrows(0,0,s2$x,s2$y,lwd = 3)
text(p1$x,p1$y, "{$\\mathfrak{L}$}",col = 1, adj = c(-.1, -.1),cex = 2)
text(p2$x,p2$y, "{$\\mu$}",col = 1, pos=1,offset = 1,cex = 2)
text(p3$x,p3$y, "{$\\bar{\\mu}$}",col = 1, pos=1,offset = 1,cex = 2)

text(3,5.2,"{\\Huge ${\\color{blue} s\\circ g}$}",pos=4)
text(4, 3, "{\\Huge $\\mathfrak{s}$}",col = 1, adj = c(-.1, -.1))
dev.off()


tikz('probforerec_schematic.tex',height=5,pointsize = 7, standAlone = TRUE)
plot.new()
plot.window(xlim = c(-0.25,4),ylim = c(-0.5,5))
lines(c(0,0),c(-1,5))
lines(c(-1,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,3.5,1.75,lwd=3)
text(3.5,1.75,"{\\huge $\\mathfrak{s}$}",pos=4)

#lines(c(-0.25,1),c(-1,4),lwd=2)
arrows(0,0,1,4,lwd=3)
text(1,4,"{\\Large ${\\bm R}$}",pos=3)

lines(c(0,1.5),c(-1,5),lty=2)

lines(c(1,2.5),c(-1,5),lty=2)

lines(c(0.25,1.25),c(0,0),lwd=4)
text(0.75,0,"{\\huge $\\mathcal{B}$}",pos = 1,offset = 1)
lines(c(2/7,10/7),c(1/7,5/7),lwd=4,col='red')
text(1.5,-0.5,"{\\huge $\\color{red}{s(\\mathcal{B})}$}",pos = 4,offset = 1.2)
arrows(1.5,-0.5,6/7,3/7,col='red')

cord.x<-c(-0.25, 0.75, 2.75, 1.75)
cord.y<-c(-2, -2, 6, 6)

polygon(cord.x,cord.y,col=adjustcolor('blue',alpha=0.1),border = NA)

text(3,3,"{\\huge $\\color{blue}{g^{-1}(\\mathcal{B}})$}",col='blue',pos = 3,offset=1.2)
arrows(3,3,1.5,2,col='blue')

dev.off()

tools::texi2dvi('probforerec_schematic.tex',pdf=T)
