library(spatial)
library(plot3D)
library(plot3Drgl)

ozone <- read.table("https://stats.idre.ucla.edu/stat/r/faq/ozone.csv", sep=",", header=T)

#bubble plot (1)
symbols(ozone$Lon, ozone$Lat, circles=ozone$Av8top)

#bubble plot(2)
radius <- sqrt(ozone$Av8top/ pi )
symbols(ozone$Lon, ozone$Lat, circles=radius)
symbols(ozone$Lon, ozone$Lat, circles=radius, inches=0.25, fg="white", bg="red")


plot<-ggplot()+
geom_point(data=ozone, aes(x.coord,y.coord),col="blue", shape=19, size=2.5)+
theme_bw()+
xlab("Longitude")+
ylab("Latitude")
plot
plot+
geom_point(data = ozone, aes(x=x.coord,y=y.coord,col=exam[,3]),shape=19,size=2.5)+
theme_bw()+
xlab("Longitude")+
ylab("Latitude")+
scale_colour_gradientn(name= "CO",colours = rainbow(15,start=0.45,end=1))

library(spdep)

dnearneigh(coords, 0.1, 0.8)


dn<-dnearneigh(coords, 0.1, 0.8)

moran.plot(ozone$Av8top, nb2listw(dn))
resI <- localmoran(ozone$Av8top, nb2listw(dn))
resI


plot2<-ggplot()+
geom_point(data=Lmoran, aes(ozone.Lon,ozone.Lat),col="blue", shape=19, size=2.5)+
theme_bw()+
xlab("Longitude")+
ylab("Latitude")
plot
plot2+
geom_point(data = Lmoran, aes(x=ozone.Lon,y=ozone.Lat,col=Lmoran[,3]),shape=19,size=2.5)+
theme_bw()+
xlab("Longitude")+
ylab("Latitude")+
scale_colour_gradientn(name= "Lmoran",colours = rainbow(15,start=0.45,end=1))

# 3D PLOT
scatter3D(ozone$Lon,ozone$Lat,ozone$Av8top, zcol=ozone$Av8top)
scatter3D(ozone$Lon,ozone$Lat,ozone$Av8top, zcol=ozone$Av8top,ticktype="detailed")
scatter3D(ozone$Lon,ozone$Lat,ozone$Av8top, zcol=ozone$Av8top,pty="g",ticktype="detailed")
library("plot3Drgl")

plotrgl()

##fitting a trend surface model by least squares
library(spatial)
x<-ozone$Lon
y<-ozone$Lat
z<-ozone$Av8top

fit.sfc3 <- surf.ls(3,x,y,z)
summary(fit.sfc3)
fit.sfc3$beta

#evaluate trend surface over a grid
trsurf3 <- trmat(fit.sfc3, min(x), max(x), min(y), max(y), 50)

trsurf3

scatter3D(x,y,z,surf = list(x = trsurf3$x, y = trsurf3$y, z = trsurf3$z,
NAcol = "grey", shade = 0.1))

plotrgl()


fit.sfc2 <- surf.ls(2,x,y,z)
summary(fit.sfc2)
fit.sfc2$beta

trsurf2 <- trmat(fit.sfc2, min(x), max(x), min(y), max(y), 50)

trsurf2

scatter3D(x,y,z,surf = list(x = trsurf2$x, y = trsurf2$y, z = trsurf2$z,
NAcol = "grey", shade = 0.1))


library(plot3Drgl)

contour(trsurf2)

#####to simulate data

library(gstat)
xy <- expand.grid(1:50, 1:50)
names(xy) <- c('x','y')
g.dummy <- gstat(formula=z~1+x+y, locations=~x+y, dummy=T, beta=1, model=vgm(psill=0.025, range=5, model='Exp'), nmax=20)
yy <- predict(g.dummy, newdata=xy, nsim=4)



rho <- 0.9
N <- 100
x.coord <- runif(N,0,50)
y.coord <- runif(N,0,50)
points <- cbind(x.coord,y.coord)
e <- rnorm(N,0,1)
dnb <- dnearneigh(points, 0, 150)
dsts <- nbdists(dnb, points)
idw <- lapply(dsts, function(x) 1/x)
lw <- nb2listw(dnb, glist=idw, style="W")
inv <- invIrW(lw, rho)
y <- as.vector(inv %*% e)
moran.test(y, lw)
moran.plot(y, lw)




N <- 100
x.coord <- runif(N,0,100)
y.coord <- runif(N,0,100)
z=3*(x.coord)^2+(y.coord)^2+x.coord+y.coord+e

