                      ## data exploration ##

data_Envo <- read.csv("Envodata1980.csv", header = T)
View(data_Envo)
dim(data_Envo)
str(data_Envo)

                      ## Spatial autocorrelation ##

Temp_dists <- as.matrix(dist(cbind(data_Envo$long, data_Envo$lat)))
Temp_dists_inv <- 1/Temp_dists
diag(Temp_dists_inv) <- 0
library(ape)
Moran.I(data_Envo$Temp, Temp_dists_inv)

# calculate the distances using the spherical law of cosines.

library(codep)
gcd <- gcd.slc(Temp_dists, radius = 6371)
gcd_inv <- 1/gcd
gcd_inv <- as.matrix(gcd_inv)
Moran.I(data_Envo$Temp, gcd_inv)

# localized Moran's I.

library(lctools)
localized_Moran <- l.moransI(Temp_dists, 6, data_Envo$Temp, WType = "Bi-square"
                             , scatter.plot = T, family = "adaptive")
localized_Moran
localized_Moran[c(1:5, 108:112),]
localized_Moran[51:55,]

# check the outliers.
layout(matrix(c(1,2,3,3),2,2, byrow = T))
boxplot(data_Envo$Temp, main = "Boxplot of Temperature", col = "lightblue",
        horizontal = T, xlab = "Temperature")
hist(data_Envo$Temp, main = "Histogram of Temperature", col = "lightgreen",
      xlab = "Temperature")
vector <- data_Envo$Temp
qqnorm(vector)
qqline(vector, col = "blue")

# long and lat. with Temp

par(mfrow = c(1,2))
plot(data_Envo$lat, data_Envo$Temp, 
     main = "scatter plot of latitude with Temperature", xlab = "latitude",
     ylab = "Temperature", col = "blue")
plot(data_Envo$long, data_Envo$Temp, 
     main = "scatter plot of longitude with Temperature", xlab = "longitude",
     ylab = "Temperature", col = "red")


# test normality

library(MVN)
result <- mvn(data_Envo[,c(1,2,6)], mvnTest =  "mardia")
result

# plots.
#sp plot.

library(lattice)
library(sp)
library(spdep)
library(spData)
library(sf)
library(spatialEco)
 
# prepare coordinates, data, and proj 4string.

coords_Temp <- data_Envo[ , c("long", "lat")]   # coordinates.
data_Temp   <- data_Envo[ , c("Temp")]   # data.
class(data_Temp)
data_Temp <- as.data.frame(data_Temp)
local <- data.frame(Ii = localized_Moran[,2])
class(local)
View(local)
crs_Temp    <- CRS("+init=epsg:28992") # proj 4string of coords.

# make the Spatial Points Data Frame object.

spdf_Temp <- SpatialPointsDataFrame(coords      = coords_Temp,
                               data        = data_Temp, 
                               proj4string = crs_Temp)
spdf_local <- SpatialPointsDataFrame(coords      = coords_Temp,
                                  data    = local, 
                               proj4string = crs_Temp)
spplot(spdf_Temp)
spplot(spdf_Temp, main = "Distribution of Temp. in the area",
       xlab = "Longitude", ylab = "Latitude",colorkey = T, cex = 1.5)
        
spplot(spdf_local)
spplot(spdf_local, main = "Distribution of Localized Moran in the area",
       xlab = "Longitude", ylab = "Latitude",colorkey = T, cex = 1.5) 
       

# Moran's I plot.

knn <- knearneigh(spdf_Temp, k=3, longlat = NULL)
knn2nb<-knn2nb(knn)
mp <- moran.plot(data_Envo$Temp, nb2listw(knn2nb), main = "Moran's I plot",
                 xlab = "Temperature", ylab = "Spatially lagged of Temperature")

Local <- localmoran(x = data_Envo$Temp, listw = nb2listw(knn2nb))
moran.map <- cbind(data_Envo, local)
quadrant <- vector(mode="numeric",length=nrow(Local))

# centers the variable of interest around its mean
m.Temp <- data_Envo$Temp - mean(data_Envo$Temp)     

# centers the local Moran's around the mean
m.local <- Local[,1] - mean(Local[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
quadrant[m.Temp >0 & m.local>0] <- 4  
quadrant[m.Temp <0 & m.local<0] <- 1      
quadrant[m.Temp <0 & m.local>0] <- 2
quadrant[m.Temp >0 & m.local<0] <- 3
quadrant[Local[,5]>signif] <- 0   

# LISA plot

library(ncf)
lisa <- lisa(data_Envo$long, data_Envo$lat, data_Envo$Temp, neigh = 3,
             latlon=TRUE)
lisa
plot(lisa)

brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(data_Envo$Temp,
     pch=16,col=colors[findInterval(quadrant,brks,all.inside=FALSE)],
     main = "LISA plot", xlab = "location", ylab = "Temperature", cex = 1.5)
box()
legend("topright", legend = c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")
                      ## Trend Surface Model ##


















                      ## IDW  ##
library(phylin)
library(tidyverse)
library(gstat)
library(sp)
library(spatial)
library(spdep)
library(terra)
library(tmap)

data_Envo <- read.csv("Envodata1980.csv", header = T)
View(data_Envo)


data1<-data_Envo[,c("Temp")]
coords<-data_Envo[,c("long","lat")]
data<-as.data.frame(data1)
crs<- CRS("+init=epsg:28992")  #proj4string of coords

#make the SpatialpointsDataFrame object

spdf<-SpatialPointsDataFrame(coords = coords,
                             data   = data,
                             proj4string=crs)

s.grid<-spsample(spdf,type="regular",n=30000)
idw<-gstat::idw(data_Envo$Temp~1,locations=spdf,newdata=s.grid,idp=2)
idw
spplot(idw)
spplot(idw["var1.pred"])

                      ## Kriging ##

library(gstat)
# Variogram.

evgmcloud_Temp <- variogram( data_Envo[, c("Temp")]~1,spdf_Temp,cloud=T)
plot(evgmcloud_Temp, main = "Variogram Cloud", xlab = "Distance",
     ylab = "Semivariogram")
evgm1_Temp <- variogram( data_Envo[, c("Temp")]~1,spdf_Temp, cressie = F)
plot(evgm1_Temp, main = "Variogram plot using Mathern", xlab = "Distance",
     ylab = "Semivariogram", col = "blue")
summary(evgm1_Temp) # from 0.8392 to 16.6425
evgm2_Temp <- variogram(data_Envo[, c("Temp")]~1,spdf_Temp,boundaries=seq(1,17,0.5))
plot(evgm2_Temp, main = "Variogram plot ", xlab = "Distance",
     ylab = "Semivariogram", col = "blue")
evgm3_Temp <- variogram(data_Envo[, c("Temp")]~1,spdf_Temp, cressie = T)
plot(evgm3_Temp, main = "Variogram plot using Cressie-Hawkins", xlab = "Distance",
     ylab = "Semivariogram", col = "blue")

# it is clear to be linear Variogram, but we can try all possible to find the best

evgm2 <- variogram(data_Envo[, c("Temp")]~1,spdf_Temp,boundaries=seq(0,20,0.5))
plot(evgm2)
evgm3 <- variogram(data_Envo[, c("Temp")]~1,spdf_Temp,boundaries=seq(0,20,1))
plot(evgm3)

# choose the best parametric model.
                               # evgm1_Temp #

fvgm11 <- fit.variogram(evgm1_Temp,vgm("Sph"))
fvgm11
fvgm21 <- fit.variogram(evgm1_Temp,vgm("Exp"))
fvgm21
fvgm31 <- fit.variogram(evgm1_Temp,vgm("Gau"))
fvgm31
fvgm41 <- fit.variogram(evgm1_Temp,vgm("Lin"))
fvgm41
fvgm51 <- fit.variogram(evgm1_Temp,vgm("Lin", nugget = 1, range = 20, psill = 42 ))
fvgm51

plot(evgm1_Temp,model=fvgm11)
plot(evgm1_Temp,model=fvgm21)
plot(evgm1_Temp,model=fvgm31, main = "Guassian fitting using Mathern", 
     xlab = "Distance", ylab = "Semivariogram")
plot(evgm1_Temp,model=fvgm41, main = "Linear fitting", 
     xlab = "Distance", ylab = "Semivariogram")
plot(evgm1_Temp,model=fvgm51)

attr(fvgm11, "SSErr")
attr(fvgm21, "SSErr")
attr(fvgm31, "SSErr")  # minimum error = 39.76464.
attr(fvgm41, "SSErr") 
attr(fvgm51, "SSErr")

                          # evgm3_Temp #

fvgm113 <- fit.variogram(evgm3_Temp,vgm("Sph"))
fvgm113
fvgm213 <- fit.variogram(evgm3_Temp,vgm("Exp"))
fvgm213
fvgm313 <- fit.variogram(evgm3_Temp,vgm("Gau"))
fvgm313
fvgm413 <- fit.variogram(evgm3_Temp,vgm("Lin"))
fvgm413
fvgm513 <- fit.variogram(evgm3_Temp,vgm("Lin", nugget = 1, range = 20, psill = 42 ))
fvgm513

plot(evgm3_Temp,model=fvgm113)
plot(evgm3_Temp,model=fvgm213)
plot(evgm3_Temp,model=fvgm313, main = "Guassian fitting using Cressie-Hawkins", 
     xlab = "Distance", ylab = "Semivariogram")
plot(evgm3_Temp,model=fvgm413, main = "Linear fitting", 
     xlab = "Distance", ylab = "Semivariogram")
plot(evgm3_Temp,model=fvgm513)

attr(fvgm113, "SSErr")
attr(fvgm213, "SSErr")
attr(fvgm313, "SSErr")  # minimum error = 116.1622.
attr(fvgm413, "SSErr") 
attr(fvgm513, "SSErr")

# there is no a difference whether using Cressie in estimate the variogram or 
# not and the fitted models using both does not different too much. both give
# us the best model to fit the variogram is the Guassian.
# so, we will complete with matheron estimation.
                            # evgm2 #

fvgm12 <- fit.variogram(evgm2,vgm("Sph"))
fvgm12
fvgm22 <- fit.variogram(evgm2,vgm("Exp"))
fvgm22
fvgm32 <- fit.variogram(evgm2,vgm("Gau"))
fvgm32
fvgm42 <- fit.variogram(evgm2,vgm("Lin"))
fvgm42
fvgm52 <- fit.variogram(evgm2,vgm("Lin", nugget = 1, range = 20, psill = 42 ))
fvgm52

plot(evgm2,model=fvgm12)
plot(evgm2,model=fvgm22)
plot(evgm2,model=fvgm32)
plot(evgm2,model=fvgm42)
plot(evgm2,model=fvgm52)

attr(fvgm12, "SSErr")
attr(fvgm22, "SSErr")
attr(fvgm32, "SSErr")  
attr(fvgm42, "SSErr") # minimum error = 333.2122.
attr(fvgm52, "SSErr")
                               # evgm3 #

fvgm13 <- fit.variogram(evgm3,vgm("Sph"))
fvgm13
fvgm23 <- fit.variogram(evgm3,vgm("Exp"))
fvgm23
fvgm33 <- fit.variogram(evgm3,vgm("Gau"))
fvgm33
fvgm43 <- fit.variogram(evgm3,vgm("Lin"))
fvgm43
fvgm53 <- fit.variogram(evgm3,vgm("Lin", nugget = 1, range = 20, psill = 42 ))
fvgm53

plot(evgm3,model=fvgm13)
plot(evgm3,model=fvgm23)
plot(evgm3,model=fvgm33)
plot(evgm3,model=fvgm43)
plot(evgm3,model=fvgm53)

attr(fvgm13, "SSErr")
attr(fvgm23, "SSErr")
attr(fvgm33, "SSErr") # minimum error = 248.908.
attr(fvgm43, "SSErr")
attr(fvgm53, "SSErr")


# fitting simple kriging on a grid

s_grid <- spsample(spdf_Temp, type = "regular", n = 30000)
plot(spdf_Temp)
plot(s_grid)
points(spdf_Temp, col = "red")

# Ordinary Kriging.


Long <- data_Envo[, c("long")]
Lat <- data_Envo[, c("lat")]
krig.est <- krige(data_Envo[,"Temp"] ~1, spdf_Temp, newdata = s_grid, model = fvgm31)

#plotting using sp plot.

spplot(krig.est)
spplot(krig.est["var1.pred"], main = "Prediction map")
spplot(krig.est["var1.pred"], main = "Prediction map", colorkey = T)
spplot(krig.est["var1.var"], main = "Standard error map")
spplot(krig.est["var1.var"], main = "Standard error map", colorkey = T)


                           ## GWRM ##

library(spgwr)
attach(data_Envo)

#fixed bandwidth#

col.lm<- lm(Temp~elev,data=data_Envo)
summary(col.lm)

col.bw<-gwr.sel(Temp~elev,data=data_Envo,
                coords=cbind(long,lat))
col.gauss<-gwr(Temp~elev,data=data_Envo,
               coords=cbind(long,lat),bandwidth = col.bw,gweight=gwr.Gauss,hatmatrix = TRUE)
col.gauss
spplot(col.gauss$SDF,"localR2",colorkey=TRUE,main = "Local:R2")
spplot(col.gauss$SDF, "elev", colorkey=TRUE,main = "Local slope: Elevation")

#####################################################
#adaptive bandwidth
data.adapt.guess<-gwr.sel(Temp~elev,data=data_Envo,coords=cbind(long,lat),adapt = TRUE)
res.adapt<-gwr(Temp~elev,data=data_Envo,
               coords=cbind(long,lat),adapt = data.adapt.guess)
res.adapt
spplot(res.adapt$SDF,"localR2",col.regions = bpy.colors(20),main = "Local:R2")
spplot(res.adapt$SDF,"elev",colorkey=TRUE,main = "Local slope: Elevation")

# Significance plot of elev.

t_ELEV<-elev/sd(elev)

# Assuming t_ELEV is a list or array
for (i in 1:length(t_ELEV)) {
  if (abs(t_ELEV[i]) > 1.96) {  # alph = 0.05
    t_ELEV[i] = 1
  } else {
    t_ELEV[i] = 0
  }
}

data_Envo$sig = t_ELEV

knn <- knearneigh(spdf, k=3, longlat = NULL)
knn2nb<-knn2nb(knn)

Local <- localmoran(x = data_Envo$Temp, listw = nb2listw(knn2nb))
moran.map <- cbind(data_Envo, local)
quadrant <- vector(mode="numeric",length=nrow(Local))

# centers the variable of interest around its mean
m.elev <- data_Envo$elev - mean(data_Envo$elev)     

# centers the local Moran's around the mean
m.local <- Local[,1] - mean(Local[,1])    


# builds a data quadrant

quadrant[m.elev == 0] <- 0
quadrant[data_Envo$sig == 1] <- 1
table(data_Envo$sig)  # only 10 are 1 (significant)

brks <- c(0,1)
colors <- c("red","green")
plot(data_Envo$elev,
     pch=16,col=colors[findInterval(quadrant,brks,all.inside=FALSE)],
     main = "Significance plot of Elevation", xlab = "location", ylab = "Elevation", cex = 1.5)
box()
legend("topleft", legend = c("insignificant", "significant"),
       fill=colors,bty="n")


############ Trend Surface Model ############ 

library(spatial)
library(plot3D)
library(plot3Drgl)


#bubble plot (1)
symbols(data_Envo$long, data_Envo$lat, circles=data_Envo$Temp)

#bubble plot(2)
radius <- sqrt(data_Envo$Temp/ pi )
symbols(data_Envo$long, data_Envo$lat, circles=radius)
symbols(data_Envo$long, data_Envo$lat, circles=radius, inches=0.25, fg="white", bg="red")

library(ggplot2)
x.coord <- data_Envo$long
y.coord <- data_Envo$lat

plot<-ggplot()+
  geom_point(data=data_Envo, aes(x.coord,y.coord),col="blue", shape=19, size=2.5)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")
plot
plot+
  geom_point(data = data_Envo, aes(x=x.coord,y=y.coord,col=data_Envo[,6]),shape=19,size=2.5)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_colour_gradientn(name= "temperature",colours = rainbow(15,start=0.45,end=1))

library(spdep) 

dnearneigh(coords, 0.1, 0.8)


dn<-dnearneigh(coords, 0.1, 0.8)

moran.plot(data_Envo$Temp, nb2listw(dn))
resI <- localmoran(data_Envo$Temp, nb2listw(dn))
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
scatter3D(data_Envo$long,data_Envo$lat,data_Envo$Temp, zcol=data_Envo$Temp)
scatter3D(data_Envo$long,data_Envo$lat,data_Envo$Temp, zcol=data_Envo$Temp,ticktype="detailed")
scatter3D(data_Envo$long,data_Envo$lat,data_Envo$Temp, zcol=data_Envo$Temp,pty="g",ticktype="detailed" ,phi = 30, theta = 25)
library("plot3Drgl")

plotrgl()

##fitting a trend surface model by least squares
library(spatial)
x<-data_Envo$long
y<-data_Envo$lat
z<-data_Envo$Temp

# fir TSM with p = 1 ,2 ,3 , 4
tsm1 <- surf.ls(1,x,y,z)
summary(tsm1)
# Assuming tsm1 is the linear regression model
predicted_values <- tsm1$beta[1] + tsm1$beta[2] * data_Envo$long + tsm1$beta[3] * data_Envo$lat
mse1 <- mean((data_Envo$Temp - tsm1$wz)^2) # is this true ?


tsm2 <- surf.ls(2,x,y,z)
summary(tsm2)
mse2 <- mean((data_Envo$Temp - tsm2$wz)^2)

tsm3 <- surf.ls(3,x,y,z)
summary(tsm3)
mse3 <- mean((data_Envo$Temp - tsm3$wz)^2)

tsm4 <- surf.ls(4,x,y,z)
summary(tsm4)
tsm4$beta
mse4 <- mean((data_Envo$Temp - tsm4$wz)^2)

#evaluate trend surface over a grid(using p = 3)
trsurf3 <- trmat(tsm3, min(x), max(x), min(y), max(y), 50)

trsurf3



scatter3D(x,y,z,surf = list(x = trsurf3$x, y = trsurf3$y, z = trsurf3$z,
                            NAcol = "grey", shade = 0.1) , phi = 30, theta = 60 , main = '3D plot when p = 3')

# phi to adjust angle up & down , theta for left & right


trsurf2 <- trmat(tsm2, min(x), max(x), min(y), max(y), 50)
scatter3D(x,y,z,surf = list(x = trsurf2$x, y = trsurf2$y, z = trsurf2$z,
                            NAcol = "grey", shade = 0.1) , phi = 30, theta = 60 , main = '3D plot when p = 2')



plotrgl()


#evaluate trend surface over a grid(using p = 2)

trsurf2 <- trmat(tsm2, min(x), max(x), min(y), max(y), 50)

trsurf2

scatter3D(x,y,z,surf = list(x = trsurf2$x, y = trsurf2$y, z = trsurf2$z,
                            NAcol = "grey", shade = 0.1) , phi = 30, theta = 60, main = '3D plot when p = 2')

# plot contour(using p = 3)

library(plot3Drgl)

contour(trsurf3)
title(main = "Contour Plot")



filled.contour(trsurf3, color.palette = function(n) rev(heat.colors(n)), 
               plot.axes = {
                 contour(trsurf3, add = TRUE, lwd = 1, labels = NULL)
               })
# Add a title to the plot
title(main = "Contour Plot")

# Add a color legend in the top left corner
legend("topleft", legend = "Temp Values", fill = rev(heat.colors(10)))




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
inv <- invIrW(lw, rho)                        ## which package contains this function??
y <- as.vector(inv %*% e)
moran.test(y, lw)
moran.plot(y, lw)




N <- 100
x.coord <- runif(N,0,100)
y.coord <- runif(N,0,100)
z=3*(x.coord)^2+(y.coord)^2+x.coord+y.coord+e

