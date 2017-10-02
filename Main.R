#Library
library(jpeg)
library(ggplot2)
library(rpart)
library(sqldf)

setwd("D:/work/R workspace/")

#Data processing functions saved here
#Calls the following functions: 
#   pic2df - Converts image to dataframe
#   nearestpixel - Adds surrounding pixel information for each pixel
#   xyspacer - Adds spaces between each pixel to be filled in later by prediction model

source("Image Scaling Functions.R")


#Import Picture color information AH_Start
#This is the image that will be resized
painting <- readJPEG("D:/work/R workspace/AH_start.jpg")

#Image to resize, no filled boarder
pic1 <- pic2df(painting)


#Training data to build the models
#Using starting image and a horizontal flipped version

train1        <- readJPEG("D:/work/R workspace/AH_start - altered1.jpg")
#train2       <- readJPEG("D:/work/R workspace/AH_start - altered2.jpg")
#train3       <- readJPEG("D:/work/R workspace/AH_start - altered3.jpg")
#train4       <- readJPEG("D:/work/R workspace/AH_start - altered4.jpg")



#Training Data for Models
#Train on itself
t0 <- pic2df(painting)
t0 <- nearestpixel(t0, range = 2, fill = TRUE)

#Train on horizontal flip
t1 <- pic2df(train1)
t1 <- nearestpixel(t1, range = 2, fill = TRUE)

#t2 <- pic2df(train2)
#t2 <- nearestpixel(t2, range = 2, fill = TRUE)

#t3 <- pic2df(train3)
#t3 <- nearestpixel(t3, range = 2, fill = TRUE)

#t4 <- pic2df(train4)
#t4 <- nearestpixel(t4, range = 2, fill = TRUE)

#t5 <- pic2df(train5)
#t5 <- nearestpixel(t5, range = 2, fill = TRUE)


#Stack training data for modeling

training <- rbind(t0, t1)

#Clearing Memory due to RAM limitations
t1 <- t2 <- t3 <- t4 <- t5 <- painting <- train1 <- train2 <- train3 <- train4 <- train5 <- NULL

#####################
#Fitting rpart model#
#####################

#CART Decision Tree (as big as you can make it)
#Only using data available after the splits
#     X   X
#     X   X
#     X - X
#     X   X
#     X   X
xcartmodel <- rpart(grey ~ x1y0 + x1y1 + x1y2 +  x1y_1 + x1y_2 +
                    x_1y0 + x_1y1 + x_1y2 + x_1y_1 + x_1y_2, data = training, cp = .000000002)


#Only using data available after the splits
#
#     
#   X X X X X  
#       -
#   X X X X X  
#     

ycartmodel <- rpart(grey ~ x_2y1 + x_1y1 + x0y1 + x1y1 + x2y1 +
                          x_2y_1 + x_1y_1 + x0y_1 + x1y_1 + x2y_1, data = training, cp = .000000002)


#Clearing Memory due to RAM limitations
training <- NULL

####################
#Spacing Pixels Out#
####################

pic2 <- xyspacer(pic1, spacer = "x")

pic3 <- nearestpixel(pic2, range = 2, fill = FALSE)

#Clearing Memory due to RAM limitations
pic2 <- NULL



###########################
#Fill in spaces with Model#
###########################

pic3$pred <- predict(xcartmodel, pic3)
head(pic3)

pic3[pic3$i_flg==1, 'grey'] <- pic3[pic3$i_flg==1, 'pred']
head(pic3)


.f = function() {
  
  funcdat <- pic3
  #look at the potential scaling ability
  ggplot(data = funcdat, aes(x = x, y = y, color = grey, shape = "_"))+ 
    geom_point(size = 1) + 
    scale_colour_gradient(low = "black", high = "white")+
    scale_x_continuous(limits = c(min(funcdat[, "x"]), 
                                  max(funcdat[, "x"])))+
    scale_y_continuous(limits = c(min(funcdat[, "y"]), 
                                  max(funcdat[, "y"])))+
    scale_shape_manual(values = 15) +
    coord_fixed() +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(), 
          panel.background = element_blank()) 
  funcdat <- NULL
  
  
}

#################
#strip variables#
#################
pic4 <- pic3[, c("x", "y", "grey")]

#Clearing Memory due to RAM limitations
pic3 <- NULL

####################
#Spacing Pixels Out#
####################
pic5 <- xyspacer(pic4, spacer = "y")
pic5 <- nearestpixel(pic5, range = 2, fill = FALSE)
str(pic5)

pic5$pred <- predict(ycartmodel, pic5)
pic5[pic5$i_flg==1, 'grey'] <- pic5[pic5$i_flg==1, 'pred']


############################################
# Reverse generation order, y spacer then x#
# Second verse, same as the first          #
############################################

####################
#Spacing Pixels Out#
####################

rpic2 <- xyspacer(pic1, spacer = "y")
head(rpic2)

rpic3 <- nearestpixel(rpic2, range = 2, fill = FALSE)
head(rpic3)

#Clearing Memory due to RAM limitations
rpic2 <- NULL

###########################
#Fill in spaces with Model#
###########################

rpic3$pred <- predict(ycartmodel, rpic3)
head(rpic3)

rpic3[rpic3$i_flg==1, 'grey'] <- rpic3[rpic3$i_flg==1, 'pred']
head(rpic3)


.f = function() {
  
  funcdat <- rpic3
  #look at the potential scaling ability
  ggplot(data = funcdat, aes(x = x, y = y, color = grey, shape = "_"))+ 
    geom_point(size = 1) + 
    scale_colour_gradient(low = "black", high = "white")+
    scale_x_continuous(limits = c(min(funcdat[, "x"]), 
                                  max(funcdat[, "x"])))+
    scale_y_continuous(limits = c(min(funcdat[, "y"]), 
                                  max(funcdat[, "y"])))+
    scale_shape_manual(values = 15) +
    coord_fixed() +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(), 
          panel.background = element_blank()) 

  funcdat<-rpic3
  funcdat$x <- funcdat$x-min(funcdat$x)+1
  funcdat$y <- max(funcdat$y)-funcdat$y+1
  funcdat <- funcdat[order(-funcdat$x, funcdat$y), ]
  
  out <- matrix(0, nrow = length(unique(funcdat$y)), ncol = length(unique(funcdat$x)), 1:3)
  out[cbind(funcdat$y, funcdat$x)] <- c(funcdat$grey)
  
  
  writeJPEG((out), target = "AH y-filler.jpg", quality = 1)
  
}

#################
#strip variables#
#################
rpic4 <- rpic3[, c("x", "y", "grey")]

#Clearing Memory due to RAM limitations
rpic3 <- NULL
####################
#Spacing Pixels Out#
####################

rpic5 <- xyspacer(rpic4, spacer = "x")
rpic5 <- nearestpixel(rpic5, range = 2, fill = FALSE)

rpic5$pred <- predict(xcartmodel, rpic5)
rpic5[rpic5$i_flg==1, 'grey'] <- rpic5[rpic5$i_flg==1, 'pred']
str(rpic5)

#Clearing Memory due to RAM limitations
rpic3 <- NULL

######################################
# Merge images together
######################################

rpic5$grey2 <- rpic5$grey

#create unique ID for SQL join
rpic5$id <- paste(rpic5$x, rpic5$y)
pic5$id <- paste(pic5$x, pic5$y)

mixpic <- sqldf(" 
select a.x, a.y, a.grey as grey1, b.grey2
from pic5 as a left join rpic5 as b
on a.id = b.id
              ")

summary(mixpic)
mixpic$grey <- (mixpic$grey1+mixpic$grey2)/2


.f = function() {
  
  funcdat <- mixpic
  #look at the potential scaling ability
  ggplot(data = funcdat, aes(x = x, y = y, color = grey, shape = "_"))+ 
    geom_point(size = 1) + 
    scale_colour_gradient(low = "black", high = "white")+
    scale_x_continuous(limits = c(min(funcdat[, "x"]), 
                                  max(funcdat[, "x"])))+
    scale_y_continuous(limits = c(min(funcdat[, "y"]), 
                                  max(funcdat[, "y"])))+
    scale_shape_manual(values = 15) +
    coord_fixed() +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(), 
          panel.background = element_blank()) 
  
}
######################################
######################################
.f = function() {
  
  funcdat <- pic1
  #look at the potential scaling ability
  ggplot(data = funcdat, aes(x = x, y = y, color = grey, shape = "_"))+ 
    geom_point(size = 1) + 
    scale_colour_gradient(low = "black", high = "white")+
    scale_x_continuous(limits = c(min(funcdat[, "x"]), 
                                  max(funcdat[, "x"])))+
    scale_y_continuous(limits = c(min(funcdat[, "y"]), 
                                  max(funcdat[, "y"])))+
    scale_shape_manual(values = 15) +
    coord_fixed() +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(), 
          panel.background = element_blank()) 
  
}

###############################################################
# Evaluate image from higher resolution source image available#
###############################################################

#Import Validation image
  #painting     <- readJPEG("D:/work/R workspace/AH_PS_Bicubic.jpg")
  #psbicub <- pic2df(painting)
painting <- readJPEG("D:/work/R workspace/AH_source.jpg")
orgpic <- pic2df(painting)

names(orgpic)[names(orgpic)=="grey"] <- "og_grey"
summary(orgpic)

residual <- sqldf("
                select a.*, b.og_grey
                from mixpic as A left join orgpic as b
                on a.x = b.x and a.y = b.y
                ")
residual$resid <- (residual$og_grey-residual$grey)
residual$absresid <- abs(residual$og_grey-residual$grey)
residual$rse <- sqrt((residual$og_grey-residual$grey)^2)
sum(residual$absresid)
mean(residual$rse)
sd(residual$rse)

plot(residual$grey, residual$og_grey)

summary(pic5)


.f = function() {
###################################
# Manually Save resized image to JPG       #
###################################
  
  funcdat <- mixpic
  #look at the potential scaling ability
  ggplot(data = funcdat, aes(x = x, y = y, color = grey, shape = "_"))+ 
    geom_point(size = 1) + 
    scale_colour_gradient(low = "black", high = "white")+
    scale_x_continuous(limits = c(min(funcdat[, "x"]), 
                                  max(funcdat[, "x"])))+
    scale_y_continuous(limits = c(min(funcdat[, "y"]), 
                                  max(funcdat[, "y"])))+
    scale_shape_manual(values = 15) +
    coord_fixed() +
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(), 
          panel.background = element_blank()) 

  
  funcdat$x <- funcdat$x-min(funcdat$x)+1
  funcdat$y <- max(funcdat$y)-funcdat$y+1
  funcdat <- funcdat[order(-funcdat$x, funcdat$y), ]
  
  out <- matrix(0, nrow = length(unique(funcdat$y)), ncol = length(unique(funcdat$x)), 1:3)
  out[cbind(funcdat$y, funcdat$x)] <- c(funcdat$grey)
  
  
  writeJPEG((out), target = "AH_mean-xyyx-base+alter1_resized.jpg", quality = 1)
  
}


######################################################################
#Evaluate Neural Network Resizing image against source image         #
######################################################################
painting <- readJPEG("D:/work/R workspace/waifu_2x_none_AH.jpg")
psbicub <- pic2df(painting)
residual2 <- sqldf("
                select a.*, b.grey as bc_grey
                from residual as A left join psbicub as b
                on a.x = b.x and a.y = b.y
                ")
str(residual2)
residual2$resid2 <- (residual2$og_grey-residual2$bc_grey)
residual2$absresid2 <- abs(residual2$og_grey-residual2$bc_grey)
residual2$rse2 <- sqrt((residual2$og_grey-residual2$bc_grey)^2)
sum(residual2$absresid2)
mean(residual2$rse2)
sd(residual2$rse2)
######################################################################
#####################################################################

#Benchmark Bicubic Photoshop
#> sum(residual2$absresid2)
#[1] 2836.557
#> mean(residual2$rse2)
#[1] 0.01424682
#> sd(residual2$rse2)
#[1] 0.02091745

#Benchmark Bilinear Photoshop
#> sum(residual$absresid)
#[1] 2897.216
#> mean(residual$rse)
#[1] 0.01448608

#Benchmark Nearest Neighbor Photoshop
#> sum(residual$absresid)
#[1] 3110.443
#> mean(residual$rse)
#[1] 0.01555222

#Benchmark: Actual Fully Sized Image as training data
#> sum(residual$absresid)
#[1] 2677.138
#> mean(residual$rse)
#[1] 0.01344613


#Base image: X then Y
#> sum(residual$absresid)
#[1] 2888.957
#> mean(residual$rse)
#[1] 0.01451001

#Base Image: Averaged  x-y then y-x
#> sum(residual$absresid)
#[1] 2764.054
#> mean(residual$rse)
#[1] 0.01388267

#Base Image + Alter Image 1: Averaged  x-y then y-x
#> sum(residual$absresid)
#[1] 2745.826
#> mean(residual$rse)
#[1] 0.01379112

#Base Image + Alter Image 2: Averaged  x-y then y-x
#> sum(residual$absresid)
#[1] 2747.843
#> mean(residual$rse)
#[1] 0.01380125

#Base Image + Alter Image 5: Averaged  x-y then y-x
#> sum(residual$absresid)
#[1] 2745.436
#> mean(residual$rse)
#[1] 0.01378916
#> sd(residual$rse)
#[1] 0.01815314

#External Hi-Res Image : Averaged x-y then y-x
#> sum(residual$absresid)
#[1] 2801.119
#> mean(residual$rse)
#[1] 0.01406884
#> sd(residual$rse)
#[1] 0.01890136

#Base Image + External Hi-Res Image: Averaged x-y then y-x
#> sum(residual$absresid)
#[1] 2733.899
#> mean(residual$rse)
#[1] 0.01373122
#> sd(residual$rse)
#[1] 0.01840808

#Range 3 instead of 2
#Base Image: Averaged  x-y then y-x

#> sum(residual$absresid)
#[1] 2772.071
#> mean(residual$rse)
#[1] 0.01392294

#Range 3 instead of 2
#Base Image + Alter Image 1: Averaged  x-y then y-x

#> sum(residual$absresid)
#[1] 2764.662
#> mean(residual$rse)
#[1] 0.01388573

#Base + Alter Image 1
#> sum(residual$absresid)
#[1] 2874.737
#> mean(residual$rse)
#[1] 0.01443858

#Base + Alter Image 2
#> sum(residual$absresid)
#[1] 2868.538
#> mean(residual$rse)
#[1] 0.01440745

#Base + Alter Image 1 + 2
#> sum(residual$absresid)
#[1] 2876.497
#> mean(residual$rse)
#[1] 0.01444743

#Base + Alter Image 1 + 2 + 3
#> sum(residual$absresid)
#[1] 2876.125
#> mean(residual$rse)
#[1] 0.01444556

#Base + Alter Image 1 + 2 + 3 + 4
#> sum(residual$absresid)
#[1] 2882.018
#> mean(residual$rse)
#[1] 0.01447516



#head(test1)

#test2 <- nearestpixel(test1, fill=FALSE)
#head(test2)
