library(sqldf)
library(jpeg)
###########################################################
#Import image into data frame
###########################################################
pic2df<-function(data,greyscale=TRUE){
  painting<-data
  dimension    <- dim(painting)
  if(!(is.na(dimension[3]))){
    rgb <- data.frame(
      x = rep(1:dimension[2], each = dimension[1]),
      y = rep(dimension[1]:1, dimension[2]),
      R = as.vector(painting[,,1]), #slicing our array into three
      G = as.vector(painting[,,2]),
      B = as.vector(painting[,,3]))
    
    #Working in greyscale for simplicity
    rgb$grey<- rowSums(rgb[,c("R","G","B")])/3
    
  } else {
    rgb <- data.frame(
      x = rep(1:dimension[2], each = dimension[1]),
      y = rep(dimension[1]:1, dimension[2]),
      grey = as.vector(painting[,]))
  }
  
  if(greyscale){
    rgb<-rgb[,c("x","y","grey")]
  }  
  return(rgb)
}

#############################################################################################
#xyspacer - Double the numbers of x/y pixels by adding a space between each existing pixel
#          then shifts the numbering of x/y pixels to match the new position. Leaves NA in the
#          in the blank spaces
#     data - data frame - data frame with image data, must contain x, y and grey
#     xvar - string - variable name of the x coordinate. Defaults to "x"
#     yvar - string - variable name of the y coordinate. Defaults to "y"
#     color - string - variable name of the color value. Defaults to "grey"
#     spacer - string - "x"/"y" - REQUIRED to indicate expanding x or y direction
#############################################################################################
xyspacer<-function(data,xvar="x",yvar="y",color="grey",spacer){
  rgb<-data
  if(spacer=="x"){
    names(rgb)[names(rgb)==paste(xvar)] <- "x"
    names(rgb)[names(rgb)==paste(yvar)] <- "y"
  }  else if(spacer=="y"){
    #Simple transpose of x and y for y spaceing
    names(rgb)[names(rgb)==paste(xvar)] <- "y1"
    names(rgb)[names(rgb)==paste(yvar)] <- "x1"
    
    names(rgb)[names(rgb)=="y1"] <- "y"
    names(rgb)[names(rgb)=="x1"] <- "x"
    
  }  else{print("Missing 'spacer' parameter.")
    break}
  
  xseq <-seq(from = min(rgb$x), to = max(rgb$x), by=1)
  xseq2<-seq(from = min(rgb$x), to = (max(rgb$x)-min(rgb$x))*2+min(rgb$x),by=2)
  xseq3<-seq(from = min(rgb$x), to = (max(rgb$x)-min(rgb$x))*2+min(rgb$x),by=1)
  
  xspace<-as.data.frame(cbind(xseq,xseq2))
  
  #Merge Xseq2 on to the dataframe with original X coordinates
  rgb1<-sqldf('select a.*, b.xseq2
              from rgb as a left join xspace as b
              on a.x = b.xseq
              ')
  
  head(rgb1)
  
  #create a full set of x-y coordinates for the new image size.
  imagematrix<-unique(expand.grid(xseq3,rgb1$y))
  names(imagematrix)<-c("xseq3","y")
  head(imagematrix)
  
  rgb2<-sqldf('select a.xseq3 as x_base,
              a.y as y_base, 
              b.*
              from imagematrix as a left join rgb1 as b
              on a.xseq3 = b.xseq2 and a.y = b.y
              ')
  
  
  rgb2[,"i_flg"] <- 0
  rgb2[is.na(rgb2$grey),"i_flg"] <- 1
  
  out<-rgb2
  out[,c("x","y","yseq2","xseq2")]<- NULL
  head(out)
  if(spacer=="x"){
    #rename to designated x and y names
    names(out)[names(out)=="x_base"] <- paste(xvar)
    names(out)[names(out)=="y_base"] <- paste(yvar)
  }
  if(spacer=="y"){
    #rename to designated x and y names
    names(out)[names(out)=="x_base"] <- paste(yvar)
    names(out)[names(out)=="y_base"] <- paste(xvar) 
  }
  
  return(out)
}


#############################################################################################
# nearestpixel - This function is used turn the surrounding pixels in to separate variables
#  into a a dataframe format. Accepts to arguments:
#     data - dataframe - Must contain the variables x, y, and grey
#     range - numeric - Default 1, indicates the radius of the nearest pixels you want to 
#                       convert into separte variables
#
#   Loop I and J as values relative to each pixel
#
#   In a table like this:
#    _ _ _ _ _
#    _ _ _ _ _
#    _ _ O _ _
#    _ _ _ _ _
#    _ _ _ _ _
#
#   Returns original dataframe with variables with the naming convention x#y# (or x_#y_# for 
#   negative coordinates). This is the relative position of the pixels being converted.
#
#############################################################################################
nearestpixel<-function(data, range=1, fill=TRUE){
  temp1<-data
  for(i in -(range):range){
    for( j in -(range):range){
      
      temp2<-temp1[,c("x","y", "grey")]
      
      temp2$x<-temp2$x+i
      temp2$y<-temp2$y+j
      temp2[,gsub("-","_",paste("x",i,"y",j, sep=""))]<-temp2$grey
      temp2$grey<-NULL
      head(temp2)
      
      #merge onto main dataset by x/y_base
      temp1<-sqldf(
        gsub("-","_",
             paste('select a.* , b.',"x",i,"y",j,'
               from temp1 as a left join temp2 as b
               on a.x = b.x and a.y = b.y
               ', sep=""
             )
        )
      )
    }
  }
  if(fill){
    temp1[is.na(temp1)] <- 0
  }
  return(temp1) 
}

#Example test
#library(jpeg)
#painting     <- readJPEG("D:/work/R workspace/Grey - Original Image.jpg")
#test<-pic2df(painting)
#head(test)

#test1<-xyspacer(test,spacer="y")
#head(test1)

#test2<-nearestpixel(test1,fill=FALSE)
#head(test2)





