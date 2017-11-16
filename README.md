
# 2x Image Resolution
This is a side project inspired by neural networks being developed to provide "Super Resolution" to resized images. I was curious how other machine learning algorithms would work in a similar application. Could a “shallow” learning model be used surpass traditional scaling techniques? 

## Outcome:
Working on my personal computer proved to be a limiting factor in my research in this area. It was difficult to work with more than one or two images in the training of data since a small 40kb image resulted in a 800mb R data frame! As a result, I frequently found myself running out of memory to perform some of the tasks. This limited the number variations I could try, especially with more "data hungry" approaches, it resulted in objects too large for me to process. 

However, given these limitations, while it does not perform at levels of that of neural networks designed to resize images, it does have some improvements on traditional resizing techniques. Considering that these decision trees were generated on a single image rather than thousands, small improvements were worth noting. 

(View image in full size to see the differences)
![Alt text](/Output/out5.jpg)


## Next Steps: 
Due to memory limitations on my computer, most of the next steps involve moving this research onto a platform than can provide more processing power and memory to handle variations of this approach that I could not perform on my personal computer. For example:
* Incorporate other high resolution images into the training data, not just the starting image. 
* Test different machine learning algorithms or an ensemble method. 
* Test different geometries when using surrounding pixels as predictors.


## Overview of the Program:

This was the starting image (200 x 250) used to resize and train the model. This image was resized downward from the source image. The original high resolution image (400 x 500) was used as a benchmark in model development.

![Alt text](/Output/out2.jpg)


Below is the general process of re-scaling the image. Grey squares represent existing color values, blue represents imputed values using the row model, yellow represents imputed values using the column model. The process order was then reversed by putting spaces between columns, then rows. The values from the two iterations were then averaged into one image.

![Alt text](/Output/out3.jpg)

Because of this resizing approach, two models needed to be trained, one for spacing between column and one for spacing between rows. Using the starting image as a source for training data, the surrounding pixels would inform the prediction each pixel (indicated by "X" in the image below). 

I used a decision tree model because of its ability to handle complex interactions in the data that would be suited for image data rather than traditional linear regression, which would conceptual result in a static weighted average. (In the final model used, a 5x5 square was used as opposed to a 3x3. A 7x7 model was also test but did not yield any noticeable improvements in image quality)

![Alt text](/Output/out4.jpg)


Once the program resizes the image, data from the full sized source image (400x500) was appended to the dataframe based on their x and y cooridinates and residuals were then computed. To assess the quality of the resizing of the image, the sum and mean absolute error were used, with a lower value indicating a better match to the source image. Values were on a greyscale from 0 (black) to 1 (white).

    #Benchmark Bilinear Photoshop
    > sum(residual$absresid)
    > [1] 2897.216
    > mean(residual$rse)
    > [1] 0.01448608
   
    #Final Model (Starter Image + Mirrored Starter Image)
    > sum(residual$absresid)
    > [1] 2745.826
    > mean(residual$rse)
    > [1] 0.01379112

