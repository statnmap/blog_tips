# R-Shiny app for expertise on microtubule
This is for expert detection of bundling on random images of cells between two time steps.

This app is presented on the blog: <//statnmap.com/2016-08-02-rshiny-expert-image-comparison-app>

## A Shiny web interface for expert image comparison

I participated to a scientific publication on the analysis of plant cell images. Part of the analysis was to define observation for each cell by visual expertise. The problem is that being able to observe an entire tissue when supposed to give an expertise for each cell biased the cell-centered expertise.
Thus, I produce separated images of each cell out of the tissue, mixed the images of different plant lines and effects tested on the tissue. I then proposed a R-shiny web interface to randomly show the images to the experts and save their observations. This allowed for a non-biased expertise, as the expert had no information on the origin of the cell shown.

## A download-upload feature to save and continue analysis on shinyapps.io

The expert image comparison was long: 40min for the small analysis and 3h for the complete one. As the shinyapp is freely hosted on the Rstudio shinyapps.io servers, it was not possible to save outputs of a specific session on the server. Thus, the web application has been built such that the experts can download a zip file of their partial expertise and come back later. They were then able to upload the beginning of their expertise and continue their analysis when they wanted. The download/upload feature was a good alternative to the limit of the free hosting service.

You can try this Shinyapp here :
https://statnmap.shinyapps.io/Visual_Expert/
* Code may be available on request.
