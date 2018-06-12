library(tesseract)

text <- ocr("~/Desktop/SampleCatalogPages/UCD_Lehmann_3083.jpg")
cat(text)
text

install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("mxnet")

install.packages("devtools")
library(devtools)
require(devtools)
install_version("DiagrammeR", version = "0.9.0", repos = "http://cran.us.r-project.org")
require(DiagrammeR)

cran <- getOption("repos")
cran["dmlc"] <- "https://s3-us-west-2.amazonaws.com/apache-mxnet/R/CRAN/"
options(repos = cran)
install.packages("mxnet",dependencies = T)
library(mxnet)

source("https://bioconductor.org/biocLite.R")
biocLite("EBImage")

#put non catalog image
imageDir <- ("~/Desktop/SampleCatalogPages")
library(EBImage)
not_catalog <- readImage(file.path(imageDir,"UCD_Lehmann_3083.jpg"))
display(not_catalog)

#put catalog image
catalog <- readImage(file.path(imageDir,"UCD_Lehmann_3666.jpg"))
display(catalog)



library(pbapply)
width <- 28
height <- 28

extract_feature <- function(dir_path, width, height, is_catalog = TRUE, add_label = TRUE) {
  img_size <- width*height
  image_names <- list.files(dir_path)}
    if (add_label) {
      image_names <- image_names[grepl(ifelse(is_catalog, "catalog", "not catalog"),image_names)]
      label <- ifelse(is_catalog, 0, 1)
    }
    print(paste("Start processing", length(images_names), "images"))
    
    feature_list <- pblapply(images_names, function(imgname){
      #read image
      img <- readImage(file.path(dir_path, imgname))
      #resize image 
      img_resized <- resize(img, w = width, h = height)
      #set to grayscale
      grayimg <- channel(img_resized, "gray")
      #get image as matrix
      img_matrix <- grayimg@.Data
      #coerce to a vector
      img_vector <- as.vector(t,(img_matrix))
    return(img_vector)
    })
    #bind the list of a vector into matrix
    feature_matrix <- do.call(rbind, feature_list)
    feature_matrix <- as.data.frame(feature_matrix)
    #set names
    names(feature_matrix) <- paste0("pixel", c(1:img_size))
    if(add_label) {
      feature_matrix <- cbind(label = label, feature_matrix)
    }
    return(feature_matrix)
  }

#process catalog and non catalog images seperately and save into data.frame 
catalog_data <- extract_feature(dir_path = imgDir, width = width, height = height)
noncatalog_data <- extract_feature(dir_path = imgDir, width = width, height = height, is_catalog = FALSE)
dim(catalog_data)
