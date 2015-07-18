ShowThresholdDep <- function(pic,
                             thresh = c(0, 1, 4),
                             pic.col = c("black", "white"),
                             out.size = c(row = 2, col = 3, width = 1000, height = 700),
                             file = "threshold.png",
                             keep.orig = TRUE,
                             make.hist = TRUE){
  png(filename = file, type = "cairo",
      width = out.size[3],
      height = out.size[4])
  par(mfrow = c((out.size[1]), (out.size[2])))
  
  if(keep.orig){
    image(flip(pic), col = gray(0:255 / 255), 
          zlim = c(0,1), axes = F, main = "Grayscale original")
    box()
  }
  
  if(make.hist){
    hist(pic)
  }
  
  thresh <- seq(from = thresh[1], to = thresh[2], length.out = thresh[3])
  
  pic.matrix <- vector("list", length = length(thresh))
  
  for(i in 1:length(thresh)){
    pic.matrix[[i]] <- pic > thresh[i]
    main.text <- paste0("Threshold ", sprintf("%.3f", thresh[i]))
    image(flip(pic.matrix[[i]]), col = pic.col, 
          zlim = c(0,1), axes = F, main = main.text)
    box()
  }
  dev.off()
  return(pic.matrix)
}
