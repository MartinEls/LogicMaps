MakeThreshMatrix <- function(file = "threshold_matrix.png",
                         ch.a, ch.b,
                         thr.a = c(0, 1, 5),
                         thr.b = c(0, 1, 5),
                         operation = "and",
                         out.size = c(width = 2000, height = 2000)){
  
  list.size <- thr.a[3] + thr.b[3] + (thr.a[3] * thr.b[3])
  pic.matrix <- vector("list", length = list.size)
  list.counter <- 1
  
  png(filename = file, type = "Xlib",
      width = out.size[1],
      height = out.size[2])
  par(mfrow = c((thr.a[3]+1), (thr.b[3]+1)))
  
  frame()

  thr.a <- seq(from = thr.a[1], to = thr.a[2], length.out = thr.a[3])
  thr.b <- seq(from = thr.b[1], to = thr.b[2], length.out = thr.b[3])
  
  for(i in 1:length(thr.b)){
    pic.matrix[[list.counter]] <- ch.b > thr.b[i]
    image(pic.matrix[[list.counter]], col = c("black", "white"), 
          zlim = c(0,1), axes = F, main = "")
    paste0("ChannelB", sprintf("%03d", i))
    list.counter <- list.counter + 1
  }
  
  for(i in 1:length(thr.a)){
    pic.matrix[[list.counter]] <- ch.a > thr.a[i]
    paste0("ChannelA", sprintf("%03d", i))
    list.counter <- list.counter + 1
  } 
  
  for(i in 1:length(thr.a)){
    image(pic.matrix[[(length(thr.b) + i)]], col = c("black", "white"), 
          zlim = c(0,1), axes = F, main = "")
    for(j in 1:length(thr.b)){
      tmp <- RunLogicOperation(pic.matrix[[(length(thr.b) + i)]], 
                               pic.matrix[[j]],
                               operation)
      tmp <- Image(tmp)
      pic.matrix[[list.counter]] <- tmp
      list.counter <- list.counter + 1
      image(tmp, col = c("black", "white"), zlim = c(0,1), axes = F, main = "")
    }
  }
  dev.off()
  return(pic.matrix)
}
