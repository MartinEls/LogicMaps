ConvToBin <- function(int.vec = 0:7, cut.off = 3){
  sapply(int.vec, function(x) 
    substr(paste(sapply(strsplit(paste(rev(intToBits(x))),""),
                        `[[`,2),collapse=""),
           (32 - (cut.off - 1)), 32) )
}

RunImgTernaryOp <- function(ch.a, ch.b, ch.c,
                            tv = c(0,0,0,0,0,0,0,1)){
  # Test: dim(r) == dim(g) == dim(b)
  img.dim <- dim(ch.a)
  op <- paste0(ch.a,ch.b,ch.c)
  
  # compare pixel pattern with truth table
  x <- ConvToBin(0:7)
  
  for(i in 1:length(x)){
    op[grepl(x[i], op)] <- tv[i]
  }
  
  op <- as.numeric(op)
  dim(op) <- img.dim
  return(op)
}

### Unit Tests: ################################################################
# RunImgTernaryOp(
#   ch.a = matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3),
#   ch.b = matrix(c(1,0,1,0,1,0,0,0,1), nrow = 3),
#   ch.c = matrix(c(1,0,0,0,1,0,1,0,1), nrow = 3) )
################################################################################