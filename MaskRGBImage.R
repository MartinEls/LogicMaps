MaskRGBImage <- function(rgb.image,
                         mask.image,
                         inverse = FALSE,
                         channel = "r"){
  # TODO: größe der bilder prüfen
  
  # check channel
  if(channel == "r"){
    replace.vec = c(r = 1, g = 0, b = 0)
  }
  if(channel == "g"){
    replace.vec = c(r = 0, g = 1, b = 0)
  }
  if(channel == "b"){
    replace.vec = c(r = 0, g = 0, b = 1)
  }
  #else Fehler  
    
  r.image <- rgb.image[,,1]
  g.image <- rgb.image[,,2]
  b.image <- rgb.image[,,3]
  
  r.image[blue] <- replace.vec["r"]
  g.image[blue] <- replace.vec["g"]
  b.image[blue] <- replace.vec["b"]
  
  n.image <- rgbImage(r.image, g.image, b.image)
  return(n.image)
}