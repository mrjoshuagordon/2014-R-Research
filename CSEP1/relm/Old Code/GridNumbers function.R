


# assigned each pixel a number
gridNumber <- match(coords,fgrd) 

for(i in 1:nrow(fgrid)){
  fg = fgrid[i,]  
  fgs = paste(fg[1], fg[2], fg[3], fg[4], sep="")
  gridNumber[which(coords == fgs)] = i
  
  if(i %% 100 == 0) print(i)
  
}


