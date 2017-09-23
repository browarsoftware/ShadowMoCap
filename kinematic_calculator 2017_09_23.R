correctMe3D <- function(startC, stopC, signal1, signal2, signal3, all)
{
  diff2 <- all[stopC, signal1] - all[startC, signal1] 
  all[stopC:length(all$Hips.Dy),dz_vector] = all[stopC:length(all$Hips.Dy),dz_vector] - diff2
  
  diff2 <- all[stopC, signal2] - all[startC, signal2] 
  all[stopC:length(all$Hips.Dy),dx_vector] = all[stopC:length(all$Hips.Dy),dx_vector] - diff2
  
  
  diff2 <- all[stopC, signal3] - all[startC, signal3] 
  all[stopC:length(all$Hips.Dy),dy_vector] = all[stopC:length(all$Hips.Dy),dy_vector] - diff2

  return (all)
}



calculateKinematic <- function(dd)
{
  window_size <- 100 / length(dd$RightFoot.ax)
  
  require(smoother)
  zzR <- smth(sqrt(dd$RightFoot.ax ^ 2 + dd$RightFoot.ay ^ 2 + dd$RightFoot.az ^ 2),window = window_size,method = "gaussian") #SMOOTHING
  zzL <- smth(sqrt(dd$LeftFoot.ax ^ 2 + dd$LeftFoot.ay ^ 2 + dd$LeftFoot.az ^ 2),window = window_size,method = "gaussian") #SMOOTHING

  plot(zzR, type = 'n', xlab = "Acceleration [g]", ylab="Y [cm]")
  title("Heian Shodan")
  lines(zzR, col="red")
  lines(zzL, col="blue")

  zzR[is.na(zzR)] <- 0
  zzL[is.na(zzL)] <- 0  

  for (a in 1:(length(dd$RightFoot.ax) - 1))
  {
    if (zzR[a] > zzL[a] && zzR[a + 1] > zzL[a + 1])
    {
      dd <- correctMe3D(a, a + 1, "LeftFoot.Dz", "LeftFoot.Dx", "LeftFoot.Dy", dd)
    }
    else if (zzR[a] < zzL[a] && zzR[a + 1] < zzL[a + 1])
    {
      dd <- correctMe3D(a, a + 1, "RightFoot.Dz", "RightFoot.Dx", "RightFoot.Dy", dd)
    }
  }
  return (dd)
}

MoveToTheGround <- function (all, a, groundPosition,  signal1)
{
  diff2 <- all[a, signal1] - groundPosition
  all[a,dy_vector] = all[a,dy_vector] - diff2
  return (all)
}


dyEps <- 5

calculateKinematicCorrection <- function(dd)
{
  window_size <- 100 / length(dd$RightFoot.ax)
  
  require(smoother)
  zzR <- smth(dd$RightFoot.Dy,window = window_size,method = "gaussian") #SMOOTHING
  plot(zzR, type = 'n', xlab = "Time [ms]", ylab="Y [cm]")
  title("Heian Shodan")
  lines(zzR, col="red")
  zzL <- smth(dd$LeftFoot.Dy,window = window_size,method = "gaussian") #SMOOTHING
  lines(zzL, col="blue")
  
  zzR[is.na(zzR)] <- 0
  zzL[is.na(zzL)] <- 0
  
  for (a in 1:(length(dd$RightFoot.ax) - 1))
  {
    if (abs(zzR[a] - zzL[a]) > dyEps)
    {
      if (zzR[a] > zzL[a] && zzR[a + 1] > zzL[a + 1])
      {
        dd <- correctMe3D(a, a + 1, "LeftFoot.Dz", "LeftFoot.Dx", "LeftFoot.Dy", dd)
      }
      else if (zzR[a] < zzL[a] && zzR[a + 1] < zzL[a + 1])
      {
        dd <- correctMe3D(a, a + 1, "RightFoot.Dz", "RightFoot.Dx", "RightFoot.Dy", dd)
      }
    }
  }
  
  groundPosition <- dd$RightFoot.Dy[1]
  for (a in 1:(length(dd$RightFoot.ax)))
  {
    if (zzR[a] >= zzL[a])
    {
      #przesuñ ca³¹ sylwetkê, aby Y stopy dotyka³o ziemi
      dd <- MoveToTheGround(dd, a, groundPosition, "LeftFoot.Dy")
    }
    else if (zzR[a] < zzL[a])
    {
      dd <- MoveToTheGround(dd, a, groundPosition, "RightFoot.Dy")
    }
  }
  

  zzR <- smth(dd$RightFoot.Dy,window = window_size,method = "gaussian") #SMOOTHING
  zzL <- smth(dd$LeftFoot.Dy,window = window_size,method = "gaussian") #SMOOTHING
  plot(zzL, type = 'n', xlab = "Time [ms]", ylab="Y [cm]")
  title("Heian Shodan")
  lines(zzR, col="red")
  zzL <- smth(dd$LeftFoot.Dy,window = window_size,method = "gaussian") #SMOOTHING
  lines(zzL, col="blue")
  
  return (dd)
}

##############
#Magda
dataToCalc <- read.csv("e:\\repo\\data\\Heian Shodan -7_2.bvh.csv")
dx_vector <- names(dataToCalc)[grepl(".Dx",names(dataToCalc))]
dy_vector <- names(dataToCalc)[grepl(".Dy",names(dataToCalc))]
dz_vector <- names(dataToCalc)[grepl(".Dz",names(dataToCalc))]
dataToCalc <- calculateKinematic(dataToCalc)
dataToCalc <- calculateKinematicCorrection(dataToCalc)
write.csv(file ="e:\\repo\\results\\Heian Shodan -7_3_test2.bvh.csv",x = dataToCalc, row.names=FALSE, quote = FALSE)



dataToCalc <- read.csv("e:\\repo\\data\\Heian Nidan_2.bvh.csv")
dataToCalc <- calculateKinematic(dataToCalc)
dataToCalc <- calculateKinematicCorrection(dataToCalc)
write.csv(file ="e:\\repo\\results\\Heian Nidan -7_3_test2.bvh.csv",x = dataToCalc, row.names=FALSE, quote = FALSE)



dataToCalc <- read.csv("e:\\repo\\data\\Heian Sandan -3_2.bvh.csv")
dataToCalc <- calculateKinematic(dataToCalc)
dataToCalc <- calculateKinematicCorrection(dataToCalc)
write.csv(file ="e:\\repo\\results\\Heian Sandan -7_3_test2.bvh.csv",x = dataToCalc, row.names=FALSE, quote = FALSE)


dataToCalc <- read.csv("e:\\repo\\data\\Heian Yondan -3_2.bvh.csv")
dataToCalc <- calculateKinematic(dataToCalc)
dataToCalc <- calculateKinematicCorrection(dataToCalc)
write.csv(file ="e:\\repo\\results\\Heian Yondan -7_3_test2.bvh.csv",x = dataToCalc, row.names=FALSE, quote = FALSE)

######################
#Marcin

dataToCalc <- read.csv("e:\\repo\\data\\Pinian nidan2.bvh.csv")
dx_vector <- names(dataToCalc)[grepl(".Dx",names(dataToCalc))]
dy_vector <- names(dataToCalc)[grepl(".Dy",names(dataToCalc))]
dz_vector <- names(dataToCalc)[grepl(".Dz",names(dataToCalc))]
dataToCalc <- calculateKinematic(dataToCalc)
dataToCalc <- calculateKinematicCorrection(dataToCalc)
write.csv(file ="e:\\repo\\results\\Pinian_nidan.bvh.csv",x = dataToCalc, row.names=FALSE, quote = FALSE)

dataToCalc <- read.csv("e:\\repo\\data\\Fukyugata ni.bvh.csv")
dataToCalc <- calculateKinematic(dataToCalc)
dataToCalc <- calculateKinematicCorrection(dataToCalc)
write.csv(file ="e:\\repo\\results\\Fukyugata_ni.bvh.csv",x = dataToCalc, row.names=FALSE, quote = FALSE)


dataToCalc <- read.csv("e:\\repo\\data\\Fukyugata ichi.bvh.csv")
dataToCalc <- calculateKinematic(dataToCalc)
dataToCalc <- calculateKinematicCorrection(dataToCalc)
write.csv(file ="e:\\repo\\results\\Fukyugata_ichi.bvh.csv",x = dataToCalc, row.names=FALSE, quote = FALSE)


dataToCalc <- read.csv("e:\\repo\\data\\Pinian shodan.bvh.csv")
dataToCalc <- calculateKinematic(dataToCalc)
dataToCalc <- calculateKinematicCorrection(dataToCalc)
write.csv(file ="e:\\repo\\results\\Pinian_shodan.bvh.csv",x = dataToCalc, row.names=FALSE, quote = FALSE) 

