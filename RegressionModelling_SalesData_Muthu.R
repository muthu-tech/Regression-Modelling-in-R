setwd("C:/Users/muthu/Desktop/Fall 2017/AdvAnalyticsInR/project1")
dataForMuthu = read.csv("C:/Users/muthu/Desktop/Fall 2017/AdvAnalyticsInR/project1/project1.10.csv")

#Equation for predicting sales
n1 = nrow(dataForMuthu)
rebate= dataForMuthu$rebate
ad= dataForMuthu$ad.spent
xmas = dataForMuthu$xmas
y = dataForMuthu$sales
iterations = 5000
frame = data.frame(rebate,ad,xmas,y)
#prediction model(unrestricted model)'s equation looks like the following
# y=b0 +b1*(exterm1)+b2*(exterm2)+b3*(xmas)+b4*(xmas*rebate)+ b5*(xmas*ad)+b6*(rebate*ad)

#Least squares for simulated values Unrestricted model(full model)
checkUnrestrictedModelFit =  function(par,data){
  b0 = par[1]
  b1 = par[2]
  b2 = par[3]
  b3 = par[4]
  b4 = par[5]
  b5 = par[6]
  b6 = par[7]
  r1 = par[8]
  r2 = par[9]
  rebate = data[,1]
  ad =data[,2]
  xmas = data[,3]
  y=data[,4]
  exterm1 =(((1-exp(-r1*rebate)))/r1)
  exterm2 =(((1-exp(-r2*ad)))/r2)
  sum((y -(b0 +b1*(exterm1)+b2*(exterm2)+b3*(xmas)+b4*(xmas*rebate)+ b5*(xmas*ad)+b6*(rebate*ad)))^2)
}
fullModelOutput = nlminb(start =c(1:9),objective=checkUnrestrictedModelFit,data = data.frame(rebate,ad,xmas,y),control = list(iter.max=2000,eval.max= 5000))
unRestrictedSSR=fullModelOutput$objective
write(sprintf("\n"),file="FiveThousandIterations1.txt",append=TRUE)
write(sprintf("FullModel's Sum of Squared Residuals is %f",unRestrictedSSR),file="FiveThousandIterations1.txt",append=TRUE)
write(sprintf("\n FullModel's intercept is %f , slope of rebate is %f, slope of ad is %f, slope of xmas is %f, slope of xmas & rebate is %f, slope of xmas & ad is %f, slope of rebate & ad is %f, r1 is %f, r2 is %f",fullModelOutput$par[1],fullModelOutput$par[2],fullModelOutput$par[3],fullModelOutput$par[4],fullModelOutput$par[5],fullModelOutput$par[6],fullModelOutput$par[7],fullModelOutput$par[8],fullModelOutput$par[9]),file="FiveThousandIterations1.txt",append=TRUE)

#sprintf("Full Model's SSR is %f",unRestrictedSSR)
#for b0's existence i.e ., Ho: b0=0, Ha: b0 !=0 
checkB0Fit =  function(par,data){
  b1 = par[1]
  b2 = par[2]
  b3 = par[3]
  b4 = par[4]
  b5 = par[5]
  b6 = par[6]
  r1 = par[7]
  r2 = par[8]
  rebate = data[,1]
  ad =data[,2]
  xmas = data[,3]
  y=data[,4]
  exterm1 =(((1-exp(-r1*rebate)))/r1)
  exterm2 =(((1-exp(-r2*ad)))/r2)
  sum((y -(b1*(exterm1)+b2*(exterm2)+b3*(xmas)+b4*(xmas*rebate)+ b5*(xmas*ad)+b6*(rebate*ad)))^2)
}
randomnessCount = 0
data_list = list()
#SSR_list = list()
for (i in 1:iterations){
  data_list[[i]] = data.frame(frame[round(runif(260, 1,260),digits=0),])
  outputb0 = nlminb(start =c(5.506161e+02,  6.041887e+01, -2.045282e+02, -5.567838e+01, -2.263008e-03  ,6.771277e-03,  2.454680e+00 ,2.593530e-02),objective=checkB0Fit,data = data_list[[i]],control = list(iter.max=2000,eval.max= 5000))
  b0restrictedSSR =outputb0$objective
  #SSR_list[[i]]= b0restrictedSSR
  checkList= list('convergence','relative convergence')
  if(exists(b0restrictedSSR,where=checkList)){
    print("passed")
  }
  else{
    print("Failed convergence")
  }
  
  if(unRestrictedSSR>b0restrictedSSR){
    randomnessCount = randomnessCount + 1
  } 
}
pValueB0 =randomnessCount/iterations
write(sprintf("\n"),file="FiveThousandIterations1.txt",append=TRUE)
write(sprintf("pValue of B0 is calculated as UnrestrictedSSR  greater than RestrictedSSR upon total trials and value is %f",pValueB0),file="FiveThousandIterations1.txt",append=TRUE)
if(pValueB0 < 0.05){
  print("Reject null hypothesis ,hence conclude unrestricted model is better, b0 is  significant")
  #write(sprintf("pvalue is %f meaning that only less or equal to 5% time,restricted was better model,therefore we conclude b0 is significant ",pValueB0),file="FiveThousandIterations1.txt",append=TRUE)
}else{
  print("Fail to reject the null hypothesis( i.e b0 is zero) ,hence conclude restricted model is better ,b0 is not significant ")
}

# for b1's existence
#y =b0 +b2*(exterm2)+b3*(xmas)+b4*(xmas*rebate)+ b5*(xmas*ad)+b6*(rebate*ad) + eps
data_list= list()
checkB1Fit =  function(par,data){
  b0 = par[1]
  b2 = par[2]
  b3 = par[3]
  b4 = par[4]
  b5 = par[5]
  b6 = par[6]
  r2 = par[7]
  rebate = data[,1]
  ad =data[,2]
  xmas = data[,3]
  y=data[,4]
  #exterm1 =(((1-exp(-r1*rebate)))/r1)
  exterm2 =(((1-exp(-r2*ad)))/r2)
  sum((y -(b0+b2*(exterm2)+b3*(xmas)+b4*(xmas*rebate)+ b5*(xmas*ad)+b6*(rebate*ad)))^2)
}
randomnessCountB1 = 0
for (i in 1:iterations){
  data_list[[i]] = data.frame(frame[round(runif(260, 1,260),digits=0),])
  outputb1 = nlminb(start =c(1:6,0.5),objective=checkB1Fit,data = data_list[[i]],control = list(iter.max=2000,eval.max= 5000))
  b1restrictedSSR =outputb1$objective
  if(unRestrictedSSR>b1restrictedSSR){
    randomnessCountB1 = randomnessCountB1 + 1
  }
}
pValueB1 =randomnessCountB1/iterations
write(sprintf("\n"),file="FiveThousandIterations1.txt",append=TRUE)
write(sprintf("pValue of B1 is calculated as unRestrictedSSR  greater than RestrictedSSR upon total trials and value is is %f",pValueB1),file="FiveThousandIterations1.txt",append=TRUE)
print(pValueB1)
if(pValueB1 < 0.05){
  print(" Reject the null hypothesis and say b1 is  significant... Unrestricted model is better")
  print("Test if r1 is significant")
  
  #r1's significance
  checkr1Fit =  function(par,data){
    b0 = par[1]
    b1 = par[2]
    b2 = par[3]
    b3 = par[4]
    b4 = par[5]
    b5 = par[6]
    b6 = par[7]
    r2 = par[8]
    rebate = data[,1]
    ad =data[,2]
    xmas = data[,3]
    y=data[,4]
    #exterm1 =(((1-exp(-r1*rebate)))/r1)
    exterm2 =(((1-exp(-r2*ad)))/r2)
    sum((y -(b0+b1*(rebate)+b2*(exterm2)+b3*(xmas)+b4*(xmas*rebate)+ b5*(xmas*ad)+b6*(rebate*ad)))^2)
  }
  randomnessCountR1 = 0
  for (i in 1:iterations){
    data_list[[i]] = data.frame(frame[round(runif(260, 1,260),digits=0),])
    outputr1 = nlminb(start =c(1:7,0.5),objective=checkr1Fit,data = data_list[[i]],control = list(iter.max=2000,eval.max= 5000))
    r1restrictedSSR =outputr1$objective
    if(unRestrictedSSR>r1restrictedSSR){
      randomnessCountR1 = randomnessCountR1 + 1
    }
  }
  pValueR1 =randomnessCountR1/iterations
  print(pValueR1)
  write(sprintf("\n"),file="FiveThousandIterations1.txt",append=TRUE)
  write(sprintf("pValue of R1 is calculated as UnrestrictedSSR  greater than RestrictedSSR upon total trials and value is is %f",pValueR1),file="FiveThousandIterations1.txt",append=TRUE)
  
  if(pValueR1 < 0.05){
    print(" Reject the null hypothesis and say R1 is  significant... Unrestricted model is better")
    #write(sprintf("pvalue is %f meaning that only less or equal to 5% restricted was better model,therefore we conclude r1 is significant ",pValueR1),file="FiveThousandIterations1.txt",append=TRUE)  
  }else{
    print("r1 is not significant..restricted model is better")
  }
}else{
  print("b1 is not significant..restricted model is better")
}


# for b2's existence
#y= b0 +b1*(exterm1)+b3*(xmas)+b4*(xmas*rebate)+ b5*(xmas*ad)+b6*(rebate*ad) + eps
data_list = list()
checkB2Fit =  function(par,data){
  b0 = par[1]
  b1 = par[2]
  b3 = par[3]
  b4 = par[4]
  b5 = par[5]
  b6 = par[6]
  r1 = par[7]
  rebate = data[,1]
  ad =data[,2]
  xmas = data[,3]
  y=data[,4]
  exterm1 =(((1-exp(-r1*rebate)))/r1)
  #exterm2 =(((1-exp(-r2*ad)))/r2)
  sum((y -(b0+b1*(exterm1)+b3*(xmas)+b4*(xmas*rebate)+ b5*(xmas*ad)+b6*(rebate*ad)))^2)
}
randomnessCountB2 = 0
for (i in 1:iterations){
  data_list[[i]] = data.frame(frame[round(runif(260, 1,260),digits=0),])
  outputb2 = nlminb(start =c(1:6,0.5),objective=checkB2Fit,data = data_list[[i]],control = list(iter.max=2000,eval.max= 5000))
  b2restrictedSSR =outputb2$objective
  if(unRestrictedSSR>b2restrictedSSR){
    randomnessCountB2 = randomnessCountB2 + 1
  }
}
pValueB2 =randomnessCountB2/iterations
print(pValueB2)
write(sprintf("\n"),file="FiveThousandIterations1.txt",append=TRUE)
write(sprintf("pValue of B2 is calculated as UnrestrictedSSR  greater than RestrictedSSR upon total trials and value is %f",pValueB2),file="FiveThousandIterations1.txt",append=TRUE)
if(pValueB2 < 0.05){
  print("Reject the null hypothesis : b2 is  significant... Unrestricted model is better")
  print("Check if r2 is signicant")
  checkr2Fit =  function(par,data){
    b0 = par[1]
    b1 = par[2]
    b2 = par[3]
    b3 = par[4]
    b4 = par[5]
    b5 = par[6]
    b6 = par[7]
    r1 = par[8]
    rebate = data[,1]
    ad =data[,2]
    xmas = data[,3]
    y=data[,4]
    exterm1 =(((1-exp(-r1*rebate)))/r1)
    #exterm2 =(((1-exp(-r2*ad)))/r2)
    sum((y -(b0+b1*(exterm1)+b2*(ad)+b3*(xmas)+b4*(xmas*rebate)+ b5*(xmas*ad)+b6*(rebate*ad)))^2)
  }
  randomnessCountR2 = 0
  for (i in 1:iterations){
    data_list[[i]] = data.frame(frame[round(runif(260, 1,260),digits=0),])
    outputr2 = nlminb(start =c(1:7,0.5),objective=checkr2Fit,data = data_list[[i]],control = list(iter.max=2000,eval.max= 5000))
    r2restrictedSSR =outputr2$objective
    if(unRestrictedSSR>r2restrictedSSR){
      randomnessCountR2 = randomnessCountR2 + 1
    }
  }
  pValueR2 =randomnessCountR2/iterations
  print(pValueR2)
  write(sprintf("\n"),file="FiveThousandIterations1.txt",append=TRUE)
  write(sprintf("pValue of R2 is calculated as UnrestrictedSSR  greater than RestrictedSSR upon total trials and value is is %f",pValueR2),file="FiveThousandIterations1.txt",append=TRUE)
  
  if(pValueR2 < 0.05){
    print(" Reject the null hypothesis and say R2 is  significant... Unrestricted model is better")
  }else{
    print("r2 is not significant..restricted model is better")
  }
  
}else{
  print("Fail reject the null hypothesis: b2 is not significant..Restricted model is better")
}
##for r2's existence


# for b3's existence
#y = b0 +b1*(exterm1)+b2*(exterm2)+b4*(xmas*rebate)+ b5*(xmas*ad)+b6*(rebate*ad)+eps
checkB3Fit =  function(par,data){
  b0 = par[1]
  b1 = par[2]
  b2 = par[3]
  b4 = par[4]
  b5 = par[5]
  b6 = par[6]
  r1 = par[7]
  r2 = par[8]
  rebate = data[,1]
  ad =data[,2]
  xmas = data[,3]
  y=data[,4]
  exterm1 =(((1-exp(-r1*rebate)))/r1)
  exterm2 =(((1-exp(-r2*ad)))/r2)
  sum((y -(b0+b1*(exterm1)+b2*(exterm2)+b4*(xmas*rebate)+ b5*(xmas*ad)+b6*(rebate*ad)))^2)
}

#nlminb(start =1:8,objective=checkYourSimulationB3,data = data.frame(rebate,ad,xmas,y),control = list(trace=2, iter.max=2000,eval.max= 5000))
randomnessCountB3 = 0
data_list = list()
for (i in 1:iterations){
  data_list[[i]] = data.frame(frame[round(runif(260, 1,260),digits=0),])
  outputb3 = nlminb(start =c(1:6,0.5,0.5),objective=checkB3Fit,data = data_list[[i]],control = list(iter.max=2000,eval.max= 5000))
  b3restrictedSSR =outputb3$objective
  if(unRestrictedSSR>b3restrictedSSR){
    randomnessCountB3 = randomnessCountB3 + 1
  }
}
pValueB3 =randomnessCountB3/iterations
print(pValueB3)
write(sprintf("\n"),file="FiveThousandIterations1.txt",append=TRUE)
write(sprintf("pValue of B3 is calculated as UnrestrictedSSR  greater than RestrictedSSR upon total trials and value is %f",pValueB3),file="FiveThousandIterations1.txt",append=TRUE)
if(pValueB3 < 0.05){
  print("Reject the null hypothesis : b3 is  significant... Unrestricted model is better")
  #write(sprintf("pvalue is %f meaning that only less or equal to 5% restricted was better model,therefore we conclude b3 is significant ",pValueB3),file="FiveThousandIterations1.txt",append=TRUE)
}else{
  print("Fail to Reject the null hypothesis: b3 is not significant..restricted model is better")
}
# for b4's existence
#y = b0 +b1*(exterm1)+b2*(exterm2)+b3*(xmas)+ b5*(xmas*ad)+b6*(rebate*ad) + eps
checkB4Fit =  function(par,data){
  b0 = par[1]
  b1 = par[2]
  b2 = par[3]
  b3 = par[4]
  b5 = par[5]
  b6 = par[6]
  r1 = par[7]
  r2 = par[8]
  rebate = data[,1]
  ad =data[,2]
  xmas = data[,3]
  y=data[,4]
  exterm1 =(((1-exp(-r1*rebate)))/r1)
  exterm2 =(((1-exp(-r2*ad)))/r2)
  sum((y -(b0+b1*(exterm1)+b2*(exterm2)+b3*(xmas)+ b5*(xmas*ad)+b6*(rebate*ad)))^2)
}
#nlminb(start =1:8,objective=checkB4Fit,data = data.frame(rebate,ad,xmas,y),control = list(iter.max=200,eval.max= 500))
randomnessCountB4 = 0
data_list = list()
for (i in 1:iterations){
  data_list[[i]] = data.frame(frame[round(runif(260, 1,260),digits=0),])
  outputb4 = nlminb(start =c(1:6,0.5,0.5),objective=checkB4Fit,data = data_list[[i]],control = list(iter.max=2000,eval.max= 5000))
  b4restrictedSSR =outputb4$objective
  if(unRestrictedSSR>b4restrictedSSR){
    randomnessCountB4 = randomnessCountB4 + 1
  }
}
pValueB4 =randomnessCountB4/iterations
print(pValueB4)
write(sprintf("\n"),file="FiveThousandIterations1.txt",append=TRUE)
write(sprintf("pValue of B4 is calculated as UnrestrictedSSR  greater than RestrictedSSR upon total trials and value is %f",pValueB4),file="FiveThousandIterations1.txt",append=TRUE)
if(pValueB4 < 0.05){
  print("Reject the null hypothesis : b4 is significant... Unrestricted model is better")
  #write(sprintf("pvalue is %f meaning that only less or equal to 5% restricted was better model,therefore we conclude b4 is significant ",pValueB4),file="FiveThousandIterations1.txt",append=TRUE)
}else{
  print("Fail to reject the null hypothesis: b4 is not significant..restricted model is better")
}
#
# for b5's existence
#y = b0 +b1*(exterm1)+b2*(exterm2)+b3*(xmas)+b4*(xmas*rebate)+b6*(rebate*ad) + eps
checkB5Fit =  function(par,data){
  b0 = par[1]
  b1 = par[2]
  b2 = par[3]
  b3 = par[4]
  b4 = par[5]
  b6 = par[6]
  r1 = par[7]
  r2 = par[8]
  rebate = data[,1]
  ad =data[,2]
  xmas = data[,3]
  y=data[,4]
  exterm1 =(((1-exp(-r1*rebate)))/r1)
  exterm2 =(((1-exp(-r2*ad)))/r2)
  sum((y -(b0+b1*(exterm1)+b2*(exterm2)+b3*(xmas)+b4*(xmas*rebate)+b6*(rebate*ad)))^2)
}
randomnessCountB5 = 0
data_list = list()
for (i in 1:iterations){
  data_list[[i]] = data.frame(frame[round(runif(260, 1,260),digits=0),])
  outputb5 = nlminb(start =c(1:6,0.5,0.5),objective=checkB5Fit,data = data_list[[i]],control = list(iter.max=2000,eval.max= 5000))
  b5restrictedSSR =outputb5$objective
  if(unRestrictedSSR>b5restrictedSSR){
    randomnessCountB5 = randomnessCountB5 + 1
  }
}
pValueB5 =randomnessCountB5/iterations
print(pValueB5)
write(sprintf("\n"),file="FiveThousandIterations1.txt",append=TRUE)
write(sprintf("pValue of B5 is calculated as UnrestrictedSSR  greater than RestrictedSSR upon total trials and value is %f",pValueB5),file="FiveThousandIterations1.txt",append=TRUE)
if(pValueB5 < 0.05){
  print("Reject the null hypothesis : b5 is  significant... Unrestricted model is better")
  #write(sprintf("pvalue is %f meaning that only less or equal to 5% restricted was better model,therefore we conclude b5 is significant ",pValueB5),file="FiveThousandIterations1.txt",append=TRUE)
}else{
  print("Fail to Reject the null hypothesis: b5 is not significant..restricted model is better")
}

# for b6's existence
#y = b0 +b1*(exterm1)+b2*(exterm2)+b3*(xmas)+b4*(xmas*rebate)+ b5*(xmas*ad) + eps
checkB6Fit =  function(par,data){
  b0 = par[1]
  b1 = par[2]
  b2 = par[3]
  b3 = par[4]
  b4 = par[5]
  b5 = par[6]
  r1 = par[7]
  r2 = par[8]
  rebate = data[,1]
  ad =data[,2]
  xmas = data[,3]
  y=data[,4]
  exterm1 =(((1-exp(-r1*rebate)))/r1)
  exterm2 =(((1-exp(-r2*ad)))/r2)
  sum((y -(b0+b1*(exterm1)+b2*(exterm2)+b3*(xmas)+b4*(xmas*rebate)+b5*(xmas*ad)))^2)
}
#nlminb(start =1:8,objective=checkB6Simulation,data = data.frame(rebate,ad,xmas,y),control = list(trace=2, iter.max=200,eval.max= 500))
randomnessCountB6 = 0
data_list = list()
for (i in 1:iterations){
  data_list[[i]] = data.frame(frame[round(runif(260, 1,260),digits=0),])
  outputb6 = nlminb(start =c(1:6,0.5,0.5),objective=checkB6Fit,data = data_list[[i]],control = list(iter.max=2000,eval.max= 5000))
  b6restrictedSSR =outputb6$objective
  
  #lst[i] = outputb6$objective
  if(unRestrictedSSR>b6restrictedSSR){
      randomnessCountB6 = randomnessCountB6 + 1
    }
}
pValueB6 =randomnessCountB6/iterations
print(pValueB6)
write(sprintf("\n"),file="FiveThousandIterations1.txt",append=TRUE)
write(sprintf("pValue of B6 is calculated as UnrestrictedSSR  greater than RestrictedSSR upon total trials and value isis %f",pValueB6),file="FiveThousandIterations1.txt",append=TRUE)
if(pValueB6 < 0.05){
  print("Reject the null hypothesis : b6 is significant... Unrestricted model is better")
  #write(sprintf("pvalue is %f meaning that only less or equal to 5% restricted was better model,therefore we conclude b6 is significant ",pValueB6),file="FiveThousandIterations1.txt",append=TRUE)
}else{
  print("Fail to Reject the null hypothesis: b6 is not significant..Restricted model is better")
  #write(sprintf("pValue of B6 is calculated as restrictedSSR  greater than unRestrictedSSR upon total trials and value isis %f",pValueB6),file="FiveThousandIterations1.txt",append=TRUE)
}
