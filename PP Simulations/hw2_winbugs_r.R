#hw 2 winbugs R Script 

library(R2WinBUGS)
bd = '/Users/joshua/.wine/drive_c/Program Files/WinBUGS14'
#bd = '/Users/Joshua/Desktop/wibugs1.app/Contents/Resources/wineprefix/drive_c/Program Files/WinBUGS14'

#CHANGE WORKING DIRECTORY
setwd( "/Users/Joshua/Google Drive/PhD 2014/class work/bio 234 course work/HW/HW2")


# sim prior 1 
y =  c(2,6,3,3,1)
N = 5
a1 = 9
b1 = 3
data=list("y","N","a1", "b1")

inits=function(){list(lambda=mean(y))}
sim = bugs(data,inits, "hw2_model.txt", parameters=c("lambda"),n.chains = 1,n.iter=10100,n.burnin=100,n.thin=1,
                    bugs.directory=bd,codaPkg=FALSE)
print(sim)

t1 = sim$sims.matrix

mean.lambda1 = mean(t1[,1]) ; print(mean.lambda1)
var.lambda1 = var(t1[,1]); print(var.lambda1)


# sim prior 2  ###########
y =  c(2,6,3,3,1)
N = 5
a1 = .75
b1 = .25
data=list("y","N","a1", "b1")

inits=function(){list(lambda=mean(y))}
sim2 = bugs(data,inits, "hw2_model.txt", parameters=c("lambda"),n.chains = 1,n.iter=10100,n.burnin=100,n.thin=1,
           bugs.directory=bd,codaPkg=FALSE)
print(sim2)

t1 = sim2$sims.matrix

mean.lambda2 = mean(t1[,1]) ; print(mean.lambda2)
var.lambda2 = var(t1[,1]); print(var.lambda2)







