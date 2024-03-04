library(metaheuristicOpt)
library(e1071)
library(caTools)
library(kernlab)
library(ROSE)
library(Metrics)

training_set1<-read.csv(file.choose(),header=TRUE) #read train dataset
test_set1<-read.csv(file.choose(),header=TRUE) #read test dataset

MsE1 <- function( training_set1, test_set1, epsilon, c, gamma ) 
{
  ## train SVM model 
  model1 <- ksvm( 
    Flood_Inventory ~ ., 
    data = training_set1, 
    type = "eps-svr",
    kernel = "laplacedot",
    epsilon = epsilon,
    C = c, 
    kpar=list(sigma=gamma),
    cross = 10
  )
  
  ## test and calculate RMSD
  MSE1 <- mse(test_set1$Flood_Inventory, predict( model1, test_set1[,-11] ) )
  
  ## return calculated RMSD
  return ( MSE1 )
}

fitness_func1 <- function( x ) 
{
  
  ## fetch SVM parameters
  epsilon_val <- x[ 1 ]
  gamma_val <- x[ 2 ]
  c_val <- x[ 3 ]
  msd_vals <- MsE1( training_set1, test_set1, epsilon_val, c_val, gamma_val ) 
  return ( msd_vals )
}

epsilon <- c( 0,1 )
gamma <- c( 1e-3,2 )
c <- c( 1e-4,10 )
rangeVar <- matrix(c( epsilon, gamma,c), nrow=2)
rangeVar 
Vmax <- 2
ci <- 1.5
cg <- 1.5
w <- 0.7
numVar <- 5

resultPSO1 <- PSO(fitness_func1, optimType="MIN", numVar, numPopulation=50,
                  maxIter=100, rangeVar, Vmax, ci, cg, w)

optimum.value1 <- fitness_func1(resultPSO1)

summary(resultPSO1)
plot(resultPSO1)
summary(optimum.value1)
plot(optimum.value1)
resultPSO1
optimum.value1

