## Create sample data set
acs <- read.table("SampleData/acs_ny.csv", sep=",", header=T, stringsAsFactors = F)

#Make a binary income vriable fro building a logistic regression
acs$Income <- with(acs, FamilyIncome >= 150000)

# glmnet requires a predictor matrix. 
# In order to avoid multicollinearity, the generic model.matrix function 
# does not create an indicator variable for the base level of a factor.
# However, Elastic net prefers variables for all levels, use build.x function in useful package

require(useful)

# build predictor matrix. 
# the parameter contrast : Is a logical value indicating if the Factor's base level is removed

acsX <- build.x(Income ~ NumBedrooms + NumChildren + NumPeople + NumRooms
                 + NumUnits + NumVehicles + NumWorkers  + OwnRent + YearBuilt 
                 + ElectricBill + FoodStamp + HeatingFuel + Insurance + Language - 1,
                 data=acs, contrasts = F)
 
# Build response predictor
acsY <- build.y(Income ~ NumBedrooms + NumChildren + NumPeople + NumRooms
                + NumUnits + NumVehicles + NumWorkers  + OwnRent + YearBuilt 
                + ElectricBill + FoodStamp + HeatingFuel + Insurance + Language - 1,
                data=acs)

require(glmnet)
set.seed(1863561)

# run the cross validated glmnet
acsCV1 <- cv.glmnet(x=acsX, y=acsY, family="binomial", nfold=5)

acsCV1$lambda.min
acsCV1$lambda.1se

plot(acsCV1)

plot(acsCV1$glmnet.fit, xvar = "lambda") 
abline(v=log(c(acsCV1$lambda.min, acsCV1$lambda.1se)), lty=2)


# fit the ridge model by making alpha = 0
set.seed(71623)
acsCV2 <- cv.glmnet(x=acsX, y=acsY, family="binomial", nfold=5, alpha=0)
acsCV2$lambda.min

plot(acsCV2)

plot(acsCV2$glmnet.fit, xvar = "lambda") 
abline(v=log(c(acsCV2$lambda.min, acsCV2$lambda.1se)), lty=2)



### Finding the optimal value of alpha reqquires an additional layer of cross validation

require(parallel)
require(doParallel)

# For a 2 layered cross validation, an observation should fall in the same fold each time
# Also, it is considered better to lean towards lasso than ridge, thus consider alpha > 0.5

# set the seed for repeatability 
set.seed(2834673)

# create folds
theFolds <- sample(rep(x=1:5, length.out=nrow(acsX)))

# make sequence of œ values
alphas <- seq(from=0.5, to=1, by=.05) 



# set seed for repeatability of random results
set.seed(5127151)

# start a cluster with 2 workers
c1 <- makeCluster(2)

# register the workers
registerDoParallel(c1)

# keep track of timing
before <- Sys.time()

# build foreachloop to run in parallel several arguments
acsDouble <- foreach(i=1:length(alphas), .errorhandling = "pass", 
                     .inorder = F, .multicombine = T,
                     .export = c("acsX", "acsY", "alphas", "theFolds"),
                     .packages = "glmnet")   %dopar%
                     {
                       print(alphas[i])
                       cv.glmnet(x=acsX, y=acsY, family="binomial", nfolds=5, 
                                 foldid = theFolds, alpha=alphas[i])
                     }

after <- Sys.time()
stopCluster(c1)

after - before


# to find the optimal value of lambda and alpha 
# function for extracting info from cv.glmnet
extractGlmnetInfo <- function(object)
{
  #find lambdas
  lambdaMin <- object$lambda.min
  lambda1se <- object$lambda.1se
  
  #figure out where those lambdas fall in the path
  whichMin <- which(object$lambda == lambdaMin)
  which1se <- which(object$lambda == lambda1se)
  
  data.frame(lambda.min=lambdaMin, error.min=object$cvm[whichMin],
             lambda.1se=lambda1se, error.1se=object$cvm[which1se])
}


#build a one line data frame with each lambda & its corr error figure
alphaInfo <- plyr::ldply(acsDouble,extractGlmnetInfo)

alphaInfo$Alpha <- alphas

require(reshape2)
require(stringr)

alphaMelt <- melt(alphaInfo, id.vars="Alpha", value.name = "Value", variable.name="Measure")
alphaMelt$Type <- str_extract(string=alphaMelt$Measure, pattern = "(min)|(1se)")

# housekeeping :)
alphaMelt$Measure <- str_replace(string=alphaMelt$Measure, pattern = "\\.(min|1se)",
                                 replacement = "")
alphaCast <- dcast(alphaMelt, Alpha + Type ~ Measure, value.var = "Value")

ggplot(alphaCast, aes(x=Alpha, y=error))+
  geom_line(aes(group=Type)) +
  facet_wrap(~Type, scales="free_y", ncol=1) +
  geom_point(aes(size=lambda))


# we have found the optimal value of œ = 0.75  alphaInfo$Alpha[which.min(alphaInfo$error.1se)]
# we fit the model accordingly

set.seed(5127151)
acsCV3 <- cv.glmnet(x=acsX, y=acsY, family="binomial", nfold=5, 
                    alpha = alphaInfo$Alpha[which.min(alphaInfo$error.1se)])
plot(acsCV3)
plot(acsCV3$glmnet.fit, xvar="lambda")
abline(v=log(c(acsCV3$lambda.min, acsCV3$lambda.1se)), lty=2)


#viewing the Coeff plot for glmnet

theCoef <- as.matrix(coef(acsCV3, s="lambda.1se"))
coefDF <- data.frame(Value=theCoef, coefficient=rownames(theCoef))
coefDF <- coefDF[nonzeroCoef(coef(acsCV3, s="lambda.1se")),]

ggplot(coefDF, aes(x=X1, y=reorder(coefficient, X1))) +
  geom_vline(xintercept = 0, color="grey", linetype=2 ) +
  geom_point(color="blue") + labs(x="Value", y="Coefficient", title="Coefficient Plot")

