## Please note that while this is a not a recommended method to add variables to a model 

##Prepare the data for model to be evaluated

housing <- read.table("SampleData/housing.csv", sep="," , header = T, stringsAsFactors = F)
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt", "Sqft", 
                    "Income", "IncomePerSqft", "Expense", "ExpensePerSqft",
                    "NetIncome", "Value", "ValuePerSqft", "Boro")

## Lower and Uper bounds of the model

# the lowest model is the null model, basically the straight average
nullModel <- lm(ValuePerSqft ~ 1, data = housing)

# the largest model we will accept
fullModel <- lm(ValuePerSqft ~ Units + Sqft*Boro + Boro*Class, data=housing)

## Try different models, starting from the nullModel, working in both directions, uptil the fullModel
houseStep <- step(nullModel, scope = list(lower=nullModel, upper=fullModel), direction = "both")

houseStep