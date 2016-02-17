## Load the required packages
require(useful)
require(cluster)

# Load Member and Member detail data

 mbrDF <- read.table(file="SampleData/Member.csv", header = T, stringsAsFactors = F, sep =",")
 mbrDetDF <- read.table(file="SampleData/Member_Detail.csv", header = T, stringsAsFactors = F, sep =",")
 condDF <- read.table(file="SampleData/Conditions.csv", header = T, stringsAsFactors = F, sep =",")
 
 ## Merge member data into a single set and remove duplicate columns
 
 memberDF <- merge(mbrDF, mbrDetDF)
 memberDF <- memberDF[,!duplicated(colnames(memberDF))]
 
 
 ## Merge the condition details on the common column COND_CD and remove duplicate column names
 memberDF <- merge(memberDF, condDF, by= "COND_CD")
 memberDF <- memberDF[,!duplicated(colnames(memberDF))]
 
 ## Calculate BMI, Frailty, Age
 memberDF$BMI <- memberDF$WEIGHT_KG/memberDF$HEIGHT_M
 memberDF$FRAILTY <- as.numeric(with(memberDF, BMI>30|BMI<.18))
 memberDF$AGE <- (Sys.Date() - as.Date(memberDF$PTNT_BRTH_DT, format('%m/%d/%Y')))/365.25
 memberDF$AGE <- round(as.numeric(substr(memberDF$AGE,1,length(memberDF$AGE)-5)),digits=2)
 
 head(memberDF)
 
 ## remove housekeeping columns
 
 memberDF$REC_EFF_DT <- NULL
 memberDF$REC_ADD_TS <- NULL
 memberDF$REC_EFF_DT <- NULL
 memberDF$REC_EXPRN_DT <- NULL
 memberDF$REC_UPD_TS <- NULL
 memberDF$NON_RSP_ATMPT_EFF_DT <- NULL
 
 
 conditionDF <-  memberDF[ , c("COND_CD", "GENDER", "AGE", "BMI", "FRAILTY", 
                                "ENTHNICITY", "MARITAL_STUS" , "HEIGHT_M", "WEIGHT_KG",
                                "MBR_IN_FMLY", "PRIOR_HSP_ADMN")]
 
 #convert non numeric data to numeric
 head(conditionDF)
 conditionDF$COND_CD <- substr(conditionDF$COND_CD,3,4)
 conditionDF$MARITAL_STUS <- ifelse(conditionDF$MARITAL_STUS=="M", 1 ,0)
 conditionDF$GENDER <- ifelse(conditionDF$GENDER=="M", 1 ,0)
 conditionDF$PRIOR_HSP_ADMN <- ifelse(conditionDF$PRIOR_HSP_ADMN=="Y", 1, 0)
 
 # Ethnicity : 1 - WHITE ; 2 - AMERICAN; 3- ASIAN; 4 - AFRICAN AMERICAN; 5 - OTHER
 conditionDF$ENTHNICITY <- ifelse (conditionDF$ENTHNICITY=="WHITE" , 1 ,
                           ifelse (conditionDF$ENTHNICITY=="AMERICAN" , 2, 
                           ifelse (conditionDF$ENTHNICITY=="ASIAN" , 3,
                           ifelse (conditionDF$ENTHNICITY=="AFRICAN AMERICAN" , 4, 5))))
            
# Calculate the k means. Ideally required for 7 conditions. However, it was not that good a fit
conditionResults <- FitKMeans(conditionDF, max.clusters = 30, seed=278613)
conditionResults
 
PlotHartigan(conditionResults)    ## 17 clusters were optimal


## See if the results are better using Gap Statistics

theGap <- clusGap(conditionDF, FUNcluster = pam, K.max = 20)
condGapDF <- as.data.frame(theGap$Tab)
head(condGapDF)

