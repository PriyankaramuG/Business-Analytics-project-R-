# *************************************************************
# Machine Learning for Telemarketing in the Banking Sector
# Group Name: Titan
# *************************************************************
#
# UPDATE
# 1.00      18/03/2021   First version of Data pre-processing function
# 1.01      13/04/2021   Data Pre-processing - imputation   
# 1.02      20/04/2021   Data Pre-processing - bagging added
# 1.03      20/04/2021   Data Pre-processing - Categorical preparation
# 1.04      27/04/2021   Data Pre-processing - Numerical Data Preparation

#**************************************************************
# Practical Business Analytics Project (C0MM053) Group Project 
#**************************************************************

# ************************************************

# Data Pre-Processing 


# ************************************************
# Imputation_Unknown() :
#
# This function is used to impute the columns with unknown values
# 
#
# INPUT:   data frame - input - Telemarketing  Banking dataset
#
# OUTPUT : data frame - Unknown imputed values
# ************************************************
Imputation_Unknown <- function(dataset=bank_TeleM_Dataset) {
  
 
  print("Imputation of column job started")
  #1. Job - has lesser unknowns, will impute with mode because it wont skew the classes
  
  print("Finding the mode of job using table below:")
  print(dcast(dataset, job ~ y,value.var='y'))
  #replacing the unknown in job with 'admin' 
  dataset$job[which(dataset$job == 'unknown')] <- 'admin.'
  print("Imputation of column job completed")
  #2. Education - using the job information to identify the education level
  print("Imputation of column education started")
  
  print("Table casting education and job :")
  print(dcast(dataset,job ~ education,value.var='y'))
  
  #admin
  dataset$education[which((dataset$education == 'unknown' | dataset$education == 'illiterate')
                                     & dataset$job == 'admin.')] <- 'university.degree'
  
  #blue-collar
  dataset$education[which((dataset$education == 'unknown' | dataset$education == 'illiterate')
                                     & dataset$job == 'blue-collar')] <- 'basic.9y'
  
  #entrepreneur
  dataset$education[which((dataset$education == 'unknown' | dataset$education == 'illiterate')
                                     & dataset$job == 'entrepreneur')] <- 'university.degree'
  
  #housemaid
  dataset$education[which((dataset$education == 'unknown' | dataset$education == 'illiterate')
                                     & dataset$job == 'housemaid')] <- 'basic.4y'
  
  
  #management
  dataset$education[which((dataset$education == 'unknown' | dataset$education == 'illiterate')
                                     & dataset$job == 'management')] <- 'university.degree'
  
  #retired
  dataset$education[which((dataset$education == 'unknown' | dataset$education == 'illiterate')
                                     & dataset$job == 'retired')] <- 'basic.4y'
  
  #self-employed
  dataset$education[which((dataset$education == 'unknown' | dataset$education == 'illiterate')
                                     & dataset$job == 'self-employed')] <- 'university.degree'
  
  #services
  dataset$education[which((dataset$education == 'unknown' | dataset$education == 'illiterate')
                                     & dataset$job == 'services')] <- 'high.school'
  
  #student
  dataset$education[which((dataset$education == 'unknown' | dataset$education == 'illiterate')
                                     & dataset$job == 'student')] <- 'high.school'
  
  #technician
  dataset$education[which((dataset$education == 'unknown' | dataset$education == 'illiterate')
                                     & dataset$job == 'technician')] <- 'professional.course'
  
  #unemployed
  dataset$education[which((dataset$education == 'unknown' | dataset$education == 'illiterate')
                                     & dataset$job == 'unemployed')] <- 'university.degree'
  print("Imputation of column education completed")
  print("Imputation of column marital started")
  #3- marital - impute <= 33 age as single and > 33 as married
  
  print("Table casting marital and age")
  print(dcast(dataset, marital ~ y,value.var='y'))
  
  marital_prep <- cbind(as.data.frame(dataset$marital), as.data.frame(dataset$age))
  
  marital_prep %>% group_by(dataset$marital) %>% summarize(mean(`dataset$age`))
  
  #impute <= 33 age as single and > 33 as married
  dataset$marital[which(marital_prep$`dataset$marital` == 'unknown' & marital_prep$`dataset$age` > 33)] <- 'married'
  dataset$marital[which(marital_prep$`dataset$marital` == 'unknown' & marital_prep$`dataset$age` <= 33)] <- 'single'
  print("Imputation of column marital completed")
  
  print("Imputation of default started")
  #4- default - Dropping the field with unknowns
  
  dataset <- dataset %>% select(-default)
  print("Imputation of default completed")
  
  print("Imputation of column housing and loan started using mice")
  
  # 6 - loan and housing - MICE based imputation
  
  #Data for mice
  bank_TeleM_bank_TeleM_data_mice <- dataset
  
  #Remove outcome variable
  bank_TeleM_data_mice <- bank_TeleM_bank_TeleM_data_mice %>% select(-y)
  
  #Replace unknowns as NA 
  bank_TeleM_data_mice[bank_TeleM_data_mice == "unknown"] <- NA
  
  #print the graph which contains NA values marked in red
  print(aggr(bank_TeleM_data_mice,col=c('navyblue','red'),numbers=TRUE,sortVars=TRUE,labels=names(bank_TeleM_data_mice),cex.axis=.7,gap=3,ylab=c("","")))
  
  print("Columns with NA Values :") 
  print(sapply(bank_TeleM_data_mice, function(x) sum(is.na(x))))
  
  bank_TeleM_data_mice[sapply(bank_TeleM_data_mice, is.character)] <- lapply(bank_TeleM_data_mice[sapply(bank_TeleM_data_mice, is.character)], as.factor)
  
  md.pattern(bank_TeleM_data_mice)
  
  mice_imputes<- mice(bank_TeleM_data_mice,m=1,maxit=5)
  
  # housing and loan imputed using Logistic regression
  print(summary(mice_imputes))
  
  #printing the density plot which shows observed and imputed data for housing and loan
  print(densityplot(mice_imputes, ~housing))
  print(densityplot(mice_imputes, ~loan))
  
  completeImp_TeleBM_Data <- complete(mice_imputes,1)
  #print the graph after imputation
  print(aggr(completeImp_TeleBM_Data,col=c('navyblue','red'),numbers=TRUE,sortVars=TRUE,labels=names(completeImp_TeleBM_Data),cex.axis=.7,gap=3,ylab=c("","")))
  completeImp_TeleBM_Data<-cbind(completeImp_TeleBM_Data, y=dataset$y)
 
  print("Imputation of column housing and loan completed using mice")
  completeImp_TeleBM_Data %>% mutate_if(is.factor, as.character) -> completeImp_TeleBM_Data
  return(completeImp_TeleBM_Data)
}
#**End of Imputation_Unknown method***************************


# ************************************************
# Binning_BankTelMCols() :
#
# This function is used to Bin the columns age, campaign and pdays
# 
#
# INPUT:   Vector - input - Columns to be binned
#
# OUTPUT : data frame - Dataset that are binned
# ************************************************
Binning_BankTelMCols <- function(dataset=TeleBM_IMP_Data,fields=binary_encode_cols) {
  
  #1. Binning the age as slightly right skewed.split this into bins - <20 (Teenagers), 20-34 (Young adults), 35-59 (Adults), 60+ (Senior citizens). 
  for(bincols in fields)
  {
    if(bincols=='age'){  
      dataset <- dataset %>% mutate(age = ifelse(age < 20, 'Teenagers', ifelse(     
        age < 35 & age > 19, 'Young Adults', ifelse(
          age < 60 & age > 34, 'Adults', 'Senior Citizens' ))))
    }
  
  #2. Binning the campaign as most of them were contacted 1 to 3 times
    else if(bincols=='campaign'){
      dataset <- dataset %>%  mutate(campaign = ifelse(campaign > 2, 'morethan2_times', ifelse(     
        campaign == 2, '2_times', '1_time')))
    }
  
  #3. Binning the pdays column as most of them were not contacted which is indicated by value 999
    else if(bincols=='pdays'){
      dataset <- dataset %>%  mutate(pdays = ifelse(pdays == 999, 'not_Contacted', 'Contacted'))
    }
  }
  return(dataset)
}

#**End of Binning_BankTelMCols method*******************
# ************************************************
# Encode_OrderedColumns() :
#
# This function is used to assign the numeric values to the ordered columns
# 
#
# INPUT:   Vector - input - Ordered Columns to be assigned numeric
#
# OUTPUT : data frame - Dataset with ordered numeric values 
# ************************************************
Encode_OrderedColumns <- function(dataset=TeleMB_Bin_Data,fields=ordered_cols) {

ordinalendcoded_cols<-data.frame()
month<-vector()
day_of_week<-vector()
poutcome<-vector()
for(ordcols in fields)
  {
  
   #1. month-"jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec" convert to (1,2,3,4,5,6,7,8,9,10,11)
    if(ordcols=='month'){
      month<-as.numeric(factor(dataset$month, levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"), ordered=TRUE))
      
    }
  #2. day_of_week- "mon","tue","wed","thu","fri" convert to (1,2,3,4,5)
   else if(ordcols=='day_of_week'){
      
     day_of_week<-as.numeric(factor(dataset$day_of_week, levels = c("mon","tue","wed","thu","fri"), ordered=TRUE))
      
   }
  
  #3. Converts success to 1 and "nonexistent and failure" to 0
  else if(ordcols=='poutcome'){
    poutcome<-ifelse(dataset$poutcome=='success',1,0)
    
     
  }
 
} 
ordinalendcoded_cols <- data.frame(month,day_of_week,poutcome)
return(ordinalendcoded_cols)
}
#**End of Encode_OrderedColumns method*******************
#*# ************************************************
# BankTeleM_DataPreProcessing() :
#
# This function is the starting point for the Data cleaning and preprocessing
# 
#
# INPUT:   data frame - input - Telemarking Banking Dataset
#
# OUTPUT : data frame - cleaned, processed Dataset which is used for modelling
# ************************************************
BankTeleM_DataPreProcessing <- function(dataset=bank_TeleM_Dataset) {
  #**Dropping the fields**
  
  #Duration attribute highly affects the output target (e.g., if duration=0 then y="no").Dropping this field for the model we are building
  dataset <- dataset %>% select(-duration)
  
  
  
  #**Imputation of 'unknown' values**
  
  #finding the columns with value unknown
  print("Finding the columns with value unknown and its count")
  unkown_count<-data.frame('Unknown_Count'=colSums(dataset == "unknown"))
  print(formattable::formattable(data.frame('Unknown_Count'=colSums(dataset == "unknown"))))
  print(unkown_count)
  
  #Imputation of 'unknown' values 
  print("**Imputation of columns job, education,marital,default, housing and loan begins here**") 
  #calling the function from the file "BankTeleMDataPreparation.R"
  TeleMB_IMP_Data<-Imputation_Unknown(dataset=dataset)
  print("Finding the columns with value unknown after imputation")
  print(data.frame('Unknown_Count'=colSums(TeleMB_IMP_Data == "unknown")))
  print("Telemarketing Banking dataset with all the imputations completed and returned")
  
  #**Imputation completed**
  #*********************************************************************
  
  #**Binning the columns age, pdays and campaign**
  
  print("Binning the columns age, pdays and campaign started")
  bin_Cols<- c("age","campaign","pdays")
  TeleMB_Bin_Data<-Binning_BankTelMCols(dataset=TeleMB_IMP_Data,fields=bin_Cols)
  print("Binning the columns age, pdays and campaign completed")
  
  #*Binning completed**
  #*************************************************************************
  
  #**Preparation of Categorical fields**
  
  #group the categorical columns based on ordered, unorderd and binary encoding columns
  print("preparation of categorical fields started")
  nominal_Cols<- c("job","marital","education",'age','campaign')
  binary_encode_cols<-c("loan","housing","contact","pdays")
  ordered_cols<-c("month","day_of_week","poutcome")
  
  #one-hot encoding the categorical columns; call the function; pass the dataset and nominal_cols
  cat_oneHotencoded_cols<-NPREPROCESSING_categorical(dataset=TeleMB_Bin_Data,field_types= nominal_Cols)
  
  #binary encoding the columns with two values; call the function;pass the dataset and binary_encode_cols
  bin_encoded_cols<-PREPROCESSING_BinaryColumns(dataset=TeleMB_Bin_Data,field_types= binary_encode_cols)
  
  #encode the ordered columns to numeric;call the function;pass the dataset and ordered_cols
  encoded_ord_cols<-Encode_OrderedColumns(dataset=TeleMB_Bin_Data,fields=ordered_cols)
  #combine all the prepared categorical fields
  cat_finalprep_cols<-cbind(cat_oneHotencoded_cols,bin_encoded_cols,encoded_ord_cols)
  
  print(formattable::formattable(data.frame(CategoricalFields=names(cat_finalprep_cols))))
  print("preparation of categorical fields completed")
  
  #**Preparation of Categorical fields completed**
  #************************************************************************
  
  #preparation target variable 
  #  yes ='1' means subscribed to the telemarketing
  #  no ='0' means not subscribed to telemarketing 
  bankTeleM_target <- as.numeric(ifelse(dataset$y=='yes',1,0))
  
  #***********************************************************************
  #**Numerical fields preparation**
  
  #previous; emp.var.rate, cons.price.idx, cons.conf.idx, nr.employed, euribor3m are the "social indicator columns" 
  
  #extracting the numeric fields
  print("Numerical data preparation started")
  bankTM_numerical_cols <- TeleMB_Bin_Data[,sapply(TeleMB_Bin_Data, is.numeric)]
  
  #found the outliers in  social indicator columns cons.conf.idx during EDA. Imputing with mean
  outliers<-bankTM_numerical_cols %>% select(cons.price.idx,euribor3m)
  outliers<-NPREPROCESSING_outlier(ordinals=outliers,confidence=OUTLIER_CONF)
  zscaled<-as.data.frame(scale(outliers,center=TRUE, scale=TRUE))
  ordinalReadyforML<-Nrescaleentireframe(zscaled)
  #remove the outliers field
  bankTM_numerical_cols<-bankTM_numerical_cols %>% select(-cons.price.idx,-euribor3m)
  
  
  #Combine all the cleaned dataframes into one 
  Bank_TeleM_CleanD<-cbind(cat_finalprep_cols,bankTM_numerical_cols,ordinalReadyforML,y=bankTeleM_target)
  print(formattable::formattable(data.frame(Numeric_Fields=names(Bank_TeleM_CleanD))))
  #remove the columns that are redundant,by plotting the correlation plot
  Bank_TeleM_CleanD<-NPREPROCESSING_redundantFields(dataset=Bank_TeleM_CleanD,cutoff=OUTLIER_CONF)
  print(formattable::formattable(data.frame(Final_Fields=names(Bank_TeleM_CleanD))))
  print("Numerical data preparation completed")
  #return the cleaned and processed dataset
  return(Bank_TeleM_CleanD)
 
  
  #***********************************************************************
}#**End of BankTeleM_DataPreProcessing method*******************