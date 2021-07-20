# *************************************************************
# Machine Learning for Telemarketing in the Banking Sector
# Group Name: Titan
# *************************************************************
#
# UPDATE
# 1.00      18/03/2021   First version of Exploratory Data Analysis
# 1.01      13/04/2021   Second version of Exploratory Data Analysis
# 1.02      13/04/2021   Data Pre-processing - imputation   
# 1.03      20/04/2021   Data Pre-processing - bagging added
# 1.04      20/04/2021   Data Pre-processing - Categorical preparation
# 1.05      27/04/2021   Data Pre-processing - Numerical Data Preparation
# 1.06      27/04/2021   Modeling - Logistic
# 1.07      04/05/2021   Modeling - Random Forest
# 1.08      06/05/2021   Evaluation - Threshold added
# 1.09      07/05/2021   Evaluation - Roc analysis and threshold graph added
# 1.10      10/05/2021   EDA changes added


#  clears all objects in "global environment"
rm(list = ls())

#**************************************************************
# Practical Business Analytics Project (C0MM053) Group Project
#**************************************************************

#Global Environment variables - i.e. available to all functions

DATASET_FILENAME  <- "bank-additional-full.csv"
TYPE_DISCRETE     <- "DISCRETE"           # field is discrete (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded
DISCRETE_BINS     <- 10                   # Number of empty bins to determine discrete
OUTPUT_FIELD      <- "y" 
MAX_LITERALS      <- 55
OUTLIER_CONF      <- 0.9
KFOLDS            <- 4                    # Number of folded experiments
FOREST_SIZE       <- 1000                 # Number of trees in the forest

# ***********************************************************************************
# Define and then load the libraries used in this project
# Library from CRAN     Version
# pacman	               0.5.1
# outliers	             0.14
# corrplot	             0.84
# MASS	                 7.3.53
# formattable 	         0.2.0.1
# stats                  4.0.3
# PerformanceAnalytics   2.0.4
# ggcorrplot             3.3.3
# ROCR                   1.0.11
# mice                   3.13.0
# dplyr                  1.0.5
# VIM                    6.1.0
# randomForest           4.6.14
# smotefamily            1.3.1
# reshape2               1.4.4


MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "formattable",
               "stats",
               "caret",
               "randomForest",
               "PerformanceAnalytics",
               "ggcorrplot",
               "ROCR",
               "mice",
               "dplyr",
               "VIM",
               "randomForest",
               "smotefamily",
               "reshape2")

# User defined functions
# ************************************************
# Read_BankTeleM_Dataset() :
#
# This function is used to read the dataset from the csv file
#
# INPUT   :   string         - path name of the csv file
#
# OUTPUT  :   data frame     - Banking TeleMarketing contents
#
# ************************************************
Read_BankTeleM_Dataset <- function(DATASET_FILENAME) {
  read_bank_TeleM_Dataset = read.csv(
    DATASET_FILENAME,
    encoding = "UTF-8",
    stringsAsFactors = FALSE,
    header = TRUE,
    sep = ';'
  )
  return(read_bank_TeleM_Dataset)
}
#***********************************************************
# myModelFormula() :
#
# Create formula for column names & given output
#
# INPUT   :   Data frame - dataset         - data
#         :   String     - fieldNameOutput - name of the output field
#
# OUTPUT  :   Formula    - R formula object
#
# ************************************************
myModelFormula<-function(dataset,fieldNameOutput){
  
  inputs<-paste(names(dataset)[which(names(dataset)!=fieldNameOutput)],collapse = "+")
  
  output<-paste(fieldNameOutput,"~")
  
  formular=as.formula(paste(output,inputs))
  
  return(formular)
  
} #endof myModelFormula()
# ************************************************
# allocateFoldID() :
#
# Append a column called "foldID" that indicates the fold number
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
allocateFoldID <- function(dataset) {
  recordsPerFold <- ceiling(nrow(dataset) / KFOLDS)
  
  foldIds <- rep(seq(1:KFOLDS), recordsPerFold)
  
  foldIds <- foldIds[1:nrow(dataset)]
  
  dataset$foldId <- foldIds
  
  return(dataset)
} #endof allocateFoldID()

# ************************************************
# stratifiedDataset() :
#
# Split dataset by the class (assume 2-class)
# Calculate the number of records that will appear in each fold
# Give each of these blocks a unique foldID
# combine the datasets & randomise
# The dataset now has a foldID from which we can select the data
# for the experiments
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
stratifiedDataset <- function(originalDataset) {
  positionClassOutput = which(names(originalDataset) == OUTPUT_FIELD)
  
  # Get the unique class values
  classes <- unique(originalDataset[, positionClassOutput])
  
  # Split dataset into the two classes (so as to keep the class balance the same in the datasets)
  indexClass1 <-
    which(originalDataset[, positionClassOutput] == classes[1])
  split1 <- originalDataset[indexClass1, ]
  split2 <- originalDataset[-indexClass1, ]
  
  # Append a column that indicates the fold number for each class
  split1 <- allocateFoldID(split1)
  split2 <- allocateFoldID(split2)
  
  # Combine the two datasets
  
  newDataset <- rbind(split1, split2)
  
  #Randomise the classes
  newDataset <- newDataset[order(runif(nrow(newDataset))), ]
  
  return(newDataset)
}
# ************************************************
# stratifiedSplit() :
#
# Generate the TRAIN and TEST dataset based on the current fold
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   list               - train & test datasets
# ************************************************

stratifiedSplit <- function(newDataset, fold) {
  test <- subset(newDataset,
                 subset = foldId == fold,
                 select = -foldId)
  train <- subset(newDataset,
                  subset = foldId != fold,
                  select = -foldId)
  
  return(list(train = train,
              test = test))
}
plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pred >= threshold & df$y == 1, "TP", v)
  v <- ifelse(df$pred >= threshold & df$y == 0, "FP", v)
  v <- ifelse(df$pred < threshold & df$y == 1, "FN", v)
  v <- ifelse(df$pred < threshold & df$y == 0, "TN", v)
  
  df$pred_type <- v
  
  print(
    ggplot(data = df, aes(x = y, y = pred)) +
      geom_violin(fill = 'black', color = NA) +
      geom_jitter(aes(color = pred_type), alpha = 0.6) +
      geom_hline(
        yintercept = threshold,
        color = "red",
        alpha = 0.6
      ) +
      scale_color_discrete(name = "type") +
      labs(title = sprintf("Threshold at %.2f", threshold))
  )
}
# ************************************************
# Bank_TeLM_Smote() :
#
# Generate the balance on the train data using the function 'smote' from package 'smotefamily'
#
# INPUT   :   data frame         - dataset        - Stratified training data
#
# OUTPUT  :   data frame         - smoted train data
# ************************************************
Bank_TeLM_Smote <- function(dataset=training_BankTelM_data) {
  # For Balancing the dataset, using 'smote' function from package 'smotefamily'
  #(ref: https://cran.r-project.org/web/packages/smotefamily/smotefamily.pdf)
  #removing the target variable from the training dataset to feed as a source to smote
  train_smote<-data.frame()
  train_without_target <- dataset %>% select(-y)
  train_smote <-SMOTE(train_without_target,dataset$y,K = 5,dup_size = 0)
  names(train_smote$data)[names(train_smote$data) == "class"] <-"y"
  #updating the training data with smote data
  dataset <- train_smote$data
  return(dataset)
}
# ************************************************
# logisticmodel() :
#
# create the logistic regression model for pre processed dataset
#
# INPUT   : formula  - string - model formula
#           training data - data frame  
#           testing data - data frame 
#           allresults - data frame 
#           kfold - string   
#
# OUTPUT  : list - allresults(tp, fp,tn,fn,pgood,pbad,tpr,fpr,fnr,auc,threshold,fi-score,mcc) - returned from logistic model  
# ************************************************
logisticmodel<-function(formula=formular,train_data=training_data,test_data=testing_data,allResults=allResults,kfold=fold){
  #apply the logistic model
  logistic_model<-stats::glm(formula,data=train_data,family = "binomial")
  # Get probabilities of being class 1 from the classifier
  probabilities<-predict(logistic_model, test_data,type="response")
  # Determine threshold and Plot FPR/TPR through threshold range
  results<-NdetermineThreshold(probs=probabilities,testing_data=test_data,"Logistic",kfold)
  #prediction for test data
  BankTemlM_logreg_testresult <- prediction(probabilities, test_data$y)
  # get the AUC value
  BankTemlM_logreg_auc = as.numeric(performance(BankTemlM_logreg_testresult, "auc")@y.values)
  #add to the results returned from determinethreshold
  results$AUC<-BankTemlM_logreg_auc
  rocr.perf = performance(BankTemlM_logreg_testresult, measure = "tpr", x.measure = "fpr")
  # plot ROC curve
  plot(rocr.perf,
       lwd = 3, colorize = TRUE,
       print.cutoffs.at = seq(0, 1, by = 0.1),
       text.adj = c(-0.2, 1.7),
       main = 'ROC Curve')
  mtext(paste('logistic regression auc for fold ',kfold, ' : ',round(BankTemlM_logreg_auc, 5)))
  abline(0, 1, col = "red", lty = 2)
  # ************************************************
  # output the importance
  importance<-as.data.frame(caret::varImp(logistic_model, scale = TRUE))
  row.names(importance)<-gsub("[[:punct:][:blank:]]+", "", row.names(importance))
  barplot(t(importance[order(importance$Overall),,drop=FALSE]),las=2, border = 0, cex.names =0.8)
  #bind all the results to allResults
  #allResults<-rbind(allResults,data.frame(results))
  allResults<-rbind(allResults,data.frame(results))
  #return all results
  return(allResults)
}
#*************************************************************************************
# RandomForest() :
#
# Create Random Forest on pre-processed dataset
#
# INPUT   : formula  - string - model formula
#           training data - data frame  
#           testing data - data frame 
#           allresults - data frame 
#           kfold - string  
#
# OUTPUT  : list - allresults(tp, fp,tn,fn,pgood,pbad,tpr,fpr,fnr,auc,threshold,fi-score,mcc) - returned from randomforest model  
# ************************************************
RandomForestmodel<-function(formula=formular,train_data=training_data,test_data=testing_data,allResults=allResults,kfold=fold){
  myTitle<-(paste("Preprocessed Dataset. Random Forest=",FOREST_SIZE,"trees"))
  print(myTitle)
  # apply the randomforest model
  RandomForest_model<-randomForest(formula,data = train_data, importance=TRUE, ntree=1000)
  #importance plot
  varImpPlot(RandomForest_model)
  #predictions for the test data
  rf_test_result <- predict(RandomForest_model,test_data,type="response")
  positionClassOutput=which(names(test_data)==OUTPUT_FIELD)
  # train data: dataframe with the input fields
  test_inputs<-test_data[-positionClassOutput]
  # Generate class membership probabilities
  # Column 1 is for class 0 (bad loan) and column 2 is for class 1 (good loan)
  testPredictedClassProbs<-predict(RandomForest_model,test_data, type="prob")
  # Get the column index with the class label
  classIndex<-which(as.numeric(colnames(testPredictedClassProbs))==1)
  # Get the probabilities for classifying the subscribed clients
  test_predictedProbs<-testPredictedClassProbs[,classIndex]
  # Determine threshold and Plot FPR/TPR through threshold range
  results<-NdetermineThreshold(probs=test_predictedProbs,testing_data=test_data,"RandomForest",kfold)
  BankTemlM_forest_roc <- data.frame(prediction = testPredictedClassProbs[,2],
                           trueclass = as.numeric(test_data$y==1))
  head(BankTemlM_forest_roc)
  BankTemlM_forest_roc_prediction <- prediction(BankTemlM_forest_roc$prediction, BankTemlM_forest_roc$trueclass) 
  BankTemlM_auc_ROCR <- performance(BankTemlM_forest_roc_prediction, measure = "auc")
  BankTemlM_auc_ROCR <- BankTemlM_auc_ROCR@y.values[[1]]
  results$AUC=BankTemlM_auc_ROCR
  rocr.perf = performance(BankTemlM_forest_roc_prediction, measure = "tpr", x.measure = "fpr")
  # plot ROC curve
  plot(rocr.perf,
       lwd = 3, colorize = TRUE,
       print.cutoffs.at = seq(0, 1, by = 0.1),
       text.adj = c(-0.2, 1.7),
       main = 'ROC Curve')
  mtext(paste('Random Forest - auc for fold : ',kfold,' : ',round(BankTemlM_auc_ROCR, 5)))
  abline(0, 1, col = "red", lty = 2)
  #************************************************************************
  # Get importance of the input fields
  importance<-randomForest::importance( RandomForest_model,scale=TRUE,type=1)
  importance<-importance[order(importance,decreasing=TRUE),,drop=FALSE]
  
  colnames(importance)<-"Strength"
  barplot(t(importance),las=2, border = 0,cex.names =0.7,main="Random FOrest")
  print(formattable::formattable(data.frame(importance)))
  allResults<-rbind(allResults,data.frame(results))
  return(allResults) 
  
  
}
# end of User defined functions
# ********************************************************
# main() :
# main entry point to execute our Telemarketing Prediction
#
# INPUT:  None
# OUTPUT: None
# ********************************************************
main <- function() {
  #read the Banking Telemarketing Dataset
  print("Reading the Banking TeleMarketing Dataset")
  bank_TeleM_Dataset <- Read_BankTeleM_Dataset(DATASET_FILENAME)
  
  #******************************************************************
  # EDA Begins here : 
  #******************************************************************
  print("*******EDA for Telemarketing banking data is starting*******")
  print("The dimension of dataset 41188 rows and 21 columns")
  print(dim(bank_TeleM_Dataset))
  
  #Split the data for EDA
  print("*splitting the data as numeric and categorical*")
  var_names <- VAR_SEPERATOR(bank_TeleM_Dataset)
  bank_numerical <- bank_TeleM_Dataset[,var_names$num]
  bank_categorical <- bank_TeleM_Dataset[,var_names$cat]
  print("numerical fields are :")
  print(colnames(bank_numerical))
  print("categorical fields are :")
  print(colnames(bank_categorical))
  ########Numerical Data Exploration ###########################################
  
  #Create Histograms
  print("Plotting Numerical fields")
  HIST_PLOTS(bank_numerical)
  
  #Bivariate plots ( based on individualized exploration)
  print("Plotting bivariate numerical plots with target variable y(yes-subscribed;no-not subscribed")
  numerical_vs_target(bank_TeleM_Dataset)
  
  
  #Correlations
  print("correlation plot")
  corrplot(bank_numerical)
  
  ###############Categorical Data Exploration ###########################
  
  #Create bar plots
  print("Plotting categorical fields")
  BAR_PLOTS(bank_categorical)
  
  
  #Create bi-variate bar plots
  print("Plotting bivariate categorical plots with target variable y(yes-subscribed;no-not subscribed")
  BIVARIATE_BAR(bank_categorical)
  
  #Bar-plots absolute split with outcome
  Plot_CategoricalwithOutcome(bank_categorical) 
  print("*******EDA for Telemarketing banking data is completed*******")
  
  #******************************************************************
  # EDA End here : 
  #******************************************************************
  
  #*****************************************************************
  # Data Pre-processing Begins here : call the function located in the file "BankTeleMDataPreparation.R"
  #******************************************************************
  print("*******Data pre_processing of the data is starting*******")
  
  Bank_TeleM_ProcessedD <- BankTeleM_DataPreProcessing(bank_TeleM_Dataset)
  
  print("*******Data pre_processing of the data is completed*******")
  #*****************************************************************
  # Data Pre-processing Ends here :
  #******************************************************************
  
  #*****************************************************************
  # Modeling and Evaluation Begins here : We are trying two models using logistic regression and Random Forest
  #******************************************************************
  
  print("*******Modelling and Evaluation is started*******")
  
  #Applying the stratified cross fold validation,smote on the train data to balance the dataset
  # 1       0
  # 4640   36548     
  strat_BankTelM_data <- stratifiedDataset(Bank_TeleM_ProcessedD)
  #load all the models to be executed in modelnames list
  allmodelResults <- data.frame()
  #experiment to be executed is added in the below list
  modelnames <- list("LogisticRegression", "RandomForest")
  # run the experiment for all the models mentioned in the list model names
  for (m in modelnames) {
    allResults <- data.frame()
    print(paste("Starting the experiement for", m))
    for (k in 1:KFOLDS) {
      print(paste("Processing fold #", k))
      split_BankTelM_Data <- stratifiedSplit(newDataset = strat_BankTelM_data, fold = k)
      training_BankTelM_data <- split_BankTelM_Data$train
      testing_BankTelM_data <- split_BankTelM_Data$test
      
      #Smote the training data with oversampling the minority data
      print("The count of training data before applying smote :")
      print(table(training_BankTelM_data$y))
      formular <- myModelFormula(dataset = training_BankTelM_data, fieldNameOutput = OUTPUT_FIELD)
     
      # For Balancing the dataset, using 'smote' function from package 'smotefamily'(ref: https://cran.r-project.org/web/packages/smotefamily/smotefamily.pdf)  
      #Call the function 'Bank_TeLM_Smote' to balance the dataset
      training_BankTelM_data <-Bank_TeLM_Smote(training_BankTelM_data)
      print("The count of training data after applying smote :")
      print(table(training_BankTelM_data$y))
      #converting the target variable (y) to factor
      training_BankTelM_data$y = as.factor(training_BankTelM_data$y)
      # call the "logisticmodel function" to run the experiment for logistic regression and "RandomForest" function to run the experiment for randomforest
      allResults = switch(m,
                          
        "LogisticRegression" = logisticmodel(formular, training_BankTelM_data, testing_BankTelM_data, allResults, k),
        
        "RandomForest"       = RandomForestmodel(formular, training_BankTelM_data, testing_BankTelM_data, allResults, k)
      )
    }
    print(paste("evaluation complete for ", m))
    print(paste("Results of ",m, "for " ,k, " folds :"))
    print(allResults)
    allResults_withformat<-cbind(Model=m,allResults)
    print(formattable(allResults_withformat, align =c("l",rep("r", NCOL(allResults_withformat) - 1)), list(
      'Model' = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
      'TP'= color_tile("#DeF7E9", "#71CA97"),
      'FN'= color_tile("#DeF7E9", "#71CA97"),
      'TN'= color_tile("#DeF7E9", "#71CA97"),
      'FP'= color_tile("#DeF7E9", "#71CA97"),
      'TPR'= color_tile("#DeF7E9", "#71CA97"),
      'FPR'= color_tile("#DeF7E9", "#71CA97"),
      'Model_Profit.Loss'= color_tile("#DeF7E9", "#71CA97"),
      'accuracy'=color_tile("#DeF7E9", "#71CA97"),
      'Error'= color_bar("#FA614B")
    )))
    getMeans <- round(colMeans(allResults), digits = 2)
    #get the means of all the folds
    getMeans[1:4] <- as.integer(getMeans[1:4])  # TP, FN, TN, FP are rounded to ints
    measures <- as.list(getMeans)
    #binding the result to 'allmodelResults'
    if (m == "LogisticRegression") {
      allmodelResults <- data.frame(Logistic_regression = unlist(measures))
    }
    else{
      allmodelResults <-
        cbind(allmodelResults, data.frame(RandomForest = unlist(measures)))
    }
    
  }
  
  allmodelResults<-data.frame(t(allmodelResults))
  # Sort by highest MCC
  allmodelResults<-allmodelResults[order(allmodelResults$MCC,decreasing = TRUE),]
  
  # Output results to compare all classifiers
  allmodelResults[,1:4]<-sapply(allmodelResults[,1:4], as.integer)
  allmodelResults$folds<-KFOLDS
  print(formattable(allmodelResults, align =c("l",rep("r", NCOL(allmodelResults) - 1)), list(
    'Model' = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
    'TP'= color_tile("#DeF7E9", "#71CA97"),
    'FN'= color_tile("#DeF7E9", "#71CA97"),
    'TN'= color_tile("#DeF7E9", "#71CA97"),
    'FP'= color_tile("#DeF7E9", "#71CA97"),
    'TPR'= color_tile("#DeF7E9", "#71CA97"),
    'FPR'= color_tile("#DeF7E9", "#71CA97"),
    'Model_Profit.Loss'= color_tile("#DeF7E9", "#71CA97"),
    'accuracy'=color_tile("#DeF7E9", "#71CA97"),
    'Error'= color_bar("#FA614B")
  )))
  print("Below is the final results :")
  print((allmodelResults))
  print("*******Modelling and Evaluation is completed*******")
  #*#*****************************************************************
  # Modeling and Evaluation ends here :
  #******************************************************************

}
#endof main()

# ************************************************
# This is where R starts execution

gc() # garbage collection to automatically release memory

# clear plots and other graphics
if (!is.null(dev.list()))
  dev.off()
graphics.off()

# This clears all warning messages
assign("last.warning", NULL, envir = baseenv())

# clears the console area
cat("\014")


#Load the libraries used in this project

library(pacman)

pacman::p_load(char = MYLIBRARIES,
               install = TRUE,
               character.only = TRUE)

#load the external R script file
debugSource("BankTeleMDataPreparation.R")
debugSource("DataPreprocessingFunctions.R")
debugSource("BankTeleMEDAfunctions.R")

set.seed(123)

# ************************************************
main()

print("end of the Telemarketing Banking Prediction project")
