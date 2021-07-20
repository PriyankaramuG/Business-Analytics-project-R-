# *************************************************************
# Machine Learning for Telemarketing in the Banking Sector
# Group Name: Titan
# *************************************************************
#
# UPDATE
# 1.00      18/03/2021   First version of Data Preparation 
# 1.01      06/05/2021   Evaluation - Threshold added
# 1.02      07/05/2021   Evaluation - Roc analysis and threshold graph added to the function NdetermineThreshold



# ************************************************
# All functions with the prefix "N" are from Lab3 data preparation,CoMM053,2021  
# Prof. Nick F Ryman-Tubb
# Dept. of Computer Science
# University of Surrey
# GUILDFORD


#**************************************************************
# Practical Business Analytics Project (C0MM053) Group Project 
#**************************************************************

#*************************************************

# Pre-Processing a Dataset functions


# ************************************************
# NPREPROCESSING_categorical() :
#
# Transform SYMBOLIC or DISCRETE fields using 1-hot-encoding
#
# INPUT: data frame    - dataset      - symbolic fields
#        vector string - field_types  - types per field {ORDINAL, SYMBOLIC, DISCRETE}
#
# OUTPUT : data frame    - transformed dataset
#
# 18/2/2021 NRT Updated for efficiency
# ************************************************

NPREPROCESSING_categorical<-function(dataset,field_types){
  
  catagorical<-data.frame()
  
  #categorical_fields<-names(dataset)[which(field_types==TYPE_SYMBOLIC | field_types==TYPE_DISCRETE)]
  categorical_fields<-field_types
  # for each field
  for (field in categorical_fields){
    
    # Convert into factors. A level for each unique string
    ffield<-factor(dataset[,field])
    
    # Check if too many unique values to encode
    if (nlevels(ffield) > MAX_LITERALS) {
      stop(paste("Prof. Nick says - too many literals in:",
                 field,
                 nlevels(ffield)))
    }
    
    # Check if just one value!
    if (nlevels(ffield) ==1) {
      stop(paste("Prof. Nick says - field stuck at a single value:",
                 field))
    }
    
    # 1-hot encoding. A new column for each unique "level"
    xx<-data.frame(model.matrix(~ffield+0, data=ffield))
    
    names(xx)<-gsub("ffield",field,names(xx))
    
    
    catagorical<-as.data.frame(append(catagorical,xx))
    
  } #endof for()
  return (catagorical)
  
} # endof categorical_encoding()
# ************************************************
# PREPROCESSING_BinaryColumns() :
#
# Transform binary fields to 1 and 0
#
# INPUT: data frame    - dataset      - binary fields
#        vector string - field_types  - types per field {ORDINAL, SYMBOLIC, DISCRETE}
#
# OUTPUT : data frame    - transformed dataset
#
# ************************************************

PREPROCESSING_BinaryColumns<-function(dataset,field_types){
  
  Binarycatagorical<-data.frame()
  #categorical_fields<-names(dataset)[which(field_types==TYPE_SYMBOLIC | field_types==TYPE_DISCRETE)]
  categorical_fields<-field_types
  # for each field
  for (field in categorical_fields){
    
    # Convert into factors. A level for each unique string
    ffield<-factor(dataset[,field])
    xx<-data.frame(model.matrix(~ffield+0, data=ffield))
    names(xx)<-gsub("ffield",field,names(xx))
    # If 2 unique values, then can encode as a single "binary" column
    if (ncol(xx)==2){
      xx<-xx[,-2,drop=FALSE]
      names(xx)<-field  # Field name without the value appended
    }
    
    Binarycatagorical<-as.data.frame(append(Binarycatagorical,xx))
  }

  return (Binarycatagorical)
  
} # endof binary fields_encoding()
# ************************************************
# NPREPROCESSING_redundantFields() :
#
# Determine if an entire field is redundant
# Uses LINEAR correlation,
# so use with care as information will be lost
#
# INPUT: data frame - dataset - numeric values only
#        double     - cutoff  - Value above which is determined redundant [0,1]
#
# OUTPUT : Frame - dataset with any fields removed
# ************************************************

NPREPROCESSING_redundantFields<-function(dataset,cutoff){
  
  print(paste("Before redundancy check Fields=",ncol(dataset)))
  
  #Remove any fields that have a stdev of zero (i.e. they are all the same)
  xx<-which(apply(dataset, 2, function(x) sd(x, na.rm=TRUE))==0)+1
  
  if (length(xx)>0L)
    dataset<-dataset[,-xx]
  
  #Kendall is more robust for data do not necessarily come from a bivariate normal distribution.
  cr<-cor(dataset, use="everything")
  #cr[(which(cr<0))]<-0 #Positive correlation coefficients only
  NPLOT_correlagram(cr)
  
  correlated<-which(abs(cr)>=cutoff,arr.ind = TRUE)
  list_fields_correlated<-correlated[which(correlated[,1]!=correlated[,2]),]
  
  if (nrow(list_fields_correlated)>0){
    
    print("Following fields are correlated")
    print(list_fields_correlated)
    
    # 240220nrt print list of correlated fields as namesß
    for (i in 1:nrow(list_fields_correlated)){
      print(paste(names(dataset)[list_fields_correlated[i,1]],"~", names(dataset)[list_fields_correlated[i,2]]))
    }
    
    #We have to check if one of these fields is correlated with another as cant remove both!
    v<-vector()
    numc<-nrow(list_fields_correlated)
    for (i in 1:numc){
      if (length(which(list_fields_correlated[i,1]==list_fields_correlated[i:numc,2]))==0) {
        v<-append(v,list_fields_correlated[i,1])
      }
    }
    print("Removing the following fields")
    print(names(dataset)[v])
    
    return(dataset[,-v]) #Remove the first field that is correlated with another
  }
  return(dataset)
}

# ************************************************
# NPLOT_correlagram() :
#
# Plots PLOT_correlagram
#
# INPUT: data frame - cr - n x n frame of correlation coefficients
#
# OUTPUT : None
# 221019 - plot absolute values only
# ************************************************
NPLOT_correlagram<-function(cr){
  
  #Defines the colour range
  col<-colorRampPalette(c("green", "red"))
  
  #To fir on screen, convert field names to a numeric
  rownames(cr)<-1:length(rownames(cr))
  colnames(cr)<-rownames(cr)
  
  corrplot::corrplot(abs(cr),method="square",
                     order="FPC",
                     cl.ratio=0.2,
                     cl.align="r",
                     tl.cex = 0.6,cl.cex = 0.6,
                     cl.lim = c(0, 1),
                     mar=c(1,1,1,1),bty="n")
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
# myEvaluateClassifier() :
#
# Use dataset to generate predictions from model
# Evaluate as classifier using threshold value
#
# INPUT   :   vector double     - probs        - probability of being class 1
#             Data Frame        - testing_data - Dataset to evaluate
#             double            - threshold     -cutoff (probability) for classification
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
#
# ************************************************
myEvaluateClassifier<-function(probs,testing_data,threshold) {
  
  predictedClass<-ifelse(as.numeric(probs)<threshold,0,1)
  expectedClass<-testing_data[,OUTPUT_FIELD]
  
  results<-NcalcConfusion(expectedClass=expectedClass,
                          predictedClass=predictedClass)
  
  return(results)
} #endof myEvaluateClassifier()
# ************************************************
# Ndeterminethreshold() :
#
# Use dataset to generate predictions from model
# as classifier at range of thresholds values
# Plot the results
#
# INPUT   :   vector double  - probs        - probability of being class 1
#         :   Data Frame     - testing_data - dataset to evaluate
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
#
# ************************************************


NdetermineThreshold<-function(probs,testing_data,Title,fold){
  
  # ************************************************
  toPlot<-data.frame()
  
  #Vary the threshold
  for(threshold in seq(0,1,by=0.01)){
    results<-myEvaluateClassifier(probs=probs,testing_data=testing_data,threshold=threshold)
    toPlot<-rbind(toPlot,data.frame(x=threshold,fpr=results$FPR,tpr=results$TPR,tp=results$TP,fp=results$FP))
  }
  
  toPlot$pnl<-(toPlot$tp*50)-((toPlot$tp + toPlot$fp)*3.50)
  maxpnl<-toPlot$x[which.max(toPlot$pnl)]
  
  toPlot$distance<-sqrt(((100-toPlot$tpr)^2)+(toPlot$fpr^2))
  
  # ************************************************
  # Plot threshold graph
  
  # Sensitivity
  title_for_sens=paste(Title,"-Threshold Perfomance bank Telemarketing Classifier Model for", fold)
  plot(x=toPlot$x,
       y=toPlot$tpr,
       type="l",
       lwd=3,
       col="blue",
       xlab="Threshold",
       ylab="%Rate",
       main=paste(Title,"- Threshold Perfomance bank Telemarketing Classifier Model for kfold", fold))
  
  # Plot the specificity (1-FPR)
  lines(x=toPlot$x,y=100-toPlot$fpr,type="l",col="red",lwd=3,lty=1)
  
  # The point where specificity and sensitivity are the same
  crosspoint<-toPlot$x[which(toPlot$tpr<(100-toPlot$fpr))[1]]
  abline(v=crosspoint,col="red",lty=3,lwd=2)
  
  # Plot the Euclidean distance to "perfect" classifier (smallest the best)
  lines(toPlot$x,toPlot$pnl,type="l",col="green",lwd=2,lty=3)
  abline(v=maxpnl,col="green",lty=3,lwd=2)
  
  legend("bottom",c("TPR","1-FPR","Profit&lossDistance"),col=c("blue","red","green"),lty=1:2,lwd=2)
  text(x=0,y=50, adj = c(-0.2,2),cex=1,col="black",paste("THRESHOLDS:\nprofit&loss=",maxpnl))
  # ************************************************
  # 121020NRT ROC graph
  
  #plot(100-toPlot$fpr,toPlot$tpr,type="l",lwd=3, col="blue",main=paste(Title,"- ROC bank Telemarketing Classifier Model for kfold",fold),
       #xlab="Specificity (1-FPR) %",
       #ylab="Sensitivity (TPR) %",
       #xlim=(c(100,0)))
  
  #sensitivityROC<-toPlot$tpr[which.min(toPlot$distance)]
  #specificityROC<-100-toPlot$fpr[which.min(toPlot$distance)]
  
  #Add crosshairs to the graph
  #abline(h=sensitivityROC,col="red",lty=3,lwd=2)
  #abline(v=specificityROC,col="red",lty=3,lwd=2)
  
  #annotate<-paste("Threshold: ",round(maxpnl,digits=4L),
                  #"\nTPR: ",round(sensitivityROC,digits=2L),
                  #"%\n1-FPR: ",round(specificityROC,digits=2L),"%",sep="")
  
  #text(x=specificityROC, y=sensitivityROC, adj = c(-0.2,1.2),cex=1, col="red",annotate)
  
  #Use the "best" distance threshold to evaluate classifier
  #200320 use the value in minEuclidean as the threshold value
  results<-myEvaluateClassifier(probs=probs,
                                testing_data=testing_data,
                                threshold=maxpnl)
  
  #Use the Youdan threshold to evaluate classifier
  #results<-myEvaluateClassifier(probs=probs,
  #                              testing_data=testing_data,
  #                              threshold=maxYoudan)
  results$threshold<-maxpnl
  return(results)
} #endof Ndeterminethreshold()
# ************************************************
# NcalcConfusion() :
#
# Calculate a confusion matrix for 2-class classifier
# INPUT: vector - expectedClass  - {0,1}, Expected outcome from each row (labels)
#        vector - predictedClass - {0,1}, Predicted outcome from each row (labels)
#
# OUTPUT: A list with the  entries from NcalcMeasures()
#
# 070819NRT convert values to doubles to avoid integers overflowing
# Updated to the following definition of the confusion matrix
#
#                    ACTUAL
#               ------------------
# PREDICTED     FRAUD   |  GENUINE
#               ------------------
#     FRAUD      TP     |    FP
#               ==================
#     GENUINE    FN     |    TN
#
#
# ************************************************
NcalcConfusion<-function(expectedClass,predictedClass){
  
  confusion<-table(factor(predictedClass,levels=0:1),factor(expectedClass,levels=0:1))
  
  # This "converts" the above into our preferred format
  
  TP<-as.double(confusion[2,2])
  FN<-as.double(confusion[1,2])
  FP<-as.double(confusion[2,1])
  TN<-as.double(confusion[1,1])
  
  return(NcalcMeasures(TP,FN,FP,TN))
  
} #endof NcalcConfusion()
# ************************************************
# NcalcMeasures() :
#
# Evaluation measures for a confusion matrix
#
# INPUT: numeric  - TP, FN, FP, TN
#
# OUTPUT: A list with the following entries:
#        TP        - double - True Positive records
#        FP        - double - False Positive records
#        TN        - double - True Negative records
#        FN        - double - False Negative records
#        accuracy  - double - accuracy measure
#        pgood     - double - precision for "good" (values are 1) measure
#        pbad      - double - precision for "bad" (values are 1) measure
#        FPR       - double - FPR measure
#        TPR       - double - FPR measure
#        TNR       - double - TNR measure
#        MCC       - double - Matthew's Correlation Coeficient
#
# 080819NRT added TNR measure
# ************************************************
NcalcMeasures<-function(TP,FN,FP,TN){
  
  retList<-list(  "TP"=TP,
                  "FN"=FN,
                  "TN"=TN,
                  "FP"=FP,
                  "accuracy"=100.0*((TP+TN)/(TP+FP+FN+TN)),
                  "pgood"=   100.0*(TP/(TP+FP)),
                  "pbad"=    100.0*(TN/(FN+TN)),
                  "FPR"=     100.0*(FP/(FP+TN)),
                  "TPR"=     100.0*(TP/(TP+FN)),
                  "TNR"=     100.0*(TN/(FP+TN)),
                  "Error"=   100.0*((FP+FN)/(TP+FP+FN+TN)),
                  "MCC"=     ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)),
                  #"F1 Score"=(2 * precision * recall) / (precision + recall),
                  "F1 Score"=100.0*((2*TP)/(2*TP+FN+FP)),
                  "Model_Profit&Loss"=(TP*50)-((TP + FP)*3.50)
  )
  return(retList)
}
# ************************************************
# NConvertClass() :
#
# In original dataset, $Status is the classification label
# We need to convert this to give the minority class a value of 0
# this just makes it easiert to define the confusioon matrix!
# for the UCI-G this is a class of {0,1} being {bad, good}
#
# INPUT   :
#             Data Frame        - dataset
#
# OUTPUT  :
#             Data Frame        - dataset
#
# ************************************************
# ************************************************
# NPREPROCESSING_outlier() :
#
# Determine if a value of a record is an outlier for each field
#
# INPUT:   data frame - ordinals   - numeric fields only
#          double     - confidence - Confidence above which is determined an outlier [0,1]
#                                  - Set to negative Confidence if NOT remove outliers
#
# OUTPUT : data frame - ordinals with any outlier values replaced with the median of the field
# ************************************************
# ChiSquared method
# Uses   library(outliers)
# https://cran.r-project.org/web/packages/outliers/outliers.pdf

NPREPROCESSING_outlier<-function(ordinals,confidence){
  
  #For every ordinal field in our dataset
  for(field in 1:(ncol(ordinals))){
    
    sorted<-unique(sort(ordinals[,field],decreasing=TRUE))
    outliers<-which(outliers::scores(sorted,type="chisq",prob=abs(confidence)))
    NplotOutliers(sorted,outliers,colnames(ordinals)[field])
    
    #If found records with outlier values
    if ((length(outliers>0))){
      
      #070819NRT If confidence is positive then replace values with their means, otherwise do nothing
      if (confidence>0){
        outliersGone<-rm.outlier(ordinals[,field],fill=TRUE)
        sorted<-unique(sort(outliersGone,decreasing=TRUE))
        NplotOutliers(sorted,vector(),colnames(ordinals)[field])
        ordinals[,field]<-outliersGone #Put in the values with the outliers replaced by means
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers),"Replaced with MEAN"))
      } else {
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers)))
      }
    }
    
  }
  return(ordinals)
}

# ************************************************
# NplotOutliers() :
#
# Scatter plot of field values and colours outliers in red
#
# INPUT: Vector - sorted    -  points to plot as literal values
#        Vector - outliers  - list of above points that are considered outliers
#        String - fieldName - name of field to plot
#
# OUTPUT : None
# ************************************************
NplotOutliers<-function(sorted,outliers,fieldName){
  
  plot(1:length(sorted),sorted,pch=1,xlab="Unique records",ylab=paste("Sorted values",fieldName),bty="n")
  if (length(outliers)>0)
    points(outliers,sorted[outliers],col="red",pch=19)
}
# ************************************************
# Nrescaleentireframe() :
#
# Rescle the entire dataframe to [0.0,1.0]
#
# INPUT:   data frame - dataset - numeric data frame
#
# OUTPUT : data frame - scaled numeric data frame
# ************************************************
Nrescaleentireframe<-function(dataset){
  
  scaled<-sapply(as.data.frame(dataset),Nrescale)
  return(scaled)
}
# ************************************************
# Nrescale() :
#
# These are the real values, that we scale between 0-1
# i.e. x-min / (max-min)
#
# INPUT:   vector - input - values to scale
#
# OUTPUT : vector - scaled values to [0.0,1.0]
# ************************************************
Nrescale<-function(input){
  
  minv<-min(input)
  maxv<-max(input)
  return((input-minv)/(maxv-minv))
}