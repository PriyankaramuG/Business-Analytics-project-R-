
#####################################################################################

# EDA FUNCTIONS SECTION (All EDA functions will be stored in this section)

#####################################################################################

# 1.01      10/05/2021   EDA changes added

# ************************************************
# VAR_SEPERATOR()
#
# Reads in the dataset and gives a list of True/False vectors based on numeric 
# and categorical data
#
# INPUT:   dataset
#
# OUTPUT : list of numerical and categorical T/F vectors
#          
#
# ************************************************
VAR_SEPERATOR <- function(dataset) {
  num = c()
  cat = c()
  for (i in 1:ncol(dataset)){ 
    x <- is.numeric(dataset[,i])
    num <- append(num, x)
    y <- !is.numeric(dataset[,i])
    cat <- append(cat, y)  
  }
  print("print the complete statistics of variables")
  print("No Missing values")
  print(summary(dataset))
  
  return(list(num = num, cat = cat))
}


# ************************************************
# BAR_PLOTS()
#
# Makes a bar plot for each field for the dataset provided
# 
#
# INPUT: Dataset
#
# OUTPUT: Bar plots using ggplot
#          
#
# ************************************************
BAR_PLOTS <- function(dataset) {
  for (i in 1:ncol(dataset)) {
    d <- ggplot(dataset, aes(x=dataset[,i])) + labs(x= colnames(dataset)[i]) + geom_text(stat='count', aes(label=..count..), vjust=-1)
    print(d + geom_bar(fill="#00abff", color="blue") + theme_bw())
  }
}

# ************************************************
# HIST_PLOTS()
#
# Creates Histograms for each 
# 
#
# INPUT: Datset
#
# OUTPUT : Histograms of fields
#          
#
# ************************************************
HIST_PLOTS <- function(dataset) {
  
  for (i in 1:ncol(dataset)) {
    print(ggplot(data=dataset, aes(x=dataset[,i])) + labs(x= colnames(dataset)[i]) +  geom_histogram(fill = "#FFDB6D", color = "#C4961A", bins=20) + theme_bw())
  }
 
}
# ************************************************
# FREQUENCY_TABLE()
#
# Creates a class-distribution/freqency-table for each field
# 
#
# INPUT:   dataset
#
# OUTPUT : frequency tables
#
#
# ************************************************

FREQUENCY_TABLE <- function(dataset) {
  
  for (i in 1:ncol(dataset)) {
    if (!is.numeric(dataset[,i])) {
      print(colnames(dataset)[i])
      print(table(dataset[,i]))
    }
  }
}

# ************************************************
# BIVARIATE_BAR()
#
# Creates bivariate plot of proportions of categorical variables w.r.t to target variable
# 
#
# INPUT:   dataset
#
# OUTPUT : Bivariate stacked proportion plot
#
#
# ************************************************

BIVARIATE_BAR <- function(dataset) {
  for (i in 1:ncol(dataset)) {
    plot<- ggplot(dataset, 
                  aes(x = dataset[,i], 
                      fill = y)) + 
      geom_bar(position = "fill") +
      labs(x= colnames(dataset)[i], y = "Proportion") + theme_bw()
    
    print(plot)
  }
  
}

# ************************************************
# numerical_vs_target()
#
# Function that covers individual exploration done for numerical variables 
# against the target variable
#
# INPUT:   dataset
#
# OUTPUT : Bivariate EDA plots
#
#
# ************************************************
numerical_vs_target <- function(datset)
{
  #Age
  print(ggplot(datset, aes(x=age, fill = y, color=y)) +
          geom_density(position="identity", alpha=0.5))
  
  #Duration
  print(ggplot(datset, aes(x=duration, fill = y, color=y)) +
          geom_density(position="identity", alpha=0.5))
  
  #Campaign
  print(ggplot(datset, aes(x=campaign)) + geom_boxplot())
  
  print(ggplot(datset, aes(x=campaign, fill = y, color=y)) +
          geom_histogram(position="identity", alpha=0.5, bins=20))
  
  #emp.var.rate
  print(ggplot(datset, aes(x=emp.var.rate, fill = y, color=y)) +
          geom_histogram(position="identity", alpha=0.5, bins = 20))
  
  #cons.price.idx
  print(ggplot(datset, aes(x=cons.price.idx, fill = y, color=y)) +
          geom_histogram(position="identity", alpha=0.5, bins = 20))
  
  
  #cons.conf.idx
  print(ggplot(datset, aes(x=cons.conf.idx, fill = y, color=y)) +
          geom_histogram(position="identity", alpha=0.5, bins = 20))
  
  #euribor3m
  print(ggplot(datset, aes(x=euribor3m, fill = y, color=y)) +
          geom_histogram(position="identity", alpha=0.5, bins = 20))
  
  #nr.employed
  print(ggplot(datset, aes(x=nr.employed, fill = y, color=y)) +
          geom_histogram(position="identity", alpha=0.5, bins = 20))
  
}

# ************************************************
# corrplot()
#
# Function that can be used to plot correlation heatmap 
#
# INPUT:   numerical dataset
#
# OUTPUT : correlation EDA plots
#
#
# ************************************************
corrplot <- function(dataset){
  cormat=cor(dataset)
  ggcorrplot(cormat, hc.order = TRUE, type = "full", lab = TRUE,
             tl.cex=8,outline.color="#f0f0f000",show.diag = F)
}

#*************************************************
# Plot_CategoricalwithOutcome() :
# plots the categorical values with dependent variable (y)
#
# INPUT:  Categorical data
# OUTPUT: print the Histogram plot
# ************************************************
Plot_CategoricalwithOutcome<-function(categorical_data){
  for (field in 1:ncol(categorical_data)){ 
    print(ggplot(data=categorical_data,aes(x=categorical_data[,field],fill=categorical_data$y))+
            geom_bar(position = "dodge",color="black")+
            
            ggtitle(colnames(categorical_data)[field]) +
            xlab(colnames(categorical_data)[field])+labs(fill = "outcome")+
            scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
            theme_minimal())
  }
}

####################### END of FUNCTION SECTION #############################




