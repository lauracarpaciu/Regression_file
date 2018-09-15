# prepare the R environment
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,         # Data munging functions
  zoo,              # Feature engineering rolling aggregates
  data.table,       # Feature engineering
  ggplot2,          # Graphics
  scales,           # Time formatted axis
  readr,            # Reading input files
  stringr,          # String functions
  reshape2,         # restructure and aggregate data 
  randomForest,     # Random forests
  corrplot,         # correlation plots
  Metrics,          # Eval metrics for ML
  vcd               # Visualizing discrete distributions
)

# set options for plots
options(repr.plot.width=6, repr.plot.height=6)
# Load the data
googFile <-"C:\\Users\\Mirela\\RStudioProjects\\Regression_file\\Regression_Data\\goog.csv"
nasdaqFile<-"C:\\Users\\Mirela\\RStudioProjects\\Regression_file\\Regression_Data\\Nasdaq.csv"
tbondsFile <-"C:\\Users\\Mirela\\RStudioProjects\\Regression_file\\Regression_Data\\tbond8yr.csv"
if(!file.exists(googFile)){tryCatch(googFile)}

if(file.exists(googFile)) goog_original <- read.csv(googFile)

head(goog_original)

# eliminate any duplicates that may exist in the dataset

googs <- goog_original%>%
  distinct(.keep_all = TRUE,Date,Volume,Adj.Close)

# the date field is formatted as a string - transform that into R date
googs$Date<-as.POSIXct(strptime(googs$Date,"%d/%m/%Y",tz="UTC"))

# generate an id column for future use (joins etc)
googs$goog_id = seq.int(nrow(googs))

head(googs)
summary(googs)

# how many volumes have been realized over the years?
googs %>% 
  ggplot(mapping = aes(year(Date))) +
  geom_bar(aes(fill= Volume), width=1, color="black") +
  theme(legend.position = "bottom", legend.direction = "vertical") + ggtitle("Volumes by Year")

# what values is our dataset missing?

ggplot_missing <- function(x){
  
  x %>%
    is.na %>%
    melt %>%
    ggplot(mapping = aes(x = Var2,
                         y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) +
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

ggplot_missing(googs)
    
# Preprocessing the data

googTable <-read.table(googFile,header = TRUE, sep ="," )[,c("Date","Adj.Close")]
nasdaqTable <- read.table(nasdaqFile,header = TRUE, sep = ",")[,c("Date","Adj.Close")]
tbondsTable <- read.table(tbondsFile, header = TRUE, sep = ",")[,c("Date","Adj.Close")]

names(tbondsTable)[2] <- "tbonds.returns"
tbondsTable[,c("Date")]<- as.Date(tbondsTable[,c("Date")])

googTable <- merge(googTable,nasdaqTable, by="Date")
googTable[,c("Date")]<- as.Date(googTable[,c("Date")])
googTable <- googTable[order(googTable$Date,decreasing = TRUE),]
names(googTable)[2:3] <- c("goog.prices","nasdaq.prices")

googTable[-nrow(googTable),-1] <- googTable[-nrow(googTable),-1]/googTable[-1,-1]-1
googTable<-googTable[-nrow(googTable),]

names(googTable)[2:3] <- c("goog.returns","nasdaq.returns")

googTable<-merge(googTable,tbondsTable,by="Date")
googTable$tbonds.returns<-googTable$tbonds.returns/100

googTable[,c("goog.returns","nasdaq.returns")] <- googTable[,c("goog.returns","nasdaq.returns")]-googTable[,"tbonds.returns"]

#Build a linear model that accounts for missing values

googM<- lm(googTable$goog.returns~googTable$nasdaq.returns, na.action = na.omit)

# Deal with missing values in the preprocessing step itself

googTable[,"goog.returns"][is.na(googTable[,"goog.returns"])]<-mean(googTable[,"goog.returns"])

#Including a categorical variable

googTable$Month = format(googTable$Date,"%m")
dummyVars <- model.matrix(~Month,googTable)
goog_MLR <- lm(googTable$goog.returns~googTable$nasdaq.returns+googTable$Month)
summary(goog_MLR)

#Robust linear regression 

plot(googTable$goog.returns,googTable$nasdaq.returns)
abline(googM)
require(MASS)
googRLM<- rlm(googTable$goog.returns~googTable$nasdaq.returns)
abline(googRLM,lty ="dotdash")

#Diagnostic plots 
plot(googM) 

