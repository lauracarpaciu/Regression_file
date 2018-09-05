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
# Load the googs data
googFile <-"C:\\Users\\Mirela\\RStudioProjects\\Regression_file\\Regression_Data\\goog.csv"
nasdaqFile<-"C:\\Users\\Mirela\\RStudioProjects\\Regression_file\\Regression_Data\\Nasdaq.csv"
if(!file.exists(googFile)){tryCatch(googFile)}
if(file.exists(googFile)) goog_original <- read.csv(googFile)
if(!file.exists(nasdaqFile)){tryCatch(nasdaqFile)}
if(file.exists(nasdaqFile)) nasdaqFile_original <- read.csv(nasdaqFile)
head(goog_original)
summary(goog_original)
head(nasdaqFile_original)
summary(nasdaqFile_original)
googTable <-read.table(googFile,header = TRUE, sep ="," )[,c("Date","Adj.Close")]
head(googTable)
# generate an id column for future use (joins etc)
goog_original$goog_id = seq.int(nrow(goog_original))
head(goog_original)
summary(goog_original)
# eliminate any duplicates that may exist in the dataset
googs <- goog_original%>%
  distinct(.keep_all = TRUE,Date,Volume,Adj.Close)
# how many volumes have been realized over the years?
goog_original %>%
  ggplot(mapping = aes(year(Date))) +
  geom_bar(aes(fill= Volume), width=1, color="black") +
  theme(legend.position = "bottom", legend.direction = "vertical") + ggtitle("Volumes by Year")

