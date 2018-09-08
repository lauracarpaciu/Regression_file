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
tbondsFile <-"C:\\Users\\Mirela\\RStudioProjects\\Regression_file\\Regression_Data\\tbond5yr.csv"
if(!file.exists(googFile)){tryCatch(googFile)}
if(file.exists(googFile)) goog_original <- read.csv(googFile)
if(!file.exists(nasdaqFile)){tryCatch(nasdaqFile)}
if(file.exists(nasdaqFile)) nasdaqFile_original <- read.csv(nasdaqFile)
if(!file.exists(tbondsFile)){tryCatch(tbondsFile)}
if(file.exists(tbondsFile)) tbondsFile_original <- read.csv(tbondsFile)
head(goog_original)
head(nasdaqFile_original)
# generate an id column for future use (joins etc)
goog_original$goog_id = seq.int(nrow(goog_original))
nasdaqFile_original$nasdaq_id = seq.int(nrow(nasdaqFile_original))
head(goog_original)
summary(goog_original)
head(nasdaqFile_original)
summary(nasdaqFile_original)
# how many volumes have been realized over the years?
goog_original %>% 
  ggplot(mapping = aes(year(Date))) +
  geom_bar(aes(fill= Volume), width=1, color="black") +
  theme(legend.position = "bottom", legend.direction = "vertical") + ggtitle("Volumes by Year")
nasdaqFile_original %>% 
  ggplot(mapping = aes(year(Date))) +
  geom_bar(aes(fill= Volume), width=1, color="black") +
  theme(legend.position = "bottom", legend.direction = "vertical") + ggtitle("Volumes by Year")
# eliminate any duplicates that may exist in the dataset
googs <- goog_original%>%
  distinct(.keep_all = TRUE,Date,Volume,Adj.Close)
nasdaqs <- nasdaqFile_original%>%
  distinct(.keep_all = TRUE,Date,Volume,Adj.Close)
# the date field is formatted as a string - transform that into R date
head(googs)
summary(googs)
head(nasdaqs)
summary(nasdaqs)
googTable <-read.table(googFile,header = TRUE, sep ="," )[,c("Date","Adj.Close")]
nasdaqTable <- read.table(nasdaqFile,header = TRUE, sep = ",")[,c("Date","Adj.Close")]
tbondsTable <- read.table(tbondsFile, header = TRUE, sep = ",")[,c("Date","Adj.Close")]
names(tbondsTable)[2] <- "tbonds.outcomes"
googTable <- merge(googTable,nasdaqTable, by="Date")
googTable[,c("Date")]<- as.character.Date(googTable[,c("Date")])
googTable <- googTable[order(googTable$Date,decreasing = TRUE),]
names(googTable)[2:3] <- c("goog.prices","nasdaq.prices")
googTable[-nrow(googTable),-1] <- googTable[-nrow(googTable),-1]/googTable[-1,-1]-1
names(googTable)[2:3] <- c("goog.outcomes","nasdaq.outcomes")
tbondsTable[,c("Date")]<-as.Date(tbondsTable[,c("Date")])
googTable<-merge(googTable,tbondsTable,by="Date")
googTable$tbonds.outcomes<-googTable$tbonds.outcomes/100
googTable[,c("goog.outcomes","nasdaq.outcomes")] <- googTable[,c("goog.outcomes","nasdaq.outcomes")]-googTable[,"tbonds.outcomes"]


