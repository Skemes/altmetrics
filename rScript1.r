#RSTUDIO BS: if you have a function, you must highlt all of it to make it run
#Conversely, if you ever have a multi-line statement, MUST highlight all of it.

#R Makes all text factors, so you MUST WATCH this
#get rid of excess levels: variable <- droplevels(variable)

#str(X) is the best way to see what X is or should be

#enter ?options on an r console to get function information on bottom right of
#this screen

#LOAD DATA
counts_raw <- read.delim("data/counts-raw.txt.gz")

#LOOK At data
dim(counts_raw)
head(counts_raw, 1)
tail(counts_raw, 1)
head(counts_raw$deliciousCount, 10) #use column names
counts_raw[1,10] #first row, 10th colum
counts_raw[1:3, 10:12] #does what you think it does
counts_raw[1:3,] #display errything
counts_raw[1:10, "pmid"] #using colnames, usually better so if they change
str(counts_raw$daysSincePublished)
head(counts_raw$daysSincePublished / 7, 10) #look at weeks been published
head(counts_raw$daysSincePublished / c(7,1), 10) #will divide first by 7, 2nd by 1, 3rd by 7, etc
str(counts_raw$journal)
levels(counts_raw$journal) #displays different factors of data (stored as numbers for efficiency)
head(counts_raw$authorsCount, 5)
is.na(counts_raw$authorsCount[1:10])
summary(counts_raw$year)
hist(counts_raw$year)
hist(log(counts_raw$deliciousCount))
plot(counts_raw$daysSincePublished, counts_raw$wosCountThru2010)

#CONDITIONALS
dim(counts_raw[counts_raw$authorsCount > 7, ])
dim(counts_raw[counts_raw$authorsCount == 7, ]) #inner statement returns T/F values. Outer gives all relevant rows
dim(counts_raw[counts_raw$journal == "pone", ])
dim(counts_raw[counts_raw$journal == "pone" | counts_raw$journal == "pbio", ])
dim(counts_raw[counts_raw$journal %in% c("pone", "pbio", "pgen"), ])

dim(counts_raw[grepl("Immunology", counts_raw$plosSubjectTags), ]) #find substring in string grepl(sstring, stringList) Some lines have multiple tags

dim(counts_raw[grepl("Evolutionary Biology", counts_raw$plosSubjectTags) & counts_raw$journal %in% c("pone", "pbio", "pmed"), ]) #sol'n to conditions challenge

mean(counts_raw$facebookShareCount)

sprintf("bleh %.3f", mean(counts_raw$facebookShareCount))

results <- counts_raw$title[counts_raw$authorsCount > 5]

numbers <- numeric(length=length(counts_raw$wosCountThru2011)) #useful for for-loops

res2 <- numeric(length=length(levels(counts_raw$journal)))
names(res2) <- levels(counts_raw$journal)
res2
res2["pbio"]
for(journalType in levels(counts_raw$journal)){ res2[journalType]  = mean(counts_raw$wosCountThru2010[counts_raw$journal == journalType]) }
res2

#USING DPLYR
library("dplyr")
research <- filter(counts_raw, articleType == "Research Article")
research_2006_fb_tweet_disease <- filter(research, year == 2006, facebookCommentCount > 0 | backtweetsCount > 0, grepl("Infectious Diseases", plosSubjectTags))
nrow(research_2006_fb_tweet_disease)

metrics <- select(research, contains("Count"), -authorsCount, f1000Factor, wikipediaCites)
colnames(metrics)

research_read <-filter(research, year <= 2008 & pdfDownloadsCount > 1000 & mendeleyReadersCount > 15 & wosCountThru2011 < 10)
nrow(research_read)
nrow(research)

#Challenge #1
research %>%  arrange(desc(wosCountThru2011)) %>% select(title) %>% slice(1:3)

#Challenge #2
#Idea: first pipe organizes data by interest (in this case number of authors)
#select grabs which portion of the data we want (which columns)
#Then the slice gives us the top 3 rows, or data points, that correspond
research %>%  arrange(desc(authorsCount)) %>%select(authorsCount, title, journal, plosSubjectTags) %>% slice(1:3)


####Summarizing Data with DPLYR####
#mutate functions: add columns to data:
research <- mutate(research, weeksSincePublished = daysSincePublished / 7, yearsSincePublished = weeksSincePublished / 52)
#Can also just do research$weeksSincePublished = research$daysSincePublished / 7. Note also we can pipe the above in 1 line

#groups_by grabs subset of data by FACTORS! can grab more than one too!
#So useful
research %>% group_by(journal, year) %>% summarize(tweets_mean = mean(backtweetsCount))

#Challenge #1
#Huge note: when doing group_by, you can use the n() command to get the number in each factor. Super useful.
tweets_per_journal <- research %>% group_by(journal) %>% summarize(total_Articles = n(), tweets_mean = mean(backtweetsCount), tweets_SEM = sd(backtweetsCount)/sqrt(n()) )
tweets_per_journal

#isn't that useful
research %>% group_by(journal) %>% summarize(readCiteCorr = cor(wosCountThru2010, pdfDownloadsCount, method="spearman"))

research %>% group_by(journal) %>% summarize(citeFacebook = cor(wosCountThru2010, facebookLikeCount, method="spearman"))

research %>% group_by(journal) %>% summarize(readFacebookCorr = cor(pdfDownloadsCount, facebookLikeCount, method="spearman"))

research %>% group_by(journal) %>% summarize(readTweetCorr = cor(pdfDownloadsCount, backtweetsCount, method="spearman"))

research %>% group_by(journal) %>% summarize(facebookTweetCorr = cor(facebookLikeCount, backtweetsCount, method="spearman"))

####GGPLOT2 PLOTTING###
library("ggplot2")

p <- ggplot(data = research, mapping = aes(x = pdfDownloadsCount, y = wosCountThru2011))
p+geom_point()
p <- ggplot(research, aes(x = pdfDownloadsCount, y = wosCountThru2011)) + geom_point()

#Super useful, color by factor:
p <- ggplot(research, aes(x = pdfDownloadsCount, y = wosCountThru2011)) + geom_point(aes(color = journal))

ggplot(research, aes(x = pdfDownloadsCount, y = wosCountThru2011)) + geom_point(aes(alpha = daysSincePublished))

#Set fixed colors for stuff
ggplot(research, aes(x = pdfDownloadsCount, y = wosCountThru2011)) + geom_point(color = "red")

#plot a loess curve for total data w/ 95% conf interval
ggplot(research, aes(x = pdfDownloadsCount, y = wosCountThru2011)) + geom_point(aes(alpha = daysSincePublished)) + geom_smooth()

#specify groups in the plot itself, so we now make a loess curve for each journal
ggplot(research, aes(x = pdfDownloadsCount, y = wosCountThru2011, color = journal)) + geom_point() + geom_smooth()

#Challenge #1
ggplot(research, aes(x = daysSincePublished, y = wosCountThru2011)) + geom_point(aes(color = journal), alpha=0.5) + geom_smooth(color="black")

#this is a linear fit to the data.
ggplot(research, aes(x = daysSincePublished, y = log(wosCountThru2011+1) )) + geom_point(aes(color = journal), alpha=0.5) + geom_smooth(color="black", method="lm", level=0.99)

#LESSON #2
#How to do log better:
p + scale_x_log10() + scale_y_log10()

#However due to the zeros we're going to do what I thought was right:
ggplot(research, aes(x = log10(pdfDownloadsCount +1), y = log(wosCountThru2011 +1) )) + geom_point(aes(color = journal)) + geom_smooth()

#Now we can fix the x axis so it matches the data:
p <- ggplot(research, aes(x = log10(pdfDownloadsCount +1), y = log(wosCountThru2011 +1) )) + geom_point(aes(color = journal)) + geom_smooth() + scale_x_continuous(breaks = c(1, 3), labels = c(10, 1000)) + scale_y_continuous(breaks = c(1, 3), labels = c(10, 1000))

#With scale instead of colors:
p + scale_color_grey()

#Or manually specify them:
p + scale_color_manual(values = c("red", "yellow", "orange", "purple", "blue", "yellow", "pink"))

#Nice colors in R: RColorBrewer
library("RColorBrewer")

p + scale_color_brewer(palette = "Dark2")

#To change the name of the levels displayed:
p + scale_color_brewer(palette = "Dark2", labels = 1:7, name = "title")

#can change display of axes via scale_x_continuous

#Challenge #1
ggplot(research, aes(x = sqrt(pdfDownloadsCount), y = sqrt(wosCountThru2011) )) + scale_color_brewer(palette = "Accent") + geom_point(aes(color = journal))

##Lesson #3: Subplots and Facets
p <- ggplot(research, aes(x = log10(pdfDownloadsCount + 1), y = log10(wosCountThru2011 + 1))) + geom_point(aes(color = journal)) + geom_smooth() + scale_x_continuous(breaks = c(1, 3), labels = c(10, 1000)) + scale_y_continuous(breaks = c(1, 3), labels = c(10, 1000))

#Can see each journal as a facet!:
p + facet_wrap(~journal)

#Can specify the organization of these on the page:
p + facet_wrap(~journal, ncol = 2)

#What about 2 categorical variables? facet_grid is here for you!

#Setup: create logical vector of whether this is an immuno paper:
research <- mutate(research, immuno = grepl("Immunology", plosSubjectTags))
#IF YOU DO THIS: you must re-define p:
p <- ggplot(research, aes(x = log10(pdfDownloadsCount + 1), y = log10(wosCountThru2011 + 1))) + geom_point(aes(color = journal)) + geom_smooth() + scale_x_continuous(breaks = c(1, 3), labels = c(10, 1000)) + scale_y_continuous(breaks = c(1, 3), labels = c(10, 1000))

#See the influence of immuno:
p + facet_grid(journal~immuno)

#ALTERNATIVELY, if we are dynamically changing and messing with the dataframe,
#we can just specify 'use the newest version of the dataframe via "%+%:
#(you can guess this is slow)
p %+% research + facet_grid(journal~immuno)

#Challenge #1
#To do multiple facets simultaneously, + them together
research <- mutate(research, evolution = grepl("Evolutionary Biology", plosSubjectTags))
p %+% research + facet_grid(journal~immuno+evolution)

#To re-label the axes, you have to make a labeller function, for example:
#mf_labeller <- function(var, value){
#  value <- as.character(value)
#  if (var=="sex") { 
#    value[value=="Female"] <- "Woman"
#    value[value=="Male"]   <- "Man"
#  }
#  return(value)
#}
#
#sp + facet_grid(. ~ sex, labeller=mf_labeller)
#from: http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/

##LESSON #4: GEOMS## Moar plotters
#mapping categorical variables (BOXPLOT):
p_box <- ggplot(research, aes(x = journal, y = log10(wosCountThru2011 + 1))) + geom_boxplot() + scale_y_continuous(breaks = c(1, 3), labels = c(10, 1000))

#Mapping categorical variables (BARPLOT):
#Setup: make our summar stats:
tweets_per_journal <- research %>% group_by(journal) %>% summarize(num = n(), mean = mean(backtweetsCount), sem = sd(backtweetsCount) / sqrt(num))

tweets_bar <- ggplot(tweets_per_journal, aes(x = journal, y = mean)) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem))

tweets_bar + labs(y = "Avg Number of Tweets")

#How to make the bar length smaller:
tt <- ggplot(tweets_per_journal, aes(x = journal, y = mean)) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.1)

#How to add the total number of articles, and other text, to the visualization:
tt + geom_text(aes(label = num), hjust = 0, vjust = 0)

#Challenge #1: Mean number of tweets per journal per year
#TRICK:Need to find this info and shove it in tweets_per_journal
#then just split by facet_wrap
researchYear <- research
researchYear$year = as.factor(researchYear$year)
tweets_per_journal <- researchYear %>% group_by(journal,year) %>% summarize(num = n(), mean = mean(backtweetsCount), sem = sd(backtweetsCount) / sqrt(num))
tt <- ggplot(tweets_per_journal, aes(x = journal, y = mean)) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.1)
levels(researchYear$year)
tt +facet_wrap(~year)

#Challenge #2: Visualizing a single distribution
#ezpz
histo <- ggplot(research %>% filter(wosCountThru2010 > 2), aes(x=log(wosCountThru2010+1))) + geom_histogram()
histo

#Adding a curve to fit the data:
specialPoints <- research %>% filter(wosCountThru2010 > 2) %>% select(wosCountThru2010)
specialPoints <- log10(specialPoints + 1)
histo2 <- histo <- ggplot(research %>% filter(wosCountThru2010 > 2), aes(x=log(wosCountThru2010+1)))
histo2 <- histo2 + geom_histogram(aes(y=density(specialPoints)))
histo + stat_function(fun=dexp, args=with(research, c(rate=1/mean(wosCountThru2010))))

#Review of R Loops -- apply along axes
counts_sub <- counts_raw[,c("wosCountThru2011", "backtweetsCount", "plosCommentCount")]

sum_stat <- apply(counts_sub, 1, mean, na.rm = TRUE) #apply along rows, 1, columns is 2
#NOTE: anything after the function, it passes it to mean.
summary(sum_stat)

#Functions
#root is optional
myFunc <- function(x, y, root){
  if(missing(root)){
    root <- 2
  }
  return(2^(x*y))
}

getMeanforJournal <- function(dframe, journalType, metric, functionMetric){
  return(functionMetric(dframe$metric[dframe$journal == journalType]))
}


#works! huzzah!
getFunctionforMetric <- function(dframe, variable, metric, functionMetric, ...){
  stopifnot(is.data.frame(dframe), is.function(functionMetric), variable %in% colnames(dframe), metric %in% colnames(dframe))
  if(!is.factor(dframe[[variable]])){
    warning("Variable is not a factor")
    dframe[[variable]] <- as.factor(dframe[[variable]])
  }
  resultFrame <- numeric(length = length(levels(dframe[[variable]])))
  names(resultFrame) <- levels(dframe[[variable]])

  for(vlevel in levels(dframe[[variable]])){
    intermediate <- dframe[ dframe[, variable] == vlevel, metric ]
    resultFrame[vlevel] = functionMetric(intermediate, ...)
  }
  stopifnot(!is.na(resultFrame))
  return(resultFrame)
}

#myDat <- getFunctionforMetric(counts_raw, "journal", "facebookLikeCount", mean, na.rm = TRUE)
myDat <- getFunctionforMetric(counts_raw, "journal", "facebookLikeCount", mean) 
str(myDat)
myDat

#Defensive Programming
#Lets you list a bunch of tests to conduct. If any goes wrong, will list that.
#Essentially lets you check a bunch of conditions on portions of code
#syntax: stopifnot(test1, test2, test3, etc) ex: test1 = is.character(x), etc.
#or length(metric1) == length(metric2)
#How this works: always dframe[row, column]
#this selects the subset of rows with the given column property and returns the value at the 
#specified column, in this case "backtweetsCount"
xxx <- counts_raw[ counts_raw[, "journal"] == "pone", "backtweetsCount"]

levels(counts_raw[["journal"]])

str(xxx)

###DEBUGGING TUTORIAL###
#All: https://jdblischak.github.io/r-intermediate-altmetrics/
#https://jdblischak.github.io/r-intermediate-altmetrics/20-debug.html
#print(n) within debugger gets you the value of n if n is in your code (since it's a command)
#(in the debugger)
#stopifnot(!is.na(result)) is very useful

#to pass a warning: Warning("This is not a factor")

#Testit library
#Gives you very nice assert function:
#assert("You gave me a vector, not a dframe", is.data.frame(dframe) == FALSE)
#has_error is also useful:
#assert("Attempting to add different types", has_error(1 + "a"))

#Idea is to convert these to unit tests AFTER solving a problem:
#for example, we know that the above function should work with na's for mean.
#thus, we can make this into a unit test:
#assert("Not handling NA's properly", has_error(getFunctionforMetric(counts_raw, "journal", "facebookLikeCount", mean))
#the pass case is:
#getFunctionforMetric(counts_raw, "journal", "facebookLikeCount", mean, na.rm=TRUE)
library("testit")
#Practicing with Unit Tests
my_mean <- function(x) {
  result <- sum(x) / length(x)
  return(result)
}

#Note that has_error() evaluates to TRUE if the code within has an error
#However, assert() shows the message ONLY if the statement evaluates to false
#so if we use has_error in assert(), then assert() only shows if the thing DOESN'T
#generate an error, which is a little awkward.

#The best way is to make fake data with known answers and see if functions are working

assert("x has a NA's within it", !is.na(my_mean(c(1,2,NA,3))) )

assert("my_mean does not check if x is a vector", !has_error(my_mean(counts_raw)))

assert("my_mean does not check if x has elements", has_error(my_mean(c())))

assert("my_mean does not throw out character vectors", !has_error(my_mean(c("a", "b", "c"))))
