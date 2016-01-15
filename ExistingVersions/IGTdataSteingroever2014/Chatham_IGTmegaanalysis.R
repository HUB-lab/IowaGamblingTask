#clear workspace
rm(list = ls())

#set working directory
setwd("C:/Users/CHATHC01/Documents/D3Project/Spivack/Tasks/IowaGamblingTask/ExistingVersions/IGTdataSteingroever2014") 

#Install or load necessary packages
list.of.packages <- c("vioplot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#Load data from Steingroever et al: Steingroever, Fridberg, Horstmann, Kjome ... & Wagenmakers. (2015) Data from 617 Healthy Participants Performing the Iowa Gambling Task: A "Many Labs" Collaboration. Open Science Framework https://osf.io/8t7rm
load('IGTdata.rdata')

#find unique authors 
uniqueauthors <- c(unique(index_100[,2]), unique(index_95[,2]), unique(index_150[,2]))
#DO NOT OVERWRITE: write.csv(uniqueauthors,"uniqueauthorlist.csv")
#I then manually added a column indicating the payoff scheme used by each author, based on: Steingroever, Fridberg, Horstmann, Kjome ... & Wagenmakers. (2015) Data from 617 Healthy Participants Performing the Iowa Gambling Task: A "Many Labs" Collaboration. Open Science Framework https://osf.io/8t7rm
AuthorScheme <- read.csv("uniqueauthorlist.csv")
names(AuthorScheme)[names(AuthorScheme)=="x"] <- "Author"
AuthorScheme <- subset(AuthorScheme, select = -c(1,4) )

#Now, make master dataframe with every subject, choice, including authors and payoffs
levels=c(1,2,3,4)

numtrialstoaverage <- 50 #number of trials, from  end of the experiment, to use in calculations; set to zero to use ALL
if (numtrialstoaverage>0) {
  choice_100 <- choice_100[,(ncol(choice_100)-(numtrialstoaverage-1)):ncol(choice_100)]
  choice_95 <- choice_95[,(ncol(choice_95)-(numtrialstoaverage-1)):ncol(choice_95)]
  choice_150 <- choice_150[,(ncol(choice_150)-(numtrialstoaverage-1)):ncol(choice_150)]
}

choice_100 <- sapply(levels,function(x)rowSums(choice_100==x)) #count occurrences of x in each row
choice_95 <- sapply(levels,function(x)rowSums(choice_95==x)) #count occurrences of x in each row
choice_150 <- sapply(levels,function(x)rowSums(choice_150==x)) #count occurrences of x in each row
colnames(choice_100) <- c('A','B','C','D')
colnames(choice_95) <- c('A','B','C','D')
colnames(choice_150) <- c('A','B','C','D')
choice_100 <- as.data.frame(choice_100)
choice_95 <- as.data.frame(choice_95)
choice_150 <- as.data.frame(choice_150)
choice_100$Author <- index_100[,2]
choice_95$Author <- index_95[,2]
choice_150$Author <- index_150[,2]
choice_100$Scheme <- 0
choice_95$Scheme <- 0
choice_150$Scheme <- 0
for (i in seq(1:length(AuthorScheme$Author))) {
  if (length(choice_100[choice_100$Author==AuthorScheme$Author[i],]$Scheme) > 0) {
    choice_100[choice_100$Author==AuthorScheme$Author[i],]$Scheme <- AuthorScheme$PayoffScheme[i]
  }
  if (length(choice_95[choice_95$Author==AuthorScheme$Author[i],]$Scheme) > 0) {
    choice_95[choice_95$Author==AuthorScheme$Author[i],]$Scheme <- AuthorScheme$PayoffScheme[i]
  }
  if (length(choice_150[choice_150$Author==AuthorScheme$Author[i],]$Scheme) > 0) {
    choice_150[choice_150$Author==AuthorScheme$Author[i],]$Scheme <- AuthorScheme$PayoffScheme[i]
  }
}
IGTChoices <- rbind(choice_95,choice_150,choice_100)
IGTChoices$Trials <- sapply(levels,function(x)rowSums(IGTChoices[1:4]))[,1] #count number of trials on each row
IGTChoices$PropA <- IGTChoices$A / IGTChoices$Trials
IGTChoices$PropB <- IGTChoices$B / IGTChoices$Trials
IGTChoices$PropC <- IGTChoices$C / IGTChoices$Trials
IGTChoices$PropD <- IGTChoices$D / IGTChoices$Trials

pairs(~mpg+disp+drat+wt,data=mtcars, 
      main="Simple Scatterplot Matrix")

MeanProportionsbyScheme <- aggregate(IGTChoices[,c('PropA','PropB','PropC','PropD')], list(IGTChoices$Scheme), function(x) c(M = mean(x), SE = sd(x)/sqrt(length(x))))
colnames(MeanProportionsbyScheme)[1] <- "Scheme"

# Violin Plots
pdf(paste(getwd(),"/IGT_Payoff_ViolinPlots.pdf",sep=""))
  layout(matrix(c(1,2,3,4), 4, 1, byrow = TRUE), respect = FALSE)
  x1 <- IGTChoices$PropA[IGTChoices$Scheme==1]
  x2 <- IGTChoices$PropA[IGTChoices$Scheme==2]
  x3 <- IGTChoices$PropA[IGTChoices$Scheme==3]
  vioplot(x1, x2, x3, names=c("Scheme 1", "Scheme 2", "Scheme 3"), 
          col="gold")
  title("Proportion Deck A choices by Payoff Scheme")
  x1 <- IGTChoices$PropB[IGTChoices$Scheme==1]
  x2 <- IGTChoices$PropB[IGTChoices$Scheme==2]
  x3 <- IGTChoices$PropB[IGTChoices$Scheme==3]
  vioplot(x1, x2, x3, names=c("Scheme 1", "Scheme 2", "Scheme 3"), 
          col="gold")
  title("Proportion Deck B choices by Payoff Scheme")
  x1 <- IGTChoices$PropC[IGTChoices$Scheme==1]
  x2 <- IGTChoices$PropC[IGTChoices$Scheme==2]
  x3 <- IGTChoices$PropC[IGTChoices$Scheme==3]
  vioplot(x1, x2, x3, names=c("Scheme 1", "Scheme 2", "Scheme 3"), 
          col="gold")
  title("Proportion Deck C choices by Payoff Scheme")
  x1 <- IGTChoices$PropD[IGTChoices$Scheme==1]
  x2 <- IGTChoices$PropD[IGTChoices$Scheme==2]
  x3 <- IGTChoices$PropD[IGTChoices$Scheme==3]
  vioplot(x1, x2, x3, names=c("Scheme 1", "Scheme 2", "Scheme 3"), 
          col="gold")
  title("Proportion Deck D choices by Payoff Scheme")
dev.off()

save.image('Chatham_IGTmegaanalysis.rdata')
