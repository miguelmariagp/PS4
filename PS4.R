install.packages("rvest")
library(rvest)    
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

## Grab the tables from the page and use the html_table function to extract the tables.
## You need to subset temp to find the data you're interested in (HINT: html_table())

temp <- wikiURL %>% 
  read_html %>%
  html_nodes("table")

#The table we want is the second one on the page.
tab.list<-html_table(temp[2])



#Transforming into dataframe and removing row 2 
tab<-data.frame(tab.list[[1]][-c(1,2),])

names(tab)<-c("Number","Year","Winner","PartyW","VoteShare","VoteShareMargin",
              "Vote","VoteMargin","Runner-up","PartyR","Turnout")

# The columns with margins are all messy so I need clean them

########################################################
#Cleaning VoteShare
########################################################
tab$VoteShare<-as.numeric(gsub("%","",tab$VoteShare))


########################################################
#Cleaning VoteShareMargin
########################################################

#There are three types of data
#Second, for negative percentages, strings include â

#Hence, I use grep() to create a variable that identifies negative margins
tab$NegShareMargin<-ifelse(1:dim(tab)[1] %in% grep("â",tab$VoteShareMargin),-1,1)

#Now I start by making the characters associated with negative values look similar to the others
tab$VoteShareMargin<-gsub(".*'","",tab$VoteShareMargin)

#Next I extract everything before the first % sign and make it numeric
tab$VoteShareMargin<-as.numeric(gsub("%.*","",tab$VoteShareMargin))

#And finally I add the sign to the negative values
tab$VoteShareMargin<-tab$VoteShareMargin*tab$NegShareMargin


########################################################
#Cleaning Vote (absolute number)
########################################################
tab$Vote<-as.numeric(gsub(",","",tab$Vote))

########################################################
#Cleaning Turnout
########################################################
tab$Turnout<-as.numeric(gsub("%","",tab$Turnout))

######################################
##NOTE: CAN'T FIND A PATTERN IN VOTEMARGIN SO I CAN CLEAN IT
#####################################

#Questions to visualize

##########################################################
#Did elections became more competitive with time?
##########################################################
plot(tab$Year,tab$VoteShareMargin, main="Margin of victory, by election and party",
     xlab="Election", ylab="Margin (percentage points)")
lines(loess.smooth(tab$Year,tab$VoteShareMargin, span=1/2),col="gray",lwd=2)
#lines(loess.smooth(tab$Year[tab$PartyW=="Dem."],tab$VoteShareMargin[tab$PartyW=="Dem."],
 #                  span=1/2),col="blue",lwd=1)
#lines(loess.smooth(tab$Year[tab$PartyW=="Rep."],tab$VoteShareMargin[tab$PartyW=="Rep."],
 #                  span=1/2),col="Red",lwd=1)
abline(0,0,lty=3)
points(tab$Year[tab$PartyW=="Rep."],
       tab$VoteShareMargin[tab$PartyW=="Rep."], col="Red",pch=19)
points(tab$Year[tab$PartyW=="Dem."],
       tab$VoteShareMargin[tab$PartyW=="Dem."], col="blue",pch=19)
legend("bottom",legend=c("Smoother", "Democrats","Republicans"),
       ncol=3, lwd=c(2,1,1), col=c("gray","blue","Red"), bty="n",
       lty=c(1,NA,NA),pch=c(NA,19,19))


#################################################################
#Are there differences in the margin of victory by party?
#################################################################
tabbipart<-tab[tab$PartyW=="Dem."|tab$PartyW=="Rep.",]

par(mfrow=c(1,3))
boxplot(VoteShareMargin~PartyW,data=tabbipart,ylim=c(-5,30),col=c("blue","Red"),
        xaxt="n",ylab="Margin of Victory (percentage points)",xlab="1824-2012")
abline(0,0,lty=3)
boxplot(VoteShareMargin~PartyW,data=tabbipart[tabbipart$Year<=1968,],ylim=c(-5,30),
        xaxt="n",yaxt="n",xlab="Pre-Civil Rights Movement",col=c("blue","Red"))
abline(0,0,lty=3)
boxplot(VoteShareMargin~PartyW,data=tabbipart[tabbipart$Year>1968,],ylim=c(-5,30),
    xaxt="n",yaxt="n",xlab="Post-Civil Rights Movement",col=c("blue","Red"))
abline(0,0,lty=3)


##############################################
#Total number of votes and turnout by time
##############################################
#Ordering the dataset by Year
tab<-tab[with(tab,order(Year)),]

par(mfrow=c(1,1),mar=c(5,4,4,5))
plot(tab$Year,log(tab$Vote),type="p",xlab="Election",ylab="# Votes for winner (logged)",
     main="Votes for Winner and Turnout, by Election",pch=16,col="Navyblue")
par(new=TRUE)
plot(tab$Year,tab$Turnout,type="l",col="Gold",xaxt="n",yaxt="n",xlab="",ylab="",lwd=1.5)
axis(4)
mtext("Turnout (%)",side=4,line=3)
legend("bottomright",legend=c("Votes for winner (log)", "Turnout (%)"),
        lwd=c(1,1.5), col=c("Navyblue","Gold"), bty="n", lty=c(NA,1),pch=c(16,NA))





##############
###############
################
###NEW EXERCISE
################
###############
##############

wikiURL <- 'https://en.wikipedia.org/wiki/United_States_presidential_election'

## Grab the tables from the page and use the html_table function to extract the tables.
## You need to subset temp to find the data you're interested in (HINT: html_table())

temp <- wikiURL %>% 
  read_html %>%
  html_nodes("table")

#The table we want is the second one on the page.
tab2.list<-html_table(temp[3])
tab2.list[[1]]
#Transforming into dataframe and removing row 2 
tab2<-data.frame(tab2.list[[1]])


#There are multiple ways to extract the numbers from the different columns, but I found
#this approach to be the neatest. It creates a list with each element containing all the numbers in each string.
library(stringr)

#To extract the number of EC votes for the winner I first create the list with all numbers in each string
WinnerECVotes<-str_extract_all(tab2$Winner,"\\(?[0-9]+\\)?")[[1]]
#And here I extract the first number of each object in the list, which is the number I want.
#Note: For James Monroe, two votes are given. My approach captures the lower bound.
tab2$WinnerECVotes<-as.numeric(sapply(WinnerECVotes, "[[", 1))


#Now, for the remaining candidates I use a similar approach
#First, creating the list
ECRunnerupVotes<-str_extract_all(tab2$Other.major.candidates.27.,"\\(?[0-9]+\\)?")
#And then extracting the highest number in the string which in all cases corresponds to the number of EC votes collected by the runner up
tab2$RUpECVotes<-laply(ECRunnerupVotes,function(x) max(as.numeric(x)))


############
#Finally, to merge I need to clean the variable Election.year and I will use the same approach
#used just above since in some cases the variable has more than one number/date
############
tab2$Election.year
Years<-str_extract_all(tab2$Election.year,"\\(?[0-9]+\\)?")
tab2$Year<-laply(Years,function(x) max(as.numeric(x)))

finaltab<-merge(tab,tab2[,c("Year","WinnerECVotes","RUpECVotes")], by="Year")
names(finaltab)
