#Inside Edge script
#Set working directory to wherever you have this script and a fangraphs leaderboard csv.  When you export the data, you should set the 
#min number of innings to 0 so that pitchers get included; if you only use qualified you won't get pitchers.  Also you won't have the 'true'
#average, only the average from qualified fielders.
# Set variable 'filename' to the name of the file
library(plyr)

d = read.csv(filename)
colnames(d)[6] <- "Impossible.Total"

colnames(d)[7] <- "Remote.Total"
colnames(d)[8] <- "Remote.Pct"
#Convert Remote percentages from factor to numeric value between 0 and 1
d$Remote.Pct = as.numeric(sub(" %", "",d$Remote.Pct))/100
#Calculate remote plays made 
d$Remote.Made = round(d$Remote.Total*d$Remote.Pct,digits=0)

colnames(d)[9] <- "Unlikely.Total"
colnames(d)[10] <- "Unlikely.Pct"
#Convert factor to numeric
d$Unlikely.Pct = as.numeric(sub(" %", "",d$Unlikely.Pct))/100
#Calculate unlikely plays made
d$Unlikely.Made = round(d$Unlikely.Total*d$Unlikely.Pct,digits=0)


colnames(d)[11] <- "Even.Total"
colnames(d)[12] <- "Even.Pct"
#Convert factors to numeric
d$Even.Pct = as.numeric(sub(" %", "",d$Even.Pct))/100
#Calculate Even plays made
d$Even.Made = round(d$Even.Total*d$Even.Pct,digits=0)

colnames(d)[13] <- "Likely.Total"
colnames(d)[14] <- "Likely.Pct"
d$Likely.Pct = as.numeric(sub(" %", "",d$Likely.Pct))/100
#Calculate Likely plays made
d$Likely.Made = round(d$Likely.Total*d$Likely.Pct,digits=0)


colnames(d)[15] <- "Routine.Total"
colnames(d)[16] <- "Routine.Pct"
d$Routine.Pct = as.numeric(sub(" %", "",d$Routine.Pct))/100
#Calculate Routine plays made
d$Routine.Made = round(d$Routine.Total*d$Routine.Pct,digits=0)

#Our data frame has an NA for a Pct column whenever the corresponding Total column has a zero.  Lets change these NAs to 0s
d[is.na(d)] <- 0

# Let's calculate the Total Number of Plays
d$Total.Plays = d$Impossible.Total + d$Remote.Total + d$Unlikely.Total + d$Even.Total + d$Likely.Total + d$Routine.Total
d$Hard.Plays = d$Impossible.Total + d$Remote.Total + d$Unlikely.Total
d$Easy.Plays = d$Even.Total + d$Likely.Total + d$Routine.Total


#Expected Plays Made Formula
#d.Pos$expected.plays = d.Pos$Remote.Total*Pos.remote.avg + d.Pos$Unlikely.Total*Pos.unlikely.avg + d.Pos$Even.Total*Pos.even.avg + d.Pos$Likely.Total*Pos.likely.avg + d.Pos$Routine.Total*Pos.routine.avg

#PAA formula
#d.Pos$PAA = d.Pos$Remote.Made + d.Pos$Unlikely.Made + d.Pos$Even.Made + d.Pos$Likely.Made + d.Pos$Routine.Made - d.Pos$expected.plays


#Now we need to calculate the average percent of plays made by position
#First for Pitcher
d.p = subset(d,Pos=="P")
p.remote.avg = sum(d.p$Remote.Made)/sum(d.p$Remote.Total)
p.unlikely.avg = sum(d.p$Unlikely.Made)/sum(d.p$Unlikely.Total)
p.even.avg = sum(d.p$Even.Made)/sum(d.p$Even.Total)
p.likely.avg = sum(d.p$Likely.Made)/sum(d.p$Likely.Total)
p.routine.avg = sum(d.p$Routine.Made)/sum(d.p$Routine.Total)
d.p$expected.plays = d.p$Remote.Total*p.remote.avg + d.p$Unlikely.Total*p.unlikely.avg + d.p$Even.Total*p.even.avg + d.p$Likely.Total*p.likely.avg + d.p$Routine.Total*p.routine.avg
d.p$PAA = d.p$Remote.Made + d.p$Unlikely.Made + d.p$Even.Made + d.p$Likely.Made + d.p$Routine.Made - d.p$expected.plays


#Now for catcher
d.c = subset(d,Pos=="C")
c.remote.avg = sum(d.c$Remote.Made)/sum(d.c$Remote.Total)
c.unlikely.avg = sum(d.c$Unlikely.Made)/sum(d.c$Unlikely.Total)
c.even.avg = sum(d.c$Even.Made)/sum(d.c$Even.Total)
c.likely.avg = sum(d.c$Likely.Made)/sum(d.c$Likely.Total)
c.routine.avg = sum(d.c$Routine.Made)/sum(d.c$Routine.Total)
d.c$expected.plays = d.c$Remote.Total*c.remote.avg + d.c$Unlikely.Total*c.unlikely.avg + d.c$Even.Total*c.even.avg + d.c$Likely.Total*c.likely.avg + d.c$Routine.Total*c.routine.avg
d.c$PAA = d.c$Remote.Made + d.c$Unlikely.Made + d.c$Even.Made + d.c$Likely.Made + d.c$Routine.Made - d.c$expected.plays

#First baseman
d.1b = subset(d,Pos=="1B")
fb.remote.avg = sum(d.1b$Remote.Made)/sum(d.1b$Remote.Total)
fb.unlikely.avg = sum(d.1b$Unlikely.Made)/sum(d.1b$Unlikely.Total)
fb.even.avg = sum(d.1b$Even.Made)/sum(d.1b$Even.Total)
fb.likely.avg = sum(d.1b$Likely.Made)/sum(d.1b$Likely.Total)
fb.routine.avg = sum(d.1b$Routine.Made)/sum(d.1b$Routine.Total)
d.1b$expected.plays = d.1b$Remote.Total*fb.remote.avg + d.1b$Unlikely.Total*fb.unlikely.avg + d.1b$Even.Total*fb.even.avg + d.1b$Likely.Total*fb.likely.avg + d.1b$Routine.Total*fb.routine.avg
d.1b$PAA = d.1b$Remote.Made + d.1b$Unlikely.Made + d.1b$Even.Made + d.1b$Likely.Made + d.1b$Routine.Made - d.1b$expected.plays



#Second baseman
d.2b = subset(d,Pos=="2B")
sb.remote.avg = sum(d.2b$Remote.Made)/sum(d.2b$Remote.Total)
sb.unlikely.avg = sum(d.2b$Unlikely.Made)/sum(d.2b$Unlikely.Total)
sb.even.avg = sum(d.2b$Even.Made)/sum(d.2b$Even.Total)
sb.likely.avg = sum(d.2b$Likely.Made)/sum(d.2b$Likely.Total)
sb.routine.avg = sum(d.2b$Routine.Made)/sum(d.2b$Routine.Total)
d.2b$expected.plays = d.2b$Remote.Total*sb.remote.avg + d.2b$Unlikely.Total*sb.unlikely.avg + d.2b$Even.Total*sb.even.avg + d.2b$Likely.Total*sb.likely.avg + d.2b$Routine.Total*sb.routine.avg
d.2b$PAA = d.2b$Remote.Made + d.2b$Unlikely.Made + d.2b$Even.Made + d.2b$Likely.Made + d.2b$Routine.Made - d.2b$expected.plays

#Third baseman
d.3b = subset(d,Pos=="3B")
tb.remote.avg = sum(d.3b$Remote.Made)/sum(d.3b$Remote.Total)
tb.unlikely.avg = sum(d.3b$Unlikely.Made)/sum(d.3b$Unlikely.Total)
tb.even.avg = sum(d.3b$Even.Made)/sum(d.3b$Even.Total)
tb.likely.avg = sum(d.3b$Likely.Made)/sum(d.3b$Likely.Total)
tb.routine.avg = sum(d.3b$Routine.Made)/sum(d.3b$Routine.Total)
d.3b$expected.plays = d.3b$Remote.Total*tb.remote.avg + d.3b$Unlikely.Total*tb.unlikely.avg + d.3b$Even.Total*tb.even.avg + d.3b$Likely.Total*tb.likely.avg + d.3b$Routine.Total*tb.routine.avg
d.3b$PAA = d.3b$Remote.Made + d.3b$Unlikely.Made + d.3b$Even.Made + d.3b$Likely.Made + d.3b$Routine.Made - d.3b$expected.plays


#Shortstop
d.ss = subset(d,Pos=="SS")
ss.remote.avg = sum(d.ss$Remote.Made)/sum(d.ss$Remote.Total)
ss.unlikely.avg = sum(d.ss$Unlikely.Made)/sum(d.ss$Unlikely.Total)
ss.even.avg = sum(d.ss$Even.Made)/sum(d.ss$Even.Total)
ss.likely.avg = sum(d.ss$Likely.Made)/sum(d.ss$Likely.Total)
ss.routine.avg = sum(d.ss$Routine.Made)/sum(d.ss$Routine.Total)
d.ss$expected.plays = d.ss$Remote.Total*ss.remote.avg + d.ss$Unlikely.Total*ss.unlikely.avg + d.ss$Even.Total*ss.even.avg + d.ss$Likely.Total*ss.likely.avg + d.ss$Routine.Total*ss.routine.avg
d.ss$PAA = d.ss$Remote.Made + d.ss$Unlikely.Made + d.ss$Even.Made + d.ss$Likely.Made + d.ss$Routine.Made - d.ss$expected.plays

#Left Field
d.lf = subset(d,Pos=="LF")
lf.remote.avg = sum(d.lf$Remote.Made)/sum(d.lf$Remote.Total)
lf.unlikely.avg = sum(d.lf$Unlikely.Made)/sum(d.lf$Unlikely.Total)
lf.even.avg = sum(d.lf$Even.Made)/sum(d.lf$Even.Total)
lf.likely.avg = sum(d.lf$Likely.Made)/sum(d.lf$Likely.Total)
lf.routine.avg = sum(d.lf$Routine.Made)/sum(d.lf$Routine.Total)
d.lf$expected.plays = d.lf$Remote.Total*lf.remote.avg + d.lf$Unlikely.Total*lf.unlikely.avg + d.lf$Even.Total*lf.even.avg + d.lf$Likely.Total*lf.likely.avg + d.lf$Routine.Total*lf.routine.avg
d.lf$PAA = d.lf$Remote.Made + d.lf$Unlikely.Made + d.lf$Even.Made + d.lf$Likely.Made + d.lf$Routine.Made - d.lf$expected.plays

#Center Field
d.cf = subset(d,Pos=="CF")
cf.remote.avg = sum(d.cf$Remote.Made)/sum(d.cf$Remote.Total)
cf.unlikely.avg = sum(d.cf$Unlikely.Made)/sum(d.cf$Unlikely.Total)
cf.even.avg = sum(d.cf$Even.Made)/sum(d.cf$Even.Total)
cf.likely.avg = sum(d.cf$Likely.Made)/sum(d.cf$Likely.Total)
cf.routine.avg = sum(d.cf$Routine.Made)/sum(d.cf$Routine.Total)
d.cf$expected.plays = d.cf$Remote.Total*cf.remote.avg + d.cf$Unlikely.Total*cf.unlikely.avg + d.cf$Even.Total*cf.even.avg + d.cf$Likely.Total*cf.likely.avg + d.cf$Routine.Total*cf.routine.avg
d.cf$PAA = d.cf$Remote.Made + d.cf$Unlikely.Made + d.cf$Even.Made + d.cf$Likely.Made + d.cf$Routine.Made - d.cf$expected.plays


#Right Field
d.rf = subset(d,Pos=="RF")
rf.remote.avg = sum(d.rf$Remote.Made)/sum(d.rf$Remote.Total)
rf.unlikely.avg = sum(d.rf$Unlikely.Made)/sum(d.rf$Unlikely.Total)
rf.even.avg = sum(d.rf$Even.Made)/sum(d.rf$Even.Total)
rf.likely.avg = sum(d.rf$Likely.Made)/sum(d.rf$Likely.Total)
rf.routine.avg = sum(d.rf$Routine.Made)/sum(d.rf$Routine.Total)
d.rf$expected.plays = d.rf$Remote.Total*rf.remote.avg + d.rf$Unlikely.Total*rf.unlikely.avg + d.rf$Even.Total*rf.even.avg + d.rf$Likely.Total*rf.likely.avg + d.rf$Routine.Total*rf.routine.avg
d.rf$PAA = d.rf$Remote.Made + d.rf$Unlikely.Made + d.rf$Even.Made + d.rf$Likely.Made + d.rf$Routine.Made - d.rf$expected.plays

defense <- rbind(d.p,d.c,d.1b,d.2b,d.3b,d.ss,d.lf,d.cf,d.rf)

defense <- defense[order(defense$PAA),]
defense$PAA = round(defense$PAA,digits=2)
defense$Easy.Play.Pct = round(defense$Easy.Plays/defense$Total.Plays,digits=2)
defense$PAA.per.150.Inn = round(defense$PAA/defense$Inn*150,digits=2)
defense$PAA.per.100.Plays = round(defense$PAA/defense$Total.Plays*100,digits=2)

#Compare outfield positions across Left right and Center
d.of = subset(d,Pos=="RF" | Pos=="CF" | Pos=="LF")
of.remote.avg = sum(d.of$Remote.Made)/sum(d.of$Remote.Total)
of.unlikely.avg = sum(d.of$Unlikely.Made)/sum(d.of$Unlikely.Total)
of.even.avg = sum(d.of$Even.Made)/sum(d.of$Even.Total)
of.likely.avg = sum(d.of$Likely.Made)/sum(d.of$Likely.Total)
of.routine.avg = sum(d.of$Routine.Made)/sum(d.of$Routine.Total)
d.of$expected.plays = d.of$Remote.Total*of.remote.avg + d.of$Unlikely.Total*of.unlikely.avg + d.of$Even.Total*of.even.avg + d.of$Likely.Total*of.likely.avg + d.of$Routine.Total*of.routine.avg
d.of$PAA = d.of$Remote.Made + d.of$Unlikely.Made + d.of$Even.Made + d.of$Likely.Made + d.of$Routine.Made - d.of$expected.plays

d.of <- d.of[order(d.of$PAA),]

d.of$PAA = round(d.of$PAA,digits=2)
d.of$Easy.Play.Pct = round(d.of$Easy.Plays/d.of$Total.Plays,digits=2)
d.of$PAA.per.150 = round(d.of$PAA/d.of$Inn*150,digits=2)
d.of$PAA.per.100.Plays = round(d.of$PAA/d.of$Total.Plays*100,digits=2)

