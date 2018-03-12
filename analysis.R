# Bethany Autumn semester 2017 thesis analysis

# load libraries
library(ggplot2)
theme_set(theme_bw())
library(ordinal)
library(plyr)
library(reshape2)

# load the data and helper functions
d <- read.csv("data.csv")
source('helpers.R')

#delete extraneous columns
d <- d[-1,]
d <- d[-1,]

# convert the data into long format
idv <- c("ResponseId", "A", "i", "ii", "iii", "iv", "iv_1_TEXT")
msv <- c("X1a.nd_1", "X1b.nd_1", "X1a.rd_1", "X1b.rd_1", "X2a.nd_1", "X2b.nd_1", "X2a.rd_1", "X2b.rd_1", "X3a.nd_1", "X3b.nd_1", "X3a.rd_1", "X3b.rd_1", "X4a.nd_1", "X4a.rd_1", "X4b.nd_1", "X4b.rd_1", "X5a.nd_1", "X5a.rd_1", "X5b.nd_1", "X5b.rd_1", "X6a.nd_1", "X6b.nd_1", "X6a.rd_1", "X6b.rd_1", "X7a.nd_1", "X7b.nd_1", "X7a.rd_1", "X7b.rd_1", "X8a.nd_1", "X8a.rd_1", "X8b.nd_1", "X8b.rd_1")
d_long <- melt(d, id.vars=idv, measure.vars=msv, variable.name="label", value.name = "acceptability")

#getting rid of all the blanks
df <- d_long[!(is.na(d_long$acceptability) | d_long$acceptability==""), ]

#getting rid of all the non-consents
df <- df[!(is.na(df$A) | df$A==""), ]
df <- droplevels(df)

#will need this for boxplots later
df$acceptability <- as.numeric(df$acceptability)

#make the levels for ResponseId not cancer
levels(df$ResponseId) <- c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15", "R16", "R17", "R18", "R19", "R20", "R21", "R22", "R23", "R24", "R25", "R26", "R27", "R28", "R29", "R30", "R31", "R32", "R33", "R34", "R35", "R36", "R37", "R38")

#sort the regions
df$region <- df$iii
levels(df$region) <- c("Other", "North West", "South West", "South East", "East", "North West", "West Midlands", "North West", "Other", "South West", "South East", "East", "North West", "East", "South East", "North West", "North West", "Yorkshire and the Humber", "North East", "North East", "Other", "North West", "Other", "Yorkshire and the Humber", "South West", "South East", "South East", "Yorkshire and the Humber", "Yorkshire and the Humber", "South East", "West Midlands", "West Midlands", "Yorkshire and the Humber", "Yorkshire and the Humber")
droplevels(df$region)

#make the column names better
colnames(df) <- c("ResponseId", "Consent", "Gender", "Age", "Location", "NonEnglishEver", "Language", "Q", "Acceptability", "Region")

#make it so that I can sort the items by sentence & context type (& item) without killing myself
df$Type <- df$Q
levels(df$Type) <- c("AgreeND", "DisagreeND", "AgreeRD", "DisagreeRD", "AgreeND", "DisagreeND", "AgreeRD", "DisagreeRD", "AgreeND", "DisagreeND", "AgreeRD", "DisagreeRD", "AgreeND", "AgreeRD", "DisagreeND", "DisagreeRD", "AgreeND", "AgreeRD", "DisagreeND", "DisagreeRD", "AgreeND", "DisagreeND", "AgreeRD", "DisagreeRD", "AgreeND", "DisagreeND", "AgreeRD", "DisagreeRD", "AgreeND", "AgreeRD", "DisagreeND", "DisagreeRD")
droplevels(df$Type)
df$Context <- df$Q
levels(df$Context) <- c("Agree", "Disagree", "Agree", "Disagree", "Agree", "Disagree", "Agree", "Disagree", "Agree", "Disagree", "Agree", "Disagree", "Agree", "Agree", "Disagree", "Disagree", "Agree", "Agree", "Disagree", "Disagree", "Agree", "Disagree", "Agree", "Disagree", "Agree", "Disagree", "Agree", "Disagree", "Agree", "Agree", "Disagree", "Disagree")
droplevels(df$Context)
df$Utterance <- df$Q
levels(df$Utterance) <- c("Non-Dislocated", "Non-Dislocated", "Right Dislocated", "Right Dislocated", "Non-Dislocated", "Non-Dislocated", "Right Dislocated", "Right Dislocated", "Non-Dislocated", "Non-Dislocated", "Right Dislocated", "Right Dislocated", "Non-Dislocated", "Right Dislocated", "Non-Dislocated", "Right Dislocated", "Non-Dislocated", "Right Dislocated", "Non-Dislocated", "Right Dislocated", "Non-Dislocated", "Non-Dislocated",  "Right Dislocated", "Right Dislocated", "Non-Dislocated", "Non-Dislocated", "Right Dislocated", "Right Dislocated", "Non-Dislocated", "Right Dislocated", "Non-Dislocated", "Right Dislocated")
droplevels(df$Utterance)
df$Item <- df$Q
levels(df$Item) <- c("1", "1", "1", "1", "2", "2", "2", "2", "3", "3", "3", "3", "4", "4", "4", "4", "5", "5", "5", "5", "6", "6", "6", "6", "7", "7", "7", "7", "8", "8", "8", "8")
droplevels(df$Item)

#Graph 'Em By Item

ggplot(df, aes(x=Item, y=Acceptability)) +
  geom_boxplot(width=0.2, position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2, position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  facet_wrap(~Type, nrow=4) +
  theme(strip.text.y=element_text(angle=0)) +
 ylab("Acceptability Rating") +
  xlab("Item")
ggsave(f="OFFICIAL_per_item_2.pdf",height=10,width=10)

#Graph by item but only AgreeND
simplesent <- subset(df, Type=="AgreeND")
ggplot(simplesent, aes(x=Item, y=Acceptability)) +
 geom_boxplot(width=0.2, position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2, position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  facet_wrap(~Type, nrow=1) +
  theme(strip.text.y=element_text(angle=0)) +
  ylab("Acceptability Rating") +
  xlab("Item")
ggsave(f="OFFICIAL_per_item_aND.pdf", height=3.5, width=10)

#Get Rid of 5 & 6 because AgreeND means were below 4
db <- df[!(df$Item=="5" | df$Item=="6"), ]

#Plot 'Em by Type
ggplot(db, aes(x=Context, y=Acceptability)) +
  geom_boxplot(width=0.2, position=position_dodge(.9)) +
 stat_summary(fun.y=mean, geom="point", color="blue", size=2, position=position_dodge(.9)) +
 theme(text = element_text(size=12)) +
  theme(strip.text.y=element_text(angle=0)) +
  facet_wrap(~Utterance, nrow=1) +
  ylab("Acceptability Rating") +
  xlab("Variant")
ggsave(f="OFFICIAL_per_type.pdf",height=3.5,width=8)

#By Item Again
ggplot(db, aes(x=Type, y=Acceptability)) +
  geom_boxplot(width=0.2, position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2, position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  facet_wrap(~Item, nrow=2) +
  theme(strip.text.y=element_text(angle=0)) +
  ylab("Acceptability Rating") +
  xlab("Variant")
ggsave(f="OFFICIAL_per_item_1.pdf",height=5,width=10)

#Graph 'Em by Participant!
#First Make the Participants sorted in order 
mean_acc <- aggregate(Acceptability~ResponseId, data=db, FUN="mean")
mean_acc$YMin <- mean_acc$Acceptability - aggregate(Acceptability~ResponseId, data=db, FUN="ci.low")$Acceptability
mean_acc$YMax <- mean_acc$Acceptability + aggregate(Acceptability~ResponseId, data=db, FUN="ci.high")$Acceptability
db$Participant <- factor(db$ResponseId, levels=mean_acc[order(mean_acc$Acceptability), "ResponseId"])

#make a subset for informational purposes & order based on it
simpleton <- subset(db, Type=="AgreeND")
mean_agg <- aggregate(Acceptability~ResponseId, data=simpleton, FUN="mean")
mean_agg$YMin <- mean_agg$Acceptability - aggregate(Acceptability~ResponseId, data=simpleton, FUN="ci.low")$Acceptability
mean_agg$YMax <- mean_agg$Acceptability + aggregate(Acceptability~ResponseId, data=simpleton, FUN="ci.high")$Acceptability
simpleton$Participant2 <- factor(simpleton$ResponseId, levels=mean_agg[order(mean_agg$Acceptability), "ResponseId"])

ggplot(simpleton, aes(x=Participant2, y=Acceptability)) +
  geom_boxplot(width=0.2, position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2, position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  theme(axis.text.x = element_text(angle=60, vjust=0.5, hjust=0.4)) +
  ylab("Acceptability Rating") +
  xlab("Participant")
ggsave(f="OFFICIAL_per_participant_1.pdf",height=3.5,width=15)

ggplot(db, aes(x=Type, y=Acceptability)) +
  geom_boxplot(width=0.2, position=position_dodge(.9)) +
 stat_summary(fun.y=mean, geom="point", color="blue", size=2, position=position_dodge(.9)) +
 theme(text = element_text(size=12)) +
  theme(strip.text.y=element_text(angle=0)) +
  facet_wrap(~ResponseId, nrow=7) +
  ylab("Acceptability Rating") +
  xlab("Variant")
ggsave(f="OFFICIAL_per_participant_2.pdf",height=12,width=20)

ggplot(db, aes(x=Participant, y=Acceptability)) +
  geom_boxplot(width=0.2, position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2, position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  theme(axis.text.x = element_text(angle=60, vjust=0.5, hjust=0.4)) +
  ylab("Acceptability Rating") +
  xlab("Participant")
ggsave(f="OFFICIAL_per_participant_3.pdf",height=3.5,width=15)

#Graph 'Em by Location
ggplot(db, aes(x=Type, y=Acceptability)) +
 geom_boxplot(width=0.2, position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2, position=position_dodge(.9)) +
 theme(text = element_text(size=12)) +
theme(strip.text.y=element_text(angle=0)) +
  facet_wrap(~Region, nrow=2) +
 ylab("Acceptability Rating") +
  xlab("Variant")
ggsave(f="OFFICIAL_per_region_1.pdf",height=5,width=12.5)

ggplot(db, aes(x=Region, y=Acceptability)) +
  geom_boxplot(width=0.2, position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2, position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  theme(strip.text.y=element_text(angle=0)) +
 facet_wrap(~Type, nrow=2) +
  ylab("Acceptability Rating") +
 xlab("Region")
#ggsave(f="OFFICIAL_per_region_2.pdf",height=5,width=15)

#make the fake dataset for visual demonstration of predictions

fakepeople <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")
faketypes <- c("aRD", "aRD", "aRD", "aRD", "dRD", "dRD", "dRD", "dRD", "aND", "aND", "aND", "aND", "dND", "dND", "dND", "dND")
fakevalues <- c("4", "4", "3", "2", "3", "2", "1", "2", "5", "5", "5", "4", "5", "4", "5", "4")
fake.data <- data.frame(fakepeople, faketypes, fakevalues)
fake.data$fakevalues <- as.numeric(fake.data$fakevalues)
fake.data$fakecontext <- c("agree", "agree", "agree", "agree", "disagree", "disagree", "disagree", "disagree", "agree", "agree", "agree", "agree", "disagree", "disagree", "disagree", "disagree")
fake.data$fakeutterance <- c("RD", "RD", "RD", "RD", "RD", "RD", "RD", "RD", "ND", "ND", "ND", "ND", "ND", "ND", "ND", "ND")

revalue(fake.data$fakecontext, c("agree"="Agree", "disagree"="Disagree")) -> fake.data$fakecontext
revalue(fake.data$fakeutterance, c("RD"="Right Dislocated", "ND"="Non-Dislocated")) -> fake.data$fakeutterance

#graph the fake dataset
ggplot(fake.data, aes(x=fakecontext, y=fakevalues)) +
geom_boxplot(width=0.2, position=position_dodge(.9)) +
stat_summary(fun.y=mean, geom="point", color="blue", size=2, position=position_dodge(.9)) +
theme(text = element_text(size=12)) +
theme(strip.text.y=element_text(angle=0)) +
facet_wrap(~fakeutterance, nrow=1) +
ylab("Acceptability Rating") +
xlab("Variant")
ggsave(f="OFFICIAL_fake_prediction.pdf",height=3.5,width=8)