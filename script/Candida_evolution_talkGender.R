library("gender")
library("here")
library("tidyverse")

# Candida conference talks
conf <- read.csv(here("data_in", "CandidaConfSpeakers_Ed.csv"), as.is=TRUE)

d <- gender(str_trim(conf$Name), method = "genderize")

table(d$gender)
#female   male
#146    160

confGender <- c()
for(i in 1:nrow(conf)){
  confGender[i] <- subset(d, name == str_trim(conf[i, "Name"]))$gender[1]
}

conf$gender <- confGender
table(conf$gender,conf$Year, conf$Type)
#Basically 50:50 every year for all conference types :-)

plot(table(conf$Year, conf$Type, conf$gender))

t <- table(conf$Year, conf$gender)


# our cited authors
authors <- read.csv(here("data_in", "Candida_evolution_authorGender.csv"), as.is=TRUE)

first <- gender(str_trim(authors$First), method="genderize")
last <- gender(str_trim(authors$Last), method="genderize")

table(first$gender)
# female   male
# 40     42
table(last$gender)
#female   male
#33     54

fisher.test(matrix(c(42, 33, 42, 54), 2))

first_unique <- gender(unique(str_trim(authors$First)))
last_unique <- gender(unique(str_trim(authors$Last)))

table(first_unique$gender)
#female   male
#30     26

table(last_unique$gender)
#female   male
#19     29

t <- paste(first$gender, last$gender, sep="-")
table(t)
