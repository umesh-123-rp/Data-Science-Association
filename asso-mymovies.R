# Association Rules
# Preparation of Rules with My_Movies data set :
install.packages("arules")
library(arules)
# Loading My movies dataset
mymovies <-read.csv("C:\\Users\\IN102385\\OneDrive - Super-Max Personal Care Pvt. Ltd\\Assignment - Data Science -UL\\Association Rule-R\\my_movies.csv")
str(mymovies)
# We can take out first 5 columns as dummy variables are available from col 6 to col 15
# Finding support and confidence values by default
rules<- apriori(as.matrix(mymovies[6:15]))
# We observe support as 0.1 and confidence as 0.8 by default
# There are 77 rules. We can increase number of rules by reducing support value as follows:
rules <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=2)))
# We observe that the number of rules are 77 even after changing support and confidence
inspect(head(sort(rules, by = "lift")))  
head(quality(rules))
# Redundancies
rules_redundant<-is.redundant(rules)
summary(rules_redundant)
rules<-rules[!rules_redundant]
rules
# After removal of redundancies, the strong rules left out is 28
install.packages("arulesViz")
library(arulesViz)
plot(rules,method = "scatterplot")
plot(rules, method = "grouped")
plot(rules,method = "graph",jitter=0)

# Provided the rules with 20% Support, 50 % Confidence and watched a minimum of 2 movies
## set of 77 rules
# It looks like most of them have watched Lord of the rings movies along with Gladiator and Greenville
# Also most of them have watched Gladiator, Sixth sense along with Patrioit
# Patriot ,Braveheart and other three items along with Gladiator.
