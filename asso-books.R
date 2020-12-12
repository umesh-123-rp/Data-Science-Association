# Association Rules
# Preparation of rules for Book data set 
# Loading books dataset
books <-read.csv("C:\\Users\\IN102385\\OneDrive - Super-Max Personal Care Pvt. Ltd\\Assignment - Data Science -UL\\Association Rule-R\\book (1).csv")
str(books)
View(books)
install.packages("arules")
library(arules)
# Rules by default can be found out from the algorithm without assigning 
# any support and confidence
rules<-apriori(as.matrix(books))
# By default, we get support as 0.1 and confidence as 0.8,
# But we get 7 rules only. We can get more number of rules by 
# changing support and confidence values as follows:
rules <- apriori(as.matrix(books),parameter=list(support=0.02, confidence =0.8 ,minlen=2))
inspect(head(sort(rules, by = "lift")))  
head(quality(rules))
# We get 223 rules. If we reduce confidence value from 0.8 to 0.5, We observe the following:
rules<- apriori(as.matrix(books),parameter=list(support=0.02, confidence =0.5 ,minlen=2))
inspect(head(sort(rules, by = "lift")))  
head(quality(rules))
# No. of rules have increased to 672
# Redundancies
# We can check if there are any redundancies in 672 rules
rules_red<-is.redundant(rules)
summary(rules_red)
rules<-rules[!rules_red]
rules
# There are 320 redundancies which were eliminated from 672 rules
# Total number of strong rules are 352

# Visualisation 
install.packages("arulesViz", dependencies = TRUE)
library(arulesViz)
plot(rules,method = "scatterplot")
plot(rules, method = "grouped")
plot(rules,method = "graph")
# 352 rules are plotted in 3 different graphs. We can check the rules with minlength of 5
rules<- apriori(as.matrix(books),parameter=list(support=0.02, confidence =0.5 ,minlen=5))
# Provided 186 rules with 2 % Support, 50 % Confidence and Minimum to purchase 5 books 

# Conclusion
# The Art books are being sold at a larger extent along with other Cook, art, geo, child books
# Cook books are also being sold at a larger extent along with other chld, art, geo, Doit books.
# Depending on client requirement, different rules can be obtained 
# by changing support and confidence value.
# We also can change the number of rules by changing number of length of items
# We can have optimum number of rules based on market/customer requirements.

