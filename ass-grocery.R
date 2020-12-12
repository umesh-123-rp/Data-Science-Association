#Association Rule
# Preparation of rules for Grocery Dataset:
#1) Try different values of support and confidence. Observe the change in number of rules
# for different support and confidence values
#2) Change the minimum length in apriori algorithm
#3) Visualise the obtained rules using different plots
install.packages("arules")
library(arules)
# Grocery dataset is loaded in the form of transaction data
groceries <- read.transactions("C:\\Users\\IN102385\\OneDrive - Super-Max Personal Care Pvt. Ltd\\Assignment - Data Science -UL\\Association Rule-R\\groceries.csv",sep=",")                                  
View(groceries)
str(groceries)
# It is a dataset of 9835 transactions with 169 items
# We can check the items as:
head(groceries@itemInfo,n=12)
# We can check the transactions as 
inspect(groceries)
summary(groceries)
# The summary shows the matrix(169X9835) is with density 2.6%,most frequent items 
# and length of the different transactions; minimum length=1 & Maximum length =32
# Now we can check high frequent items in the transactions :
data.frame(head(sort(itemFrequency(groceries,type="absolute"),decreasing=TRUE),10))
itemFrequencyPlot(groceries,type="relative",topN = 20)
# The most frequent items are Whole Milk, Other Vegetables etc.
# We can generate rules by default without giving any support and confidence value
rules<-apriori(groceries)
# By deault, we get support value as 0.1 and confidence value as 0.8
# But there is no rule with the above support and confidence value
# To get a stronger rules, We can keep confidence value as 0.8 and reduce the support value to 0.001
rules <- apriori(groceries, parameter = list(support=0.001, confidence = 0.8))
# We get 410 rules with min length=1 to max length=10.
# We can check the rules by confidence as given below:
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])
# We also can check the rule by lift ratio as given below
rules<-sort(rules, by="lift", decreasing=TRUE)
inspect(rules[1:5])
# We can further reduce the number of strong rules by changing the size of length
rules <- apriori(groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))
# By restricting the size upto max =3, We get lower number of rules i.e. 29
# They are as follows sorted by confidence and Lift ratio
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])
rules<-sort(rules, by="lift", decreasing=TRUE)
inspect(rules[1:5])
# We can also observe the change number of rules by fixing the minimum of length 
# by keeping support and confidence constant
rules <- apriori(groceries, parameter = list(supp = 0.001, conf = 0.8,minlen=2))
# There are again 410 rules if we change min length up to 3 and then reduces to 
# 381 rules if we set min length at 4.
# Since the item set will have many subsets at different length. There will be 
# some subsets which will be subset of another subset. There will be certain redundancies
# There will be certain redundancies which are increasing the number of rules unncecessarily
# We need to identify the redundacies and remove them from the rules

# Redundancies
redundant_rules<-is.redundant(rules)
redundant_rules
summary(redundant_rules)
rules<- rules[!redundant_rules]
rules
# There are 18 redundant rules found and removed from 410 rules.
# Now, Total no.of rules are 410-18=392 rules
# Target Items
# We can make different rules by taking different targets as per the market reqt.
# Some of the examples are given below:
write(rules, file = "grocery.csv",sep = ",", quote = TRUE, row.names = FALSE)
rules_df<-as(rules,"data.frame")
# Visulaization
install.packages("arulesViz")
library(arulesViz)
plot(rules,method="graph",interactive=FALSE,shading=NA)
# 100 rules have been plotted with support values from 0.001 to 0.003
plot(rules,method="scatterplot",jitter=0)
# 392 rules have been plotted with different values of support and confidence
plot(rules,method="grouped")
# 392 rules have been plotted with default lhs and different rhs
# Different rules have been observed in plots 

# Target Items
# We can make different rules by taking different targets as per the market reqt.
# Some of the examples are given below:
rules_wmilk<-apriori(data=groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules_wmilk
rules_wmilk<-sort(rules_wmilk, decreasing=TRUE,by="confidence")
inspect(rules_wmilk[1:5])
rules_wmilk<-sort(rules_wmilk, decreasing=TRUE,by="lift")
inspect(rules_wmilk[1:5])
# There are 3765 rules
# We can also plot all kind of graphs with Rhs Whole Milk
rules_wmilk_df <- as(rules_wmilk, "data.frame")
str(rules_wmilk_df)
plot(rules_wmilk,method="grouped")
plot(rules_wmilk,method="scatterplot")
plot(rules_wmilk,method="graph")
# Target with lhs =Whole milk
rules_wm<-apriori(groceries, parameter=list(supp=0.001,conf = 0.1,minlen=1), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules_wm
rules_wm<-sort(rules_wm, decreasing=TRUE,by="confidence")
inspect(rules_wm[1:5])
rules_wm_df <- as(rules_wm, "data.frame")
plot(rules_wm,method="grouped")
plot(rules_wm,method="scatterplot")
plot(rules_wm,method="graph")
# There are 24 rules with lhs whole milk and different graphs can be plotted.

# Conclusion:
# Based on the market requirement, We can adjust support, confidence, min length
# and max lentgth of the rules.
# We can take out the redundant rules to make them relevant
# We should chose rhs and lhs depending on the market requirement to optimise the rules
# Plots help us to visualise the rules
 


