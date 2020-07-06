library(arules)
install.packages("arulesViz")
library("arulesViz")


grocery.df<-read.csv("grocery.csv")
binary<-as(split(grocery.df[,2], grocery.df[,1]), "transactions")
inspect(binary)

grocery.rules<-apriori(binary, parameter = list(support = 0.1, confidence = 0.1))

inspect(grocery.rules)
summary(grocery.rules)

#-----------------------------------------------------------------------------------------------------------------------

cosmetics.df<-read.csv("Cosmetics.csv")
#remove column 1 since is just the transaction number!
cosmetics.mat <- as.matrix(cosmetics.df)[,-1]

cosmetics.trans <- as(cosmetics.mat, "transactions")

cosmetics.rules<- apriori(cosmetics.trans, parameter = list(supp= 0.1, conf = 0.5, target
                                                            = "rules"))

#inspecting those rules where rhs is item Pen.
penrules<-apriori(binary, parameter = list(support = 0.1, confidence = 0.1), appearance =
                    list(default = "lhs", rhs="Pen"))

Cg0.05rules<-grocery.rules[quality(grocery.rules)$confidence>0.05]
inspect(Cg0.05rules)
Cg0.5rules<-grocery.rules[quality(grocery.rules)$confidence>0.5]
#Get top 10 lift rules
Top.10.lift.Rules<-sort(grocery.rules, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(Top.10.lift.Rules, 10))



#graph items frequency
itemFrequencyPlot(binary)
#scatter plot with different color
library(colorspace)
plot(grocery.rules, control = list(col=sequential_hcl(100)))
plot(grocery.rules, col=sequential_hcl(100))
plot(grocery.rules, method = "two-key plot")
#creating interactive plot
plot(grocery.rules, method = "graph", engine = "htmlwidget")

#--------------------------------------------------------------------------------------------------------------------

write.csv(inspect(cosmetics.rules), file = ("Cosmetics.csv")


