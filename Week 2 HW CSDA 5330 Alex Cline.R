library(arulesViz)
library(plotly)
library(arules)

#CSV
SAMCLUB.df<-read.csv("SAMCLUB.csv")
#3a summary of results
summary(SAMCLUB.df)

SAMCLUB.binary<-as(split(SAMCLUB.df[,2], SAMCLUB.df[,1]), "transactions")

inspect(SAMCLUB.binary)

#SAMCLUB rules
SAMCLUB.rules<- apriori(SAMCLUB.binary, parameter = list(supp= 0.001, conf = 0.1, target
                                                            = "rules"))
#3b static and interactive plots for SAMCLUB rules supp 0.001 conf 0.1
library(yaml)
library(colorspace)
plot(SAMCLUB.rules, control = list(col=sequential_hcl(100)))
plot(SAMCLUB.rules, col=sequential_hcl(100))
plot(SAMCLUB.rules, method = "two-key plot")
#interactive plot
plot(SAMCLUB.rules, method = "graph", engine = "htmlwidget")


#3c orange juice filter
SAMCLUB.filter <- SAMCLUB.df[SAMCLUB.df$Primary_Description == "ORANGE JUICE",]
SAMCLUB.filter

#orange juice rules
ORANGEJUICErules<-apriori(SAMCLUB.binary, parameter = list(support = 0.001, confidence = 0.1), appearance =
                            list(default = "lhs", rhs="ORANGE JUICE"))
#3d orange juice visualization
plot(ORANGEJUICErules, method = "two-key plot")
plot(ORANGEJUICErules, method = "graph", engine = "htmlwidget")


#3e and 3f conf>0.5rules and static and interactive plots
Cg0.5rules<-SAMCLUB.rules[quality(SAMCLUB.rules)$confidence>0.5]
plot(Cg0.5rules)
plot(Cg0.5rules, method = "two-key plot")
plot(Cg0.5rules, method = "graph", engine = "htmlwidget")





write.table(inspect(SAMCLUB.rules), "SAMCLUB.csv", sep="\t")
