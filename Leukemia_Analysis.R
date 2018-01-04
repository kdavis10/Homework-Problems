# KDavis - Linear Algebra DRAFT 
# 15 SEP 2017 

# Need to figure out the key things that predict each of the 3 types of Leukemia
# Need to help identify an erroneously labled sample

load('/Users/Kimbe/OneDrive/Documents/MSA/R/HW1/LeukError.Rdata')
leuk.label = leuk[,5001] 
newleuk <- lapply(leuk, as.double)
leuk.df <- data.frame(newleuk)

# Some preliminary analysis with PCA:

pcaleuk = prcomp(leuk.df[1:5000], scale = F) 
plot(pcaleuk)
pcaleuk.df = data.frame(pcaleuk$x)
row.names(pcaleuk$x[,1:3]) = row.names(leuk)
row.names(pcaleuk$x[,1:3])

# Screeplot
plot(pcaleuk$sdev^2)


# Plot of the 1833 dimensional data projected onto a plane
install.packages("vegan")
library(vegan)

samples = row.names(leuk.label)

plot(pcaleuk$x[,1:2], col=c("red", "blue", "green")[leuk.label], pch= 10 )
text(pcaleuk$x[,1:2], labels=samples , pos = 4)
ordiellipse(pcaleuk$x[,1:2], groups = leuk.label, col="black", conf=0.85)

legend(x='topleft', c('Ham','Spam'), pch='o', col=c('red','blue'), pt.cex=1)

# Scatter plot of the 3d version of the 5000 variables 

library(rgl)
library(car)

#3d model of the points - labels are only the id numbers in the list (corresponds to the row names)
scatter3d(pcaleuk$x [,'PC1'],pcaleuk$x [, 'PC2'], pcaleuk$x [,'PC3'],
          point.col=c("magenta", "cyan", "black")[leuk.label], surface=F, sphere.size = 0.5, 
          labels.id = row.names(leuk), id.n = 38)
legend3d("topright", legend = levels(leuk.label), pch = 16, col = c("magenta", "cyan", "black"), 
         cex=1, inset=c(0.02))

#3d model of the row names as points - no axes 
plot3d(pcaleuk$x[,1],pcaleuk$x[,2], pcaleuk$x[,3], type="n")
points3d(pcaleuk$x[,1],pcaleuk$x[,2], pcaleuk$x[,3])
text3d(pcaleuk$x[,1],pcaleuk$x[,2], pcaleuk$x[,3], text= row.names(leuk), 
       col=c("magenta", "cyan", "black")[leuk.label])


identify3d(pcaleuk$x[,1],pcaleuk$x[,2], pcaleuk$x[,3], labels=row.names(pcaleuk.df))

with(leuk, text3d(pcaleuk$x[,1:3], texts=rownames(leuk), col=c("magenta", "cyan", "black")[leuk.label]))


pcaleuk.df['38',]


plot3d(pcaleuk$x[,1],pcaleuk$x[,2], pcaleuk$x[,3],
          col=c("red", "blue", "green")[leuk.label], surface=F, groups=leuk.label,
          labels = row.names(pcaleuk$x), pch=16)

identify3d(pcaleuk$x[,1],pcaleuk$x[,2], pcaleuk$x[,3], labels=row.names(pcaleuk))

text(pcaleuk$x[,1],pcaleuk$x[,2], pcaleuk$x[,3], row.names(leuk))

identify3d(pcaleuk$x[,1],pcaleuk$x[,2], pcaleuk$x[,3], row.names(pcaleuk))

test = row.names(pcaleuk)

# Let's see if we can use some principal components as input to a model.
# Let k be the number of components used:

k=3
new_data = data.frame(pca$x[,1:k])
new_data = cbind(sms_raw$type, new_data)
colnames(new_data)[1]="type"

# Make a logistic regression model:

model = glm(type ~ . , family="binomial", data=new_data)
summary(model)


pred=predict(model, new_data, type="response")

c=table(pred>0.5,new_data$type) 
c
misclass=(c[1,2]+c[2,1])/5574
misclass

#' Technical note: This is not necessarily the prescribed method for modelling this problem. 
#' It is merely an illustration of the power of dimension reduction to force related observations
#' and variables close to one another. 
#' 
#' We will revisit this dataset later in the semester. Naive Bayes Classifiers tend to be well suited
#' for this type of problem, although they can be far slower to implement with new data, which can be 
#' problematic in a fast-paced solution environment.
#' 


