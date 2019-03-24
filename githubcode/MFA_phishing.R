#----------------------
library(FactoMineR)
library("factoextra")

#Reading the data into factors as all variables are factors
data <- read.csv("dataset.csv",colClasses = "factor")

#Removing the Unique value column : Index Id 
data <- data[,-1]

#group : group number of variables into one
#for eg  first 2 col are grouped into 1, then next 3 col are grouped into 1 and so on
#type indicates 
#s: scaled , c: continous, n: qualitiative, f: frequency
#number of dimensions 

res.mfa <- MFA(data[,-31],group = c(12,6,5,7), ncp = 4,
               type = rep("n",4),
               name.group = c("AddressBarBased","AbnormalBased","HTML_JAVAScriptBased",
                              "DomainBased"),
               #num.group.sup = c(1, 4),
               graph = TRUE
               )
summary(res.mfa)

library("factoextra")
eig.val <- get_eigenvalue(res.mfa)
print(eig.val)
fviz_screeplot(res.mfa)

fviz_mfa_var(res.mfa, "group")

# Contribution to the first dimension
fviz_contrib(res.mfa, "group", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.mfa, "group", axes = 2)


fviz_mfa_var(res.mfa, "group", palette = "jco", 
             col.var.sup = "violet", repel = TRUE)

fviz_mfa_var(res.mfa, "group", palette = "jco", 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"), legend = "bottom")

fviz_mfa_ind(res.mfa, 
             habillage = "Label", # color by groups 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE) # Avoid text overlapping) 

             