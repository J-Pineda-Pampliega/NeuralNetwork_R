# Neural networks using library (nnet)

library(nnet)
library(NeuralNetTools)
library(ggplot2)

# Using Phylum as input and the stage of cancer as output

table <- read.table("table_NN.txt", row.names = 1, header=TRUE)
table$Stage = as.factor(table$Stage)

# Training set

table_sort <-table[order(table$Stage),]
set.seed(1000)
training_table <-c(sample(1:6,2),sample(7:19,3),sample(20:32,3))

# Creation of Neural Network
set.seed(1000)
NN_table = nnet(Stage ~ Bacteroidetes + Firmicutes +
                   Fusobacteria + Proteobacteria + Tenericutes +
                   Chloroflexi + Marinimicrobia.SAR406clade. +
                   Elusimicrobia + Cyanobacteria + Actinobacteria +
                   Patescibacteria + Epsilonbacteraeota + Verrucomicrobia +
                   Planctomycetes + Acidobacteria + Halanaerobiaeota +
                   Spirochaetes + Atribacteria + Latescibacteria +
                   LCP.89 + Gemmatimonadetes + Kiritimatiellaeota +
                   Omnitrophicaeota + PAUC34f + Deinococcus.Thermus +
                   Dependentiae + Fibrobacteres + Synergistetes +
                   Margulisbacteria + Nitrospinae + BRC1 +
                   Schekmanbacteria + Deferribacteres + Nitrospinae +
                   Aquificae + TA06 + Cloacimonetes +
                   Lentisphaerae + WPS.2 + Chlamydiae +
                   Armatimonadetes + Hydrothermae + FBP +
                   WS4 + Desantisbacteria + Zixibacteria +
                   Dadabacteria + Coprothermobacteraeota +
                   Calditrichaeota + CK.2C2.2 +
                   WS2 + Thermotogae + Aerophobetes +
                   Aegiribacteria + Caldiserica + Fervidibacteria +
                   GN01 + Hydrogenedentes + Rokubacteria +
                   Modulibacteria + Acetothermia + Entotheonellaeota +
                   WS1 + Poribacteria + WOR.1 + AncK6 +
                   FCPU426 + BHI80.139 + MAT.CR.M4.B07,data=table_sort,subset = training_table, size=2, decay=1.0e-5, maxit=1000)

# Evaluation of Neural Network

mc_table <-table(table_sort$Stage[-training_table],predict(NN_table,table_sort[-training_table,],type="class"))
predict(NN_table, Stage = table_sort$Stage[-training_table], type="class")
actual_table <- table_sort$Stage[-training_table]
preds_table <- predict(NN_table, table_sort[-training_table, ], type="class")
mc_table <- table(actual_table, preds_table)
print(mc_table)

correct = mc_table[1,1] + mc_table[2,2] + mc_table[3,3]
accurancy = (correct / 24) * 100
message("Accurancy of the Neural Network: ", round(accurancy)," %")

olden(NN_table) + theme(axis.text.x = element_text(angle = 90))

neuralweights(NN_table)


