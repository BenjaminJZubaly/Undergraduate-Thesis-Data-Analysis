## This script is to create APA style plots, graphs, and tables for the manuscript

#Installing the apaTables package to create apa style tables
install.packages("apaTables")

# Loading the apaTables package
library(apaTables)

# Creating APA style tables for the correlational analyses
  # For numerical outcome variables
apa.cor.table(data[c("LingObf", "CertSent", "Refs")],
              filename = "num_outcomes_corr_table.doc",
              table.number = 1)

  # For subcomponents of abstraction
apa.cor.table(data[,c("article", "prep", "quantity")],
              filename = "abs_subcomponents_corr_table.doc",
              table.number = 2)
  # For sucomponents of obfuscation
apa.cor.table(data[,c("cause", "abstraction", "jargon", "emo_pos", "flesch_re")],
              filename = "obf_subcomponents_corr_table.doc",
              table.number = 3)