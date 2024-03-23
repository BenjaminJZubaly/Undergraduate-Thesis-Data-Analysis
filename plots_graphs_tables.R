## This script is to create APA style plots, graphs, and tables for the manuscript


# Correlation Tables -----------------------------------------------------------

#Installing the apaTables package to create apa style tables
install.packages("apaTables")

# Loading the apaTables package
library(apaTables)

# Creating APA style tables for the correlational analyses
  # For numerical outcome variables
apa.cor.table(data[c("LingObf", "CertSent", "Refs")],
              filename = "num_outcomes_corr_table.doc",
              table.number = 4)

  # For subcomponents of abstraction
apa.cor.table(data[,c("article", "prep", "quantity")],
              filename = "abs_subcomponents_corr_table.doc",
              table.number = 2)

  # For subcomponents of obfuscation
apa.cor.table(data[,c("cause", "abstraction", "jargon", "emo_pos", "flesch_re")],
              filename = "obf_subcomponents_corr_table.doc",
              table.number = 3)


# Plots -----------------------------------------------------------------------
# Installing packages
install.packages("tidyverse")
install.packages("jtools")
install.packages("psych")
# Loading packages
library(tidyverse)
library(jtools)
library(psych)

# Creating groups of PaperType and Fraudulent or Genuine (including the data
# without the outlier for LingObf so that we can plot that as well)
groups_pt <- group_by(data, PaperType)
groups_fg <- group_by(data, Genuine_or_Fraudulent)
groups_sm <- group_by(data, S_or_M)

# Calculating 95% confidence intervals for the groups on each outcome variable
CIs_groups_pt_ref <- summarise(groups_pt,
                           mean = mean(Refs, na.rm = TRUE),
                           sd = sd(Refs, na.rm = TRUE),
                           n = n(),
                           CI_upper = mean + 1.96*sd/sqrt(n),
                           CI_lower = mean - 1.96*sd/sqrt(n))

CIs_groups_fg_ref <- summarise(groups_fg,
                               mean = mean(Refs, na.rm = TRUE),
                               sd = sd(Refs, na.rm = TRUE),
                               n = n(),
                               CI_upper = mean + 1.96*sd/sqrt(n),
                               CI_lower = mean - 1.96*sd/sqrt(n))

CIs_groups_pt_obf <- summarise(groups_pt,
                               mean = mean(LingObf, na.rm = TRUE),
                               sd = sd(LingObf, na.rm = TRUE),
                               n = n(),
                               CI_upper = mean + 1.96*sd/sqrt(n),
                               CI_lower = mean - 1.96*sd/sqrt(n),
                               ymin = min(LingObf),
                               ymax = max(LingObf))

CIs_groups_fg_obf <- summarise(groups_fg,
                               mean = mean(LingObf, na.rm = TRUE),
                               sd = sd(LingObf, na.rm = TRUE),
                               n = n(),
                               CI_upper = mean + 1.96*sd/sqrt(n),
                               CI_lower = mean - 1.96*sd/sqrt(n),
                               ymin = min(LingObf),
                               ymax = max(LingObf))

CIs_groups_pt_cert <- summarise(groups_pt,
                               mean = mean(CertSent, na.rm = TRUE),
                               sd = sd(CertSent, na.rm = TRUE),
                               n = n(),
                               CI_upper = mean + 1.96*sd/sqrt(n),
                               CI_lower = mean - 1.96*sd/sqrt(n),
                               ymin = min(CertSent),
                               ymax = max(CertSent))

CIs_groups_fg_cert <- summarise(groups_fg,
                                mean = mean(CertSent, na.rm = TRUE),
                                sd = sd(CertSent, na.rm = TRUE),
                                n = n(),
                                CI_upper = mean + 1.96*sd/sqrt(n),
                                CI_lower = mean - 1.96*sd/sqrt(n),
                                ymin = min(CertSent),
                                ymax = max(CertSent))

CIs_groups_sm_obf <- summarise(groups_sm,
                               mean = mean(LingObf, na.rm = TRUE),
                               sd = sd(LingObf, na.rm = TRUE),
                               n = n(),
                               CI_upper = mean + 1.96*sd/sqrt(n),
                               CI_lower = mean - 1.96*sd/sqrt(n))


# Plots for References ---------------------------------------------------------
# Bar chart for references within PaperType groups (with 95% confidence intervals)
  # Statistics to see what the scales need to be
CIs_groups_pt_ref
  # Creating the bar plot
CIs_groups_pt_ref %>% ggplot(aes(factor(PaperType), mean)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab('Paper Type') +
  ylab('References') +
  theme_apa() +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = .2) +
  theme(axis.title.y = element_text(face = "bold")) +
  theme(axis.title.x = element_text(face = "bold")) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70),
                     expand = c(0,0),
                     limits = c(0, 70))

# Bar chart for references within G or F groups (with 95% confidence intervals)
  # Statistics to see what the scales need to be
CIs_groups_fg_ref
  # Creating the bar plot
CIs_groups_fg_ref %>% ggplot(aes(factor(Genuine_or_Fraudulent), mean)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  xlab('Fraudulent or Genuine Papers') +
  ylab('References') +
  theme_apa() +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = .2) +
  theme(axis.title.y = element_text(face = "bold")) +
  theme(axis.title.x = element_text(face = "bold")) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70),
                     expand = c(0,0),
                     limits = c(0, 70))

# Plots for Linguistic Obfuscation ----------------------------------------
# Plots for means of LingObf within PaperType groups (with 95% confidence intervals)
  # Statistics to see what the scales need to be
CIs_groups_pt_obf
  # Creating the plot for LingObf
CIs_groups_pt_obf %>%
  ggplot(aes(x = factor(PaperType), y = mean)) +
  geom_point(shape = 21, fill = "grey", size = 3) + 
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2, color = "black") +
  xlab('Paper Type') +
  ylab('Linguistic Obfuscation') +
  theme_apa() +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")) +
  scale_y_continuous(breaks = seq(-2, 2, 0.5),
                     expand = c(0,0),
                     limits = c(-2, 2))

# Plot for means of LingObf within G or F groups (with 95% confidence intervals)
  # Statistics to see what the scales need to be
CIs_groups_fg_obf
  # Plot for means of LingObf within F or G group
CIs_groups_fg_obf %>%
  ggplot(aes(x = factor(Genuine_or_Fraudulent), y = mean)) +
  geom_point(shape = 21, fill = "grey", size = 3) + 
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2, color = "black") +
  xlab('Paper Type') +
  ylab('Linguistic Obfuscation') +
  theme_apa() +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")) +
  scale_y_continuous(breaks = seq(-1.25, 1.25, 0.25),
                     expand = c(0,0),
                     limits = c(-1.25, 1.25))

# Plots for Certainty Sentiment -------------------------------------------
# Plot for CerSent within PaperType groups (with 95% confidence interval)
  # Stats to determine scales
CIs_groups_pt_cert
  # Plot for means of CertSent within PaperType groups (with 95% confidence interval)
CIs_groups_pt_cert %>%
  ggplot(aes(x = factor(PaperType), y = mean)) +
  geom_point(shape = 21, fill = "grey", size = 3) + 
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2, color = "black") +
  xlab('Paper Type') +
  ylab('Certainty Sentiment') +
  theme_apa() +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")) +
  scale_y_continuous(breaks = seq(5.9, 6.4, .1),
                     expand = c(0,0),
                     limits = c(5.9, 6.4))
# Plot for CertSent within Fraudulent or Genuine Groups (with 95% confidence intervals)
  #Stats to determine scales
CIs_groups_fg_cert
  # Plot for CertSent within Fraudulent or Genuine Groups
CIs_groups_fg_cert %>%
  ggplot(aes(x = factor(Genuine_or_Fraudulent), y = mean)) +
  geom_point(shape = 21, fill = "grey", size = 3) + 
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2, color = "black") +
  xlab('Fraudulent or Genuine Papers') +
  ylab('Certainty Sentiment') +
  theme_apa() +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")) +
  scale_y_continuous(breaks = seq(5.9, 6.4, .1),
                     expand = c(0,0),
                     limits = c(5.9, 6.4))

# Plot for follow up analysis of single- and multi-author groups on obfuscation
  # Statistics
CIs_groups_sm_obf
  # Plotting the means
CIs_groups_sm_obf %>%
  ggplot(aes(x = factor(S_or_M), y = mean)) +
  geom_point(shape = 21, fill = "grey", size = 3) + 
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2, color = "black") +
  xlab('Author Number Group') +
  ylab('Linguistic Obfuscation') +
  theme_apa() +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")) +
  scale_y_continuous(breaks = seq(-1.25, 1.25, 0.25),
                     expand = c(0,0),
                     limits = c(-1.25, 1.25))
