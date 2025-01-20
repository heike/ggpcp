# Downloaded from table in

# Landis, J R, and G G Koch. "An Application of Hierarchical Kappa-type Statistics in the Assessment of Majority Agreement among Multiple Observers." Biometrics 33.2 (1977): 363-74. Web.

# Holmquist, N. S., McMahan, C. A. and Williams, 0. D. (1967). Variability in classification of carcinoma in situ of the uterine cervix. Arch Pathol, 84, 4:334-45.


library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpcp)

Carcinoma <- read_delim("data-raw/Carcinoma.txt", delim="\t")
Carcinoma <- Carcinoma %>% arrange(No)
Carcinoma$Average <- rowMeans(Carcinoma %>% select(A:G))
Carcinoma <- Carcinoma %>% mutate(across(A:G, as.factor))

Carcinoma %>%
  pcp_select(F, D, C, A, G, E, B, Average) %>%
  pcp_scale(method="uniminmax") %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = Average > 2)) +
  geom_pcp_boxes(colour="black", alpha=0) +
  geom_pcp_labels(aes(label = pcp_level), fill="white", alpha = 1) +
  theme_bw() +
  scale_x_discrete(expand = expansion(add=0.25)) +
  xlab("Pathologist") + ylab(NULL) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.position="none")

library(usethis)
use_data(Carcinoma)
