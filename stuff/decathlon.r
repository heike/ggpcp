remotes::install_github("heike/ggpcp", build_vignettes = TRUE)
library(tidyverse)
library(ggpcp)

Decath21p <- readr::read_delim("~/Downloads/regettingusedtonewways/Decath21p.txt", delim="\t")

decNames <- c(Decath21p$Decathlete[1:4], "Others")

Decath21p %>%
  arrange(-Hil) %>%
  pcp_select(P1500, Pdt, Psp, Pjt, Phj, P400m, P100m, Ppv, P110h, Plj) %>%
#  pcp_scale(method="raw") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour=as.factor(Hil)), overplot="none") +
  geom_boxplot(aes(x = pcp_x, y = pcp_y),
               inherit.aes = FALSE, fill=NA, width = 0.25, colour = "grey50") +
  ylab(NULL) + xlab(NULL) +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        legend.title=element_blank()) +
  scale_colour_manual(values=c("red", "darkgoldenrod1", "blue", "brown", "grey80"),
                      labels=decNames) +
  theme_bw()

