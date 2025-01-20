library(tidyverse)
library(ggpcp)
data(flea, package = "GGally")
flea %>%
  pcp_select(species, 2:7, species) %>%
  pcp_scale(method="uniminmax") %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp_boxes(aes(fill=species), alpha = 0.5) +
  geom_pcp(aes(colour = species)) +
  geom_pcp_labels() +
  theme_pcp() +
  theme(legend.position="bottom") +
  ggtitle("Parallel Coordinate Plot of the flea data")
