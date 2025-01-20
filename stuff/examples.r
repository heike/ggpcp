library(palmerpenguins)
library(tidyverse)
library(ggpcp)

penguins %>%
  pcp_select(species, bill_length_mm:body_mass_g, species) %>%
  pcp_scale() %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) +
    geom_pcp_axes() +
    geom_pcp(aes(colour = species)) +
    geom_pcp_labels(aes(label = pcp_level))


penguins %>%
  pcp_select(bill_length_mm:body_mass_g) %>%
  pcp_scale() %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = species)) +
  geom_pcp_labels(aes(label = species))

penguins %>%
  pcp_select(species, bill_length_mm:body_mass_g, bill_length_mm, species, sex) %>%
  pcp_scale() %>%
  ggplot(aes(x = pcp_x, y = pcp_y, group = pcp_id)) +
  geom_pcp_axes() +
  geom_pcp(aes(colour=species, class=pcp_class)) +
  facet_wrap(~sex)

penguins %>%
  pcp_select(species, bill_length_mm:body_mass_g, bill_length_mm, species, sex) %>%
  pcp_scale() %>%
  pcp_arrange(method="from-left") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp_boxes(aes(y=pcp_level)) +
  geom_pcp(aes(colour=species)) +
  facet_wrap(~sex)


################
data(banknote, package="mclust")
banknote %>%
  pcp_select(everything(), Status) %>%
  pcp_scale() %>%
  pcp_arrange(method="from-left") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp_boxes() +
  geom_pcp(aes(colour = Status), alpha = 0.5) +
  scale_colour_manual(values=c("steelblue", "darkorange")) +
  theme_bw() +
  ylim(c(0,1))


titanic <- as.data.frame(Titanic)
titanic <- titanic %>%
  purrr::map(.f = function(x) rep(x, titanic$Freq)) %>%
  as.data.frame() %>% select(-Freq)

titanic %>%
  mutate(noise = rnorm(n())) %>%
  pcp_select(everything()) %>%
  pcp_scale() %>%
  pcp_arrange(method="from-left") %>%
  ggplot(aes_pcp()) +
  geom_pcp_boxes(aes(class=pcp_class, y=level), boxwidth=0.1) +
  geom_pcp(aes(colour = Survived, class=pcp_class), alpha = 0.1) +
  scale_colour_manual(values=c("darkorange", "steelblue"))

titanic %>%
  mutate(
    Class = forcats::fct_rev(Class)
  ) %>%
  pcp_select(Survived, Age, Sex, Class) %>%
  pcp_scale() %>%
  pcp_arrange(method="from-left") %>%
  ggplot(aes_pcp(colour = Survived)) +
  geom_pcp_boxes(aes(y=pcp_level), boxwidth=0.1) +
  geom_pcp( alpha = 0.1) +
  geom_pcp_labels(aes(label = pcp_label), alpha = 0.1) +
  scale_colour_manual(values=c("darkorange", "steelblue")) +
  theme(legend.position =  "none" )



##############
wide <- nasa %>% dplyr::select(id, month, year, starts_with("cloud"), ozone, pressure, surftemp, elevation) %>%
  gather(key = variable, value = value, -id, -month, -year) %>%
  filter(month %in% c("Jan","Jul")) %>%
  unite(var_date, variable, year, month, sep="-") %>%
  spread(key=var_date, value)

d <- dist(wide[,-1])
cl <- hclust(d, method = "ward.D")

wide$cl2 <- cutree(cl, k = 2)
wide$cl3 <- cutree(cl, k = 3)
wide$cl4 <- cutree(cl, k = 4)
wide$cl5 <- cutree(cl, k = 5)
wide$cl6 <- cutree(cl, k = 6)
wide$cl7 <- cutree(cl, k = 7)
wide$cl8 <- cutree(cl, k = 8)
wide$cl9 <- cutree(cl, k = 9)
wide$cl10 <- cutree(cl, k = 10)

wide.sum <- nasa %>% dplyr::select(id, month, year, starts_with("cloud"), ozone, pressure, surftemp, elevation) %>%
  gather(key = variable, value = value, -id, -month, -year) %>%
  filter(month %in% c("Jan", "Jul")) %>%
  group_by(id, variable, month) %>%
  summarize(
    value = mean(value)
  ) %>%
  unite(var_month, variable, month, sep="-") %>%
  spread(var_month, value)

wide <- wide %>% left_join(wide.sum, by="id")


wide %>%
  pcp_select(86:94) %>%
  pcp_scale() %>%
  ggplot(aes(x = pcp_x, y = pcp_y, group=pcp_id, class=pcp_class)) +
  geom_pcp() +
  xlab("Number of clusters")

wide %>%
  mutate(
    cl2 = factor(cl2),
    cl3 = factor(cl3),
    cl4 = factor(cl4),
    cl5 = factor(cl5),
    cl6 = factor(cl6),
    cl7 = factor(cl7),
    cl8 = factor(cl8),
    cl9 = factor(cl9),
    cl10 = factor(cl10)
  ) %>%
  pcp_select(86:94) %>%
  pcp_scale() %>%
  pcp_arrange(space=0.15) %>%
  ggplot(aes_pcp()) +
  geom_pcp_boxes(boxwidth=0.1, aes(y = level), space = 0.15) +
  geom_pcp(alpha = 0.25, aes(colour = cl10)) +
  xlab("Number of clusters") +
  scale_colour_brewer("Cluster", palette="Paired") +
  theme(legend.position = "none")

#######################
set.seed(20200924)
x1 <- c(rep(1, 5), rep(2, 3), rep(3, 4))
x2 <- c(1,2,1,2,1,1,1,1,2,2,2,1)
jitter1 <- runif(n = 12, min = -0.1, max = 0.1)
jitter2 <- runif(n = 12, min = -0.1, max = 0.1)
y <- c("a", "b", "c")[x1]
dfnew <- tibble(v1 = x2, v2 = x1,
                v3 = as.factor(x1),
                v4 = as.factor(x2),
                z = y,
                v5 = x2 + jitter1,
                v6 = x1 + jitter2,
                v7 = x2 + c(0, -0, 0.05, -0.05, 0.1, 0.15, 0.20, 0.25, -0.1, -0.15, -0.2, 0.3)/0.05*0.083,
                v8 = (x1-1)/2+1 + c(0:4*0.05, 0:2*0.05, 3:0*(-0.05))/0.05*0.083 + c(rep(0,5), rep(-0.04, 3), rep(0,4)))

p1 <- dfnew %>%
  pcp_select(v1:v2) %>%
  pcp_scale() %>%
  ggplot(aes(x = name, y = value, class=class, group = pcp_id)) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = z), size=1) +
  theme_bw() +
  scale_colour_manual(values=c( "darkorange", "purple4", "darkgreen")) +
  xlab("") + ylab("") +
  facet_grid(.~"Integers") +
  theme(axis.title.y = NULL, axis.text.y = NULL, axis.ticks.y = NULL) +
  theme(legend.position="none")



p2 <- dfnew %>%
  mutate(v1 = v4, v2 = v3) %>%
  pcp_select(v1:v2) %>%
  pcp_scale() %>%
  pcp_arrange() %>%
  ggplot(aes(x = name, y = value, class=class, group = pcp_id)) +
  geom_pcp_axes() +
  geom_pcp_boxes(boxwidth=0.3, fill="grey90", alpha=0.5, aes(y=level)) +
  geom_pcp(aes(colour = z), size=1) +
  theme_bw() +
  scale_colour_manual(values=c( "darkorange", "purple4", "darkgreen")) +
  xlab("") + ylab("") +
  facet_grid(.~"GPCP style") +
  theme(axis.title.y = NULL, axis.text.y = NULL, axis.ticks.y = NULL) +
  theme(legend.position="none")



p3 <- dfnew %>%
  mutate(v1 = v5, v2 = v6) %>%
  pcp_select(v1:v2) %>%
  pcp_scale() %>%
  ggplot(aes(x = name, y = value, class=class, group = pcp_id)) +
  geom_pcp_axes() +
  geom_pcp(size=1, aes(colour = z)) +
  theme_bw() + ylab("") +
  scale_colour_manual(values=c( "darkorange", "purple4", "darkgreen")) + xlab("") +
  facet_grid(.~"Jitter") +
  theme(axis.title.y = NULL, axis.text.y = NULL, axis.ticks.y = NULL) +
  theme(legend.position="none")

p4 <- dfnew %>%
  mutate(v1 = v7, v2 = v8) %>%
  pcp_select(v1:v2) %>%
  pcp_scale() %>%
  ggplot(aes(x = name, y = value, class=class, group = pcp_id)) +
  geom_pcp_axes() +
  geom_pcp(size=1, aes(colour = z)) +
  theme_bw() + ylab("") +
  scale_colour_manual(values=c( "darkorange", "purple4", "darkgreen")) + xlab("") +
  facet_grid(.~"Equally spaced") +
  theme(axis.title.y = NULL, axis.text.y = NULL, axis.ticks.y = NULL) +
  theme(legend.position="none")

library(gridExtra)
grid.arrange(p1, p3, p4, p2, nrow=1)

#####################
wide <- nasa %>% dplyr::select(id, month, year, starts_with("cloud"), ozone, pressure, surftemp, elevation) %>%
  gather(key = variable, value = value, -id, -month, -year) %>%
  filter(month %in% c("Jan")) %>%
  unite(var_date, variable, year, month, sep="-") %>%
  spread(key=var_date, value)

d <- dist(wide[,-1])
cl <- hclust(d, method = "ward.D")

wide$cl2 <- cutree(cl, k = 2)
wide$cl3 <- cutree(cl, k = 3)
wide$cl4 <- cutree(cl, k = 4)
wide$cl5 <- cutree(cl, k = 5)
wide$cl6 <- cutree(cl, k = 6)
wide$cl7 <- cutree(cl, k = 7)
wide$cl8 <- cutree(cl, k = 8)
wide$cl9 <- cutree(cl, k = 9)
wide$cl10 <- cutree(cl, k = 10)

wide.sum <- nasa %>% dplyr::select(id, month, year, starts_with("cloud"), ozone, pressure, surftemp, elevation) %>%
  gather(key = variable, value = value, -id, -month, -year) %>%
  filter(month %in% c("Jan")) %>%
  group_by(id, variable, month) %>%
  summarize(
    value = mean(value)
  ) %>%
  unite(var_month, variable, month, sep="-") %>%
  spread(var_month, value)

wide <- wide %>% left_join(wide.sum, by="id")

wide %>%
  mutate(
    cl2 = factor(cl2),
    cl3 = factor(cl3),
    cl4 = factor(cl4),
    cl5 = factor(cl5),
    cl6 = factor(cl6),
    cl7 = factor(cl7),
    cl8 = factor(cl8),
    cl9 = factor(cl9),
    cl10 = factor(cl10)
  ) %>%
  pcp_select(cl2:cl10) %>%
  pcp_scale() %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) +
  geom_pcp_boxes( boxwidth=0.1, fill=NA) +
  geom_pcp(alpha = 0.35, aes(colour = factor(cl10))) +
  xlab("Number of clusters") +
  scale_colour_brewer("Cluster", palette="Paired") +
  ylab("")

