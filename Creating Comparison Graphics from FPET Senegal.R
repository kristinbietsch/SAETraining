
setwd("C:/Users/KristinBietsch/Avenir Health Dropbox/Kristin kbietsch@avenirhealth.org/Avenir Track20/Training Materials/Small Area Estimates 2026/Senegal/Senegal Adm2 Bottom Up_all_data")

library(tidyverse)

library(RColorBrewer)
library(viridis)
library(ggrepel)
turbo(26)

data <- read.csv("Senegal Adm2 Bottom Up-results.csv")

data_small <- data %>% select(4,5,6,7,11) %>%
  rename(mCP=5) %>%
  mutate(mCP=100*mCP)

data_2025 <- data_small %>% filter(Year==2025) %>%
  filter(Percentile %in% c("0.025",  "0.975",  "median")) %>% 
  spread(Percentile, mCP) %>%
  rename(LCI=4, UCI=5) %>%
  mutate(National=case_when(Region.name=="National" ~ 1, TRUE ~ 0))

data_20102030 <-  data_small %>% filter(Year>=2010) %>%
  filter(Percentile %in% c("median")) %>%
  mutate(National=case_when(Region.name=="National" ~ 1, TRUE ~ 0))


############################################################################

ggplot(data=subset(data_2025, Marital.status=="all"), aes(x=median, y=fct_reorder(Region.name, desc(-median)))) +
  geom_point(aes(color=as.factor(National)), size=4) +
  geom_linerange(aes(xmin = LCI, xmax = UCI)) +
  labs(x="", y="", title="mCP, All Women, 2025") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("mCP All Women 2025.png", height = 12, width=5, units = "in")


ggplot(data=subset(data_2025, Marital.status=="married"), aes(x=median, y=fct_reorder(Region.name, desc(-median)))) +
  geom_point(aes(color=as.factor(National)), size=4) +
  geom_linerange(aes(xmin = LCI, xmax = UCI)) +
  labs(x="", y="", title="mCP, Married Women, 2025") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("mCP Married Women 2025.png", height = 12, width=5, units = "in")


###################################################################################
# 
# 
# colors <- c()
# 
# ggplot(data=subset(data_20102030, Marital.status=="all"), aes(x=Year, y=mCP)) +
#   geom_line(data=subset(data_20102030, Marital.status=="all" & Region.name!="National"), aes(color=Region.name )) +
#   geom_line(data=subset(data_20102030, Marital.status=="all" & Region.name=="National"), color="black", size=1.5) +
#   scale_size_manual(values=c(1, 2)) +
#   geom_text_repel(data=subset(data_20102030, Marital.status=="all" & Region.name!="National" & Year==2030), aes(x=2030, y=mCP, label = Region.name, color=Region.name),   nudge_x = 0.2, direction = "y", hjust = "left") +
#   geom_text_repel(data=subset(data_20102030, Marital.status=="all" & Region.name=="National" & Year==2030), aes(x=2030, y=mCP, label = Region.name),  color="black" , nudge_x = 0.2, direction = "y", hjust = "left") +
#   labs(x="", y="", title="mCP, All Women, 2010-2030") +
#   xlim(2010, 2034) +
#   theme_bw() +
#   theme(legend.position = "none")
# 
# ggsave("mCP All Women 20102030 One.png", height = 6, width=10, units = "in")
# 
# ggplot(data=subset(data_20102030, Marital.status=="married"), aes(x=Year, y=mCP)) +
#   geom_line(data=subset(data_20102030, Marital.status=="married" & Region.name!="National"), aes(color=Region.name )) +
#   geom_line(data=subset(data_20102030, Marital.status=="married" & Region.name=="National"), color="black", size=1.5) +
#   scale_size_manual(values=c(1, 2)) +
#   geom_text_repel(data=subset(data_20102030, Marital.status=="married" & Region.name!="National" & Year==2030), aes(x=2030, y=mCP, label = Region.name, color=Region.name),   nudge_x = 0.2, direction = "y", hjust = "left") +
#   geom_text_repel(data=subset(data_20102030, Marital.status=="married" & Region.name=="National" & Year==2030), aes(x=2030, y=mCP, label = Region.name),  color="black" , nudge_x = 0.2, direction = "y", hjust = "left") +
#   labs(x="", y="", title="mCP, Married Women, 2010-2030") +
#   xlim(2010, 2034) +
#   theme_bw() +
#   theme(legend.position = "none")
# ggsave("mCP Married Women 20102030 One.png", height = 6, width=10, units = "in")

###################################################################################
ggplot(data=subset(data_20102030, Marital.status=="all" & Region.name!="National"), aes(x=Year, y=mCP)) +
  geom_line() + 
  facet_wrap(~ Region.name) +
  labs(x="", y="", title="mCP, All Women, 2010-2030") +
  theme_bw()
ggsave("mCP All Women 20102030 Multi.png", height = 20, width=20, units = "in")

ggplot(data=subset(data_20102030, Marital.status=="married" & Region.name!="National"), aes(x=Year, y=mCP)) +
  geom_line() + 
  facet_wrap(~ Region.name) +
  labs(x="", y="", title="mCP, Married Women, 2010-2030") +
  theme_bw()
ggsave("mCP Married Women 20102030 Multi.png", height = 20, width=20, units = "in")
