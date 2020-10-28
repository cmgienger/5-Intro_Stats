#dplyR and ggplot quiz

library(ggplot2)
library(dplyr)

data(msleep)

data_summary <- msleep %>% filter(order == "Rodentia" | order == "Carnivora") %>%
  filter(brainwt>0) %>%
  select(name, order, sleep_total, brainwt, bodywt) %>%
  mutate(ratio=brainwt/bodywt) %>%
  arrange(desc(bodywt))

ggplot(data_summary, aes(x = order, y = ratio)) +
  geom_boxplot() +
  geom_point(size = 4, color = 'red', alpha = 0.5) +
  labs(x = "Mammalian Order", y = "Ratio of Brain Weight to Body Weight")+
  annotate("text", x = 1.5, y = 0.035, label = "t = 1.73, df = 12.4, P = 0.10")

t.test(data=data_summary, ratio~order)



