library(ggplot2)
# raw data
raw_data <- iris

# easily calculating mean and sd
plot_data <- raw_data %>% 
  gather(., key = "Trait", value = "value", -Species) %>% 
  group_by(Species, Trait) %>% 
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups = "drop"
  )


# normality test
## qq plot
qqnorm(raw_data$Sepal.Length);qqline(raw_data$Sepal.Length)
## hist plot
hist(raw_data$Sepal.Length,main = "",xlab = "", breaks = 10, 
     col = "lightblue", border = "pink")

## Shapiro-Wilk test
shapiro.test(raw_data$Sepal.Length) # need > 0.05

# Homogeneity of Variance test
## F test
var.test(Sepal.Length ~ Species, data = raw_data) 

## levene test
leveneTest(Sepal.Length ~ Species, # default: center = median
           data = raw_data) # need P >0.05

# one-way anova test
owanova <- aov(Sepal.Length ~ Species, data = raw_data)
summary(owanova)

# easy to do one-way anova and get statistic results
SAFana(data = raw_data, method = "lsd", group = "Species", 
       value = "Sepal.Length")


# bar plot
plot_data %>% 
  filter(Trait == "Petal.Length") %>% 
  ggplot(aes(x = Species, y = mean, fill = Species))+
  geom_bar(stat = "identity",position = "dodge")+
  geom_errorbar(aes(ymax = mean+sd, ymin = mean-sd),width = 0)+
  scale_fill_brewer(palette = "Set1")+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Petal.Length (cm)")+
  theme_classic()

## group bar
plot_data %>% 
  # filter(Trait == "Petal.Length") %>% 
  ggplot(aes(x = Trait, y = mean, fill = Species))+
  geom_bar(stat = "identity",position = "dodge")+
  geom_errorbar(aes(ymax = mean+sd, ymin = mean-sd),
                position = position_dodge(width = 0.9),width = 0)+
  scale_fill_brewer(palette = "Set1")+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Petal.Length (cm)")+
  theme_classic()

# point
raw_data %>% 
  ggplot(aes(x = Species, y = Petal.Width, color = Species))+
  geom_point(position = position_jitter(0.2))+
  stat_summary(fun.y='mean',geom='point',shape=4,size=3,color = "black")+
  scale_color_brewer(palette = "Set1")+
  labs(y = "Petal.Width (cm)")+
  theme_bw()

# box
raw_data %>% 
  ggplot(aes(x = Species, y = Petal.Width, color = Species))+
  geom_boxplot()+
  scale_color_brewer(palette = "Set1")+
  labs(y = "Petal.Width (cm)")+
  theme_bw()

# violin
raw_data %>% 
  ggplot(aes(x = Species, y = Petal.Width, fill = Species))+
  geom_violin(alpha=0.4,width=.9,size=0.6,
              position=position_dodge(width=0.8))+
  geom_boxplot(alpha= .9, width=0.15,size = 0.5,
               position = position_dodge(width=0.8))+
  scale_fill_brewer(palette = "Set1")+
  labs(y = "Petal.Width (cm)")+
  theme_bw()

# point showing data distribution and reg
## facet
raw_data %>% 
  ggplot(aes(x = Petal.Length, y = Petal.Width, color = Species))+
  geom_point()+
  geom_smooth(method = "lm",formula = "y~x")+
  stat_poly_eq(use_label(c("eq","R2","p.value.label")),
               formula = y ~ x,parse=TRUE,color="black")+
  facet_wrap(.~Species,scales = "free")+ # facet
  scale_color_brewer(palette = "Set1")+
  labs(x = "Petal.Length (cm)",y = "Petal.Width (cm)")+
  theme_bw()









