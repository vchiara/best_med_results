library(tidyverse)
library(rmarkdown)
library(plotly)
library(summarytools)
st_options(char.split = 15)

#load and clean data
gap_analysis <- read_csv("gap_analysis2.csv", 
                         col_types = cols(not_covered_short = col_skip()))

gap_analysis <- na_if(gap_analysis, "N/A")

colnames(gap_analysis) <- c("partner", "dimension", "covered", "part_covered", "not_covered")

gap_analysis <- gap_analysis %>%
  mutate_if(is.character, as.factor)

gap_analysis$dimension <- factor(gap_analysis$dimension, 
                                 levels = c("Sustainable Management",
                                            "Economic Sustainability",
                                            "Socio-Cultural Sustainability",
                                            "Environmental Sustainability"))

#covered needs
covered_data <- gap_analysis[,1:3]

covered_data <- covered_data[order(covered_data$dimension),]

covered_data <- covered_data %>%
  drop_na(covered)

covered_data <- covered_data %>% 
  group_by(partner) %>% 
  distinct(covered, .keep_all = TRUE)

summ_covered <- summary(covered_data)

freq_covered <- view(ctable(covered_data$partner, covered_data$dimension), 
                  method="render")

#plot
pal <- c('#0e5188', '#1bb1c8', '#5b7ece', '#a5c882')

covered_plot <- ggplot(covered_data, 
                  aes(fill = dimension, x = partner, text = covered)) + 
           geom_bar(stat = "count")

covered_plot <- covered_plot  +
  scale_fill_manual(values = alpha(pal, 0.85))+
  scale_y_discrete(breaks = NULL) +
  ggtitle("Needs covered by criteria") + 
  theme(
  plot.title = element_text(),
  axis.ticks = element_blank(),
  axis.text.y.left = element_blank()
  )

covered_plotly <- ggplotly(covered_plot, tooltip = "text")

#partly covered needs
part_covered_data <- gap_analysis[,c(1,2,4)]

part_covered_data <- part_covered_data[order(part_covered_data$dimension),]

part_covered_data <- part_covered_data %>%
  drop_na(part_covered)

part_covered_data <- part_covered_data %>% 
  group_by(partner) %>% 
  distinct(part_covered, .keep_all = TRUE)

summ_part <- summary(part_covered_data)

freq_part <- view(ctable(part_covered_data$partner, part_covered_data$dimension), 
                  method="render")

#plot
part_covered_plot <- ggplot(part_covered_data, 
                       aes(fill = dimension, y = part_covered, x = partner, text = part_covered)) + 
                geom_bar(stat = "identity")

part_covered_plot <- part_covered_plot  +
  scale_fill_manual(values = alpha(pal, 0.85))+
  scale_y_discrete(breaks = NULL) +
  ggtitle("Needs partially covered by criteria") + 
  theme(
    plot.title = element_text(),
    axis.ticks = element_blank(),
    axis.text.y.left = element_blank()
  )

part_covered_plotly <- ggplotly(part_covered_plot, tooltip = "text")

#not covered needs
not_covered_data <- gap_analysis[,c(1,2,5)]

not_covered_data <- not_covered_data[order(not_covered_data$dimension),]

not_covered_data <- not_covered_data %>%
  drop_na(not_covered)

summ_not_covered <- summary(not_covered_data)
summ_not_covered <- summ[,1:2]

freq_not_covered <- view(ctable(not_covered_data$partner, not_covered_data$dimension), 
                  method = "render")

not_covered_plot <- ggplot(not_covered_data, 
                      aes(fill = dimension, y = dimension, x = partner, text = not_covered)) + 
               geom_bar(stat = "identity")

not_covered_plot <- not_covered_plot +
  scale_fill_manual(values = alpha(pal, 0.85))+
  scale_y_discrete(breaks = NULL) +
  ggtitle("Needs not covered by criteria") + 
  theme(
    plot.title = element_text(),
    axis.ticks = element_blank(),
    axis.text.y.left = element_blank()
  )

not_covered_plotly <- ggplotly(not_covered_plot, tooltip = "text")