library(tidyverse)
library(plotly)

#palette
rd_bu_pu <- c("#4393C3", "#7190cd", "#9d8acb", "#c383bd", "#d980ac", "#ec838a")

#results from question 1
q1 <- data.frame(
  working_group = c("CR managers, local and regional actors",
    "CR managers and local actors",
    "Other key stakeholders"
                    ),
  count = c(5,1,2),
  perc = c("62.50%","12.50%","25.00%"))

q1$working_group <- factor(q1$working_group, 
                                levels = c("CR managers, local and regional actors",
                                           "Other key stakeholders",
                                           "CR managers and local actors"
                                           ))

q1_bp <- ggplot(data = q1, 
             aes(x = working_group, y = count, fill = working_group)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = perc), colour="white",
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(name = "",
                    labels = c("CR managers, local and regional actors",
                               "Other key stakeholders",
                               "CR managers and local actors"
                    ),
                    values= rd_bu_pu[c(1,2,5)])

q1_bp <- q1_bp + 
  ggtitle("Who should be part of the Working Group?")+
  xlab("Members of the working group")+
  theme(
    plot.title = element_text(vjust = 2),
    axis.title.y = element_blank(),
    axis.text.x = element_blank()
  )

ggsave("q1_bp.png")

q1_plotly <- ggplotly(q1_bp, tooltip = "count")

#results from question 7
q7 <- data.frame(
  fund = c("Local Government",
           "Local Tourism Board",
           "Other",
           "Regional Government",
           "Regional Tourism Board"),
  count = c(4,1,2,5,2),
  perc = c("28.57%",
           "7.14%",
           "14.29%",
           "35.71%",
           "14.29%"
  ))

q7$fund <- factor(q7$fund,
                  levels = c(
                    "Regional Government",
                    "Local Government",
                    "Regional Tourism Board",
                    "Other",
                    "Local Tourism Board"))

q7_bp <- ggplot(data = q7, 
                aes(x = fund, y = count, fill = fund)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = perc), colour="white",
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = rd_bu_pu, name="")

q7_bp <- q7_bp + 
  ggtitle("Who should fund the data collection process?") +
  xlab("Stakeholders")+
  theme(
    plot.title = element_text(vjust = 2),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom"
  )

ggsave("q7_bp.png")

q7_plotly <- ggplotly(q7_bp, tooltip = "count")

#results from question 11
q11 <- data.frame(
  incentive = c("A certification or label",
                "A yearly award",
                "An online platform",
                "Education and capacity building",
                "Funding to implement the model",
                "Other"),
  count = c(6,5,6, 6,5,2),
  perc = c("20.0%","16.67%","20.0%", "20.0%","16.67%","6.67%"))

q11$incentive <- factor(q11$incentive,
                           levels = c("A certification or label",
                                      "An online platform",
                                      "Education and capacity building",
                                      "A yearly award",
                                      "Funding to implement the model",
                                      "Other"))

q11_bp <- ggplot(data = q11, 
                aes(x = incentive, y = count, fill = incentive)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = perc), colour="white",
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = rd_bu_pu, name="")

q11_bp <- q11_bp + 
  ggtitle("Which could be good incentives to implement the Med S&C Path model?") +
  xlab("Incentives")+
  theme(
    plot.title = element_text(vjust = 2),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom"
  )

ggsave("q11_bp.png")

q11_plotly <- ggplotly(q11_bp, tooltip = "count")