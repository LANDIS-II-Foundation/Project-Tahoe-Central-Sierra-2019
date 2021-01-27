library(dplyr)
library(ggplot2)

rxfires <- read.csv("I:/TCSI/Round2_outputs/rx_fires_byscen_byclim.csv")
hardata <- read.csv("I:/TCSI/Round2_outputs/h_data_2_cumulative.csv")
hsms_data <- read.csv("I:/TCSI/Round2_outputs/HSMS_fire_area_cumulative_2.csv")

fire_data <- left_join(hsms_data, rxfires, by = c("Scenario","Climate","Year")) 

mgmt_data <- left_join(fire_data, hardata, by = c("Scenario", "Year"))

mgmt_data[is.na(mgmt_data)] <- 0

mgmt_data$mgmtAREA <- mgmt_data$cumAREA.x + mgmt_data$cumAREA.y

write.csv(mgmt_data, "I:/TCSI/Round2_outputs/mgmt_fire_join.csv")

mgmt2 <- read.csv("I:/TCSI/Round2_outputs/mgmt_fire_join2.csv")

mgmt3 <- mgmt2 %>%
  group_by(Scenario, Year, Type) %>%
  summarise(meanAREA = mean(Cumulative), sdAREA = sd(Cumulative))

mgmt_nomiroc <- subset(mgmt2, Climate != "MIROC5_85") %>%
  group_by(Scenario, Year, Type) %>%
  summarise(meanAREA = mean(Cumulative), sdAREA = sd(Cumulative))

mgmt_miroc <- subset(mgmt2, Climate == "MIROC5_85")

mgmt_gg_mm <- ggplot(data = mgmt_miroc, aes(x = Year, y = Cumulative, color = Type)) +
  facet_wrap(~Scenario) +
  geom_line() +
  theme_bw() +
  labs(y = "Cumulative Area", title = "MIROC5")

plot(mgmt_gg_mm)

mgmt_gg_nm <- ggplot(data = mgmt_nomiroc, aes(x = Year, y = meanAREA, fill = Type)) +
  facet_wrap(~Scenario) +
  geom_line() +
  geom_ribbon(aes(ymax = meanAREA + sdAREA, ymin = meanAREA - sdAREA)) +
  theme_bw() +
  labs(y = "Cumulative Area", title = "Other Climate Projections")

plot(mgmt_gg_nm)

mgmt_gg_ac <- ggplot(data = mgmt3, aes(x = Year, y = meanAREA, fill = Type)) +
  facet_wrap(~Scenario) +
  geom_line() +
  geom_ribbon(aes(ymax = meanAREA + sdAREA, ymin = meanAREA - sdAREA)) +
  theme_bw() +
  labs(y = "Cumulative Area", title = "All Climate Projections")

plot(mgmt_gg_ac)
