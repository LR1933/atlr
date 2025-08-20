library(ggplot2)

venus_orbit_radius <- 0.723  # 金星轨道半径（天文单位）
earth_orbit_radius <- 1.0    # 地球轨道半径（天文单位）
venus_period <- 224.7        # 金星公转周期（地球日）
earth_period <- 365.25       # 地球公转周期（地球日）

# 模拟时间
days <- seq(0, 365 * 8, by = 1)  # 模拟8年的时间

# 计算金星相对地球的位置
x_relative <- venus_orbit_radius * cos(2 * pi * days / venus_period) - 
              earth_orbit_radius * cos(2 * pi * days / earth_period)
y_relative <- venus_orbit_radius * sin(2 * pi * days / venus_period) - 
              earth_orbit_radius * sin(2 * pi * days / earth_period)

data <- data.frame(x = x_relative, y = y_relative)

ggplot(data, aes(x = x, y = y)) +
  geom_path(color = "white", size=1.2) +
  coord_fixed() +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid = element_blank(),  
    axis.line = element_blank(),  
    axis.ticks = element_blank(),  
    axis.text = element_blank(),   
    panel.border = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA), # 透明背景
    plot.background = element_rect(fill = "transparent", color = NA)  # 透明画布
  )
ggsave(
  "venus_orbit.png",
  width = 10,
  height = 10,
  units = "cm",
  bg = "transparent"
)
