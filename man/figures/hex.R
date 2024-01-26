library(hexSticker)

s <- sticker(
    subplot = "toolbox.png",
    s_x = 1.0,
    s_y = 0.95,
    s_width = 0.8,
    s_height = 1.0,
    package = '',
    p_x = 1,
    p_y = 1.7,
    p_color = "#111111",
    p_family = "serif",
    p_fontface = "plain",
    p_size = 18,
    h_size = 1.2,
    h_fill = "#512141", #"#904878",
    h_color = "#000000",
    spotlight = FALSE,
    l_x = 0.7,
    l_y = 1.6,
    l_width = 3,
    l_height = 3,
    l_alpha = 0.3,
    url = "",
    u_x = 1,
    u_y = 0.08,
    u_color = "black",
    u_family = "Aller_Rg",
    u_size = 1.5,
    u_angle = 30,
    white_around_sticker = FALSE,
    filename = "qwraps2logo.png",
    asp = 1,
    dpi = 300
  )

plot(s)
