# derived the color of the text by reversing the brightness and hue
# see http://www.nbdtech.com/Blog/archive/2008/04/27/Calculating-the-Perceived-Brightness-of-a-Color.aspx
v_color_text <-
  as.data.frame(t(col2rgb(v_color_fill))) %>%
  dplyr::mutate(
    brightness = sqrt(red*red*.241 + green*green*.691 + blue*blue*.068),
    color      = ifelse(brightness>130, "gray2", "gray98")
  ) %>%
  dplyr::select(color) %>%
  unlist()
