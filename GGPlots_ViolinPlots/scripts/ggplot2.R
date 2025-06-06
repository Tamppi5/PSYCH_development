# This is where we left off.

plot <-
  Violin |>
  ggplot(aes(x=condition, y=value))+
  geom_violin(
    fill = "black",
    colour = "white",
    alpha = 0.15,
    draw_quantiles = 0.5,
    position = position_dodge(0.8)
  ) + 
  facet_wrap(~muscle)+
  stat_summary(
    # We want the function (fun) to be set to mean
    # We want this shape (geom) to equal "point" 
    # We want the size (size) to be set to 3
  ) +
  stat_summary(
    # Repeat what you did above a second time below. (Truly, copy and paste!). 
    # Now change the code so that geom should equal "line"
    # And instead of size=3, have it say linewidth=1
    # We also need to add one more argument aes(), in which we set group to be equal to muscle.
  ) 