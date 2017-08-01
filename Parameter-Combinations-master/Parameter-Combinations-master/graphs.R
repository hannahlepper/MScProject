library(ggplot2)

singlelinegraph <- function(data, y) {
  ggplot(data, aes(x = time, y = y)) +
    geom_line()
}

singlelinegraph(sol_base_df, sol_base_df$Inc)

ggplot(sol_base_df) +
  geom_line(mapping = aes(x = time, y = sol_base_df$I), color = "red") +
  geom_line(mapping = aes(x = time, y = sol_base_df$N), color = "blue") +
  legend()
