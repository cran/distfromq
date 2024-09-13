## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 8,
  out.width = "100%"
)

## ----setup--------------------------------------------------------------------
library(distfromq)
library(ggplot2)
library(dplyr)

## -----------------------------------------------------------------------------
quantile_probs <- seq(from = 0.1, to = 0.9, by = 0.1)

meanlog <- 4.0
sdlog <- 0.5
q_lognormal <- qlnorm(quantile_probs, meanlog = meanlog, sdlog = sdlog)

## -----------------------------------------------------------------------------
x <- seq(from = 0.0, to = 400.0, length = 501)
cdf_lognormal <- plnorm(x, meanlog = meanlog, sdlog = sdlog)

p_lognormal_approx <- make_p_fn(ps = quantile_probs,
                                qs = q_lognormal,
                                tail_dist = "lnorm")
cdf_lognormal_approx <- p_lognormal_approx(x)

# note that `tail_dist = "norm"` is the default; we specify it here for clarity
p_normal_approx <- make_p_fn(ps = quantile_probs,
                             qs = q_lognormal,
                             tail_dist = "norm")
cdf_normal_approx <- p_normal_approx(x)

p_cauchy_approx <- make_p_fn(ps = quantile_probs,
                             qs = q_lognormal,
                             tail_dist = "cauchy")
cdf_cauchy_approx <- p_cauchy_approx(x)

dplyr::bind_rows(
  data.frame(
    x = x,
    y = cdf_lognormal,
    dist = "Log normal"
  ),
  data.frame(
    x = x,
    y = cdf_lognormal_approx,
    dist = "Spline interpolation,\nlog-normal tails"
  ),
  data.frame(
    x = x,
    y = cdf_normal_approx,
    dist = "Spline interpolation,\nnormal tails"
  ),
  data.frame(
    x = x,
    y = cdf_cauchy_approx,
    dist = "Spline interpolation,\nCauchy tails"
  )
) %>%
  ggplot() +
  geom_line(
    mapping = aes(x = x, y = y, color = dist, linetype = dist),
    size = 0.8
  ) +
  geom_point(
    data = data.frame(q = q_lognormal, p = quantile_probs),
    mapping = aes(x = q, y = p),
    size = 1.2
  ) +
  scale_color_viridis_d(
    "Distribution",
    end = 0.9
  ) +
  scale_linetype_discrete("Distribution") +
  ylab("Probability") +
  xlab("") +
  theme_bw()

## -----------------------------------------------------------------------------
d_lognormal_approx <- make_d_fn(ps = quantile_probs,
                                qs = q_lognormal,
                                tail_dist = "lnorm")

d_normal_approx <- make_d_fn(ps = quantile_probs,
                             qs = q_lognormal,
                             tail_dist = "norm")

d_cauchy_approx <- make_d_fn(ps = quantile_probs,
                             qs = q_lognormal,
                             tail_dist = "cauchy")

pdf_lognormal <- dlnorm(x, meanlog = meanlog, sdlog = sdlog)
pdf_lognormal_approx <- d_lognormal_approx(x)
pdf_normal_approx <- d_normal_approx(x)
pdf_cauchy_approx <- d_cauchy_approx(x)

dplyr::bind_rows(
  data.frame(
    x = x,
    y = pdf_lognormal,
    dist = "Log normal"
  ),
  data.frame(
    x = x,
    y = pdf_lognormal_approx,
    dist = "Spline interpolation,\nlog-normal tails"
  ),
  data.frame(
    x = x,
    y = pdf_normal_approx,
    dist = "Spline interpolation,\nnormal tails"
  ),
  data.frame(
    x = x,
    y = pdf_cauchy_approx,
    dist = "Spline interpolation,\nCauchy tails"
  )
) %>%
  ggplot() +
  geom_vline(
    data = data.frame(q = q_lognormal),
    mapping = aes(xintercept = q),
    size = 0.2
  ) +
  geom_line(
    mapping = aes(x = x, y = y, color = dist, linetype = dist),
    size = 0.8
  ) +
  scale_color_viridis_d(
    "Distribution",
    end = 0.9
  ) +
  scale_linetype_discrete("Distribution") +
  ylab("Probability Density") +
  xlab("") +
  theme_bw()

## -----------------------------------------------------------------------------
d_lognormal_approx_n_grid_1 <- make_d_fn(ps = quantile_probs,
                                         qs = q_lognormal,
                                         tail_dist = "lnorm",
                                         interior_args = list(n_grid = 1))

pdf_lognormal_approx_n_grid_1 <- d_lognormal_approx_n_grid_1(x)

dplyr::bind_rows(
  data.frame(
    x = x,
    y = pdf_lognormal_approx,
    dist = "Spline interpolation,\n n_grid = 20"
  ),
  data.frame(
    x = x,
    y = pdf_lognormal_approx_n_grid_1,
    dist = "Spline interpolation,\n n_grid = 1"
  )
) %>%
  ggplot() +
  geom_vline(
    data = data.frame(q = q_lognormal),
    mapping = aes(xintercept = q),
    size = 0.2
  ) +
  geom_line(
    mapping = aes(x = x, y = y, color = dist),
    size = 0.8
  ) +
  scale_color_viridis_d(
    "Distribution",
    end = 0.7
  ) +
  scale_linetype_discrete("Distribution") +
  ylab("Probability Density") +
  xlab("") +
  theme_bw()

## -----------------------------------------------------------------------------
r_normal_approx <- make_r_fn(ps = quantile_probs,
                             qs = q_lognormal,
                             tail_dist = "norm")
r_lognormal_approx <- make_r_fn(ps = quantile_probs,
                                qs = q_lognormal,
                                tail_dist = "lnorm")
r_cauchy_approx <- make_r_fn(ps = quantile_probs,
                             qs = q_lognormal,
                             tail_dist = "cauchy")

normal_approx_sample <- r_normal_approx(n = 10000)
lognormal_approx_sample <- r_lognormal_approx(n = 10000)
cauchy_approx_sample <- r_cauchy_approx(n = 10000)

bind_rows(
  data.frame(x = normal_approx_sample, dist = "Spline interpolation,\nnormal tails"),
  data.frame(x = lognormal_approx_sample, dist = "Spline interpolation,\nlog-normal tails"),
  data.frame(x = cauchy_approx_sample, dist = "Spline interpolation,\nCauchy tails")
) %>%
  ggplot() +
  geom_density(mapping = aes(x = x, color = dist, linetype = dist)) +
  scale_color_viridis_d(
    "Distribution",
    end = 0.9
  ) +
  scale_linetype_discrete("Distribution") +
  theme_bw()

## -----------------------------------------------------------------------------
bind_rows(
  data.frame(x = normal_approx_sample, dist = "Spline interpolation,\nnormal tails"),
  data.frame(x = lognormal_approx_sample, dist = "Spline interpolation,\nlog-normal tails"),
  data.frame(x = cauchy_approx_sample, dist = "Spline interpolation,\nCauchy tails")
) %>%
  ggplot() +
  geom_density(mapping = aes(x = x, color = dist, linetype = dist)) +
  scale_color_viridis_d(
    "Distribution",
    end = 0.9
  ) +
  scale_linetype_discrete("Distribution") +
  xlim(-100, 300) +
  theme_bw()

## -----------------------------------------------------------------------------
ps <- seq(from = 0.01, to = 0.99, by = 0.01)

q_normal_approx <- make_q_fn(ps = quantile_probs,
                             qs = q_lognormal,
                             tail_dist = "norm")
q_lognormal_approx <- make_q_fn(ps = quantile_probs,
                                qs = q_lognormal,
                                tail_dist = "lnorm")
q_cauchy_approx <- make_q_fn(ps = quantile_probs,
                             qs = q_lognormal,
                             tail_dist = "cauchy")

quantiles_lognormal <- qlnorm(ps, meanlog = meanlog, sdlog = sdlog)
quantiles_normal_approx <- q_normal_approx(ps)
quantiles_lognormal_approx <- q_lognormal_approx(ps)
quantiles_cauchy_approx <- q_cauchy_approx(ps)


dplyr::bind_rows(
  data.frame(
    x = ps,
    y = quantiles_lognormal,
    dist = "Log normal"
  ),
  data.frame(
    x = ps,
    y = quantiles_normal_approx,
    dist = "Spline interpolation,\nnormal tails"
  ),
  data.frame(
    x = ps,
    y = quantiles_lognormal_approx,
    dist = "Spline interpolation,\nlognormal tails"
  ),
  data.frame(
    x = ps,
    y = quantiles_cauchy_approx,
    dist = "Spline interpolation,\nCauchy tails"
  )
) %>%
  ggplot() +
  geom_line(
    mapping = aes(x = x, y = y, color = dist, linetype = dist),
    size = 0.8
  ) +
  scale_color_viridis_d(
    "Distribution",
    end = 0.9
  ) +
  scale_linetype_discrete("Distribution") +
  ylab("Quantile") +
  xlab("Probability Level") +
  theme_bw()

## -----------------------------------------------------------------------------
# mixture of a LogNormal(4, 0.5) distribution with weight 0.8 and
# a point mass at 0 with weight 0.2

# probabilities and quantiles for the lognormal component
lnorm_ps <- seq(from = 0.1, to = 0.9, by = 0.1)
lnorm_qs <- qlnorm(lnorm_ps, meanlog = 4.0, sdlog = 0.5)
adj_lnorm_ps <- 0.2 + lnorm_ps * 0.8

# quantile at 0 with probability 0.2 for the point mass at 0
point_p <- 0.2
point_q <- 0.0

ps <- c(point_p, adj_lnorm_ps)
qs <- c(point_q, lnorm_qs)

## -----------------------------------------------------------------------------
x <- seq(from = -100.0, to = 400.0, length = 501)

p_lognormal_approx <- make_p_fn(ps = ps,
                                qs = qs,
                                tail_dist = "lnorm")
cdf_lognormal_approx <- p_lognormal_approx(x)

data.frame(
  x = x,
  y = cdf_lognormal_approx
) %>%
  ggplot() +
  geom_line(
    mapping = aes(x = x, y = y),
    size = 0.8
  ) +
  geom_point(
    data = data.frame(q = qs, p = ps),
    mapping = aes(x = q, y = p),
    size = 1.2
  ) +
  ylim(0, 1) +
  ylab("Probability") +
  xlab("") +
  theme_bw()

## -----------------------------------------------------------------------------
plot_ps <- seq(from = 0.00, to = 0.99, by = 0.001)

q_lognormal_approx <- make_q_fn(ps = ps,
                                qs = qs,
                                tail_dist = "lnorm")
qf_lognormal_approx <- q_lognormal_approx(plot_ps)

data.frame(
  x = plot_ps,
  y = qf_lognormal_approx
) %>%
  ggplot() +
  geom_line(
    mapping = aes(x = x, y = y),
    size = 0.8
  ) +
  geom_point(
    data = data.frame(q = ps, p = qs),
    mapping = aes(x = q, y = p),
    size = 1.2
  ) +
  xlim(0, 1) +
  xlab("Probability") +
  ylab("") +
  theme_bw()

## -----------------------------------------------------------------------------
dplyr::bind_rows(
  data.frame(
    x = x,
    y = cdf_lognormal_approx,
    method = "CDF Estimate"
  ),
  data.frame(
    x = qf_lognormal_approx,
    y = plot_ps,
    method = "Flipped QF Estimate"
  )
) %>%
  ggplot() +
  geom_line(
    mapping = aes(x = x, y = y, color = method, linetype = method),
    size = 0.8
  ) +
  geom_point(
    data = data.frame(q = qs, p = ps),
    mapping = aes(x = q, y = p),
    size = 1.2
  ) +
  ylim(0, 1) +
  ylab("Probability") +
  xlab("") +
  theme_bw()


## -----------------------------------------------------------------------------
r_fn <- make_r_fn(ps = ps, qs = qs, tail_dist = "lnorm")

sampled_values_df <- data.frame(x = r_fn(10000))

ggplot(sampled_values_df) +
  geom_histogram(mapping = aes(x = x), bins = 100) +
  theme_bw()

mean(sampled_values_df$x == 0)

## ----error=TRUE---------------------------------------------------------------
d_fn_lnorm <- make_d_fn(ps = ps, qs = qs, tail_dist = "lnorm")

## -----------------------------------------------------------------------------
d_fn_norm <- make_d_fn(ps = ps, qs = qs, tail_dist = "norm")

## ----error=TRUE---------------------------------------------------------------
quantile_probs <- seq(from = 0.1, to = 0.9, by = 0.1)
quantile_values <- c(1.0, 2.0, 3.0, 3.0, 3.0, 3.0, 3.0, 8.0, 9.0)

d_normal_approx <- make_d_fn(ps = quantile_probs,
                             qs = quantile_values,
                             tail_dist = "norm")
p_normal_approx <- make_p_fn(ps = quantile_probs,
                             qs = quantile_values,
                             tail_dist = "norm")
q_normal_approx <- make_q_fn(ps = quantile_probs,
                             qs = quantile_values,
                             tail_dist = "norm")
r_normal_approx <- make_r_fn(ps = quantile_probs,
                             qs = quantile_values,
                             tail_dist = "norm")

x <- seq(from = 0.0, to = 20.0, length = 5001)
cdf_normal_approx <- p_normal_approx(x)
ggplot() +
  geom_line(data = data.frame(x = x, y = cdf_normal_approx),
            mapping = aes(x = x, y = y)) +
  geom_point(data = data.frame(x = quantile_values, y = quantile_probs),
             mapping = aes(x = x, y = y))

ps <- seq(from = 0.0, to = 1.0, length = 5001)
qf_normal_approx <- q_normal_approx(ps)
ggplot() +
  geom_line(data = data.frame(p = ps, y = qf_normal_approx),
            mapping = aes(x = p, y = y)) +
  geom_point(data = data.frame(x = quantile_probs, y = quantile_values),
             mapping = aes(x = x, y = y))

samples_normal_approx <- r_normal_approx(n = 10000)
mean(samples_normal_approx == 3.0)

## -----------------------------------------------------------------------------
ps <- seq(from = 0.0, to = 1.0, length = 101)
out_ps <- p_normal_approx(q_normal_approx(ps))
out_ps

ps[(ps < 0.3) | (ps > 0.7)] - out_ps[(ps < 0.3) | (ps > 0.7)]

## -----------------------------------------------------------------------------
p_normal_approx_spline <- make_p_fn(ps = quantile_probs,
                                    qs = quantile_values,
                                    tail_dist = "norm",
                                    interior_args = list(n_grid = NULL))
q_normal_approx_spline <- make_q_fn(ps = quantile_probs,
                                    qs = quantile_values,
                                    tail_dist = "norm",
                                    interior_args = list(n_grid = NULL))

x <- seq(from = 0.0, to = 20.0, length = 5001)
plot_df <- rbind(
  data.frame(
    x = x,
    cdf = p_normal_approx(x),
    type = "piecewise linear"
  ),
  data.frame(
    x = x,
    cdf = p_normal_approx_spline(x),
    type = "spline"
  )
)
ggplot() +
  geom_line(data = plot_df,
            mapping = aes(x = x, y = cdf, color = type, linetype = type)) +
  geom_point(data = data.frame(x = quantile_values, y = quantile_probs),
             mapping = aes(x = x, y = y))

## -----------------------------------------------------------------------------
ps <- seq(from = 0.0, to = 1.0, length = 5001)
plot_df <- rbind(
  data.frame(
    p = ps,
    qf = q_normal_approx(ps),
    type = "piecewise linear"
  ),
  data.frame(
    p = ps,
    qf = q_normal_approx_spline(ps),
    type = "spline"
  )
)
ggplot() +
  geom_line(data = plot_df,
            mapping = aes(x = p, y = qf, color = type, linetype = type)) +
  geom_point(data = data.frame(x = quantile_probs, y = quantile_values),
             mapping = aes(x = x, y = y))

## -----------------------------------------------------------------------------
quantile_probs <- seq(from = 0.1, to = 0.9, by = 0.1)
quantile_values <- c(1.0, 1.0, 3.0, 3.0, 3.0, 3.0, 3.0, 9.0, 9.0)

p_normal_approx <- make_p_fn(ps = quantile_probs,
                             qs = quantile_values,
                             tail_dist = "norm")
p_normal_approx_lin <- make_p_fn(ps = quantile_probs,
                                 qs = quantile_values,
                                 tail_dist = "norm",
                                 interior_args = list(n_grid = 20))
q_normal_approx <- make_q_fn(ps = quantile_probs,
                             qs = quantile_values,
                             tail_dist = "norm")
q_normal_approx_lin <- make_q_fn(ps = quantile_probs,
                                 qs = quantile_values,
                                 tail_dist = "norm",
                                 interior_args = list(n_grid = 20))

x <- seq(from = 0.0, to = 20.0, length = 5001)
plot_df <- rbind(
  data.frame(
    x = x,
    cdf = p_normal_approx(x),
    type = "spline"
  ),
  data.frame(
    x = x,
    cdf = p_normal_approx_lin(x),
    type = "piecewise linear"
  )
)
ggplot() +
  geom_line(data = plot_df,
            mapping = aes(x = x, y = cdf, color = type, linetype = type)) +
  geom_point(data = data.frame(x = quantile_values, y = quantile_probs),
             mapping = aes(x = x, y = y))

## -----------------------------------------------------------------------------
ps <- seq(from = 0.0, to = 1.0, length = 5001)
plot_df <- rbind(
  data.frame(
    p = ps,
    qf = q_normal_approx(ps),
    type = "spline"
  ),
  data.frame(
    p = ps,
    qf = q_normal_approx_lin(ps),
    type = "piecewise linear"
  )
)
ggplot() +
  geom_line(data = plot_df,
            mapping = aes(x = p, y = qf, color = type, linetype = type)) +
  geom_point(data = data.frame(x = quantile_probs, y = quantile_values),
             mapping = aes(x = x, y = y))

