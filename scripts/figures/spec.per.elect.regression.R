# Mapping for demo.spec

#### Libraries ####

library(tidyverse)
library(sf)
library(viridis)
library(grid)
library(cartogram)
library(httpgd)
library(plyr)
library(dplyr)
library(jsonlite)
library(broom)
library(ggfortify)
library(sars)

#### Import ####

spec.per.elect.counts.summary <- st_read(
    "analysed_data/local_analysis_output/spec.per.elect.counts.summary.gpkg"
) %>%
    st_set_geometry(NULL) %>%
    select(
        electorate, state_territory,
        demographic_class, electorate_area_sqkm,
        total_unique_species
    )

# spec.range.elect.expanded.summary <- read.csv(
#   "analysed_data/local_analysis_output/spec.range.elect.expanded.summary.csv"
# )
#
# spec.per.elect.expanded.summary <- read.csv(
#   "analysed_data/local_analysis_output/spec.per.elect.expanded.summary.csv"
# )

#### Linear regression ####

spec.per.elect.IM <- spec.per.elect.counts.summary %>%
    filter(
        demographic_class %in% "Inner metropolitan"
    ) %>%
    mutate(log_electorate_area_sqkm = log(electorate_area_sqkm))
hist(spec.per.elect.IM$electorate_area_sqkm)
plot(spec.per.elect.IM$electorate_area_sqkm), spec.per.elect.IM$total_unique_species)
boxplot(spec.per.elect.IM$electorate_area_sqkm, horizontal=TRUE)
rug(jitter(log(spec.per.elect.IM$electorate_area_sqkm)), side=1)
plot(total_unique_species~log(electorate_area_sqkm), spec.per.elect.IM)
with(spec.per.elect.IM, lines(lowess(total_unique_species~log(electorate_area_sqkm))))

IM.lm <- lm(total_unique_species ~ log(electorate_area_sqkm), data = spec.per.elect.IM)

# https://b-rodrigues.github.io/modern_R/statistical-models.html#diagnostics
autoplot(IM.lm, which = 1:6) + theme_minimal()

# https://www.flutterbys.com.au/stats/tut/tut10.4.html
p <- ecdf(log(spec.per.elect.counts.summary$electorate_area_sqkm))
plot(p)


spec.per.elect.OM <- spec.per.elect.counts.summary %>%
    filter(
        demographic_class %in% "Outer metropolitan"
    )
hist(log(spec.per.elect.OM$electorate_area_sqkm))
plot(log(spec.per.elect.OM$electorate_area_sqkm), spec.per.elect.OM$total_unique_species)
shapiro.test(log(spec.per.elect.OM$electorate_area_sqkm))

spec.per.elect.P <- spec.per.elect.counts.summary %>%
    filter(
        demographic_class %in% "Provincial"
    )
hist(log(spec.per.elect.P$electorate_area_sqkm))
plot(log(spec.per.elect.P$electorate_area_sqkm), spec.per.elect.P$total_unique_species)
shapiro.test(log(spec.per.elect.P$electorate_area_sqkm))

spec.per.elect.R <- spec.per.elect.counts.summary %>%
    filter(
        demographic_class %in% "Rural"
    )
hist(log(spec.per.elect.R$electorate_area_sqkm))
plot(log(spec.per.elect.R$electorate_area_sqkm), spec.per.elect.R$total_unique_species)

boxplot(log(spec.per.elect.counts.summary$electorate_area_sqkm), horizontal=TRUE)
rug(jitter(log(spec.per.elect.counts.summary$electorate_area_sqkm)), side=1)

plot(total_unique_species~log(electorate_area_sqkm), spec.per.elect.counts.summary)
with(spec.per.elect.counts.summary, lines(lowess(total_unique_species~log(electorate_area_sqkm))))


lm_labels <- function(dat) {
    mod <- lm(total_unique_species ~ log2(electorate_area_sqkm), data = dat)
    formula <- sprintf(
        "italic(y) == %.2f %+.2f * italic(x)",
        round(coef(mod)[1], 2), round(coef(mod)[2], 2)
    )
    r <- cor(log2(dat$electorate_area_sqkm), dat$total_unique_species)
    r2 <- sprintf("italic(R^2) == %.2f", r^2)
    data.frame(formula = formula, r2 = r2, stringsAsFactors = FALSE)
}

labels <- spec.per.elect.counts.summary %>%
    group_by(demographic_class) %>%
    do(lm_labels(.))

# spec.per.elect.lm.facet <-
    ggplot(spec.per.elect.counts.summary) +
    aes(
        x = electorate_area_sqkm,
        y = total_unique_species
    ) +
    geom_point(
        alpha = 0.4
    ) +
    geom_smooth(
        # method = "gam",
        show.legend = FALSE,
        colour = "black",
        se = FALSE
        # method.args = list(
        #     family = quasipoisson()
        # )
    )
    # geom_line(
    #     data = predicted,
    #     size = 1
    # )
    # scale_x_continuous(
        # trans = "log",
        # labels = scales::comma,
        # limits = c(NA, 6.25)
    # ) +
    # scale_y_continuous(
        # limits = c(0, 280)
    # ) +
    labs(
        x = bquote(Log[2] ~ "Commonwealth Electoral Division area" ~ (km^2)),
        y = "Threatened species"
    ) +
    # guides(
    #     colour = guide_legend(
    #         title = "Demographic class",
    #         override.aes = list(size = 3)
    #     )
    # ) +
    theme_bw() +
    facet_wrap(~demographic_class) +
    geom_text(
        data = labels, aes(
            label = formula
        ), x = 5.2, y = 205, parse = TRUE, hjust = 0
    ) +
    geom_text(
        x = 5.2, y = 180, aes(
            label = r2
        ), data = labels, parse = TRUE, hjust = 0
    ) +
    theme(
        strip.background = element_blank()
    )

ggsave("figures/spec.per.elect.lm.facet.png",
    # spec.per.elect.lm.facet,
    width = 20, height = 15, units = "cm"
)

ggplot(spec.per.elect.counts.summary) +
    aes(
        x = elect_area_sqkm,
        y = total_unique_spec
    ) +
    geom_point(aes(colour = Demographic_class)) +
    geom_smooth(
        method = "lm",
        show.legend = FALSE,
        colour = "black"
    )

#### GLM ####

#### sars R package ####

data(galap)
fit <- sar_loga(data = galap, grid_start = "partial")
summary(fit)
plot(fit)

sars_spec_per_elects <- spec.per.elect.counts.summary %>%
select(electorate_area_sqkm, total_unique_species)

fit_loga <- sar_loga(sars_spec_per_elects)
plot(fit_loga)

fit_C <- sar_multi(sars_spec_per_elects, obj = c("power", "loga", "monod"))
plot(fit_C)

fit_lm <- sar_linear(sars_spec_per_elects, normaTest ="shapiro", homoTest = "cor.fitted")
summary(fit_lm)

fit_av <- sar_average(
    data = sars_spec_per_elects,
    normaTest = "shapiro",
    homoTest = "cor.fitted"
)

fit_lm_log <- lin_pow(
    data = sars_spec_per_elects,
    compare = TRUE,
    normaTest = "shapiro"
    )
summary(fit_lm_log)
plot(fit_lm_log)

#### Calculations ####
#
# spec.per.elect <- read.csv(
#   "analysed_data/local_analysis_output/spec.per.elect.csv"
# )

summary(spec.per.elect.counts.summary)

proportions(table(spec.per.elect.counts.summary$demographic_class))
proportions(table(spec.per.elect.counts.summary$state_territory))

spec.per.elect.demo.area <- spec.per.elect.counts.summary %>%
    group_by(demographic_class) %>%
    summarise(sum_elect_area_sqkm = sum(electorate_area_sqkm))
proportions(spec.per.elect.demo.area$sum_elect_area_sqkm)
0.0308560479 + 0.9654603253
0.0007473473 + 0.0029362795

spec.per.elect.state.area <- spec.per.elect.counts.summary %>%
    group_by(state_territory) %>%
    summarise(sum_state_area_sqkm = sum(electorate_area_sqkm))

spec.per.elect.demo.counts <- spec.per.elect.expanded.summary %>%
    group_by(demographic_class) %>%
    summarise(total_unique_species = n_distinct(scientific_name)) %>%
    ungroup() %>%
    mutate(percentage_of_total_EPBC_species = total_unique_species / 1651)

spec.per.elect.state.counts <- spec.per.elect.expanded.summary %>%
    group_by(state_territory) %>%
    summarise(total_unique_species = n_distinct(scientific_name)) %>%
    ungroup() %>%
    mutate(percentage_of_total_EPBC_species = total_unique_species / 1651)

spec.per.elect.top.ten.counts <- spec.range.elect.expanded.summary %>%
    filter(electorate %in% c(
        "O'Connor", "Durack",
        "Maranoa", "Kennedy",
        "Eden-Monaro", "New England",
        "Page", "Leichhardt",
        "Lyons", "Grey"
    )) %>%
    summarise(total_unique_species = n_distinct(scientific_name))
1134 / 1651

spec.per.elect.endemic.demo.counts <- spec.per.elect.counts.summary %>%
    filter(total_endemic_unique_species >= 1)
proportions(table(spec.per.elect.endemic.demo.counts$demographic_class))

spec.per.elect.eighty.demo.counts <- spec.per.elect.counts.summary %>%
    filter(total_eighty_unique_species >= 1)
proportions(table(spec.per.elect.eighty.demo.counts$demographic_class))