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

spec.per.elect.expanded.summary <- read_csv(
    "analysed_data/local_analysis_output/spec.per.elect.expanded.summary.csv"
)

#### Linear regression ####

model <- lm(
    log2(total_unique_species) ~ log2(electorate_area_sqkm),
    data = spec.per.elect.counts.summary
)

eqn <- as.character(
    as.expression(
        substitute(
            italic(y) == a + b * italic(x) * "," ~ ~ italic(r)^2 ~ "=" ~ r2,
            list(
                a = format(unname(coef(model)[1]), digits = 3),
                b = format(unname(coef(model)[2]), digits = 3),
                r2 = format(summary(model)$r.squared, digits = 2)
            )
        )
    )
)

# spec.per.elect.lm.facet <-
ggplot(spec.per.elect.counts.summary) +
    aes(
        x = log2(electorate_area_sqkm),
        y = log2(total_unique_species)
    ) +
    geom_point(
        alpha = 0.8,
        aes(colour = demographic_class),
        size = 2
    ) +
    geom_smooth(
        method = "lm",
        show.legend = FALSE,
        colour = "black",
        se = FALSE
        # method.args = list(
        #     family = quasipoisson()
        # )
    ) +
    scale_colour_brewer(
        palette = "Paired"
    ) +
    # scale_colour_viridis_d(
    #     option = "H"
    # ) +
    labs(
        x = bquote(log[2] ~ "Commonwealth Electoral Division area" ~ (km^2)),
        y = bquote(log[2] ~ "number of threatened species"),
        tag = "A"
    ) +
    guides(
        colour = guide_legend(
            title = "Demographic class",
            override.aes = list(size = 3)
        )
    ) +
    theme_bw() +
    annotate(
        "text",
        label = eqn,
        parse = TRUE, x = Inf,
        y = -Inf, hjust = 1.1, vjust = -.5
    ) +
    theme(
        strip.background = element_blank()
    )

ggsave("figures/spec.per.elect.lm.png",
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

#### LM demo faceted ####

lm_labels <- function(dat) {
    mod <- lm(log2(total_unique_species) ~ log2(electorate_area_sqkm), data = dat)
    formula <- sprintf(
        "italic(y) == %.2f %+.2f * italic(x)",
        round(coef(mod)[1], 2), round(coef(mod)[2], 2)
    )
    r <- as.numeric(format(cor(log2(dat$electorate_area_sqkm), dat$total_unique_species), digits = 2))
    r2 <- sprintf("italic(r^2) == %.2f", as.numeric(format(r^2, digits = 2)))
    data.frame(formula = formula, r2 = r2, stringsAsFactors = FALSE)
}

lm_full_summary <- function(dat) {
    model <- lm(
        log2(total_unique_species) ~ log2(electorate_area_sqkm),
        data = dat
    )
    formula <- sprintf(
        "italic(y) == %.2f %+.2f * italic(x)",
        round(coef(model)[1], 2), round(coef(model)[2], 2)
    )
    r2 <- summary(model)$r.squared
    f_stat <- summary(model)$fstatistic[["value"]]
    p_value <- summary(model)$coefficients[2,4]
    data.frame(
        formula = formula, r2 = r2,
        f_stat = f_stat, p_value = p_value,
        stringsAsFactors = FALSE
    )
}

labels <- spec.per.elect.counts.summary %>%
    group_by(demographic_class) %>%
    do(lm_labels(.))

full_summary <- spec.per.elect.counts.summary %>%
    group_by(demographic_class) %>%
    do(lm_full_summary(.))

# spec.per.elect.lm.facet <-
    ggplot(spec.per.elect.counts.summary) +
    aes(
        x = log2(electorate_area_sqkm),
        y = log2(total_unique_species)
    ) +
    geom_point(
        alpha = 0.4
    ) +
    geom_smooth(
        method = "lm",
        show.legend = FALSE,
        colour = "black",
        se = FALSE
    ) +
    labs(
        x = bquote(log[2] ~ "Commonwealth Electoral Division area" ~ (km^2)),
        y = bquote(log[2] ~ "number of threatened species"),
        tag = "B"
    ) +
    theme_bw() +
    facet_wrap(~demographic_class) +
    geom_text(
        data = labels, aes(
            label = formula
        ), x = 5.2, y = 7.8, parse = TRUE, hjust = 0
    ) +
    geom_text(
        x = 5.2, y = 7.4, aes(
            label = r2
        ), data = labels, parse = TRUE, hjust = 0
    ) +
    theme(
        strip.background = element_blank()
    )

ggsave("figures/spec.per.elect.lm.facet.png",
    width = 20, height = 15, units = "cm"
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

fit_lm <- sar_linear(sars_spec_per_elects, normaTest = "shapiro", homoTest = "cor.fitted")
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

spec.per.elect.expanded.summary %>%
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