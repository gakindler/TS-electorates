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

#### Functions #### As per Chang 2018

predictvals <- function(model, xvar, yvar, xrange = NULL, samples = 100, ...) {
    # If xrange isn't passed in, determine xrange from the models.
    # Different ways of extracting the x range, depending on model type
    if (is.null(xrange)) {
        if (any(class(model) %in% c("lm", "glm"))) {
            xrange <- range(model$model[[xvar]])
        } else if (any(class(model) %in% "loess")) {
            xrange <- range(model$x)
        }
    }

    newdata <- data.frame(x = seq(xrange[1], xrange[2], length.out = samples))
    names(newdata) <- xvar
    newdata[[yvar]] <- predict(model, newdata = newdata, ...)
    newdata
}

make_model <- function(data) {
    lm(total_unique_species ~ electorate_area_sqkm, data)
}

#### Import ####

spec.per.elect.counts.summary <- st_read(
    "analysed_data/local_analysis_output/spec.per.elect.counts.summary.gpkg"
) %>%
    st_set_geometry(NULL)

# spec.range.elect.expanded.summary <- read.csv(
#   "analysed_data/local_analysis_output/spec.range.elect.expanded.summary.csv"
# )
#
# spec.per.elect.expanded.summary <- read.csv(
#   "analysed_data/local_analysis_output/spec.per.elect.expanded.summary.csv"
# )

print(object.size(spec.per.elect.counts.summary), units = "Kb")

#### Sub-setting ####

spec.per.elect.IM <- spec.per.elect.counts.summary %>%
    filter(
        demographic_class %in% "Inner metropolitan"
    )

spec.per.elect.OM <- spec.per.elect.counts.summary %>%
    filter(
        demographic_class %in% "Outer metropolitan"
    )

spec.per.elect.P <- spec.per.elect.counts.summary %>%
    filter(
        demographic_class %in% "Provincial"
    )

spec.per.elect.R <- spec.per.elect.counts.summary %>%
    filter(
        demographic_class %in% "Rural"
    )

#### Regression model ####

# LM, Gaussian, log10
# https://towardsdatascience.com/poisson-regression-and-generalised-linear-models-606fe5f7c1fd
# Do you think Linear Regression would be a suitable model? The answer is NO for the following reasons:
# The number of calls have to be greater or equal to 0, whereas in Linear Regression the output can be negative as well as positive.
# The number of calls only take integer values but Linear Regression can output fractional values.

# GLM, Gamma, log10
glm_IM <- glm(
    total_unique_species ~ log10(electorate_area_sqkm),
    family = poisson(),
    data = spec.per.elect.IM
)
glm_OM <- glm(
    total_unique_species ~ log10(electorate_area_sqkm),
    family = poisson(),
    data = spec.per.elect.OM
)
glm_P <- glm(
    total_unique_species ~ log10(electorate_area_sqkm),
    family = poisson(),
    data = spec.per.elect.P
)
glm_R <- glm(
    total_unique_species ~ log10(electorate_area_sqkm),
    family = poisson(),
    data = spec.per.elect.R
)

coef(summary(glm_IM))
coef(summary(glm_OM))
coef(summary(glm_P))
coef(summary(glm_R))


lm_IM <- lm(
    total_unique_species ~ log10(electorate_area_sqkm),
    data = spec.per.elect.IM
)
lm_OM <- lm(
    total_unique_species ~ log10(electorate_area_sqkm),
    data = spec.per.elect.OM
)
lm_P <- lm(
    total_unique_species ~ log10(electorate_area_sqkm),
    data = spec.per.elect.P
)
lm_R <- lm(
    total_unique_species ~ log10(electorate_area_sqkm),
    data = spec.per.elect.R
)

coef(summary(lm_IM))
coef(summary(lm_OM))
coef(summary(lm_P))
coef(summary(lm_R))

# https://stats.stackexchange.com/questions/477598/equivalent-of-r-squared-in-generalized-linear-model-regression-results


mean(log10(spec.per.elect.IM$electorate_area_sqkm))
var(log10(spec.per.elect.IM$electorate_area_sqkm))

# TODO: getting coefficients etc into paper - https://data.library.virginia.edu/interpreting-log-transformations-in-a-linear-model/
# https://www.statology.org/interpret-glm-output-in-r/


#### Scatter plot with regression line ####

spec.per.elect.glm.facet <-
ggplot(spec.per.elect.counts.summary) +
    aes(
        x = log10(electorate_area_sqkm),
        y = total_unique_species
    ) +
    geom_point(
        alpha = 0.4
    ) +
    stat_smooth(
        method = "glm",
        show.legend = FALSE,
        colour = "black",
        method.args = list(
            family = poisson()
        )
    ) +
    # geom_line(
    #     data = predicted,
    #     size = 1
    # ) +
    scale_x_continuous(
        # trans = "log10",
        # labels = scales::comma,
        limits = c(NA, 6.25)
    ) +
    scale_y_continuous(
        limits = c(0, 280)
    ) +
    labs(
        x = bquote(Log[10] ~ "electorate area" ~ (km^2)),
        y = "Threatened species"
    ) +
    # guides(
    #     colour = guide_legend(
    #         title = "Demographic class",
    #         override.aes = list(size = 3)
    #     )
    # ) +
    theme_classic() +
    facet_wrap(~demographic_class) +
    theme(
        strip.background = element_blank()
    )

ggsave("figures/spec.per.elect.glm.facet.pdf",
    spec.per.elect.glm.facet,
    width = 15, height = 10, units = "cm"
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