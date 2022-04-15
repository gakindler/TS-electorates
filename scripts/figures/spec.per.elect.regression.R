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
# Even discrete responses (such as counts that can only logically be positive integers) can occasionally be approximately described by a Gaussian distribution, particularly if either the samples are very large and the values free from boundary conditions (such as being close to a lower limit of 0), or else we are dealing with average counts.

# https://towardsdatascience.com/poisson-regression-and-generalised-linear-models-606fe5f7c1fd
# Do you think Linear Regression would be a suitable model? The answer is NO for the following reasons:
# The number of calls have to be greater or equal to 0, whereas in Linear Regression the output can be negative as well as positive.
# The number of calls only take integer values but Linear Regression can output fractional values.

lm_IM <- lm(
    total_unique_species ~ log(electorate_area_sqkm),
    data = spec.per.elect.IM
)
lm_OM <- lm(
    total_unique_species ~ electorate_area_sqkm,
    data = spec.per.elect.OM
)
lm_P <- lm(
    total_unique_species ~ electorate_area_sqkm,
    data = spec.per.elect.P
)
lm_R <- lm(
    total_unique_species ~ electorate_area_sqkm,
    data = spec.per.elect.R
)

broom::glance(lm_IM)
broom::glance(lm_OM)
broom::glance(lm_P)
broom::glance(lm_R)
broom::tidy(lm_IM)
broom::tidy(lm_OM)
broom::tidy(lm_P)
broom::tidy(lm_R)



xmin <- min(log(spec.per.elect.IM$electorate_area_sqkm))
xmax <- max(log(spec.per.elect.IM$electorate_area_sqkm))

predicted <- data.frame(electorate_area_sqkm = seq(xmin, xmax, length.out = 100))

predicted$total_unique_species <- predict(lm_IM, predicted)

# https://stats.stackexchange.com/questions/477598/equivalent-of-r-squared-in-generalized-linear-model-regression-results

# https://stats.stackexchange.com/questions/298/in-linear-regression-when-is-it-appropriate-to-use-the-log-of-an-independent-va

# TODO: getting coefficients etc into paper - https://data.library.virginia.edu/interpreting-log-transformations-in-a-linear-model/
# https://www.statology.org/interpret-glm-output-in-r/


lm_labels <- function(dat) {
    mod <- lm(total_unique_species ~ log(electorate_area_sqkm), data = dat)
    formula <- sprintf(
        "italic(y) == %.2f %+.2f * italic(x)",
        round(coef(mod)[1], 2), round(coef(mod)[2], 2)
    )
    r <- cor(log(dat$electorate_area_sqkm), dat$total_unique_species)
    r2 <- sprintf("italic(R^2) == %.2f", r^2)
    data.frame(formula = formula, r2 = r2, stringsAsFactors = FALSE)
}

labels <- spec.per.elect.counts.summary %>%
    group_by(demographic_class) %>%
    do(lm_labels(.))


#### Scatter plot with regression line ####

spec.per.elect.IM$electorate_area_sqkm <- log(spec.per.elect.IM$electorate_area_sqkm)

# spec.per.elect.lm.facet <-
    # ggplot(spec.per.elect.counts.summary) +
    ggplot(spec.per.elect.IM) +
    aes(
        x = electorate_area_sqkm,
        y = total_unique_species
    ) +
    geom_point(
        alpha = 0.4
    ) +
    # geom_smooth(
    #     method = "lm",
    #     show.legend = FALSE,
    #     colour = "black",
    #     se = FALSE
    #     # method.args = list(
    #     #     family = quasipoisson()
    #     # )
    # ) +
    geom_line(
        data = predicted,
        size = 1
    )
    # scale_x_continuous(
        # trans = "log",
        # labels = scales::comma,
        # limits = c(NA, 6.25)
    # ) +
    # scale_y_continuous(
        # limits = c(0, 280)
    # ) +
    labs(
        x = bquote(Log ~ "electorate area" ~ (km^2)),
        y = "Threatened species"
    ) +
    # guides(
    #     colour = guide_legend(
    #         title = "Demographic class",
    #         override.aes = list(size = 3)
    #     )
    # ) +
    theme_bw()
    facet_wrap(~demographic_class) +
    geom_text(
        data = labels, aes(
            label = formula
        ), x = 5, y = 200, parse = TRUE, hjust = 0
    ) +
    geom_text(
        x = 5, y = 180, aes(
            label = r2
        ), data = labels, parse = TRUE, hjust = 0
    ) +
    theme(
        strip.background = element_blank()
    )

ggsave("figures/spec.per.elect.lm.facet.png",
    spec.per.elect.lm.facet,
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