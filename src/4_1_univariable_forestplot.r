#forest plot of univariable logistic regression odds ratios (complete-case data)
#depends on univariable_logistic_table_complete and the *_terms variables from src/4_univariable_models.r
library(ggplot2)

forest_axis_limits = c(0.5, 2)

#readable display names for the variable codes used in model_data
forest_label_lookup = c(
    SpO2 = "SpO2 (%)",
    RR = "Respiratory rate",
    HR = "Heart rate",
    MAP = "Mean arterial pressure",
    Temperature = "Temperature",
    GCS = "Glasgow Coma Scale",
    CRP = "CRP",
    Lactate = "Lactate",
    Procalcitonin = "Procalcitonin",
    MRproADM = "MR-proADM",
    Baseline_StO2_level = "Baseline StO2 level",
    rate_of_deoxygenation = "Rate of deoxygenation (%/sec)",
    rate_of_reoxygenation = "Rate of reoxygenation (%/sec)",
    peak_StO2_post_vascular_occlusion = "Peak StO2 post vascular occlusion (%)",
    Microvascular_flox_index = "Microvascular flow index"
)

forest_plot_data = univariable_logistic_table_complete$table_body |>
    dplyr::filter(row_type == "label", !is.na(estimate)) |>
    dplyr::select(variable, label, estimate, conf.low, conf.high, p.value) |>
    dplyr::mutate(
        category = dplyr::case_when(
            variable %in% baseline_terms ~ "Observations",
            variable %in% biomarkers_terms ~ "Biomarkers",
            variable %in% tissue_oxygenation_terms ~ "Tissue oxygenation",
            variable %in% microvascular_function_terms ~ "Video microscopy"
        ),
        category = factor(category, levels = c("Observations", "Biomarkers", "Tissue oxygenation", "Video microscopy")),
        label = dplyr::recode(label, !!!forest_label_lookup),
        significant = dplyr::if_else(p.value < 0.05, "p < 0.05", "p \u2265 0.05"),
        estimate_plot = pmin(pmax(estimate, forest_axis_limits[1]), forest_axis_limits[2]),
        low_clipped = conf.low < forest_axis_limits[1],
        high_clipped = conf.high > forest_axis_limits[2],
        xmin_plot = pmax(conf.low, forest_axis_limits[1]),
        xmax_plot = pmin(conf.high, forest_axis_limits[2]),
        or_label = sprintf("%.2f (%.2f\u2013%.2f)", estimate, conf.low, conf.high)
    ) |>
    dplyr::arrange(category, estimate)

#category header rows are inserted into the axis itself so they share the same text column as the variable labels
forest_category_headers = forest_plot_data |>
    dplyr::distinct(category) |>
    dplyr::mutate(label = as.character(category), is_header = TRUE)

forest_plot_data = forest_plot_data |>
    dplyr::mutate(is_header = FALSE) |>
    dplyr::bind_rows(forest_category_headers) |>
    dplyr::arrange(dplyr::desc(category), is_header, estimate) |>
    dplyr::mutate(label = forcats::fct_inorder(label))

forest_header_labels = forest_category_headers$label

#clipped CI whiskers are drawn as arrows pointing off the edge of the plot
univariable_forest_plot = ggplot(forest_plot_data, aes(y = label)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_segment(
        data = dplyr::filter(forest_plot_data, !is_header, !low_clipped),
        aes(x = estimate_plot, xend = conf.low, yend = label, colour = significant)
    ) +
    geom_segment(
        data = dplyr::filter(forest_plot_data, !is_header, low_clipped),
        aes(x = estimate_plot, xend = xmin_plot, yend = label, colour = significant),
        arrow = arrow(length = unit(0.1, "cm"), type = "closed")
    ) +
    geom_segment(
        data = dplyr::filter(forest_plot_data, !is_header, !high_clipped),
        aes(x = estimate_plot, xend = conf.high, yend = label, colour = significant)
    ) +
    geom_segment(
        data = dplyr::filter(forest_plot_data, !is_header, high_clipped),
        aes(x = estimate_plot, xend = xmax_plot, yend = label, colour = significant),
        arrow = arrow(length = unit(0.1, "cm"), type = "closed")
    ) +
    geom_point(data = dplyr::filter(forest_plot_data, !is_header), aes(x = estimate_plot, colour = significant), size = 2.5) +
    geom_text(
        data = dplyr::filter(forest_plot_data, !is_header),
        aes(x = forest_axis_limits[2], label = or_label),
        hjust = -0.08, size = 3, colour = "grey20"
    ) +
    scale_x_log10(breaks = c(0.5, 1, 2), expand = expansion(mult = 0)) +
    scale_y_discrete(drop = FALSE) +
    scale_colour_manual(values = c("p < 0.05" = "#c0392b", "p \u2265 0.05" = "grey40")) +
    coord_cartesian(xlim = forest_axis_limits, clip = "off") +
    labs(
        x = "Odds ratio (95% CI)",
        y = NULL,
        colour = NULL
    ) +
    theme_classic(base_size = 12) +
    theme(
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust = 0),
        legend.position = "bottom",
        legend.justification = "center",
        plot.margin = margin(t = 5.5, r = 100, b = 5.5, l = 5.5)
    )

#bold the category header rows directly in the rendered axis text so they line up with the variable labels
univariable_forest_grob = ggplotGrob(univariable_forest_plot)
axis_text_grob_index = which(univariable_forest_grob$layout$name == "axis-l")
axis_text_grob = univariable_forest_grob$grobs[[axis_text_grob_index]]
axis_title_grob = axis_text_grob$children$axis$grobs[[2]]
axis_label_text_grob = axis_title_grob$children[[1]]
axis_label_text_grob$gp$font = ifelse(axis_label_text_grob$label %in% forest_header_labels, 2, 1)
axis_title_grob$children[[1]] = axis_label_text_grob
axis_text_grob$children$axis$grobs[[2]] = axis_title_grob
univariable_forest_grob$grobs[[axis_text_grob_index]] = axis_text_grob

ggsave(
    filename = "output/figures/forest_plots/univariable_forest_plot.png",
    plot = univariable_forest_grob,
    width = 9,
    height = 0.35 * nrow(forest_plot_data) + 2,
    dpi = 300
)
