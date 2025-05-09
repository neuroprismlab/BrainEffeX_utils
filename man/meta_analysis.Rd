% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta_analysis.R
\name{meta_analysis}
\alias{meta_analysis}
\title{Group Studies by Factor}
\usage{
meta_analysis(v, brain_masks, combo_name, grouping_var = "category")
}
\arguments{
\item{v}{A list containing effect size data}

\item{brain_masks}{A list containing the brain masks}

\item{combo_name}{A string specifying the combo to plot - # TODO: note: in app, this is saved directly in v}

\item{group_by}{A string to specify grouping: "orig_stat_type" or "category"}
}
\value{
An updated list with the grouped_by data
}
\description{
This function plots effect sizes (Cohen's d or R-squared) and simulated confidence intervals (CIs)
for a given dataset. It allows optional grouping, visualization, and file saving.
}
\examples{
# Example usage
# meta_analysis(v,v$brain_masks, "pooling.none.motion.none.mv.none")
}
