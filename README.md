# BrainEffeX_utils: Neuroimaging Effect Size Visualization and Meta-Analysis

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-%3E%3D3.6.0-blue.svg)](https://www.r-project.org/)

## Overview

**BrainEffeX_utils** is an R package that provides utilities for visualizing and analyzing neuroimaging effect sizes. This package is specifically designed to support the [BrainEffeX Shiny web app](https://github.com/neuroprismlab/BrainEffeX) and related manuscripts by providing tools for:

- **Effect size visualization** 
- **Effect size meta-analyses** 

The package handles both task-based activation (voxel-wise) and connectivity (edge-wise) neuroimaging data, supporting Cohen's d and R² effect size measures.

## Key Features

### Visualization Types
- **SimCI Plots**: Plot effect sizes in order of increasing magnitude, with simultaneous confidence intervals
- **Density Plots**: Effect size density distributions with optional binning
- **Activation Effect Maps**: 3D brain visualizations of effect sizes
- **Connectivity Matrices**: Effect size matrices for FC studies
- **Power Analysis Plots**: Statistical power and sample size recommendations

### Meta-Analysis
- **Effect Size Meta-Analysis**: Perform meta-analyses of effect sizes across multiple studies

## Example Usage

We included a vignette that shows how to use BrainEffeX utils for plotting a single study's effect sizes. To access the vignette, make sure you install the package with `install_github("neuroprismlab/braineffex_utils", build_vignettes = TRUE)`. Load the package with `library(BrainEffeX.utils)`, then run `browseVignettes("BrainEffeX.utils")`. This should take you to a page with any available vignettes which you can view as an html file or as code. 

To see an example of how the package was used to generate figures for the BrainEffeX application, check out [this notebook](github.com/neuroprismlab/calculate_effex/effect_size/effect_size_example.ipynb).

## Installation

### From GitHub (Recommended)

```r
# Install devtools if you haven't already
if (!require(devtools)) {
  install.packages("devtools")
}

# Install BrainEffeX_utils
devtools::install_github("neuroprismlab/BrainEffeX_utils", build_vignettes = TRUE)

# Load the package
library(BrainEffeX.utils)
```

### Dependencies

The package requires several neuroimaging and statistical R packages:

```r
# Core dependencies (automatically installed)
install.packages(c(
  "ggplot2", "metafor", "oro.nifti", "neurobase", 
  "reshape2", "pwr", "ggbeeswarm", "ggpubr",
  "colorspace", "png", "grid"
))
```

## Citation
[![DOI](https://zenodo.org/badge/904945550.svg)](https://doi.org/10.5281/zenodo.17238783)

If you use BrainEffeX_utils in your research, please cite:

```
@misc{braineffex_utils,
  title = {BrainEffeX_utils},
  author = {Shearer, Hallee and Noble, Stephanie},
  year = {2025},
  url = {https://github.com/neuroprismlab/BrainEffeX_utils},
  doi = {10.5281/zenodo.17238783}
}
```

To get citation information in R:

```r
citation("BrainEffeX.utils")
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Support

- **Issues**: Report bugs or request features on [GitHub Issues](https://github.com/neuroprismlab/BrainEffeX_utils/issues)
- **Documentation**: Function documentation available via `?function_name`

## Authors

- **Hallee Shearer** 
- **Stephanie Noble** 

---

*Part of the NeuroPRISM Lab at Northeastern University*
