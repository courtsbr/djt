# djt

[![Travis-CI Build Status](https://travis-ci.org/abjur/djt.svg?branch=master)](https://travis-ci.org/abjur/djt) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/courtsbr/djt?branch=master&svg=true)](https://ci.appveyor.com/project/courtsbr/djt)

## Overview

The `djt` R package allows you to download and parse Brazilian DJTs
(*Diários da Justiça do Trabalho*). Its functions are simple and well documented,
supplying the user with a consistent and robust toolbox to explore these PDFs.

To install `djt`, run the code below:

```r
# install.packages("devtools")
devtools::install_github("courtsbr/djt")
```

## Usage

Downloading DJTs is very simple. By using the `download_djt()` function
with its arguments, you can select a TRT (*Tribunal Regional do Trabalho*), a
booklet (*caderno*), and start and end dates to download all DJTs that fit the
specified filters.

```r
# Download all TRT02 booklets from 2017-10-19
download_djt(trt = 2, path = "~/Desktop/djt_files", booklet = "all",
             date_min = "2017-10-19", date_max = "2017-10-19")
#> # A tibble: 2 x 6
#>    page    id                                                           title       date
#>   <int> <int>                                                           <chr>     <date>
#> 1     1     0     Edição 2337/2017 - Caderno do TRT da 2ª Região - Judiciário 2017-10-19
#> 2     1     1 Edição 2337/2017 - Caderno do TRT da 2ª Região - Administrativo 2017-10-19
#> # ... with 2 more variables: file <chr>, result <chr>
```

After you have downloaded as many DJTs as you want, you can start parsing them.
Functions like `pdf_to_text()`, `find_all()`, `chop_at()`, and `pre_process()`
are your best tools for this step.
