
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rawrr

<!-- badges: start -->

<!-- badges: end -->

<img src="rawrr.jpg" width="40%" />

This package gives you access to Reddit’s API in order to gather
information on *threads*, *subreddits*, and *users.*

Under the hood, it calls Python’s
[**PRAW**](https://praw.readthedocs.io/) library via the
[**reticulate**](https://rstudio.github.io/reticulate/) package.

RAWRR stands for **Reddit API Wrapper for R using Reticulate** ;)

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("acastroaraujo/rawrr")
```

It currently has these functions:

  - `install_praw()`: A simple wrapper for
    `reticulate::py_install("praw", method = method, conda = conda)`

  - `init_reddit(username, password, client_id, client_secret)`

  - `download_sub_urls()`: Downloads urls by subreddit

  - `download_keyword_urls()`: Downloads urls by [search
    query](https://www.reddit.com/wiki/search)

  - `extract_thread()`: Extracts a thread from a path

  - `add_threads()` Adds thread information to a data frame, like the
    one produced by the `download_*_urls()` functions

  - `create_aff_net()`: Creates a bipartite network of users and recent
    subreddits from a list of users

  - `user_net()`: Creates an edge list (i.e. a network) of user
    interactions from a data frame, like the one produced by
    `add_threads()`
