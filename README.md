
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

<!-- badges: end -->

# mixpanel

Fetch Mixpanel data from R.

## Installation

You can install the development version from gitlab with the `remotes`
package, make sure you are on Forum network.

``` r
# install.packages("remotes")
remotes::install_git("https://git.weforum.local/intelligence/mixpanel")
```

## Usage

Simply two functions, 1) get the data, 2) parse it to R (optionally).
You will need an API secret which you can specify in the
`get_mixpanel_data` function or as environment variable
(`MIXPANEL_API_SECRET`).

``` r
library(mixpanel)

dir <- "./temp"

raw_data <- get_mixpanel_data()  
md <- parse_mixpanel_data(raw_data, dir)

unlink(dir)
```

⚠️ For reasons unbeknownst to me the above does not work on an actual
temp directory.

You are advised to use the `get_mixpanel_data_by_day` function to split
your call into multiple calls, it is more efficient. Moreover as the
data tends to be large connections to download data over multiple days
have a tendency to hang.

``` r
# get the last 3 days of specific events
ev <- c(
  "Browse page", "Signup", "Signup (API)",
  "Login", "Login (API)"
)
md <- get_mixpanel_data_by_day(
    from_date = Sys.Date() - 3,
    to_date = Sys.Date() - 1, 
    event = ev
  ) %>% 
  parse_mixpanel_data()
```

There is a global `mixpanel_sleep` option to set the sleep time between
calls, it defaults to `1` second.
