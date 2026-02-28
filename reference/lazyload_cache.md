# Lazyload Cache

Lazyload Cached label(s) or a whole directory.

## Usage

``` r
lazyload_cache_dir(
  path = "./cache",
  envir = parent.frame(),
  ask = FALSE,
  verbose = TRUE,
  ...
)

lazyload_cache_labels(
  labels,
  path = "./cache/",
  envir = parent.frame(),
  verbose = TRUE,
  filter,
  ...
)
```

## Arguments

- path:

  the path to the cache directory.

- envir:

  the environment to load the objects into

- ask:

  if TRUE ask the user to confirm loading each database found in `path`

- verbose:

  if TRUE display the chunk labels being loaded

- ...:

  additional arguments passed to
  [`list.files`](https://rdrr.io/r/base/list.files.html).

- labels:

  a character vector of the chunk labels to load.

- filter:

  an optional function passed to `lazyLoad`. when called on a character
  vector of object names returns a logical vector: only objects for
  which this is true will be loaded.

## Details

These functions helpful for loading cached chunks into an interactive R
session. Consider the following scenario: you use knitr and have cached
chunks for lazyloading. You've created the document, close up your IDE
and move on to the next project. Later, you revisit the initial project
and need to retrieve the objects created in the cached chunks. One
option is to reevaluate all the code, but this could be time consuming.
The other option is to use `lazyload_cache_labels` or
`lazyload_cache_dir` to quickly (lazy)load the chunks into an active R
session.

Use `lazyload_cache_dir` to load a whole directory of cached objects.

Use `lazyload_cache_labels` to load and explicit set of cached chunks.

## Examples

``` r
# this example is based on \url{https://stackoverflow.com/a/41439691/1104685}

# create a temp directory for a and place a .Rmd file within
tmpdir <- normalizePath(paste0(tempdir(), "/llcache_eg"), mustWork = FALSE)
tmprmd <- tempfile(pattern = "report", tmpdir = tmpdir, fileext = "Rmd")
dir.create(tmpdir)
oldwd <- getwd()
setwd(tmpdir)

# build and example .Rmd file
# note that the variable x is created in the first chunck and then over
# written in the second chunk
cat("---",
    "title: \"A Report\"",
    "output: html_document",
    "---",
    "",
    "```{r first-chunk, cache = TRUE}",
    "mpg_by_wt_hp <- lm(mpg ~ wt + hp, data = mtcars)",
    "x_is_pi <- pi",
    "x <- pi",
    "```",
    "",
    "```{r second-chunk, cache = TRUE}",
    "mpg_by_wt_hp_am <- lm(mpg ~ wt + hp + am, data = mtcars)",
    "x_is_e <- exp(1)",
    "x <- exp(1)",
    "```",
    sep = "\n",
    file = tmprmd)

# knit the file.  evaluate the chuncks in a new environment so we can compare
# the objects after loading the cache.
kenv <- new.env()
knitr::knit(input = tmprmd, envir = kenv)
#> 
#> 
#> processing file: /tmp/Rtmp7YTT9S/llcache_eg/report199765572904Rmd
#> 1/4               
#> 2/4 [first-chunk] 
#> 3/4               
#> 4/4 [second-chunk]
#> output file: report199765572904Rmd.txt
#> [1] "report199765572904Rmd.txt"

# The objects defined in the .Rmd file are now in kenv
ls(envir = kenv)
#> [1] "mpg_by_wt_hp"    "mpg_by_wt_hp_am" "x"               "x_is_e"         
#> [5] "x_is_pi"        

# view the cache
list.files(path = tmpdir, recursive = TRUE)
#> [1] "cache/__packages"                                         
#> [2] "cache/first-chunk_4e3140196fb03528521fad11269d001c.RData" 
#> [3] "cache/first-chunk_4e3140196fb03528521fad11269d001c.rdb"   
#> [4] "cache/first-chunk_4e3140196fb03528521fad11269d001c.rdx"   
#> [5] "cache/second-chunk_8e4f33e2310468ad8f108c019d707830.RData"
#> [6] "cache/second-chunk_8e4f33e2310468ad8f108c019d707830.rdb"  
#> [7] "cache/second-chunk_8e4f33e2310468ad8f108c019d707830.rdx"  
#> [8] "report199765572904Rmd"                                    
#> [9] "report199765572904Rmd.txt"                                

# create three more environment, and load only the first chunk into the
# first, and the second chunck into the second, and then load all of the
# cache into the third
env1 <- new.env()
env2 <- new.env()
env3 <- new.env()

lazyload_cache_labels(labels = "first-chunk",
                      path = paste0(tmpdir, "/cache"),
                      envir = env1)
#> Lazyloading /tmp/Rtmp7YTT9S/llcache_eg/cache/first-chunk_4e3140196fb03528521fad11269d001c

lazyload_cache_labels(labels = "second-chunk",
                      path = paste0(tmpdir, "/cache"),
                      envir = env2)
#> Lazyloading /tmp/Rtmp7YTT9S/llcache_eg/cache/second-chunk_8e4f33e2310468ad8f108c019d707830

lazyload_cache_dir(path = paste0(tmpdir, "/cache"), envir = env3)
#> Lazyloading: /tmp/Rtmp7YTT9S/llcache_eg/cache/first-chunk
#> Lazyloading: /tmp/Rtmp7YTT9S/llcache_eg/cache/second-chunk

# Look at the conents of each of these environments
ls(envir = kenv)
#> [1] "mpg_by_wt_hp"    "mpg_by_wt_hp_am" "x"               "x_is_e"         
#> [5] "x_is_pi"        
ls(envir = env1)
#> [1] "mpg_by_wt_hp" "x"            "x_is_pi"     
ls(envir = env2)
#> [1] "mpg_by_wt_hp_am" "x"               "x_is_e"         
ls(envir = env3)
#> [1] "mpg_by_wt_hp"    "mpg_by_wt_hp_am" "x"               "x_is_e"         
#> [5] "x_is_pi"        

# The regression models are only fitted once an should be the same in all the
# environments where they exist, as should the variables x_is_e and x_is_pi
all.equal(kenv$mpg_by_wt_hp, env1$mpg_by_wt_hp)
#> [1] TRUE
all.equal(env1$mpg_by_wt_hp, env3$mpg_by_wt_hp)
#> [1] TRUE

all.equal(kenv$mpg_by_wt_hp_am, env2$mpg_by_wt_hp_am)
#> [1] TRUE
all.equal(env2$mpg_by_wt_hp_am, env3$mpg_by_wt_hp_am)
#> [1] TRUE

# The value of x, however, should be different in the differnet
# environments.  For kenv, env2, and env3 the value should be exp(1) as that
# was the last assignment value.  In env1 the value should be pi as that is
# the only relevent assignment.

all.equal(kenv$x, exp(1))
#> [1] TRUE
all.equal(env1$x, pi)
#> [1] TRUE
all.equal(env2$x, exp(1))
#> [1] TRUE
all.equal(env3$x, exp(1))
#> [1] TRUE

# cleanup
setwd(oldwd)
unlink(tmpdir, recursive = TRUE)
```
