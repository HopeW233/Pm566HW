Assignment 04 - HPC and SQL
================
Lin Wang
10/27/2020

## HPC

### Problem 1: Make sure your code is nice

Rewrite the following R functions to make them faster. It is OK (and
recommended) to take a look at Stackoverflow and Google

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}

fun1alt <- function(mat) {
  for (i in 1:n) {
    ans[i] <- <- rowsum(mat, group =[i, ] )
  }
  ans 
}

# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}

fun2alt <- function(mat) {
  # YOUR CODE HERE
}


# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), unit = "relative", check = "equivalent"
)

# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), unit = "relative", check = "equivalent"
)
```

The last argument, check = “equivalent”, is included to make sure that
the functions return the same result.

### Problem 2: Make things run faster with parallel computing

The following function allows simulating PI

``` r
sim_pi <- function(n = 1000, i = NULL) {
  p <- matrix(runif(n*2), ncol = 2)
  mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000) # 3.132
```

In order to get accurate estimates, we can run this function multiple
times, with the following code:

``` r
# This runs the simulation a 4,000 times, each with 10,000 points
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

Rewrite the previous code using `parLapply()` to make it run faster.
Make sure you set the seed using `clusterSetRNGStream()`:

``` r
library(parallel)
# YOUR CODE HERE
cl <- makePSOCKcluster(4)
clusterSetRNGStream(cl, iseed = 1231)

system.time({
  clusterExport(cl, varlist = "ans")
  ans <- parLapply(cl, 1:4000, sim_pi)
  print(mean(ans))
  stopCluster(cl)
})
```

## SQL

Setup a temporary database by running the following chunk

``` r
# install.packages(c("RSQLite", "DBI"))

library(RSQLite)
```

    ## Warning: package 'RSQLite' was built under R version 4.0.3

``` r
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)
```

When you write a new chunk, remember to replace the `r` with `sql,
connection=con`. Some of these questions will require you to use an
inner join. Read more about them here
<https://www.w3schools.com/sql/sql_join_inner.asp>

## Question 1

How many movies is there available in each `rating` category.

``` sql
SELECT rating, COUNT(*) AS count
FROM film
GROUP BY rating
ORDER BY count DESC
```

<div class="knitsql-table">

| rating | count |
| :----- | ----: |
| PG-13  |   223 |
| NC-17  |   210 |
| R      |   195 |
| PG     |   194 |
| G      |   180 |

5 records

</div>

## Question 2

What is the average replacement cost and rental rate for each `rating`
category.

``` sql
SELECT rating,
       AVG(replacement_cost) AS avg_replacement_cost,
       AVG(rental_rate)      AS avg_rental_rate
FROM film
GROUP BY rating
```

<div class="knitsql-table">

| rating | avg\_replacement\_cost | avg\_rental\_rate |
| :----- | ---------------------: | ----------------: |
| G      |               20.12333 |          2.912222 |
| NC-17  |               20.13762 |          2.970952 |
| PG     |               18.95907 |          3.051856 |
| PG-13  |               20.40256 |          3.034843 |
| R      |               20.23103 |          2.938718 |

5 records

</div>

## Question 3

Use table `film_category` together with `film` to find the how many
films there are with each category ID

``` sql
SELECT c.category_id, COUNT(*) AS count
FROM film AS f
INNER JOIN film_category AS c
ON f.film_id = c.film_id
GROUP BY c.category_id
```

<div class="knitsql-table">

| category\_id | count |
| :----------- | ----: |
| 1            |    64 |
| 2            |    66 |
| 3            |    60 |
| 4            |    57 |
| 5            |    58 |
| 6            |    68 |
| 7            |    62 |
| 8            |    69 |
| 9            |    73 |
| 10           |    61 |

Displaying records 1 - 10

</div>

## Question 4

Incorporate table `category` into the answer to the previous question to
find the name of the most popular category.

``` sql
SELECT c.category_id, COUNT(*) AS count, ca.name
FROM film AS f
INNER JOIN film_category AS c
ON f.film_id = c.film_id
INNER JOIN category AS ca
ON c.category_id = ca.category_id
GROUP BY c.category_id
ORDER BY count DESC
```

<div class="knitsql-table">

| category\_id | count | name        |
| -----------: | ----: | :---------- |
|           15 |    74 | Sports      |
|            9 |    73 | Foreign     |
|            8 |    69 | Family      |
|            6 |    68 | Documentary |
|            2 |    66 | Animation   |
|            1 |    64 | Action      |
|           13 |    63 | New         |
|            7 |    62 | Drama       |
|           14 |    61 | Sci-Fi      |
|           10 |    61 | Games       |

Displaying records 1 - 10

</div>

  - The most popular category is Sports with 74 films.
