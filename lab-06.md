Lab 06 - Ugly charts and Simpson’s paradox
================
Ben Hardin
2/14/2023

### Load packages and data

``` r
library(tidyverse) 
library(dsbox)
library(mosaicData) 
```

``` r
#reading data
staff <- read.csv("data/instructional-staff.csv")

#clean the years
staff <- staff %>%
  rename("1975" = X1975, "1989" = X1989, "1993" = X1993, "1995" = X1995, "1999" = X1999, "2001" = X2001, "2003" = X2003, "2005" = X2005, "2007" = X2007, "2009" = X2009, "2011" = X2011)

#reshaping data
staff_long <- staff %>%
  pivot_longer(cols = -faculty_type, names_to = "year") %>%
  mutate(value = as.numeric(value))

staff_long
```

    ## # A tibble: 55 × 3
    ##    faculty_type              year  value
    ##    <chr>                     <chr> <dbl>
    ##  1 Full-Time Tenured Faculty 1975   29  
    ##  2 Full-Time Tenured Faculty 1989   27.6
    ##  3 Full-Time Tenured Faculty 1993   25  
    ##  4 Full-Time Tenured Faculty 1995   24.8
    ##  5 Full-Time Tenured Faculty 1999   21.8
    ##  6 Full-Time Tenured Faculty 2001   20.3
    ##  7 Full-Time Tenured Faculty 2003   19.3
    ##  8 Full-Time Tenured Faculty 2005   17.8
    ##  9 Full-Time Tenured Faculty 2007   17.2
    ## 10 Full-Time Tenured Faculty 2009   16.8
    ## # … with 45 more rows

### Exercise 1

Here is a new graph displaying how the proportion of different faculty
types making up new hires has changed over time.

``` r
staff_long %>%
  ggplot(aes(x = year, y = value, group = faculty_type, color = faculty_type))+
  geom_line(size = 1.1)+
  geom_point(aes(shape = faculty_type), size = 1.5)+
  theme_bw()+
  labs(title = "Trends in faculty hires (1975 - 2011)",
       y = "Percentage of faculty hires",
       x = "Year",
       color = "Faculty type",
       shape = "Faculty type")
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.

![](lab-06_files/figure-gfm/making-first-plot-1.png)<!-- -->

### Exercise 2

I do think the plot does an ok job displaying how the proportion of
part-time faculty has increased dramatically compared to other
instructors, but this piece of information is obscured a little bit by
the amount of things happening in the plot, and that they are all about
equally prominent in the graph. For that reason, I want to propose
making 2 changes to the plot.

1.  Combining all full-time faculty into 1 category, so that we are only
    comparing 3 kinds of instructor types and not cluttering the graph
    quite so much.
2.  Making the line for part-time faculty brighter and more colorful
    than the other lines, so that our attention is drawn to the
    instructor type that we are most interested in learning about.

``` r
staff_long <-  staff_long %>%
  mutate(full_time = case_when(
    faculty_type == "Part-Time Faculty" ~ "Part-Time Faculty",
    faculty_type == "Graduate Student Employees" ~ "Graduate Student Employees",
    faculty_type %in% c("Full-Time Tenured Faculty", "Full-Time Tenure-Track Faculty", "Full-Time Non-Tenure-Track Faculty") ~ "Full-Time Faculty"))

staff_full <- aggregate(value ~ year + full_time, data = staff_long, FUN = sum)
```

``` r
ggplot(data = staff_full, aes(x = year, y = value, group = full_time, color = full_time))+
  geom_line(size = 1.1)+
  geom_point(aes(shape = full_time), size = 1.75)+
  theme_bw()+
  scale_color_manual(values = c("Full-Time Faculty" = "gray50", "Graduate Student Employees" = "gray80", "Part-Time Faculty" = "orange"))+
  labs(title = "Trends in faculty hires (1975 - 2011)",
       y = "Percentage of faculty hires",
       x = "Year",
       color = "Faculty type",
       shape = "Faculty type")
```

![](lab-06_files/figure-gfm/making-new-plot-1.png)<!-- -->

I’m pretty pleased with the result! In particular, it highlights
something pretty striking that would have been really hard to deduce
from the previous graph – that since 2007, the proportion of part-time
faculty hires has actually exceeded the proportion of all full-time
faculty types combined! :0

### Exercise 3

``` r
fisheries <- read_csv("data/fisheries.csv")
```

    ## Rows: 216 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): country
    ## dbl (3): capture, aquaculture, total
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
fisheries %>%
  mutate(new_total = total/1000000) %>%
  filter(total > 100000)%>%
ggplot(aes(x = new_total))+
  geom_histogram(binwidth = 1, color = "black", fill = "blue")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 82))+
  theme_bw()
```

    ## Warning: Removed 2 rows containing missing values (`geom_bar()`).

![](lab-06_files/figure-gfm/doin-stuff-1.png)<!-- -->

``` r
fish_long <- fisheries %>%
  pivot_longer(cols = c(-total, -country), names_to = "type") %>%
  mutate(value = as.numeric(value))

fish_long %>%
  mutate(new_country = case_when(
    country == "China" ~ "China",
    country == "Indonesia" ~ "Indonesia",
    country == "India" ~ "India",
    !country %in% c("China", "India", "Indonesia") ~ "Other")) %>%
  ggplot(aes(x = type, y = value, fill = fct_reorder(new_country, total)))+
  geom_bar(stat = "identity")
```

![](lab-06_files/figure-gfm/try-something-better-1.png)<!-- -->

``` r
fish_long %>%
  filter(!country %in% c("China", "India", "Indonesia"), total > 1000000)%>%
  ggplot(aes(x = type, y = value, fill = fct_reorder(country, total)))+
  geom_bar(stat = "identity")
```

![](lab-06_files/figure-gfm/try-something-better-2.png)<!-- -->
