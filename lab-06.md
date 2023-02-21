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

Now it’s time to figure out a better way of representing this fisheries
data. My first thought is that a histogram just showing the distribution
of fish production per country might be useful. This shows that there
are 3 countries that are pretty significant outliers, while most
countries are producing 1 million tons or less. Not bad, but we can
probably do better.

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
  theme_bw()+
  labs(title = "Distribution of total fish production per country",
       y = "Number of countries",
       x = "Fish production (in million tons)")
```

    ## Warning: Removed 2 rows containing missing values (`geom_bar()`).

![](lab-06_files/figure-gfm/doin-stuff-1.png)<!-- -->

Since the author of the original visualization only included 6 countries
in the legend of their plot (China, Indonesia, Vietnam, India, the
United States, and Russia), I’m going to start by assuming that those
are the primary countries of interest. Let’s try to improve on the pie
chart by representing the portion of total fish production accounted for
by each of these 6 countries, as well as all other countries with a
stacked bar chart. We can get some pretty neat information from this
plot! For example, we can see that China accounts for more of the total
aquaculture than all other countries combined! However, countries that
aren’t included in this plot have more of a stronghold the amount of
fish production coming from capture. Let’s generate a second,
complimentary plot to learn more about other major fish producing
countries.

The second plot shows a similar stacked bar for other countries that
produce at least 1 million tons of fish. One striking thing is that,
when we take out the major aquaculture countries that are producing tens
of millions of tons of fish, the majority of fish are now produced
through capture. It looks like Japan, the Phillipines, and Bangladesh
are noteworthy producers of fish, with a number of other countries also
taking up sizeable chunks of the fish market.

``` r
fish_long <- fisheries %>%
  pivot_longer(cols = c(-total, -country), names_to = "type") %>%
  mutate(value = as.numeric(value))%>%
  mutate(new_value = value/1000000)

fish_long %>%
  mutate(new_country = case_when(
    country == "China" ~ "China",
    country == "Indonesia" ~ "Indonesia",
    country == "India" ~ "India",
    country == "Vietnam" ~ "Vietnam",
    country == "United States" ~ "United States",
    country == "Russia" ~ "Russia",
    !country %in% c("China", "India", "Indonesia", "Vietnam", "United States", "Russia") ~ "Other")) %>%
  ggplot(aes(x = type, y = new_value, fill = fct_reorder(new_country, total)))+
  geom_bar(stat = "identity")+
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 115))+
  scale_fill_viridis_d()+
  theme_bw()+
  labs(title = "Fish production in 6 major fish countries",
       y = "Fish production (in million tons)",
       x = "Source",
       fill = "Country")
```

![](lab-06_files/figure-gfm/try-something-better-1.png)<!-- -->

``` r
fish_long %>%
  filter(!country %in% c("China", "India", "Indonesia", "Vietnam", "United States", "Russia"), total > 1000000) %>%
  ggplot(aes(x = type, y = new_value, fill = fct_reorder(country, value)))+
  geom_bar(stat = "identity")+
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 35))+
  scale_fill_viridis_d()+
  theme_bw()+
  labs(title = "Fish production in other major fish countries",
       y = "Fish production (in million tons)",
       x = "Source",
       fill = "Country")
```

![](lab-06_files/figure-gfm/try-something-better-2.png)<!-- -->

I want to try one more plot that I think might do a nice job of telling
the whole fish production story. This graph shows all the information
from the two barplots before all at once. Mainly, what this plot shows
is that there are a couple countries producing an enourmous amount of
fish, largely through aquaculture, as well as lots of other countries
producing sizeable, but relatively small amounts of fish, largely
through capture.

``` r
fish_long %>%
  filter(total > 1000000) %>%
  ggplot(aes(x = fct_reorder(country, value), fill = type, y = new_value))+
  geom_bar(stat = "identity")+
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 84))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = -40, hjust = -0.05))+
  labs(title = "Fish production in countries producing at least 1 million tons of fish",
       x = "Country",
       y = "Fish production (in million tons)",
       fill = "Source")
```

![](lab-06_files/figure-gfm/one-last-plot-1.png)<!-- -->

### Stretch Exercises

``` r
library(mosaicData)

data(Whickham)
```

### Exercise 1

The data is most likely observational, because it is described as a
survey conducted at 2 time points, and also because the IV is whether
someone is a smoker. Given the health risks involved in smoking, it is
unlikely that the researchers would have wanted to, or have been
ethically permitted to, randomly assigned people to become smokers for
20 years.

### Exercise 2

There are 1314 observations in the dataset, each of which represents a
woman in the town of Whickham.

### Exercise 3

There are 3 variables in the dataset, which represent:

1.  Whether the participant was still alive 20 years after the initial
    survey (dichotomous)
2.  The participants smoker status at the time of the initial survey
    (dichotomous)
3.  The participants age at the time of the initial survey (continuous)

``` r
ggplot(Whickham, aes(x = outcome))+
  geom_bar()+
  theme_bw()+
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1000))+
  labs(title = "Mortality of participants at Time 2",
       x = "Outcome",
       y = "Count")
```

![](lab-06_files/figure-gfm/viz-vars-1.png)<!-- -->

``` r
ggplot(Whickham, aes(x = smoker))+
  geom_bar()+
  theme_bw()+
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 800))+
  labs(title = "Smoking status of participants at Time 1",
       x = "Do you smoke?",
       y = "Count")
```

![](lab-06_files/figure-gfm/viz-vars-2.png)<!-- -->

``` r
ggplot(Whickham, aes(x = age))+
  geom_histogram(color = "black", fill = "grey80")+
  theme_bw()+
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 100))+
  labs(title = "Distribution of ages at Time 1",
       x = "Age",
       y = "Frequency")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](lab-06_files/figure-gfm/viz-vars-3.png)<!-- -->

### Exercise 4

My expectation would be that smoking would be associated with worse
health outcomes. In this case, that would mean greater or earlier
mortality among smokers.

### Exercise 5

Based on the visualization, people who reported being smokers at time 1
don’t seem to differ much from people who reported not being smokers in
terms of their likelihood of surviving to time 2. The conditional
probabilities reveal that, apparently contrary to my prediction, smokers
were descriptively less likely to have died by time 2 than non-smokers.

``` r
ggplot(Whickham, aes(x = smoker, fill = outcome))+
  geom_bar(position = "fill", color = "black")+
  scale_fill_manual(values = c("grey75", "black"))+
  theme_bw()+
  labs(title = "Probability of mortality by smoker status",
       fill = "Mortality",
       x = "Smoker status",
       y = "Probability")
```

![](lab-06_files/figure-gfm/viz-results-1.png)<!-- -->

``` r
Whickham %>%
  count(smoker, outcome) %>%
  group_by(smoker) %>%
  mutate(prob_dead = n/sum(n))
```

    ## # A tibble: 4 × 4
    ## # Groups:   smoker [2]
    ##   smoker outcome     n prob_dead
    ##   <fct>  <fct>   <int>     <dbl>
    ## 1 No     Alive     502     0.686
    ## 2 No     Dead      230     0.314
    ## 3 Yes    Alive     443     0.761
    ## 4 Yes    Dead      139     0.239

### Exercise 6

``` r
Whickham <- Whickham %>%
  mutate(age_cat = case_when(
    age <= 44 ~ "18 - 44",
    age > 44 & age <= 64 ~ "45 - 64",
    age > 64 ~ "65+"))
```

### Exercise 7

Not too surprisingly, the likelihood of mortality was much higher for
people who were 65 and above at time 1, and much lower for people
between 18-44. For younger people, the those who were smokers were
somewhat more likely to have died by time 2 than non-smokers. For 65 and
older people, if we were to only focus on comparing the “dead” columns,
it would look like more non-smokers than smokers are dying. However,
when we compare the size of the bars, we can see that very few smokers
above the age of 65 were ever surveyed, and a higher proportion of them
died compared to non-smokers over 65. The original plot was misleading
about the effect, because a majority of 65+ individuals died regardless
of their smoker status and there weren’t very many 65+ smokers available
to sample in the first place. Combining all the age categories together
masked these nuances of the data. This is an example of Simpson’s
paradox.

``` r
ggplot(Whickham, aes(x = smoker, fill = outcome))+
  facet_wrap(~ age_cat)+
  geom_bar(color = "black")+
  scale_fill_manual(values = c("grey75", "black"))+
  theme_bw()+
  labs(title = "Relationship between mortality and smoker status by age",
       fill = "Mortality",
       x = "Smoker status",
       y = "Frequency")
```

![](lab-06_files/figure-gfm/unraveling-age-1.png)<!-- -->

``` r
Whickham %>%
  count(smoker, outcome, age_cat) %>%
  pivot_wider(names_from = age_cat, values_from = n)
```

    ## # A tibble: 4 × 5
    ##   smoker outcome `18 - 44` `45 - 64` `65+`
    ##   <fct>  <fct>       <int>     <int> <int>
    ## 1 No     Alive         327       147    28
    ## 2 No     Dead           12        53   165
    ## 3 Yes    Alive         270       167     6
    ## 4 Yes    Dead           15        80    44
