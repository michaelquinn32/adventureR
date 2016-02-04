# adventureR

This package contains all of the functions written for posts on the [Big Blog of R Adventures](http://michaelquinn32.github.io/). Obviously, since it accompanies a blog, the package will always be "in development". Nonetheless, this should make it a lot easier to organize, document and share the code produced for this project.

## Posts

### Old School Classification Methods

A very simple suite of functions and methods to accompany a post on classification. During the post, I fit and create predictions with Naive Bayes and Linear Discriminant Analysis.

### Simulating the Three Geyser Problem

I take three different approaches to solving a [Riddler on FiveThirtyEight.com](http://fivethirtyeight.com/features/which-geyser-gushes-first/). In the problem, we are visiting a national park where a set of geysers erupt at fixed intervals. We do not know when these intervals first began, but we need to find the probabilities for each geyser erupting first.

### Variations on the bootstrap 

The standard version of the bootstrapping algorithm involves resampling data with replacement in order to repeatedly calculate a statistic on random data of the same size. In this post, I experiment with alternative sampling algorithms, including *subsampling* and *m* out of *n* bootstrapping. Both take a sample smaller than the original data, but the former doesn't use replace while the latter does.

## Installation

This package is intended for very limited use, since most of the functions a drafted to illustrate something in a blog post. But if you would like to experiment with its few functions, feel free to install it.

```R
# install.packages("devtools")
devtools::install_github("michaelquinn32/adventureR")
```
