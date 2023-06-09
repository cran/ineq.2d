% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theil.2d.R
\name{theil.2d}
\alias{theil.2d}
\title{Function performing two-dimensional decomposition of the Theil index}
\usage{
theil.2d(
  data,
  total,
  feature = NULL,
  sources = NULL,
  weights = NULL,
  perc = FALSE
)
}
\arguments{
\item{data}{Data frame containing income data. Must contain at least one
column with numeric values.}

\item{total}{String specifying the name of the column containing data on
total income.}

\item{feature}{String specifying the name of the column containing
information about the feature used for inequality decomposition. If left
blank, total income is not decomposed by feature.}

\item{sources}{Vector containing strings specifying the names of the columns
with data on income sources, the sum of which must be equal to total income.
If left blank, or the same value as in "total" is specified, then total
income is not decomposed by income source.}

\item{weights}{String specifying the name of the column containing population
weights.}

\item{perc}{If set to TRUE, then the function returns percentage shares of
every inequality component in overall inequality. Set to FALSE by default.}
}
\value{
Data frame containing values of components of the Theil index.

Columns of the data frame represent values of the feature used for
decomposition. There can be inequality within groups formed by this feature
and between them - there are twice as much columns as values of the given
feature. Whether a column contains a value of within or between-group
inequality is indicated by ".W" and ".B" suffixes respectively.

Every row of the data frame represents an income source.

Thus, every value in this data frame is the contribution of inequality in
income earned from i-th source by members of j-th population cohort to
overall income inequality.

Remember that overall Theil index, which is the sum of all values in the data
frame, is always positive. However, some components of the index can have
negative contribution to inequality.
}
\description{
The function performs two-dimensional decomposition of the
Theil index according to Giammatteo (2007). That is, the index can be
decomposed by some feature that members of the studied population possess
(e.g., sex, education, age) and their income source at the same time.

The Theil index contains natural logarithm in its formula. This is why
non-positive values of total income are removed during calculation.
}
\examples{
# Load the test data set.
data("us16")

# No decomposition, just Theil index of total income.
result <- theil.2d(us16, "hitotal", weights = "hpopwgt")

# Decomposition of income inequality by gender.
result <- theil.2d(us16, "hitotal", "sex", "hitotal", "hpopwgt")

# Decomposition of income inequality by gender and income source.
result <- theil.2d(us16, "hitotal", "sex", c("hilabour", "hicapital",
"hitransfer"), "hpopwgt")
}
\references{
Giammatteo, M. (2007). The Bidimensional Decomposition of Inequality:
A nested Theil Approach. LIS Working papers, Article 466, 1-30.
}
