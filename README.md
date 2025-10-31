# Data on all Premier League tables ⚽

Data has been pulled from wikipedia. It contains all the premier league tables, which have then been unified, with a column added to indicate the year. 

To get to the data. 

```{r}
# option 1 - load using readr
eurovision <- readr::read_csv("https://raw.githubusercontent.com/andrewmoles2/premier_league_tables/main/data/premier_league_tables.csv")

# option 2 - load using base R read.csv
eurovision <- read.csv("https://raw.githubusercontent.com/andrewmoles2/premier_league_tables/main/data/premier_league_tables.csv")
``` 

For Python:

```{python}
import pandas as pd

eurovision = pd.read_csv("https://raw.githubusercontent.com/andrewmoles2/premier_league_tables/main/data/premier_league_tables.csv")
```
