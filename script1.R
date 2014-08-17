Title
========================================================


```{r}
if (file.exists("activity.csv")) {
  print("File already unsipped.")
} else {
  print("Unzipping File")
  unzip("activity.zip")
}

data1<-read.csv("activity.csv")
```

You can also embed plots, for example:

```{r fig.width=7, fig.height=6}

```

