###how to fix header issue with HIC data going into R. From Iris
read_csv("data/file.csv",
         locale=locale(encoding="latin1"))

