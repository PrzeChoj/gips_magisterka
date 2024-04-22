# gips_magisterka

To repozytorium zawiera kod użyty do pracy magisterskiej Adama Przemysława Chojeckiego dotyczącej pakietu `gips` dla języka R.

Kod jest zaprojektowany do działania z `gips` 1.2.1.

Aby go uruchomić, należy zainstalować następujące pakiety (w podanej kolejności):
```
install.packages("gips")

install.packages(c("ggplot2", "HSAUR2", "DAAG", "dplyr", "tidyr", "BiocManager",
                   "igraph", "gRim", "magrittr", "huge", "mvtnorm"))
BiocManager::install(c("GEOquery", "RBGL"))
install.packages("rags2ridges")
```

Wszystkie foldery są niezależne i tworzą wykresy do folderu `plots`. Wewnątrz folderów `3_(1|2|3)_*` skrypty są ponumerowane w kolejności uruchamiania. Jeśli kod tworzy jakieś pliki wyjściowe, są one zapisywane w tym repozytorium w odpowiednich folderach `data`. Czasochłonne skrypty mają w pierwszym wierszu informację o tym, ile czasu zajęło ich uruchomienie.
