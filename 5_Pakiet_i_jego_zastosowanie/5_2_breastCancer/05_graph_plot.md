Wykres (Rysunek 7) jest oparty na plikach `g_MAP_edge_list.csv` i `g_MAP_vertices_colors.csv` utworzonych w `04_save_graph.R`.

Został on utworzony za pomocą [Cytoscape](https://cytoscape.org/).

Jeśli użyjemy pliku `g_python_edge_list.csv` (również utworzonego w `04_save_graph.R`), odtworzymy wykres na rysunku 6 z Graczyk et al. (2022) <doi:10.1214/22-AOS2174> ("Model selection in the space of Gaussian models invariant by symmetry").

Układ i kolory:
1. styl default-black
2. Layout "Edge-weighted Spring Embedded Layout" oparty na kolumnie "value" pliku `g_MAP_edge_list.csv`. Zamierzaliśmy użyć kolumny "for_embedding", ale okazała się ona gorsza, więc ją odrzuciliśmy. To była podstawa układu, ale później dostosowaliśmy rozmieszczenie węzłów zgodnie z naszymi preferencjami.
3. Kolory wierzchołków zostały oparte na kolumnie "colors" pliku `g_MAP_vertices_colors.csv`.
