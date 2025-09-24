# Mallinnusprojekti

## Aineisto

Viimeisin tallennettu aineisto löytyy **Projektikurssi** Seafile-kansiosta.  
Aineisto on jaettu kahteen osaan: **prepartum**- ja **postpartum**-aikajaksoihin.

Viimeisin aineistopäivitys on päivätty **24.9.2025**.

Tallennettujen tiedostojen nimet:
- `pregnancy_2025-09-24.rds`
- `postpartum_2025-09-24.rds`

```r
pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-09-24.rds"))  %>% as.data.frame()
postpartum <- readRDS(file.path(dir.path, "postpartum_2025-09-24.rds")) %>% as.data.frame()
```


## Tausta

Kirjoita tähän lyhyt kuvaus projektin taustasta, tavoitteista ja aineiston käytöstä.
