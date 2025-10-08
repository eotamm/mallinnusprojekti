# Mallinnusprojekti

## Aineisto

Viimeisin tallennettu aineisto löytyy **Projektikurssi** Seafile-kansiosta.  
Aineisto on jaettu kahteen osaan: **prepartum**- ja **postpartum**-aikajaksoihin.

Viimeisin aineistopäivitys on päivätty **9.10.2025**.

Tallennettujen tiedostojen nimet:
- `pregnancy_2025-10-09.rds`
- `postpartum_2025-10-09.rds`

```r
pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-10-09.rds"))  %>% as.data.frame()
postpartum <- readRDS(file.path(dir.path, "postpartum_2025-10-09.rds")) %>% as.data.frame()
```


## Tausta

Aineisto pohjautuu alun perin **Johanna Saarikon väitöskirjatutkimukseen (2024)**.  
Tutkimusaineisto sisältää päivittäisiä raskaus- ja synnytyksen jälkeisen ajan mittaustuloksia.  
Tutkimus tehtiin naisilla, joiden **BMI oli yli 25**, ja aineisto kerättiin käyttäen **2nd generation Oura -sormusta**.

Tutkimukseen osallistui noin **50 naishenkilöä**.

Yleisiksi taustamuuttujiksi on valittu:

- Ikä, koulutus, masennusmuuttuja (EPDS), BMI, synnyttäneisyys sekä raskauden aikana kertynyt paino.  
- Toissijaiset taustamuuttujat ovat käytössä vain **postpartum**-ajan analyysissä.

Mallinnuksen tavoitteena on selvittää liikunnan ja unen välistä yhteyttä eri raskauden vaiheissa, sekä verrata raskausajan ja synnytyksen jälkeisen ajan eroja.  
Tuloksia voidaan hyödyntää äitiysterveyden ja hyvinvointisuositusten kehittämisessä.


## Tutkimuskysymykset

**Primääriset kysymykset:**
- Miten liikunnan määrä on yhteydessä unen laatuun?  
- Miten liikunnan määrä on yhteydessä unen kestoon?  

**Sekundääriset kysymykset:**
- Miten taustamuuttujat vaikuttavat yhteyteen?  
- Miten raskaudenvaihe vaikuttaa yhteyteen?  
- Raskausajan ja raskauden jälkeisen ajan yhteyden vertailu.  
