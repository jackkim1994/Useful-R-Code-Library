# Library of Useful R Codes
This repository is used for a storage of useful R codes that I can use for my EDA with R Programming.
Some useful packages currently on file are:

### Forcats
  - fct_reorder()
  - fct_collapse()
  - fct_other()
  - fct_relevel()
  - fct_rev()
  - fct_recode()

### Tidyverse
  - case_when()
  - mutate_if()
  - gather()
  - str_remove()

### Leaflet
Used for Interactive Map Visualization
**Example:**
``` R
names(providers) 
## Brings 100+ options you can use with Leaflet

names(providers)[str_detect(names(providers), "CartoDB")]
## Brings in options that have CartoDB tiles
```

``` R
leaflet() %>% 
  addProviderTiles(provider = "CartoDB")
```
![Rplot](https://user-images.githubusercontent.com/42131127/56390095-0e2e6180-61e0-11e9-9fea-ce917106ca08.png)

``` R
leaflet() %>% 
  addProviderTiles(provider = "Esri")
```
![Rplot01](https://user-images.githubusercontent.com/42131127/56390107-18506000-61e0-11e9-8681-757aa57898ee.png)

** New York
``` R
leaflet()  %>% 
  addProviderTiles("CartoDB")  %>% 
  setView(lng = -73.98575, lat = 40.74856, minzoom = 10, dragging = TRUE) %>%
  setMaxBounds(lng1 = -73.98575 + .05, 
               lat1 = 40.74856 + .05, 
               lng2 = -73.98575 - .05, 
               lat2 = 40.74856 - .05) 
```
![Rplot02](https://user-images.githubusercontent.com/42131127/56390549-25218380-61e1-11e9-824f-d51303bcffd4.png)

The function `setMaxBounds` sets a boundary on the map even though the function allows dragging.

** UCLA
``` R
map <- leaflet()  %>% 
  addProviderTiles("Esri")  %>% 
  setView(lng = -118.4452, lat = 34.0689, zoom = 14) %>%
  addMarkers(lng = -118.4452, lat = 34.0689)
```
![Rplot](https://user-images.githubusercontent.com/42131127/56392190-9fec9d80-61e5-11e9-9162-a708bc2f7377.png)

**Note:** `popup` in `addMarkers()` = Add in popup of location name

``` R
map %>%
        clearMarkers() %>% 
        clearBounds()
```
![Rplot01](https://user-images.githubusercontent.com/42131127/56400572-747baa00-6209-11e9-91c0-5021a3c5e6f2.png)

- `clearMarkers()`: Removes the Blue Pinpoint on the map
- `clearBounds()`: Clear the boundary on the map

``` R
map %>%
  clearMarkers() %>%
  addCircleMarkers(lng = -118.4452, lat = 34.0689, radius = 3, color = 'red')
```
![Rplot](https://user-images.githubusercontent.com/42131127/56400966-72b2e600-620b-11e9-8546-77781560ee4e.png)

`addCircleMarkers()` function alllows the customization of the pinpoint location on the map.

