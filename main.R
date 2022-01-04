#Lectura de datos
summerClothes <- read.csv(file = '~/R-projects/data/summer-products-with-rating-and-performance_2020-08.csv')
head(summerClothes)

#Tipo de dato asignado a cada campo
sapply(summerClothes,  function(x) class(x))

#Seleccion de columnas
summerClothes <- summerClothes %>% select(product_id, price, retail_price, merchant_id, 
                         merchant_rating, merchant_rating_count, origin_country, 
                         inventory_total, countries_shipped_to, shipping_option_name, 
                         shipping_option_price, product_color, product_variation_size_id,
                         product_variation_inventory, rating, rating_count, rating_one_count,
                         rating_two_count, rating_three_count, rating_four_count, rating_five_count,
                         units_sold, tags)

summerClothes

# Números de valores desconocidos por campo
sapply(summerClothes, function(x) sum(is.na(x)))

# Completar con ceros los valores desconocidos
summerClothes$rating_one_count <- ifelse(is.na(summerClothes$rating_one_count), 0, summerClothes$rating_one_count)
summerClothes$rating_two_count <- ifelse(is.na(summerClothes$rating_two_count), 0, summerClothes$rating_two_count)
summerClothes$rating_three_count <- ifelse(is.na(summerClothes$rating_three_count), 0, summerClothes$rating_three_count)
summerClothes$rating_four_count <- ifelse(is.na(summerClothes$rating_four_count), 0, summerClothes$rating_four_count)
summerClothes$rating_five_count <- ifelse(is.na(summerClothes$rating_five_count), 0, summerClothes$rating_five_count)

sapply(summerClothes, function(x) sum(is.na(x)))


#Valores perdidos
boxplot(summerClothes$price)

#price, retail_price, merchant_rating, merchant_rating_count, inventory_total, countries_shipped_to, 
#shipping_option_price, product_variation_inventory, rating, rating_count, rating_one_count, 
#rating_two_count, rating_three_count, rating_four_count, rating_five_count, units_sold

boxplot.stats(summerClothes$price)$out
boxplot.stats(summerClothes$retail_price)$out
boxplot.stats(summerClothes$merchant_rating)$out
boxplot.stats(summerClothes$merchant_rating_count)$out
boxplot.stats(summerClothes$inventory_total)$out
boxplot.stats(summerClothes$countries_shipped_to)$out
boxplot.stats(summerClothes$shipping_option_price)$out
boxplot.stats(summerClothes$product_variation_inventory)$out
boxplot.stats(summerClothes$rating)$out
boxplot.stats(summerClothes$rating_count)$out
boxplot.stats(summerClothes$rating_one_count)$out
boxplot.stats(summerClothes$rating_two_count)$out
boxplot.stats(summerClothes$rating_three_count)$out
boxplot.stats(summerClothes$rating_four_count)$out
boxplot.stats(summerClothes$rating_five_count)$out
boxplot.stats(summerClothes$units_sold)$out

# Agrupación por valoracion
summerClothes.bien_valorados    <- summerClothes[summerClothes$rating >= 2.5,]
summerClothes.mal_valorados  <- summerClothes[summerClothes$rating < 2.5,]

# Agrupación por precio
summerClothes.precio_bajo    <- summerClothes[summerClothes$price <10,]
summerClothes.precio_medio  <- summerClothes[summerClothes$price >= 10 && summerClothes$price <20,]
summerClothes.precio_alto  <- summerClothes[summerClothes$price >= 20,]

# Agrupación por disponibilidad del producto
summerClothes.baja_disponibilidad    <- summerClothes[summerClothes$inventory_total <20,]
summerClothes.media_disponibilidad  <- summerClothes[summerClothes$inventory_total >= 20 && summerClothes$inventory_total <40,]
summerClothes.alta_disponibilidad  <- summerClothes[summerClothes$inventory_total >= 40,]

library(nortest) 

for (i in 1:ncol(summerClothes)) {
  if (i == 1) 
    cat("Variables que no siguen una distribución normal:\n") 
  if (is.integer(summerClothes[,i]) | is.numeric(summerClothes[,i])) {
    p_val = ad.test(summerClothes[,i])$p.value 
    if (p_val < 0.05) {
      cat(colnames(summerClothes)[i])
      # Format output
      if (i < ncol(summerClothes) - 1) 
        cat(", ")
      if (i %% 3 == 0) 
        cat("\n") }
  } 
}

# Test de homogeneidad de las varianzas
fligner.test(price ~ rating, data = summerClothes)

#Visualizaciones
hist(summerClothes$rating)

hist(summerClothes$price)


barplot(table(summerClothes$rating))


plot(summerClothes$price, summerClothes$rating)

#Escribir CSV
write.csv(summerClothes, file = '~/R-projects/data/output.csv')



