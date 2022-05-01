--TP1 PDEP COMMERCE

type Producto = (String,Number)

productoCorriente :: Producto -> Bool
productoCorriente (nombreDelProducto,_) = elem (head nombreDelProducto) "aeiouAEIOU"

productoCodiciado :: Producto -> Bool
productoCodiciado (nombreDelProducto,_) = length nombreDelProducto > 10

descodiciarProducto :: String -> [Char]
descodiciarProducto = take 10

productoDeLujo :: Producto -> Bool
productoDeLujo (nombreDelProducto,_) = elem 'x' nombreDelProducto || elem 'z' nombreDelProducto

productoXL :: Producto -> [Char]
productoXL (nombreDelProducto,_) = nombreDelProducto ++ "XL"

versionBarata :: [Char] -> [Char]
versionBarata = reverse.descodiciarProducto

productoDeElite :: Producto -> Bool
productoDeElite producto = productoDeLujo producto  && productoCodiciado producto  && not (productoCorriente producto)

entregaSencilla :: [a] -> Bool
entregaSencilla = even.length

aplicarCostoDeEnvio :: Number -> Number -> Number
aplicarCostoDeEnvio precioDelProducto costoDeEnvio = precioDelProducto + costoDeEnvio

aplicarDescuento :: Producto -> Number -> Number
aplicarDescuento (_,precioDelProducto) descuento = precioDelProducto - descuento

precioTotal :: Producto -> Number -> Number -> Number -> Number
precioTotal unProducto descuento cantidad costoDeEnvio = aplicarCostoDeEnvio ((aplicarDescuento unProducto descuento) * cantidad) costoDeEnvio
