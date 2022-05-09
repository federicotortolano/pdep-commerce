type Producto = (String,Number)

productoCorriente :: Producto -> Bool
productoCorriente (nombreDelProducto,_) = elem (head nombreDelProducto) "aeiouAEIOU"

productoCodiciado :: Producto -> Bool
productoCodiciado (nombreDelProducto,_) = length nombreDelProducto > 10

descodiciarProducto :: String -> String
descodiciarProducto = take 10

productoDeLujo :: Producto -> Bool
productoDeLujo (nombreDelProducto,_) = elem 'x' nombreDelProducto || elem 'z' nombreDelProducto

productoXL :: Producto -> String
productoXL (nombreDelProducto,_) = nombreDelProducto ++ "XL"

versionBarata :: String -> String
versionBarata = reverse.descodiciarProducto

productoDeElite :: Producto -> Bool
productoDeElite producto = productoDeLujo producto  && productoCodiciado producto  && (not.productoCorriente $ producto)

entregaSencilla :: [a] -> Bool
entregaSencilla = even.length

aplicarCostoDeEnvio :: Number -> Number -> Number
aplicarCostoDeEnvio precioDelProducto costoDeEnvio = precioDelProducto + costoDeEnvio

aplicarDescuento :: Producto -> Number -> Number
aplicarDescuento (_,precioDelProducto) descuento = precioDelProducto - descuento*precioDelProducto/100

precioTotal :: Producto -> Number -> Number -> Number -> Number
precioTotal unProducto descuento cantidad costoDeEnvio = ((*cantidad).(aplicarDescuento unProducto) $ descuento) + costoDeEnvio
