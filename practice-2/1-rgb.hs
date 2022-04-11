-- 1)
{-
    1. El modelo de color RGB es un modelo aditivo que tiene al rojo, verde y azul como colores
    primarios. Cualquier otro color se expresa en terminos de las proporciones de estos tres colores que
    es necesario combinar en forma aditiva para obtenerlo. Dichas proporciones caracterizan a cada
    color de manera biunivoca, por lo que usualmente se utilizan estos valores como representacion de
    un color.
    Definir un tipo Color en este modelo y una funcion mezclar que permita obtener el promedio
    componente a componente entre dos colores.
-}
type Color = (Float, Float, Float)
 
mezclar :: Color -> Color -> Color
mezclar (r1, g1, b1) (r2, g2, b2) = ( (r1 + r2)/2 , (g1 + g2)/2, (b1 + b2)/2 )
