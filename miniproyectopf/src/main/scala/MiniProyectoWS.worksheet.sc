def integracion(b:Int, a:Int, f:Double=>Double): Double = {
    val x = ((a+b)/2).toDouble
    (b-a)*((f(a)+4*f(x)+f(b))/6)
}

val p1 = integracion(5, 3, x => (-1*math.pow(x,2) + 8*x  - 12))


