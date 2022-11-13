//Simpson 1/3

def simpsonSimple(a: Int, b: Int, f: Double => Double) = {
    val intermedio = ((a+b) / 2.0)
    val fa = f(a)
    val fi = f(intermedio)
    val fb = f(b)
    (b - a) * (fa + 4 * fi + fb) / 6
}

//Simpson Compuesta

def simpsonCompuesta(a: Double, b: Double, n: Int, f: Double => Double) = {
    val h = ((b - a) / n)
    val x = new Array[Double](n+1) 
    x(0) = a
    val j = (1 to n).map(z => x(z) = (a + h * z))
    val i = (1 to n / 2).map(z => (f(x(2 * z - 2)) + 4 * f(x(2 * z - 1)) + f(x(2 * z)))).sum
    (i * h) / 3
}

// Simpson Extendida


// F1

simpsonSimple(3, 5, x => (-math.pow(x, 2) + 8 * x - 12))
simpsonCompuesta(3, 5, 10, x => (-math.pow(x, 2) + 8 * x - 12))

