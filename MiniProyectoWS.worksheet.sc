//Simpson 1/3

def simpsonSimple(a: Int, b: Int, f: Double => Double) = {
    val intermedio = ((a+b) / 2.0)
    val fa = f(a)
    val fi = f(intermedio)
    val fb = f(b)
    (b - a) * (fa + 4 * fi + fb) / 6
}

//Simpson 1/3 Compuesta

def simpsonCompuesta(a: Double, b: Double, n: Int, f: Double => Double) = {
    val h = ((b - a) / n)
    val x = new Array[Double](n+1) 
    x(0) = a
    val j = (1 to n).map(j => x(j) = (a + h * j))
    val i = (1 to n / 2).map(j => (f(x(2 * j - 2)) + 4 * f(x(2 * j - 1)) + f(x(2 * j)))).sum
    (i * h) / 3
}

// Simpson 1/3 Extendida

def simpsonExtendida(a: Int, b: Int, f: Double => Double) = {
    val n = 2 * (b - a)
    val h = (b - a) / n.toDouble
    val j = f(a) + 4 * (1 to (n - 1) by 2).map(j => f(a + j * h)).sum
    val i = f(b) + 2 * (2 to (n - 2) by 2).map(i => f(a + i * h)).sum 
    (h / 3) * (i + j)
}

// F1

simpsonSimple(3, 5, x => (-math.pow(x, 2) + 8 * x - 12))
simpsonCompuesta(3, 5, 10, x => (-math.pow(x, 2) + 8 * x - 12))
simpsonExtendida(3, 5, x => (-math.pow(x, 2) + 8 * x - 12))

//F2 

simpsonSimple(0, 2, x => (3 * math.pow(x, 2)))
simpsonCompuesta(0, 2, 6, x => (3 * math.pow(x, 2)))
simpsonExtendida(0, 2, x => (3 * math.pow(x, 2)))

//F3

simpsonSimple(-1, 1, x => (x + 2 * math.pow(x, 2) - math.pow(x, 3) + 5 * math.pow(x, 4)))
simpsonCompuesta(-1, 1, 6, x => (x + 2 * math.pow(x, 2) - math.pow(x, 3) + 5 * math.pow(x, 4)))
simpsonExtendida(-1, 1, x => (x + 2 * math.pow(x, 2) - math.pow(x, 3) + 5 * math.pow(x, 4)))

//F4

simpsonSimple(1, 2, x => ((2 * x + 1) / (math.pow(x, 2) + x)))
simpsonCompuesta(1, 2, 8, x => ((2 * x + 1) / (math.pow(x, 2) + x)))
simpsonExtendida(1, 2, x => ((2 * x + 1) / (math.pow(x, 2) + x)))

//F5

simpsonSimple(0, 1, x => (math.pow(math.E, x)))
simpsonCompuesta(0, 1, 2, x => (math.pow(math.E, x)))
simpsonExtendida(0, 1, x => (math.pow(math.E, x)))

//F6

simpsonSimple(2, 3, x => (1 / math.sqrt(x - 1)))
simpsonCompuesta(2, 3, 12, x => (1 / math.sqrt(x - 1)))
simpsonExtendida(2, 3, x => (1 / math.sqrt(x - 1)))

//F7

simpsonSimple(0, 1, x => (1 / (1 + math.pow(x, 2))))
simpsonCompuesta(0, 1, 8, x => (1 / (1 + math.pow(x, 2))))
simpsonExtendida(0, 1, x => (1 / (1 + math.pow(x, 2))))