//Simpson 1/3

def SimpsonSimple(a: Int, b: Int, f: Double => Double) = {
    val intermedio = ((a+b) / 2.0)
    val fa = f(a)
    val fi = f(intermedio)
    val fb = f(b)
    (b - a) * (fa + 4 * fi + fb) / 6
}

//Simpson 1/3 Compuesta

def f(x: Double) : Double = {
    return x * Math.sin(x)
}

def SimpsonCompuesta(a: Double, b: Double, n: Int) : Double = {
    var h = ((b-a) / n)
    var x = new Array[Double](n+1)
    var sum = 0.0
    x(0) = a
    
    for(j <- 1 to n) {
        x(j) = a + h * j
    }

    for(j <- 1 to n/2) {
        sum = sum + f(x(2 * j - 2)) + 4 * f(x(2 * j - 1)) + f(x(2 * j))
    }

    return sum * h/3
}










