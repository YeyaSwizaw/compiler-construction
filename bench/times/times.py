def add_five(n):
    return n + 5

def times(n, f, x):
    val = x
    for i in range(0, n):
        val = f(val)

    return val

print(times(1000, add_five, 0))
        
