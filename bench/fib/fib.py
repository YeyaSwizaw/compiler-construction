def fib(n):
    def fib_rec(n, acc1, acc2):
        if n == 0:
            return acc2
        else:
            return fib_rec(n - 1, acc1 + acc2, acc1)

    return fib_rec(n, 0, 1)

print(fib(47))
