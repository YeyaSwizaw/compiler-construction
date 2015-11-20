@FunctionalInterface
interface Func {
    public long run(long x);
}

public class times {
    public static long add_five(long x) {
        return x + 5;
    }

    public static long times(long n, Func f, long x) {
        long val = x;
        for(int i = 0; i < n; ++i) {
            val = f.run(val);
        }

        return val;
    }

    public static void main(String[] args) {
        System.out.printf("%d\n", times(1000, times::add_five, 0));
    }
}
