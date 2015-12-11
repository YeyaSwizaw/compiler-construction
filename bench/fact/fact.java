public class fact {
    public static long fact(long n) {
        long result = 1;
        while(n > 0) {
            result *= n;
            n--;
        }
        return result;
    }

    public static void main(String[] args) {
        for(int i = 0; i < 620; ++i) {
            System.out.printf("%d", fact(12));
        }
    }
}
