public class Problem075{
  public static void main(String[] args){
    int bound = 1500000;
    int[] ar = new int[bound + 1];
    for(int a = 1; a < bound / 3; a++) {
      if (a % 1000 == 0) System.out.println(a);
      int limit = (int) Math.min((long)((bound -a)/2), (1L * a * a - 1)/2);
      for (int b = a; b <= limit; b++) {
        long sq = 1L * a * a + 1L * b * b;
        int c = (int)Math.sqrt(sq);
        int l = a + b + c;
        if (l <= bound && 1L * c * c == sq)
          ar[l]++;
      }
    }
    int count = 0;
    for (int i = 0; i < ar.length; i++)
      if (ar[i] == 1) count++;
    System.out.println("Final result is: " + count);
  }
}