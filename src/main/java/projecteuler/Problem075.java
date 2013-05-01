package projecteuler;
//http://mathworld.wolfram.com/PythagoreanTriple.html
public class Problem075{
    private static final int bound = 1500000;
    private static final byte[] ar = new byte[bound + 1];
    
    private static void transform(int x, int y, int z) {
      int len = x + y + z;
      if (len > bound) return;
      for (int l = len; l <= bound; l += len) ar[l]++;
      transform(x-2*y+2*z, 2*x-y+2*z, 2*x-2*y+3*z);
      transform(x+2*y+2*z, 2*x+y+2*z, 2*x+2*y+3*z);
      transform(-x+2*y+2*z, -2*x+y+2*z, -2*x+2*y+3*z);
    }
  public static void main(String[] args){
    transform(3,4,5);
    int count = 0;
    for (int i = 0; i < ar.length; i++)
      if (ar[i] == 1) count++;
    System.out.println("Final result is: " + count);
  }
}