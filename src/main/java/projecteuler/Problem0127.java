package projecteuler;

import java.util.*;

public class Problem0127 {
  private PrimeFactorSieve sieve;
  private int limit;

  public Problem0127(int limit) {
    this.limit = limit;
    this.sieve = new PrimeFactorSieve(limit);
  }

  public boolean isMutualPrime(int a, int b) {
    if (a == 1 || b == 1)
      return true;
    else if (sieve.isPrime(a) || sieve.isPrime(b))
      return true;
    else {
      Set<Integer> pa = sieve.getPrimeFactors(a);
      Set<Integer> pb = sieve.getPrimeFactors(b);
      for (Integer p : pa) {
        if (pb.contains(p))
          return false;
      }
      return true;
    }
  }

  public int rad(int a, int b, int c) {
    Set<Integer> pa = sieve.getPrimeFactors(a);
    Set<Integer> pb = sieve.getPrimeFactors(b);
    Set<Integer> pc = sieve.getPrimeFactors(c);
    int result = 1;
    Set<Integer> all = new HashSet<Integer>();
    for (Integer i : pa)
      all.add(i);
    for (Integer i : pb)
      all.add(i);
    for (Integer i : pc)
      all.add(i);
    for (Integer i : all)
      result = result * i.intValue();
    return result;
  }

  public long abc() {
    long result = 0;
    for (int a = 1; a < limit / 2; a++) {
      int step = 1;
      if (a % 2 == 0)
        step = 2;
      for (int b = a + 1; b < limit - a; b += step) {
        int c = a + b;
        if (isMutualPrime(a, b) && isMutualPrime(a, c) && isMutualPrime(b, c)
            && rad(a, b, c) < c) {
          result += c;
        }

      }
    }
    return result;
  }

  /**
   * @param args
   */
  public static void main(String[] args) {
    int limit = 1000;
    if (args.length > 0)
      limit = Integer.valueOf(args[0]);
    Problem0127 p = new Problem0127(limit);
    System.out.println(p.abc());
  }

}

class PrimeFactorSieve {
  private List<Set<Integer>> sieve;

  public PrimeFactorSieve(int bound) {
    sieve = new ArrayList<Set<Integer>>(bound);
    for (int i = 0; i < bound; i++)
      sieve.add(new HashSet<Integer>());
    for (int i = 2; i < bound; i++) {
      if (sieve.get(i).isEmpty()) {
        for (int j = i + i; j < bound; j += i) {
          sieve.get(j).add(i);
        }
      }
    }
  }

  public Set<Integer> getPrimeFactors(int n) {
    if (n > 1 && sieve.get(n).isEmpty()) {
      Set<Integer> result = new HashSet<Integer>();
      result.add(n);
      return result;
    }
    return sieve.get(n);
  }

  public boolean isPrime(int n) {
    return n > 1 && sieve.get(n).isEmpty();
  }
}
