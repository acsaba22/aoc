#include <iostream>
#include <fstream>
#include <cassert>
#include <unordered_set>
#include <unordered_map>

using namespace std;

// P1: 20401393616
// P2: 2272
int main() {
  ifstream fin("input.txt");

  long n = 123;
  long p1 = 0;

  const long P19_4 = 19 * 19 * 19 * 19;
  unordered_map<long, long> total; // sequence -> banana count

  while (fin >> n) {
    long prevDigit = n%10;
    long sequence = prevDigit;

    unordered_set<long> seen;

    for (int i = 0; i < 2000; i++) {
      n ^= n*64;
      n %= 16777216;
      n ^= n/32;
      n %= 16777216;
      n ^= n*2048;
      n %= 16777216;

      long lastDigit = n%10;
      long diff = lastDigit - prevDigit;
      prevDigit = lastDigit;

      sequence *= 19;
      sequence += diff+9;
      sequence %= P19_4;

      if (3 <= i && seen.count(sequence) == 0) {
        total[sequence] += lastDigit;
        seen.insert(sequence);
      }
    }
    p1 += n;
  }

  cout << "P1: " << p1 << endl;

  long p2 = -1;
  for (auto [sequence, bananas]: total) {
    if (p2 < bananas) {
      p2 = bananas;
    }
  }
  cout << "P2: " << p2 << endl;
  return 0;
}