#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <unordered_map>

using namespace std;

// res0: 2815556
// res1: 23927637
//
// TODO debug in vscode.
// TODO read from cin and make it comfortable in vscode to pipe in the file with run/debug.
int main() {
  vector<int> a, b;
  unordered_map<int, int> bcount;

  ifstream fin("input.txt");
  int ax, bx;

  while (fin >> ax) {
    a.push_back(ax);
    fin >> bx;
    b.push_back(bx);
    bcount[bx]++;
  }

  sort(a.begin(), a.end());
  sort(b.begin(), b.end());

  int res0 = 0;
  int res1 = 0;
  for (int i = 0; i < a.size(); i++) {
    res0 += abs(a[i] - b[i]);

    // TODO [] creates new element, at() throws.
    // Is there something which returns default but doesn't create?
    res1 += a[i] * bcount[a[i]];
  }
  cout << res0 << "\n" << res1;
  return 0;
}
