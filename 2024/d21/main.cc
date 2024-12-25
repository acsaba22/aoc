#include <iostream>
#include <fstream>
#include <cassert>
#include <tuple>
#include <map>
#include <vector>
#include <array>
#include <climits>

using namespace std;

struct Point {
  int y = -1;
  int x = -1;
  Point operator+(const Point& p2) const {
      return Point{y + p2.y, x + p2.x};
  }
  Point operator-(const Point& p2) const {
    return Point{y - p2.y, x - p2.x};
  }
  bool operator==(const Point& p2) const {
      return x == p2.x && y == p2.y;
  }
};

enum ArrowKeys{
  AARROW, UP, LEFT, DOWN, RIGHT, NArrowKeys
};

vector<Point> arrowCoordinates {{0, 2}, {0, 1}, {1, 0}, {1, 1}, {1, 2}};

enum NumpadKeys {
  AKEYPAD, K0, K1, K2, K3, K4, K5, K6, K7, K8, K9, NNumpadKeys
};
vector<Point> numpadCoordinates {
  {3, 2}, {3, 1},
  {2, 0}, {2, 1}, {2, 2},
  {1, 0}, {1, 1}, {1, 2},
  {0, 0}, {0, 1}, {0, 2},
};

NumpadKeys ToKeypadKey(char c) {
  if (c == 'A') {
    return AKEYPAD;
  }
  assert('0' <= c && c <= '9');
  int ret = K0 + (c - '0');
  return (NumpadKeys)ret;
}

struct Keyboard {
  int numKeys;
  vector<Point> keyCoordinates;
  Point nogo; // empty space where we shouldn't go
};

Keyboard arrowKeyboard{NArrowKeys, arrowCoordinates, {0, 0}};
Keyboard numpadKeyboard{NNumpadKeys, numpadCoordinates, {3, 0}};


// Dimensions: numkeys x numkeys
// For one level the number of work needed from one Key to another and pressing the other.
// All lower keys are on A.
typedef vector<vector<long>> WorkCount;

void PrintWork(WorkCount & wc) {
    cout << "============\n";
    for (auto row: wc) {
      for (auto elem: row) {
        cout << elem << " ";
      }
      cout << endl;
    }

}


// the keyboard on the previous level is arrows
// so workCount is 5x5
WorkCount NextLevel(WorkCount& workCount, Keyboard& nextKeyboard) {
  WorkCount nextCounts(nextKeyboard.numKeys, vector<long>(nextKeyboard.numKeys, 0));

  for (int from = 0; from < nextKeyboard.numKeys; from++) {
    for (int to = 0; to < nextKeyboard.numKeys; to++) {
      Point fromCoor = nextKeyboard.keyCoordinates[from];
      Point toCoor = nextKeyboard.keyCoordinates[to];
      Point diff = toCoor - fromCoor;
      vector<pair<ArrowKeys, int>> toVisit;
      if (diff.y < 0) {
        toVisit.push_back({UP, -diff.y});
      }
      if (0 < diff.y) {
        toVisit.push_back({DOWN, diff.y});
      }
      if (diff.x < 0) {
        toVisit.push_back({LEFT, -diff.x});
      }
      if (0 < diff.x) {
        toVisit.push_back({RIGHT, diff.x});
      }

      auto calcCost = [&workCount, &toVisit]() {
        long ret = 0;
        ArrowKeys lastPlace = AARROW;
        for (auto [nextPlace, pushCount]: toVisit) {
          ret += workCount[lastPlace][nextPlace];
          lastPlace = nextPlace;
          ret += pushCount - 1; // it was already pushed once in WorcCount
        }
        if (lastPlace != AARROW) {
          ret += workCount[lastPlace][AARROW];
        } else {
          assert(toVisit.size() == 0);
          ret += 1;
        }
        return ret;
      };

      long cost = calcCost();
      assert(toVisit.size() <= 2);
      if (toVisit.size() == 2) {
        std::swap(toVisit[0], toVisit[1]);
        long cost2 = calcCost();
        if (fromCoor + Point{diff.y, 0} == nextKeyboard.nogo) {
          cost = INT_MAX;
        }
        if (fromCoor + Point{0, diff.x} == nextKeyboard.nogo) {
          cost2 = INT_MAX;
        }
        // bool okPath1 = from + diff.y == nogo?
        if (cost2 < cost) {
          cost = cost2;
        }
      }

      nextCounts[from][to] = cost;
    }
  }
  return nextCounts;
}

long solve(const string& fileName, int levels) {
  ifstream fin(fileName);

  WorkCount workCount(NArrowKeys, {1,1,1,1,1}); // all zeros

  PrintWork(workCount);

  for (int level = 0; level < levels; level++) {
    WorkCount nextCounts = NextLevel(workCount, arrowKeyboard);
    workCount.swap(nextCounts);
    PrintWork(workCount);
  }

  WorkCount numericWork  = NextLevel(workCount, numpadKeyboard);
  PrintWork(numericWork);

  long complexity = 0;
  string line;
  for (getline(fin, line); line != ""; getline(fin, line)) {
    assert(line[line.size()-1] == 'A');
    long numeric = stol(line.substr(0, line.size()-1));
    long work = 0;
    NumpadKeys lastKey = AKEYPAD;
    for (char c: line) {
      NumpadKeys nextKey = ToKeypadKey(c);
      cout << numericWork[lastKey][nextKey] << " | ";
      work += numericWork[lastKey][nextKey];
      lastKey = nextKey;
    }
    cout << line << ": " << work << endl;

    complexity += numeric*work;
  }

  // cout << complexity;
  return complexity;
}

// P1: 162740
// P2:
int main() {
  string file = "example.txt";

  // long p1 = solve(file, 2);
  // cout << "P1: " << p1 << endl;

  long p2 = solve(file, 25);
  cout << "P2: " << p2 << endl;

  // should be 154115708116294 for the example

  // // for input 116045081626426 too low
  // // for input 216045081626426 too high
  return 0;
}
