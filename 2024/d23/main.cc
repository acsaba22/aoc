#include <iostream>
#include <fstream>
#include <cassert>
#include <tuple>
#include <map>
#include <vector>
#include <array>

using namespace std;

struct Point {
  int y = -1;
  int x = -1;
  Point operator-(const Point& p2) const {
    return Point{y - p2.y, x - p2.x};
  }
};

enum ArrowKeys{
  AARROW, UP, LEFT, DOWN, RIGHT, NArrowKeys
};

vector<Point> arrowCoordinates {{0, 2}, {0, 1}, {1, 0}, {1, 1}, {1, 2}};


int main() {
  ifstream fin("example.txt");
  string line;
  assert(getline(fin, line));
  cout << line << endl;

  // DistanceCache distances;


  // Number of work needed from one Key to another and pressing the other.
  // All lower keys are on A.
  typedef array<array<long, NArrowKeys>, NArrowKeys> WorkCount;
  WorkCount workCount; // all zeros

  cout << workCount[0][1] << endl;


  workCount.fill({1,1,1,1,1});

  auto print = [](WorkCount &wc) {
    cout << "============\n";
    for (int from = AARROW; from < NArrowKeys; from++) {
      for (int to = AARROW; to < NArrowKeys; to++) {
        cout << wc[from][to] << " ";
      }
      cout << endl;
    }
  };
  print(workCount);

  for (int level = 0; level < 2; level++) {
    WorkCount nextCounts;
    for (int from = AARROW; from < NArrowKeys; from++) {
      for (int to = AARROW; to < NArrowKeys; to++) {
        Point diff = arrowCoordinates[to] - arrowCoordinates[from];
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
          // doublecheck my suspicion that whichever path is the same cost.
          std::swap(toVisit[0], toVisit[1]);
          long cost2 = calcCost();
          assert(cost == cost2);
        }

        nextCounts[from][to] = cost;
      }
    }
    workCount.swap(nextCounts);
    print(workCount);
  }

  // cout <<
  return 0;
}

// void init() {
// }
// map<char, map<char, char>> arrowKeyboard {
//   {'^', {{'>', 'A'}, {'v', 'v'}}},
//   {'A', {{'<', '^'}, {'v', '>'}}},
//   {'<', {{'>', 'v'}}},
//   {'v', {{'^', '^'}, {'<', '<'}, {'>', '>'}}},
//   {'>', {{'^', 'A'}, {'<', 'v'}}},
// };

// struct State {
//   int level; // 0 keypad 1,2,... arrows
//   char topButton = 'A'; // All lower buttons are at A
// };

// struct Simulation {
//   typedef tuple<State, State> Edge; // Between same level only

//   int myLevel = 2;

//   // Cache decoration would be so nice :(
//   long distance(State from, State to);
//   long distanceImpl(State from, State to) {
//     assert(from.level == to.level && from.level <= myLevel);
//     if (to.level == myLevel) {
//       return 0;
//     }

//   }

//   typedef map<Edge, long> DistanceCache;
//   DistanceCache distanceCache;
//   long distance(State from, State to) {
//     Edge edge{from, to};
//     auto cacheIt =  distanceCache.find(edge);
//     if (cacheIt != distanceCache.end()) {
//       return cacheIt->second;
//     }
//     long result = distanceImpl(from, to);
//     distanceCache[edge] = result;
//     return result;
//   }
// };
