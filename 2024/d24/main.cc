#include <iostream>
#include <fstream>
#include <vector>
#include <regex>
#include <cassert>
#include <algorithm>
#include <random>
#include <set>

#include <boost/algorithm/string.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/range/adaptor/reversed.hpp>

using namespace std;

vector<string> xnames, ynames, znames;

map<string, bool> variable;
set<string> currentlyCalculating;

void reset() {
  variable.clear();
  currentlyCalculating.clear();
}

struct Gate {
  string gate, op1, op2;
};

map<string, Gate> gates;

struct IsRecursive{};

bool getValue(const string& name) {
  auto it = variable.find(name);
  if (it != variable.end()) {
    return it->second;
  }
  if (currentlyCalculating.count(name) != 0) {
    throw IsRecursive{};
  }
  currentlyCalculating.insert(name);
  auto it2 = gates.find(name);
  assert(it2 != gates.end());
  Gate& gate = it2->second;
  bool val1 = getValue(gate.op1);
  bool val2 = getValue(gate.op2);
  bool result = false;
  if (gate.gate == "AND") {
    result = val1 && val2;
  } else if (gate.gate == "OR") {
    result = val1 || val2;
  } else {
    assert(gate.gate == "XOR");
    result = val1 ^ val2;
  }
  variable[name] = result;
  return result;
}

// xnames, ynames or znames
long getIntValue(const vector<string>& names) {
  long ret = 0;
  for (auto it = names.rbegin(); it != names.rend(); it++) {
    bool val = getValue(*it);
    ret <<= 1;
    ret += val ? 1 : 0;
  }
  return ret;
}

void setIntValue(const vector<string>& names, long value) {
  for (auto it = names.begin(); it != names.end(); it++) {
    variable[*it] = value % 2 != 0;
    value /= 2;
  }
}

int getBitError(long x, long y) {
  reset();
  setIntValue(xnames, x);
  setIntValue(ynames, y);

  long expectedZ = x + y;
  long actualZ = getIntValue(znames);

  int ret = 0;
  while (expectedZ != 0 || actualZ != 0) {
    if (expectedZ % 2 != actualZ % 2) {
      ret++;
    }
    expectedZ >>= 1;
    actualZ >>= 1;
  }
  return ret;
}

typedef pair<long, long> XY;
typedef vector<XY> Samples;

double averageError(const Samples& samples) {
  double ret = 0;
  try {
    for (auto& [x, y]: samples) {
      ret += getBitError(x, y);
    }
  } catch (IsRecursive &err) {
    return xnames.size()+1;
  }
  return ret / samples.size();
}

Samples generateSamples(int n, int bitCount) {
  Samples samples;
  long maxVal = 1;
  maxVal <<= bitCount;
  maxVal -= 1;

  mt19937_64 randomGenerator(random_device{}());
  uniform_int_distribution<long> dist(0, maxVal);

  for (int i = 0; i < n; i++) {
    samples.push_back(make_pair(dist(randomGenerator), dist(randomGenerator)));
  }
  return samples;
}

// P1: 51837135476040
// P2: hjf,kdh,kpp,sgj,vss,z14,z31,z35
int main() {
  ifstream fin("input.txt");

  regex reInput(R"((\w+): (\d+))");
  string line;
  for (getline(fin, line); line != ""; getline(fin, line)) {
    smatch m;
    assert(regex_match(line, m, reInput));
    const string& name = m[1];
    const string& value = m[2];
    assert(value == "1" || value == "0");
    variable[name] = value == "1";
    if (name[0] == 'x') {
      xnames.push_back(name);
    } else {
      assert(name[0] == 'y');
      ynames.push_back(name);
    }
  }

  regex reGates(R"((\w+) (AND|XOR|OR) (\w+) -> (\w+))");
  for (getline(fin, line); line != ""; getline(fin, line)) {
    smatch m;
    assert(regex_match(line, m, reGates));
    // cout << m[1] << " " << m[2] << " " << m[3] << " " << m[4] << endl;
    string outputName = m[4];
    gates[outputName] = Gate{m[2], m[1], m[3]};
    if (outputName[0] == 'z') {
      znames.push_back(outputName);
    }
  }
  sort(xnames.begin(), xnames.end());
  sort(ynames.begin(), ynames.end());
  sort(znames.begin(), znames.end());

  long p1 = getIntValue(znames);

  assert(xnames.size() == ynames.size());
  assert(xnames.size() + 1 == znames.size());

  Samples bigSamples = generateSamples(400, xnames.size());
  Samples smallSamples = generateSamples(10, xnames.size());

  vector<string> p2;

  for (int correction = 0; correction < 4; correction++) {
    double origError = averageError(smallSamples);
    cout << "Correction " << correction << " origial small sample error "": " << origError << endl;

    double bestSmallError = xnames.size();
    set<pair<string, string>> interestingPairs;
    int step = 0;
    for (auto it = gates.begin(); it != gates.end(); it++) {
      if (step%10 == 0) {
        cout << "Trying gate: " << it->first
          << " (Step: " << step << " / " << gates.size() << " )" << endl;
      }
      step++;
      auto it2 = it;
      for (it2++; it2 != gates.end(); it2++) {
        swap(it->second, it2->second);
        double currentErr = averageError(smallSamples);
          if (currentErr < origError && currentErr <= bestSmallError * 1.2) {
          cout << "Interesting "
            << " Error : " << currentErr << " / " << bestSmallError
            << " Pair: " << it->first << "-" << it2->first
            << " Step: ( " << step << " / " << gates.size() << " )" << endl;
          interestingPairs.insert(make_pair(it->first, it2->first));
          if (currentErr < bestSmallError) {
            bestSmallError = currentErr;
          }
        }
        swap(it->second, it2->second);
      }
    }


    origError = averageError(bigSamples);
    cout << "Correction " << correction << " origial big sample error "": " << origError << endl;

    long minError = origError;
    pair<string, string> bestPair;
    for (auto [name1, name2]: interestingPairs) {
      swap(gates[name1], gates[name2]);
      double currentError = averageError(bigSamples);
      if (currentError < minError) {
        cout << "Best pair: " << currentError << " " << name1 << "-" << name2 << endl;
        bestPair = make_pair(name1, name2);
        minError = currentError;
      }
      swap(gates[name1], gates[name2]);
    }

    if (origError <= minError) {
      cout << "Could not find better solution :(" << endl;
      return 0;
    }
    swap(gates[bestPair.first], gates[bestPair.second]);
    origError = averageError(bigSamples);
    cout << "Correction " << correction << " pair: " << bestPair.first << "-" << bestPair.second
      << " final result " << origError << endl;
    p2.push_back(bestPair.first);
    p2.push_back(bestPair.second);
  }

  cout << "P1: " << p1 << endl;
  sort(p2.begin(), p2.end());
  string p2Str =  boost::algorithm::join(p2, ",");
  cout << "P2: " << p2Str << endl;
}
