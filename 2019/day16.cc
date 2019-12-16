#include <iostream>
#include <string>
#include <vector>
using namespace std;

int64_t ap(vector<int64_t> const& cum, const unsigned ix) {
  int p = 1;
  int64_t r = 0;
  const unsigned len = cum.size() - 1;
  for (unsigned i = ix; i < len;) {
    const unsigned lim = min((unsigned)len, i + ix + 1);
    r += (cum[lim] - cum[i]) * p;
    i = lim + ix + 1;
    p = p == 1 ? -1 : 1;
  }
  return abs(r) % 10;
}

void run(vector<int64_t>& phase) {
  vector<int64_t> cum(phase.size() + 1);
  for (int p = 0; p < 100; ++p) {
    cum[0] = 0;
    for (unsigned i = 0; i < phase.size(); ++i) {
      cum[i + 1] = cum[i] + phase[i];
    }
    for (unsigned i = 0; i < phase.size(); ++i) {
      phase[i] = ap(cum, i);
    }
  }
}

int main() {
  string inp;
  cin >> inp;
  constexpr int m = 10000;
  vector<int64_t> phase(inp.size() * m);
  for (unsigned i = 0; i < inp.size(); ++i) {
    for (int j = 0; j < m; ++j) {
      phase[inp.size() * j + i] = inp[i] - '0';
    }
  }
  int offset = 0, pow = 1;
  for (int i = 6; i >= 0; --i) {
    offset += phase[i] * pow;
    pow *= 10;
  }
  run(phase);
  for (int i = offset; i < offset + 8; ++i) cout << phase[i];
  cout << '\n';
}