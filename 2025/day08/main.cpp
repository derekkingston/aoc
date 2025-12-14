#include <cmath>
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <set>
#include <vector>
#include <sstream>
#include <algorithm>

class Coords {
public:
    int64_t x;
    int64_t y;
    int64_t z;

    // Define ordering for std::set
    bool operator<(const Coords& other) const {
        if (x != other.x) return x < other.x;
        if (y != other.y) return y < other.y;
        return z < other.z;
    }
};

class CoordPair {
public:
    Coords a;
    Coords b;
    double dist;
};

double calcDist(const Coords& a, const Coords& b) {
    return sqrt( (b.x-a.x)*(b.x-a.x) + (b.y-a.y)*(b.y-a.y) + (b.z-a.z)*(b.z-a.z) );
}

// Split function
// example:
//     auto fields = split(line, ',');
//     for (const auto& f : fields) {
//         std::cout << "[" << f << "] ";
//     }
//     std::cout << std::endl;
std::vector<std::string> split(const std::string& str, char delimiter) {
    std::vector<std::string> tokens;
    std::stringstream ss(str);
    std::string item;
    while (std::getline(ss, item, delimiter)) {
        tokens.push_back(item);
    }
    return tokens;
}

bool isNumeric(const std::string& s) {
    return !s.empty() && std::all_of(s.begin(), s.end(), ::isdigit);
}

int main() {
    // std::string file_name = "test_input.txt";
    std::string file_name = "input.txt";
    std::ifstream file(file_name);
    if (!file.is_open()) {
        std::cerr << "Could not open file: '" << file_name << "'" << std::endl;
        return 1;
    }

    std::vector<Coords> coords;
    std::string line;
    while (std::getline(file, line)) {
        if(line.empty()) continue;
        auto crds = split(line, ',');
        if(crds.size() != 3 || !isNumeric(crds[0]) || !isNumeric(crds[1]) || !isNumeric(crds[2])) {
            std::cout << "Expected three coordinates each line: " << line << std::endl;
            return 1;
        }
        coords.push_back({std::stoll(crds[0]), std::stoll(crds[1]), std::stoll(crds[2])});
    }

    // calculate pair distances and sort
    std::vector<CoordPair> coordPairs;
    for(size_t i = 0; i < coords.size(); ++i) {
        for(size_t j = i+1; j < coords.size(); ++j) {
            double dist = calcDist(coords[i], coords[j]);
            CoordPair cp = {coords[i], coords[j], dist};
            coordPairs.push_back(cp);
        }
    }
    std::sort(coordPairs.begin(), coordPairs.end(), [](const CoordPair& a, const CoordPair& b) {
        return a.dist < b.dist;
    });

    size_t maxConnections = 1000;
    if(coords.size() <= 20) {
        maxConnections = 10;
    }

    // build up to the max connections into circuits
    std::vector< std::set<Coords> > circuits;
    for(size_t i = 0; i < maxConnections && i < coordPairs.size(); ++i) {
        const CoordPair& cp = coordPairs[i];
        // check to see if this combines multiple circuits into a larger one
        size_t a_idx = circuits.size();
        size_t b_idx = circuits.size();
        for(size_t j = 0; j < circuits.size(); ++j) {
            if(circuits[j].find(cp.a) != circuits[j].end() || circuits[j].find(cp.b) != circuits[j].end()) {
                if(a_idx == circuits.size()) {
                    a_idx = j;
                } else if(b_idx == circuits.size()){
                    b_idx = j;
                }
            }
        }
        // one end of each pair causes a and be to merge; choose a, delete b
        if(a_idx != circuits.size() && b_idx != circuits.size()) {
            circuits[a_idx].insert(circuits[b_idx].begin(), circuits[b_idx].end());
            circuits.erase(circuits.begin()+b_idx);
            continue;
        }
        // needs to be added to an existing circuit
        if(a_idx != circuits.size()) {
            circuits[a_idx].insert(cp.a);
            circuits[a_idx].insert(cp.b);
            continue;
        }
        // not found in any circuit, add a new one
        circuits.push_back( std::set<Coords>() );
        circuits.back().insert(cp.a);
        circuits.back().insert(cp.b);
    }

    // sort the circuits by size
    std::sort(circuits.begin(), circuits.end(), [](const std::set<Coords>& a, const std::set<Coords>& b) {
        return a.size() > b.size();
    });

    std::cout << "Part A: " << circuits[0].size()*circuits[1].size()*circuits[2].size() << std::endl;

    for(size_t i=maxConnections; i < coordPairs.size(); ++i) {
        // std::cout << "Processing connection " << i+1 << " of " << coordPairs.size() << std::endl;
        const CoordPair& cp = coordPairs[i];
        // check to see if this combines multiple circuits into a larger one
        size_t a_idx = circuits.size()+1;
        size_t b_idx = circuits.size()+1;
        for(size_t j = 0; j < circuits.size(); ++j) {
            if(circuits[j].find(cp.a) != circuits[j].end() || circuits[j].find(cp.b) != circuits[j].end()) {
                if(a_idx == circuits.size()+1) {
                    a_idx = j;
                } else if(b_idx == circuits.size()+1){
                    b_idx = j;
                }
            }
        }
        // needs to be added to an existing circuit
        if(a_idx != circuits.size()+1 && b_idx == circuits.size()+1) {
            circuits[a_idx].insert(cp.a);
            circuits[a_idx].insert(cp.b);
        }
        // one end of each pair causes a and be to merge; choose a, delete b
        if(a_idx != circuits.size()+1 && b_idx != circuits.size()+1) {
            circuits[a_idx].insert(circuits[b_idx].begin(), circuits[b_idx].end());
            circuits.erase(circuits.begin()+b_idx);
        }
        if(circuits[a_idx].size() == coords.size()) {
            std::cout << "Part B: " << cp.a.x*cp.b.x << std::endl;
            break; // all coords are now connected
        }
        // not found in any circuit, add a new one
        if(a_idx == circuits.size()+1 && b_idx == circuits.size()+1) {
            circuits.push_back( std::set<Coords>() );
            circuits.back().insert(cp.a);
            circuits.back().insert(cp.b);
        }
    }

    return 0;
}