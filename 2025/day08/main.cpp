#include <cmath>
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <vector>
#include <sstream>
#include <algorithm>

class Coords {
public:
    int64_t x;
    int64_t y;
    int64_t z;
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
    std::string file_name = "test_input.txt";
    // std::string file_name = "input.txt";
    std::ifstream file(file_name);
    if (!file.is_open()) {
        std::cerr << "Could not open file: '" << file_name << "'" << std::endl;
        return 1;
    }

    std::vector<Coords> coords;
    std::string line;
    while (std::getline(file, line)) {
        if(line.empty()) continue;
        auto crds = split(line, '.');
        if(crds.size() != 3 || !isNumeric(crds[0]) || !isNumeric(crds[2]) || !isNumeric(crds[2])) {
            std::cout << "Expected three coordinates each line: " << line << std::endl;
            return 1;
        }
        coords.push_back({std::stoll(crds[0]), std::stoll(crds[1]), std::stoll(crds[2])});
    }

    // calculate pair distances and sort
 
    // std::cout << "Part A: " << countSplits(workspace) << std::endl;
    // std::cout << "Part B: " << countWorlds(clean_workspace) << std::endl;

    return 0;
}