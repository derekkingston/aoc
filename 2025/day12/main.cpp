#include <chrono>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>
#include <array>
#include <set>
#include <map>
#include <numeric>

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

class Present {
public:
    Present() : data{0,0,0,0,0,0,0,0,0} {}
    Present(const std::array<int, 9>& d) : data(d) {}
    int& operator()(size_t x, size_t y) { return data[x*3 + y]; }
    const int& operator()(size_t x, size_t y) const { return data[x*3 + y]; }
    Present rotate(bool ccw = true) const {
        if (ccw) return Present({ data[2], data[5], data[8], data[1], data[4], data[7], data[0], data[3], data[6] });
        return Present({ data[6], data[3], data[0], data[7], data[4], data[1], data[8], data[5], data[2] });
    }
    Present flip() const {
        return Present({ data[2], data[1], data[0], data[5], data[4], data[3], data[8], data[7], data[6] });
    }
    int area() const { return std::accumulate(data.begin(), data.end(), 0); }
    std::string print() const {
        std::string s = "";
        for(size_t x=0; x<3; ++x) { 
            for(size_t y=0; y<3; ++y) {
                if(data[x*3 + y]) s += "#";
                else s += ".";
            }
            s += "\n";
        }
        return s;
    }
    bool operator==(const Present& other) const { return data == other.data; }
    bool operator<(const Present& other) const { return data < other.data; }
private:
    std::array<int, 9> data;
};

struct Tree {
    int width;
    int height;
    std::array<int, 6> pkg_count;
};

bool validTree(const Tree& tree, const std::vector<Present>& presents) {
    int allowed_area = tree.width*tree.height;
    int perfect_pack = 0;
    int naive_pack = 0;
    for(size_t i=0; i<6; ++i) {
        perfect_pack += tree.pkg_count[i] * presents[i].area();
        naive_pack += tree.pkg_count[i] * 9;
    }

    // lower bound: reject any tree that even perfect packing can't match
    if(perfect_pack > allowed_area)
        return false;

    // upper bound: accept any tree where naive packing works
    if(naive_pack <= allowed_area)
        return true;
    
    std::cout << "Indeterminate case -- " << tree.width << "x" << tree.height << ": ";
    for(size_t i=0; i<5; ++i) {
        std::cout << tree.pkg_count[i] << " ";
    }
    std::cout << tree.pkg_count[5] << std::endl;
    return false;
}

int main() {
    // std::string file_name = "test_input.txt";
    std::string file_name = "input.txt";
    std::ifstream file(file_name);
    if (!file.is_open()) {
        std::cerr << "Could not open file: '" << file_name << "'" << std::endl;
        return 1;
    }

    std::vector<Present> presents;
    std::vector<Tree> trees;

    std::string line;
    while (std::getline(file, line)) {
        if(line.empty()) continue;
        if(line.find('x') != std::string::npos) {
            Tree tree;
            auto sz_cnts = split(line, ':');
            if(sz_cnts.size() != 2) return 1;
            auto wh = split(sz_cnts[0], 'x');
            if(wh.size() != 2 || !isNumeric(wh[0]) || !isNumeric(wh[1])) return 1;
            tree.width = stoi(wh[0]);
            tree.height = stoi(wh[1]);
            auto counts = split(sz_cnts[1].substr(1), ' ');
            if(counts.size() != 6) return 1;
            for(size_t i=0; i<6; ++i) {
                if(!isNumeric(counts[i])) return 1;
                tree.pkg_count[i] = stoi(counts[i]);
            }
            trees.push_back(tree);
        } else {
            Present present;
            for(size_t i=0; i<3; i++) {
                std::string presline;
                std::getline(file, presline);
                if(presline.length() != 3) return 1;
                for(size_t j=0; j<3; ++j) {
                    if(presline[j] == '#') {
                        present(i,j) = 1;
                    } else {
                        present(i,j) = 0;
                    }
                }
            }
            presents.push_back(present);
        }
    }

    // hard-coded to 6 total presents
    if(presents.size() != 6) {
        std::cout << "Exactly 6 presents must be identified vs " << presents.size() << std::endl;
        return 1;
    }
    // for(size_t i=0; i<6; i++) {
    //     std::cout << i << ":" << std::endl << presents[i].print();
    //     std::cout << " area: " << presents[i].area() << std::endl << std::endl;
    // }

    size_t partA = 0;
    for(const auto& tree : trees) {
        if(validTree(tree, presents)) ++partA;
    }

    std::cout << "Part A: " << partA << std::endl;
    return 0;
}

