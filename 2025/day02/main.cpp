#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>

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

bool isValidID_A(int64_t v) {
    std::string s = std::to_string(v);
    // must be evenly divisible by 2
    if(s.length()%2) return true;
    size_t half_size = s.length()/2;
    for(size_t k=0; k<half_size; ++k) {
        if(s[k] != s[k+half_size]) return true;
    }
    return false;
}

bool isValidID_B(int64_t v) {
    std::string s = std::to_string(v);
    size_t slen = s.length();
    if(slen < 2) return true;

    // case 1: check for all characters the same
    if(std::all_of(s.begin(), s.end(), [&](char c){ return c == s[0]; }))
        return false;

    // otherwise: check for equal size partitions and follow part A
    size_t half_size = s.length()/2;
    for(size_t piece_len=2; piece_len <= half_size; ++piece_len) {
        if(slen%piece_len) continue;
        size_t num_pieces = slen/piece_len;
        bool matches = true;
        for(size_t k=0; k<piece_len; ++k) {
            for(size_t n=1; n<num_pieces; ++n) {
                if(s[k] != s[k+n*piece_len]) {
                    matches = false;
                    break;
                }
            }
            if(!matches) break;
        }
        if(matches) return false;
    }
    return true;
}

int main() {
    // std::string file_name = "test_input.txt";
    std::string file_name = "input.txt";
    std::ifstream file(file_name);
    if (!file.is_open()) {
        std::cerr << "Could not open file: '" << file_name << "'" << std::endl;
        return 1;
    }

    int64_t partA = 0;
    int64_t partB = 0;
    std::string line;
    while (std::getline(file, line)) {
        if(line.empty()) continue;

        auto id_sections = split(line, ',');
        for(auto& id : id_sections) {
            if(id.empty()) continue;
            auto ranges = split(id, '-');
            if(ranges.size() != 2 || !isNumeric(ranges[0]) || !isNumeric(ranges[1])) {
                std::cout << "Found an invalid range: " << id << std::endl;
                continue;
            }
            int64_t min = std::stoll(ranges[0]);
            int64_t max = std::stoll(ranges[1]);
            if(min > max) {
                std::cout << "Found a range with min greater than max: " << id << std::endl;
                continue;
            }
            for(int64_t i=min; i<=max; ++i) {
                if(!isValidID_A(i)) partA += i;
                if(!isValidID_B(i)) partB += i;
            }
        }
    }
    std::cout << "Part A: " << partA << std::endl;
    std::cout << "Part B: " << partB << std::endl;

    return 0;
}