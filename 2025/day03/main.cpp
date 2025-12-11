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

int64_t joltage_A(std::string s) {
    int64_t best_jolt = 0;
    size_t slen = s.length();
    for(size_t k=0; k<slen; ++k) {
        int tens_digit = s[k] - '0';
        if(!tens_digit) continue;
        if(10*(tens_digit+1) > best_jolt) {
            for(size_t n=(k+1); n<slen; ++n) {
                int ones_digit = s[n] - '0';
                int candidate_jolt = 10*tens_digit + ones_digit;
                if(candidate_jolt > best_jolt) {
                    best_jolt = candidate_jolt;
                }
            }
        }
    }
    return best_jolt;
}

void largestLeft(const std::string& s, size_t& idx, size_t k, int64_t& val) {
    // find the largest, left-most digit in (idx, s.length()+1-k)
    int best_digit = s[idx] - '0';
    size_t best_idx = idx;
    for(size_t cidx=(idx+1); cidx<s.length()+1-k; ++cidx) {
        int candidate_digit = s[cidx] - '0';
        if(candidate_digit > best_digit) {
            best_digit = candidate_digit;
            best_idx = cidx;
        }
    }
    idx = best_idx;
    val = best_digit;
}

int64_t joltage_B(const std::string& s) {
    int64_t best_jolt = 0;
    if(s.length() <= 12) return std::stoll(s);
    
    size_t idx = 0;
    for(size_t k=12; k>0; k--) {
        int64_t val = 0;
        largestLeft(s, idx, k, val); ++idx;
        for(size_t n=1; n<k; n++) val *= 10;
        best_jolt += val;
    }
    return best_jolt;
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
        if(!isNumeric(line)) {
            std::cout << "Found a line not all numeric: " << line << std::endl;
            continue;
        }
        partA += joltage_A(line);
        partB += joltage_B(line);
    }
    std::cout << "Part A: " << partA << std::endl;
    std::cout << "Part B: " << partB << std::endl;

    return 0;
}