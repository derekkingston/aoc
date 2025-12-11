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

int sign(int val) {
    if(val < 0) return -1;
    if(val > 0) return 1;
    return 0;
}

int main() {
    // std::string file_name = "test_input.txt";
    std::string file_name = "input.txt";
    std::ifstream file(file_name);
    if (!file.is_open()) {
        std::cerr << "Could not open file: '" << file_name << "'" << std::endl;
        return 1;
    }

    std::string line;
    int dial = 50;
    int pwdA = 0;
    int pwdB = 0;
    while (std::getline(file, line)) {
        if( line.empty() || !(line[0] == 'L' || line[0] =='R') || !isNumeric(line.substr(1)) ) {
            std::cout << "Can't understand this line: '" << line << "'" << std::endl;
            continue;
        }
        int clicks = std::stoi(line.substr(1));
        int spins = clicks/100;
        pwdB += spins;
        clicks = clicks%100;
        if(line[0] == 'L') {
            // handle carefully the case where the dial starts at zero
            if(clicks >= dial && dial) pwdB++;
            dial -= clicks;
            if(dial < 0) dial += 100;
        } else {
            if((dial+clicks) >= 100) pwdB++;
            dial += clicks;
            dial %= 100;
        }
        if(dial == 0) {
            pwdA++;
        }
    }
    std::cout << "Part A: " << pwdA << std::endl;
    std::cout << "Part B: " << pwdB << std::endl;

    return 0;
}