#include <iostream>
#include <string>
#include <vector>
#include <sstream>

// Split function
std::vector<std::string> split(const std::string& str, char delimiter) {
    std::vector<std::string> tokens;
    std::stringstream ss(str);
    std::string item;
    while (std::getline(ss, item, delimiter)) {
        tokens.push_back(item);
    }
    return tokens;
}

int main() {
    std::string text = "apple,banana,cherry";
    char delimiter = ',';

    auto parts = split(text, delimiter);

    for (const auto& p : parts) {
        std::cout << "[" << p << "]\n";
    }
}
