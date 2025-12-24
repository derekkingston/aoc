#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>
#include <unordered_map>
#include <queue>

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

std::unordered_map<std::string, size_t> memo;
size_t countPaths(const std::unordered_map<std::string, std::vector<std::string>>& graph,
                  const std::string& start, const std::string& goal) {
    // exhaustive depth-first search counting each time reaching goal
    // memo-ize the count of paths from this start node
    if(memo.count(start)) return memo[start];

    size_t count = 0;
    for(const std::string& node : graph.at(start)) {
        if(node == goal) {
            count++;
        } else {
            count += countPaths(graph, node, goal);
        }
    }
    memo[start] = count;
    return count;
}

int main() {
    // std::string file_name = "test_input.txt";
    std::string file_name = "input.txt";
    std::ifstream file(file_name);
    if (!file.is_open()) {
        std::cerr << "Could not open file: '" << file_name << "'" << std::endl;
        return 1;
    }

    std::unordered_map<std::string, std::vector<std::string>> graph;
    std::string line;
    while (std::getline(file, line)) {
        if(line.empty()) continue;
        auto source = split(line, ':');
        if(source.size() != 2) {
            std::cout << "Error: expected exactly one ':' on line " << line << std::endl;
            return 1;
        }
        graph[source[0]] = split(source[1].substr(1), ' ');
    }

    std::cout << "Part A: " << countPaths(graph, "you", "out") << std::endl;

    return 0;
}