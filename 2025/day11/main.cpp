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

// Backtracking to rebuild all shortest paths
void buildPaths(const std::string& node,
                const std::string& start,
                std::unordered_map<std::string, std::vector<std::string>>& parents,
                std::vector<std::string>& path,
                std::vector<std::vector<std::string>>& allPaths)
{
    if (node == start) {
        std::vector<std::string> temp = path;
        reverse(temp.begin(), temp.end());
        allPaths.push_back(temp);
        return;
    }

    for (const std::string& p : parents[node]) {
        path.push_back(p);
        buildPaths(p, start, parents, path, allPaths);
        path.pop_back();
    }
}

std::vector<std::vector<std::string>> bfsAllShortestPaths(
        std::unordered_map<std::string, std::vector<std::string>>& graph,
        const std::string& start,
        const std::string& target)
{
    std::unordered_map<std::string, int> dist;
    std::unordered_map<std::string, std::vector<std::string>> parents;

    std::queue<std::string> q;
    q.push(start);
    dist[start] = 0;

    while (!q.empty()) {
        std::string u = q.front();
        q.pop();

        for (const std::string& v : graph[u]) {
            if (!dist.count(v)) {
                // First time visiting v → shortest path found
                dist[v] = dist[u] + 1;
                parents[v].push_back(u);
                q.push(v);
            } else if (dist[v] == dist[u] + 1) {
                // Another shortest path found
                parents[v].push_back(u);
            }
        }
    }

    // Reconstruct all shortest paths
    std::vector<std::vector<std::string>> allPaths;
    std::vector<std::string> path = {target};
    buildPaths(target, start, parents, path, allPaths);

    return allPaths;
}

int main() {
    std::string file_name = "test_input.txt";
    // std::string file_name = "input.txt";
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

    auto paths = bfsAllShortestPaths(graph, "you", "out");
    std::cout << "Part A: " << paths.size() << std::endl;

    return 0;
}