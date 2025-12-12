#include <iostream>
#include <fstream>
#include <string>
#include <map>
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

int64_t countSplits(std::vector<std::string>& workspace) {
    int64_t count = 0;
    for(size_t n=0; n<workspace.size()-1; ++n) {
        for(size_t k=1; k<workspace[n].size()-1; ++k) {
            if(workspace[n][k] == 'S' || workspace[n][k] == '|') {
                if(workspace[n+1][k] == '.' || workspace[n+1][k] == '|') {
                    workspace[n+1][ k ] = '|';
                } else if(workspace[n+1][k] == '^') {
                    count++;
                    workspace[n+1][k-1] = '|';
                    workspace[n+1][k+1] = '|';
                } else {
                    std::cout << "Unexpected character at (" << n+1 << "," << k
                        << "): " << workspace[n+1][k] << std::endl;
                    return 0;
                }
            }
        }
    }
    return count;
}

// TOO SLOW
// int64_t countWorlds(const std::vector<std::string>& workspace, std::pair<size_t, size_t> start) {
//     bool found_split = false;
//     std::pair<size_t, size_t> split_location = {0, 0};
//     for(size_t n=start.first+1; n<workspace.size()-1; ++n) {
//         if(workspace[n][start.second] == '^') {
//             found_split = true;
//             split_location = {n, start.second};
//             break;
//         }
//     }
//     if(!found_split) return 1;
//     return countWorlds(workspace, {split_location.first, split_location.second-1}) +
//            countWorlds(workspace, {split_location.first, split_location.second+1});
// }

int64_t countWorlds(std::vector<std::string>& workspace) {
    // make a zero'd out 2D matrix the same size as the workspace
    std::vector<std::vector<int64_t>> counts(workspace.size(), std::vector<int64_t>(workspace[0].size(), 0));
    for(size_t n=0; n<workspace.size()-1; ++n) {
        for(size_t k=1; k<workspace[n].size()-1; ++k) {
            if(workspace[n][k] == 'S' || workspace[n][k] == '|') {
                if(!counts[n][k]) {
                    counts[n][k] = 1;
                }

                // move to next line down and check for combine/split
                if(workspace[n+1][k] == '.') {
                    workspace[n+1][ k ] = '|';
                    counts[n+1][k] = counts[n][k];
                } else if(workspace[n+1][k] == '|') {
                    counts[n+1][k] += counts[n][k];
                } else if(workspace[n+1][k] == '^') {
                    workspace[n+1][k-1] = '|';
                    counts[n+1][k-1] += counts[n][k];
                    workspace[n+1][k+1] = '|';
                    counts[n+1][k+1] += counts[n][k];
                } else {
                    std::cout << "Unexpected character at (" << n+1 << "," << k
                        << "): " << workspace[n+1][k] << std::endl;
                    return 0;
                }
            }
        }
    }
    int64_t count = 0;
    size_t N = workspace.size() - 1;
    for(size_t k=0; k<workspace[N].size(); ++k) {
        count += counts[N][k];
    }
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

    std::vector<std::string> workspace;
    size_t workspace_width = 0;
    std::string line;
    size_t start_idx = 0;
    while (std::getline(file, line)) {
        if(line.empty()) continue;
        if(!workspace_width) {
            workspace_width = line.length();
            start_idx = line.find('S');
            if (start_idx == std::string::npos) {
                std::cout << "First line must have 'S' character: " << line << std::endl;
                return 1;
            }
            start_idx++; // due to padding
        }
        if(line.length() != workspace_width) {
            std::cout << "Error: all rows must be of the same length" << std::endl;
            return 1;
        }
        // pad the workspace with '.' at start and end each row
        workspace.push_back('.' + line + '.');
    }
    // pad the workspace with '.' at the bottom
    workspace.push_back(std::string(workspace_width+2, '.'));

    std::vector<std::string> clean_workspace(workspace);
    std::cout << "Part A: " << countSplits(workspace) << std::endl;
    std::cout << "Part B: " << countWorlds(clean_workspace) << std::endl;

    return 0;
}