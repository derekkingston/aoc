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

int64_t countAccessible(const std::vector<std::string>& workspace) {
    size_t R = workspace.size();
    if(R < 3) return 0; // must have padding on top and bottom
    size_t C = workspace[0].length();
    if(C < 3) return 0; // must have padding on both sides

    int64_t count = 0;
    for(size_t r=1; r<(R-1); ++r) {
        for(size_t c=1; c<(C-1); ++c) {
            if(workspace[ r ][ c ] != '@') continue;
            int nearby = 0;
            if(workspace[r-1][c-1] == '@') nearby++;
            if(workspace[r-1][ c ] == '@') nearby++;
            if(workspace[r-1][c+1] == '@') nearby++;
            if(workspace[ r ][c-1] == '@') nearby++;
            if(workspace[ r ][c+1] == '@') nearby++;
            if(workspace[r+1][c-1] == '@') nearby++;
            if(workspace[r+1][ c ] == '@') nearby++;
            if(workspace[r+1][c+1] == '@') nearby++;
            if(nearby < 4) count++;
        }
    }
    return count;
}

int64_t removeAndRecount(std::vector<std::string>& workspace) {
    size_t R = workspace.size();
    if(R < 3) return 0; // must have padding on top and bottom
    size_t C = workspace[0].length();
    if(C < 3) return 0; // must have padding on both sides

    int64_t count = 0;
    std::vector< std::pair<size_t, size_t> > rollsToRemove;
    do {
        rollsToRemove.clear();
        // find coordinates of all rolls that can be removed
        for(size_t r=1; r<(R-1); ++r) {
            for(size_t c=1; c<(C-1); ++c) {
                if(workspace[ r ][ c ] != '@') continue;
                int nearby = 0;
                if(workspace[r-1][c-1] == '@') nearby++;
                if(workspace[r-1][ c ] == '@') nearby++;
                if(workspace[r-1][c+1] == '@') nearby++;
                if(workspace[ r ][c-1] == '@') nearby++;
                if(workspace[ r ][c+1] == '@') nearby++;
                if(workspace[r+1][c-1] == '@') nearby++;
                if(workspace[r+1][ c ] == '@') nearby++;
                if(workspace[r+1][c+1] == '@') nearby++;
                if(nearby < 4) rollsToRemove.push_back({r,c});
            }
        }
        // count these as removed
        count += rollsToRemove.size();
        // remove the rolls from the workspace
        for(auto roll : rollsToRemove) {
            workspace[roll.first][roll.second] = 'x';
        }
        // check if no rolls could be found to be removed
    } while (rollsToRemove.size() > 0);

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
    while (std::getline(file, line)) {
        if(line.empty()) continue;
        if(!workspace_width) {
            workspace_width = line.length();
            // add a line of all '.' to the top of workspace
            workspace.push_back(std::string(workspace_width+2, '.'));
        }
        if(line.length() != workspace_width) {
            std::cout << "Error: all rows must be of the same length" << std::endl;
            return 1;
        }
        // pad the workspace with '.' at start and end each row
        workspace.push_back('.' + line + '.');
    }
    // add a line of all '.' to the bottom of workspace
    workspace.push_back(std::string(workspace_width+2, '.'));

    std::cout << "Part A: " << countAccessible(workspace) << std::endl;
    std::cout << "Part B: " << removeAndRecount(workspace) << std::endl;

    return 0;
}