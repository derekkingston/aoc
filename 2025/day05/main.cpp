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

int64_t mergeRangesAndCount(std::vector< std::pair<int64_t, int64_t> >& ranges) {
    if(ranges.size() < 1) return 0;
    if(ranges.size() == 1) return (ranges[0].second - ranges[0].first) + 1;

    // sort so that we are processing from smallest min to largest min
    std::sort(ranges.begin(), ranges.end(),
        [](const std::pair<int64_t,int64_t>& a, const std::pair<int64_t,int64_t>& b) {
            return a.first < b.first;
        });

    // union all ranges
    std::vector< std::pair<int64_t, int64_t> > combined;
    std::pair<int64_t, int64_t> current = ranges[0];
    for(size_t n=1; n<ranges.size(); ++n) {
        if(ranges[n].first == current.first) {
            // overlap at the start, pick biggest interval end
            current.second = std::max(ranges[n].second, current.second);
        } else if(ranges[n].first <= current.second) {
            // overlap somewhere in the middle, pick biggest interval end
            current.second = std::max(ranges[n].second, current.second);
        } else {
            // not overlapping, found a gap
            combined.push_back(current);
            current = ranges[n];
        }
    }
    combined.push_back(current);

    int64_t count = 0;
    for(auto& rg : combined) {
        // count is inclusive of total range size (both endpoints)
        count += (rg.second - rg.first) + 1;
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

    size_t partA = 0;
    std::string line;
    bool build_ranges = true;
    std::vector< std::pair<int64_t, int64_t> > ranges;
    while (std::getline(file, line)) {
        if(line.empty()) {
            build_ranges = false;
            continue;
        }
        if(build_ranges) {
            auto minmax = split(line, '-');
            if(minmax.size() != 2 || !isNumeric(minmax[0]) || !isNumeric(minmax[1])) {
                std::cout << "Error processing min/max range at line: '" << line << "'" << std::endl;
                return 1;
            }
            ranges.push_back({std::stoll(minmax[0]), std::stoll(minmax[1])});
            if(ranges.back().first > ranges.back().second) {
                std::cout << "Line with min/max not in ascending order: '" << line << "'" << std::endl;
                return 1;
            }
        } else {
            if(!isNumeric(line)) {
                std::cout << "Expected a single numeric value: '" << line << "'" << std::endl;
                return 1;
            }
            int64_t val = std::stoll(line);
            bool isFresh = false;
            for(auto& rg : ranges) {
                if(val >= rg.first && val <= rg.second) {
                    isFresh = true;
                    break;
                }
            }
            if(isFresh) partA++;
        }
    }

    std::cout << "Part A: " << partA << std::endl;
    std::cout << "Part B: " << mergeRangesAndCount(ranges) << std::endl;

    return 0;
}