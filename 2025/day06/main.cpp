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

int main() {
    // std::string file_name = "test_input.txt";
    std::string file_name = "input.txt";
    std::ifstream file(file_name);
    if (!file.is_open()) {
        std::cerr << "Could not open file: '" << file_name << "'" << std::endl;
        return 1;
    }

    // read in all the lines of the homework
    std::vector< std::string > homework_page;
    size_t line_length = 0;
    std::vector< std::vector<std::string> > homework_text;
    std::string line;
    while (std::getline(file, line)) {
        if(!line_length) line_length = line.length();
        if(line_length != line.length()) {
            std::cout << "Not all lines are of the same length, expected: " << line_length << std::endl;
            return 1;
        }
        homework_page.push_back(line);
        std::stringstream s(line);
        std::string word;
        std::vector<std::string> homework_line;
        while (s >> word) {
            homework_line.push_back(word);
        }
        homework_text.push_back(homework_line);
    }

    /* PART A */
    // split off the operators from the values
    std::vector<std::string> operators(homework_text.back());
    size_t N = operators.size();
    homework_text.pop_back();

    // convert all of the values to integers
    std::vector< std::vector<int> > homework;
    for(auto& hw_line : homework_text) {
        // check to make sure that each line of values matches number of operators
        if(hw_line.size() != N) {
            std::cout << "Miss match between number of operators (" << N
                << ") and homework line (" << hw_line.size() << ")" << std::endl;
            return 1;
        }
        // make sure each value text and be converted to an integer
        std::vector<int> values;
        for(auto& wrd : hw_line) {
            if(!isNumeric(wrd)) {
                std::cout << "Error converting a value to an integer: " << wrd << std::endl;
                return 1;
            }
            values.push_back(std::stoi(wrd));
        }
        homework.push_back(values);
    }

    int64_t partA = 0;
    for(size_t n=0; n<N; ++n) {
        int64_t answer = 0;
        if(operators[n][0] == '*') answer = 1;
        for(size_t k=0; k<homework.size(); ++k) {
            if(operators[n][0] == '*') answer *= homework[k][n];
            else                       answer += homework[k][n];
        }
        partA += answer;
    }

    std::cout << "Part A: " << partA << std::endl;

    /* PART B */
    int64_t partB = 0;
    size_t K = homework_page.size();
    size_t idx = 0;
    for(size_t n=0; n<N; ++n) {
        int64_t answer = 0;
        if(operators[n][0] == '*') answer = 1;
        // operator index
        size_t oidx = idx;
        // find index of next operator
        for(size_t i=(idx+1); i<line_length; ++i) {
            idx++;
            if(homework_page[K-1][idx] != ' ') {
                break;
            }
        }
        // remove the space to get to first column
        size_t ridx = idx - 2;
        // unless we're on the last operator => go to last column
        if(n == (N-1)) ridx = line_length - 1;
        for( ; ridx >= oidx; --ridx) {
            std::string val_str;
            for(size_t k=0; k<(K-1); ++k) {
                if(::isdigit(homework_page[k][ridx])) {
                    val_str.push_back(homework_page[k][ridx]);
                }
            }
            int val = std::stoi(val_str);
            if(operators[n][0] == '*') answer *= val;
            else                       answer += val;
            if(ridx == 0) break;
        }
        partB += answer;
    }

    std::cout << "Part B: " << partB << std::endl;

    return 0;
}