#include <iostream>
#include <fstream>
#include <map>
#include <string>
#include <vector>
#include <sstream>
#include <bitset>
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

#define MAX_LIGHTS 16
// this is ugly since bitset doesn't define < operator
struct BitsetLess {
    bool operator()(const std::bitset<MAX_LIGHTS>& a,
                    const std::bitset<MAX_LIGHTS>& b) const {
        return a.to_ulong() < b.to_ulong();
    }
};

class FactoryMachine {
    public:
    size_t size;
    std::bitset<MAX_LIGHTS> goal;
    std::vector< std::bitset<MAX_LIGHTS> > buttons;
    std::vector< int64_t > joltages;

    size_t min_presses;
    std::map<std::bitset<MAX_LIGHTS>, size_t, BitsetLess> memo;
    void computeMinPresses(std::bitset<MAX_LIGHTS> light_state, size_t button_presses);
};

void FactoryMachine::computeMinPresses(std::bitset<MAX_LIGHTS> light_state, size_t button_presses) {
    // base-case: pushing even one more button will reach or exceed best so far
    if((button_presses+1) >= min_presses) {
        return;
    }

    // memo-ize: don't continue from a state already visited unless at fewer button presses
    if(memo.find(light_state) != memo.end() && button_presses >= memo[light_state]) {
        return;
    }
    memo[light_state] = button_presses;

    // try pushing all valid buttons
    std::vector< std::pair<std::bitset<MAX_LIGHTS>, size_t> > next_presses;
    for(size_t b=0; b<buttons.size(); ++b) {
        // see what the light state would be after pushing b using XOR
        std::bitset<MAX_LIGHTS> next_lights = light_state ^ buttons[b];
        if(next_lights == goal) {
            // std::cout << "Found new best: " << button_presses+1 << " vs " << min_presses << std::endl;
            min_presses = std::min(min_presses, button_presses + 1);
            return;
        }
        next_presses.push_back( {next_lights, (next_lights ^ goal).count()} );
    }

    // order children by button press that results in closest to goal first
    std::sort(next_presses.begin(), next_presses.end(),
        [](auto const& a, auto const& b) {
            return a.second < b.second;
        });

    // greedy depth first search (DFS), recurse to each valid child 
    for(auto& next_press : next_presses) {
        computeMinPresses(next_press.first, button_presses + 1);
    }
    return;
}

bool parseMachine(const std::string& s, FactoryMachine& mach) {
    mach.size = 0;
    mach.goal.reset();
    mach.buttons.clear();
    mach.joltages.clear();
    mach.memo.clear();

    // parse machine size (number of lights total)
    auto goalplus = split(s, ']');
    if(goalplus.size() != 2) return false;
    if(goalplus[0].length() < 2) return false;
    mach.size = goalplus[0].length() - 1;
    if(mach.size > MAX_LIGHTS) return false;
    mach.min_presses = 1 << mach.size;

    // parse the light goal state
    for(size_t i=0; i<mach.size; ++i) {
        // ignore the leading '[', hence i+1
        if(goalplus[0][i+1] == '#') mach.goal.set(i);
    }

    // parse the button pushes
    auto buttonlist = split(s, '(');
    if(buttonlist.size() < 2) return false;
    // ignore all the junk before the first button
    for(size_t i=1; i<buttonlist.size(); ++i) {
        // add a new bit set for this button
        mach.buttons.push_back(std::bitset<MAX_LIGHTS>());

        auto lights_only = split(buttonlist[i], ')');
        if(lights_only.size() != 2) return false;
        auto lights = split(lights_only[0], ',');
        for(auto& light : lights) {
            if(!isNumeric(light)) return false;
            int val = std::stoi(light);
            if(val < 0 || val >= (int)mach.size) return false;
            mach.buttons.back().set(val);
        }
    }

    // parse the joltages
    auto joltsplus = split(s, '{');
    if(joltsplus.size() != 2) return false;
    if(joltsplus[1].length() < 2) return false;
    joltsplus[1].pop_back(); // removes '}'
    auto jolts = split(joltsplus[1], ',');
    for(auto& jolt : jolts) {
        if(!isNumeric(jolt)) return false;
        mach.joltages.push_back(std::stoll(jolt));
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

    std::vector<FactoryMachine> machines;
    std::string line;
    while (std::getline(file, line)) {
        if(line.empty()) continue;
        FactoryMachine mach;
        if(!parseMachine(line, mach)) {
            std::cout << "Error parsing machine: " << line << std::endl;
            return 1;
        }
        machines.push_back(mach);
    }

    int64_t partA = 0;
    for(auto& mach : machines) {
        // std::cout << "Processing ..." << std::endl;
        mach.computeMinPresses(std::bitset<MAX_LIGHTS>(), 0);
        partA += mach.min_presses;
    }
    std::cout << "Part A: " << partA << std::endl;

    // std::cout << "Part B: " << countWorlds(clean_workspace) << std::endl;

    return 0;
}
