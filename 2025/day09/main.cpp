#include <cmath>
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <set>
#include <vector>
#include <sstream>
#include <algorithm>

class Coords {
public:
    int64_t x;
    int64_t y;

    // Define ordering for std::set
    bool operator<(const Coords& other) const {
        if (x != other.x) return x < other.x;
        return y < other.y;
    }
};

class Rectangle {
public:
    Coords a;
    Coords b;
    int64_t area;
    bool isInside(const Coords& c) const {
        return c.x > std::min(a.x, b.x) && c.x < std::max(a.x, b.x) &&
               c.y > std::min(a.y, b.y) && c.y < std::max(a.y, b.y);
    }
};

bool crosses(const std::pair<Coords, Coords>& p, const std::pair<Coords, Coords>& q) {
    int64_t p_deltaX = p.second.x - p.first.x;
    int64_t p_deltaY = p.second.y - p.first.y;
    int64_t q_deltaX = q.second.x - q.first.x;
    int64_t q_deltaY = q.second.y - q.first.y;
    if(p_deltaX == 0 && q_deltaX == 0) {
        // both vertical
        return false;
    }
    if(p_deltaY == 0 && q_deltaY == 0) {
        // both horizontal
        return false;
    }
    if(p_deltaX == 0 && q_deltaY == 0) {
        // p vertical, q horizontal
        return (q.first.x <= p.first.x && p.first.x <= q.second.x) &&
               (p.first.y <= q.first.y && q.first.y <= p.second.y);
    }
    if(p_deltaY == 0 && q_deltaX == 0) {
        // p horizontal, q vertical
        return (p.first.x <= q.first.x && q.first.x <= p.second.x) &&
               (q.first.y <= p.first.y && p.first.y <= q.second.y);
    }
    return false;
}

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
    std::string file_name = "test_input.txt";
    // std::string file_name = "input.txt";
    std::ifstream file(file_name);
    if (!file.is_open()) {
        std::cerr << "Could not open file: '" << file_name << "'" << std::endl;
        return 1;
    }

    std::vector<Coords> coords;
    std::string line;
    while (std::getline(file, line)) {
        if(line.empty()) continue;
        auto crds = split(line, ',');
        if(crds.size() != 2 || !isNumeric(crds[0]) || !isNumeric(crds[1])) {
            std::cout << "Expected two coordinates each line: " << line << std::endl;
            return 1;
        }
        coords.push_back({std::stoll(crds[0]), std::stoll(crds[1])});
    }
    // add back the first coordinate to close the loop
    coords.push_back(coords.front());

    // compute polygon area using triangle formula
    int64_t polygon_area = 0;
    for(size_t i = 1; i < coords.size(); ++i) {
        polygon_area += (coords[i-1].x * coords[i].y) - (coords[i].x * coords[i-1].y);
    }
    polygon_area = polygon_area / 2;
    if(polygon_area < 0) {
        // make polygon counter-clockwise oriented
        std::reverse(coords.begin(), coords.end());
        polygon_area = -polygon_area;
    }

    // compute area of every possible rectangle from coords
    std::vector<Rectangle> rectangles;
    for(size_t i = 0; i < coords.size(); ++i) {
        for(size_t j = i + 1; j < coords.size(); ++j) {
            rectangles.push_back({coords[i], coords[j],
                (std::abs((coords[j].x - coords[i].x))+1) *
                (std::abs((coords[j].y - coords[i].y))+1)});
        }
    }

    // sort for max area
    std::sort(rectangles.begin(), rectangles.end(), [](const Rectangle& r1, const Rectangle& r2) {
        return r1.area > r2.area;
    });

    std::cout << "Part A: " << rectangles.front().area << std::endl;

    // in descending area order, find first rectangle that contains no other coords in interior
    // and has no line between other coords that intersects it
    for(const auto& rect : rectangles) {
        if(rect.area >= polygon_area) continue;
        bool crosses_rectangle = false;
        // lower right to upper right
        std::pair<Coords, Coords> p = {{std::max(rect.a.x, rect.b.x), std::min(rect.a.y, rect.b.y)},
                                       {std::max(rect.a.x, rect.b.x), std::max(rect.a.y, rect.b.y)}};
        // upper right to upper left
        std::pair<Coords, Coords> q = {{std::max(rect.a.x, rect.b.x), std::max(rect.a.y, rect.b.y)},
                                       {std::min(rect.a.x, rect.b.x), std::max(rect.a.y, rect.b.y)}};
        // upper left to lower left
        std::pair<Coords, Coords> r = {{std::min(rect.a.x, rect.b.x), std::max(rect.a.y, rect.b.y)},
                                       {std::min(rect.a.x, rect.b.x), std::min(rect.a.y, rect.b.y)}};
        // lower left to lower right
        std::pair<Coords, Coords> s = {{std::min(rect.a.x, rect.b.x), std::min(rect.a.y, rect.b.y)},
                                       {std::max(rect.a.x, rect.b.x), std::min(rect.a.y, rect.b.y)}};
        for(size_t i = 1; i<coords.size(); ++i) {
            std::pair<Coords, Coords> v = {coords[i-1], coords[i]};
            if(crosses(p,v) || crosses(q,v) || crosses(r,v) || crosses(s,v)) {
                crosses_rectangle = true;
                break;
            }
        }
        if(!crosses_rectangle) {
            std::cout << "Part B: " << rect.area << std::endl;
            // 3087332664 too high
            break;
        }
    }
    return 0;
}