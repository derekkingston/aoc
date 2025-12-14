#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>

class Point {
public:
    int64_t x;
    int64_t y;

    // Define lexicographical ordering
    bool operator<(const Point& other) const {
        if (x != other.x) return x < other.x;
        return y < other.y;
    }
};

class Segment {
public:
    Point start;
    Point end;
};

bool intersects(const Segment& p, const Segment& q) {
    int64_t p_deltaX = p.end.x - p.start.x;
    int64_t p_deltaY = p.end.y - p.start.y;
    int64_t q_deltaX = q.end.x - q.start.x;
    int64_t q_deltaY = q.end.y - q.start.y;
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
        int64_t x = p.start.x;
        int64_t y = q.start.y;
        return (std::min(q.start.x,q.end.x) < x && std::max(q.start.x,q.end.x) > x) && 
               (std::min(p.start.y,p.end.y) < y && std::max(p.start.y,p.end.y) > y);
    }
    if(p_deltaY == 0 && q_deltaX == 0) {
        // p horizontal, q vertical
        int64_t x = q.start.x;
        int64_t y = p.start.y;
        return (std::min(p.start.x,p.end.x) < x && std::max(p.start.x,p.end.x) > x) && 
               (std::min(q.start.y,q.end.y) < y && std::max(q.start.y,q.end.y) > y);
    }
    return false;
}

class Rectangle {
public:
    Point a;
    Point b;
    int64_t area;
    bool isInside(const Point& c) const {
        return c.x > std::min(a.x, b.x) && c.x < std::max(a.x, b.x) &&
               c.y > std::min(a.y, b.y) && c.y < std::max(a.y, b.y);
    }
};

bool insidePolygon(const Point& p, const std::vector<Point>& polygon, const Rectangle& bbox) {
    // Ray-casting algorithm to determine if point is inside polygon
    int64_t intersections = 0;
    Segment ray = {p, {bbox.b.x+1, p.y}}; // horizontal ray to the right
    for(size_t i = 1; i < polygon.size(); ++i) {
        Segment edge = {polygon[i-1], polygon[i]};
        if(intersects(ray, edge)) {
            intersections++;
        }
    }
    return (intersections % 2) == 1; // odd = inside, even = outside
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
    // std::string file_name = "test_input.txt";
    std::string file_name = "input.txt";
    std::ifstream file(file_name);
    if (!file.is_open()) {
        std::cerr << "Could not open file: '" << file_name << "'" << std::endl;
        return 1;
    }

    std::vector<Point> coords;
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

    // compute area of every possible rectangle from coords
    std::vector<Rectangle> rectangles;
    Rectangle bbox = {coords.front(), coords.front(), 0};
    for(size_t i = 0; i < coords.size(); ++i) {
        bbox.a.x = std::min(bbox.a.x, coords[i].x);
        bbox.a.y = std::min(bbox.a.y, coords[i].y);
        bbox.b.x = std::max(bbox.b.x, coords[i].x);
        bbox.b.y = std::max(bbox.b.y, coords[i].y);
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
        // check lines from the polygon cross the box
        bool crosses_rectangle = false;
        Point lr = {std::max(rect.a.x, rect.b.x), std::min(rect.a.y, rect.b.y)};
        Point ur = {std::max(rect.a.x, rect.b.x), std::max(rect.a.y, rect.b.y)};
        Point ul = {std::min(rect.a.x, rect.b.x), std::max(rect.a.y, rect.b.y)};
        Point ll = {std::min(rect.a.x, rect.b.x), std::min(rect.a.y, rect.b.y)};
        Segment p = {lr, ur};
        Segment q = {ur, ul};
        Segment r = {ul, ll};
        Segment s = {ll, lr};
        for(size_t i = 1; i<coords.size(); ++i) {
            Segment v = {coords[i-1], coords[i]};
            if(rect.isInside(v.start) || rect.isInside(v.end)) {
                crosses_rectangle = true;
                break;
            }
            if(intersects(p,v) || intersects(q,v) || intersects(r,v) || intersects(s,v)) {
                crosses_rectangle = true;
                break;
            }
        }
        if(crosses_rectangle) continue;

        // check all corners and center is inside polygon
        if(!insidePolygon(lr, coords, bbox)) continue;
        if(!insidePolygon(ur, coords, bbox)) continue;
        if(!insidePolygon(ul, coords, bbox)) continue;
        if(!insidePolygon(ll, coords, bbox)) continue;
        Point center = {(rect.a.x + rect.b.x) / 2, (rect.a.y + rect.b.y) / 2};
        if(!insidePolygon(center, coords, bbox)) continue;
        
        std::cout << "Part B: " << rect.area << std::endl;
        break;
    }
    return 0;
}