#include <chrono>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>
#include <array>
#include <set>
#include <map>
#include <numeric>
#include <coroutine>
#include <tuple>

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

class Present {
public:
    Present() : data{0,0,0,0,0,0,0,0,0} {}
    Present(const std::array<int, 9>& d) : data(d) {}
    int& operator()(size_t x, size_t y) { return data[x*3 + y]; }
    const int& operator()(size_t x, size_t y) const { return data[x*3 + y]; }
    Present rotate(bool ccw = true) const {
        if (ccw) return Present({ data[2], data[5], data[8], data[1], data[4], data[7], data[0], data[3], data[6] });
        return Present({ data[6], data[3], data[0], data[7], data[4], data[1], data[8], data[5], data[2] });
    }
    Present flip() const {
        return Present({ data[2], data[1], data[0], data[5], data[4], data[3], data[8], data[7], data[6] });
    }
    int area() const { return std::accumulate(data.begin(), data.end(), 0); }
    std::string print() const {
        std::string s = "";
        for(size_t x=0; x<3; ++x) { 
            for(size_t y=0; y<3; ++y) {
                if(data[x*3 + y]) s += "#";
                else s += ".";
            }
            s += "\n";
        }
        return s;
    }
    bool operator==(const Present& other) const { return data == other.data; }
    bool operator<(const Present& other) const { return data < other.data; }
private:
    std::array<int, 9> data;
};

struct Tree {
    int width;
    int height;
    std::array<int, 6> pkg_count;
};

struct Pack {
    int x_size;
    int y_size;
    std::vector< std::pair<int, int> > holes;
};

std::vector<std::vector<Present>> all_combos(const std::set<Present>& s, int N)
{
    std::vector<Present> elems(s.begin(), s.end());
    int M = elems.size();

    std::vector<std::vector<Present>> result;

    if (N <= 0 || M == 0)
        return result;

    // indices[i] \in [0, M) and indices are non-decreasing
    std::vector<int> indices(N, 0);

    while (true) {
        // build one combination
        std::vector<Present> combo;
        combo.reserve(N);
        for (int i = 0; i < N; ++i)
            combo.push_back(elems[indices[i]]);
        result.push_back(std::move(combo));

        // increment like an odometer
        int pos = N - 1;
        while (pos >= 0) {
            if (indices[pos] < M - 1) {
                indices[pos]++;

                // enforce non-decreasing rule
                for (int j = pos + 1; j < N; ++j)
                    indices[j] = indices[pos];

                break;
            }
            pos--;
        }

        if (pos < 0)
            break;
    }

    return result;
}

// global: present set for each index
std::array<std::set<Present>, 6> present_set;
std::array<std::vector<Present>, 6> present_elements;
std::map<std::array<int, 6>, Pack> recipes;
std::map<std::array<int, 6>, Pack> perfect_packs;

template<typename T>
struct Generator {
    struct promise_type {
        T current_value;

        Generator get_return_object() {
            return Generator{
                std::coroutine_handle<promise_type>::from_promise(*this)
            };
        }
        std::suspend_always initial_suspend() { return {}; }
        std::suspend_always final_suspend() noexcept { return {}; }
        std::suspend_always yield_value(T value) {
            current_value = std::move(value);
            return {};
        }
        void return_void() {}
        void unhandled_exception() { throw; }
    };

    std::coroutine_handle<promise_type> h;

    // default constructor
    Generator() : h(nullptr) {}

    // constructor from coroutine handle
    explicit Generator(std::coroutine_handle<promise_type> handle) : h(handle) {}

    // existing move constructor
    Generator(Generator&& other) : h(other.h) { other.h = nullptr; }

    // prevent copying (optional but typical)
    Generator(const Generator&) = delete;
    Generator& operator=(const Generator&) = delete;

    Generator& operator=(Generator&& other) {
        if (this != &other) {
            if (h) h.destroy();
            h = other.h;
            other.h = nullptr;
        }
        return *this;
    }

    ~Generator() { if (h) h.destroy(); }

    bool next() {
        if (!h || h.done()) return false;
        h.resume();
        return !h.done();
    }

    T const& value() const { return h.promise().current_value; }
};


using Trial = std::vector<std::tuple<Present,int,int>>;

Generator<Trial> generate_trials(
    const std::vector<Present>& objs,
    int X_MAX,
    int Y_MAX)
{
    int N = objs.size();
    int RADIX = X_MAX * Y_MAX;

    std::vector<int> digits(N, 0);

    while (true) {
        Trial trial;
        trial.reserve(N);

        for (int i = 0; i < N; ++i) {
            int d = digits[i];
            int x = d % X_MAX;
            int y = d / X_MAX;
            trial.emplace_back(objs[i], x, y);
        }

        co_yield trial;

        int pos = 0;
        while (pos < N) {
            digits[pos]++;
            if (digits[pos] < RADIX)
                break;
            digits[pos] = 0;
            pos++;
        }

        if (pos == N)
            break;
    }
}

void bestPack(const std::vector<Present>& presents) {
    // no packing for fewer than 2 presents
    if(presents.size() < 2) {
        return;
    }
    static size_t placements = 0;

    int sz = 3*presents.size();
    auto gen = generate_trials(presents, sz-2, sz-2);
    while (gen.next()) {
        const Trial& t = gen.value();
        // std::vector<int> matrix(sz*sz);
        // auto at = [&](int r, int c) -> int& { return matrix[r * sz + c]; };
        // std::fill(matrix.begin(), matrix.end(), 0);
        // for(auto pc : t) {
        //     auto [p,x,y] = pc;
        //     std::cout << "Present: " << std::endl << p.print();
        //     std::cout << "@ (" << x << ", " << y << ")" << std::endl;
        // }
        // std::string dummy;
        // std::cout << "Press Enter to continue...";
        // std::getline(std::cin, dummy);
        placements++;
        if(placements % 1000000 == 0) std::cout << placements/1000000 << "M" << std::endl;
    }
}

void comboPack(std::array<int, 6> reqs, int i, std::vector<Present>& current) {
    // base case: reached a complete set of presents to try to pack
    if(i == 6 || std::accumulate(reqs.begin()+i, reqs.end(), 0) < 1) {
        bestPack(current);
        return;
    }

    // need to chose N presents, with duplication, out of total reqs[i]
    int N = reqs[i];
    if(N < 1) {
        comboPack(reqs, i + 1, current);
        return;
    }
    // reserve selected presents for this requirement
    std::vector<Present> segment(N);

    // Iterate over all N-length sequences from present_elements[i] (with replacement)
    std::vector<size_t> idx(N, 0);
    while (true) {
        // Fill segment using current indices
        for (int n=0; n<N; ++n)
            segment[n] = present_elements[i][idx[n]];

        // Append segment to current and recurse
        current.insert(current.end(), segment.begin(), segment.end());
        comboPack(reqs, i + 1, current);
        current.resize(current.size() - N);

        // Increment mixed‑radix counter
        int pos = N - 1;
        while (pos >= 0) {
            idx[pos]++;
            if (idx[pos] < present_elements[i].size())
                break;
            idx[pos] = 0;
            pos--;
        }
        if (pos < 0)
            break; // done
    }
}

void buildBaseRecipes(int bnd) {
    for(int a=0; a<=bnd; ++a) {
        for(int b=0; b<=bnd; ++b) {
            for(int c=0; c<=bnd; ++c) {
                for(int d=0; d<=bnd; ++d) {
                    for(int e=0; e<=bnd; ++e) {
                        for(int f=0; f<=bnd; ++f) {
                            std::vector<Present> empty;
                            comboPack({a,b,c,d,e,f}, 0, empty);
                        }
                    }
                }
            }
        }
    }
}

bool validTree(const Tree& tree) {
    // assume perfect packing
    int allowed_area = tree.width*tree.height;
    int package_area = 0;
    for(size_t i=0; i<6; ++i) {
        package_area += tree.pkg_count[i] * present_set[i].begin()->area();
    }
    if(package_area < allowed_area)
        return true;
    return false;
}

int main() {
    // std::string file_name = "test_input.txt";
    std::string file_name = "input.txt";
    std::ifstream file(file_name);
    if (!file.is_open()) {
        std::cerr << "Could not open file: '" << file_name << "'" << std::endl;
        return 1;
    }

    std::vector<Present> presents;
    std::vector<Tree> trees;

    std::string line;
    while (std::getline(file, line)) {
        if(line.empty()) continue;
        if(line.find('x') != std::string::npos) {
            Tree tree;
            auto sz_cnts = split(line, ':');
            if(sz_cnts.size() != 2) return 1;
            auto wh = split(sz_cnts[0], 'x');
            if(wh.size() != 2 || !isNumeric(wh[0]) || !isNumeric(wh[1])) return 1;
            tree.width = stoi(wh[0]);
            tree.height = stoi(wh[1]);
            auto counts = split(sz_cnts[1].substr(1), ' ');
            if(counts.size() != 6) return 1;
            for(size_t i=0; i<6; ++i) {
                if(!isNumeric(counts[i])) return 1;
                tree.pkg_count[i] = stoi(counts[i]);
            }
            trees.push_back(tree);
        } else {
            Present present;
            for(size_t i=0; i<3; i++) {
                std::string presline;
                std::getline(file, presline);
                if(presline.length() != 3) return 1;
                for(size_t j=0; j<3; ++j) {
                    if(presline[j] == '#') {
                        present(i,j) = 1;
                    } else {
                        present(i,j) = 0;
                    }
                }
            }
            presents.push_back(present);
        }
    }

    // hard-coded to 6 total presents
    if(presents.size() != 6) {
        std::cout << "Exactly 6 presents must be identified vs " << presents.size() << std::endl;
        return 1;
    }

    // build present_set from rotations and flips of all available presents
    for(size_t i=0; i<6; ++i) {
        Present p = presents[i];
        present_set[i].insert(p);
        p = p.rotate();
        present_set[i].insert(p);
        p = p.rotate();
        present_set[i].insert(p);
        p = p.rotate();
        present_set[i].insert(p);
        p = p.flip();
        present_set[i].insert(p);
        p = p.rotate();
        present_set[i].insert(p);
        p = p.rotate();
        present_set[i].insert(p);
        p = p.rotate();
        present_set[i].insert(p);
        present_elements[i] = std::vector<Present>(present_set[i].begin(), present_set[i].end());
    }

    // killed after 17B with no evaluation computation ... doesn't seem feasible to brute force
    // auto start = std::chrono::high_resolution_clock::now();
    // buildBaseRecipes(2);
    // auto end = std::chrono::high_resolution_clock::now();
    // std::chrono::duration<double, std::milli> ms = end - start;
    // std::cout << "Elapsed: " << ms.count() << " ms\n";

    size_t partA = 0;
    for(const auto& tree : trees) {
        if(validTree(tree)) ++partA;
    }

    std::cout << "Part A: " << partA << std::endl;
    return 0;
}

