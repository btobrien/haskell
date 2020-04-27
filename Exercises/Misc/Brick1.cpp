
#include <vector>
#include <iostream>
using std::vector;

vector<vector<int>> layers(int width, const vector<int>& above) {
    if (width == 0) return {{}};
    if (width < 0) return {};
    for (int line : above) { if (line == 0) return {}; }
    vector<vector<int>> result;
    for (int brick : {2,3}) {
        auto shifted = above;
        for (int i = 0; i < shifted.size(); i++) { shifted[i] -= brick; }
        for (auto& layer : layers(width - brick, shifted)) { 
            layer.push_back(brick + (layer.empty() ? 0 : layer.back()));
            result.push_back(layer);
        }
    }
    return result;
}

int walls(int width, int height, vector<int> above) {
    if (height == 0) return 1;
    int result = 0;
    for (auto& layer : layers(width, above)) { 
        result += walls(width, height-1, layer);
    }
    return result;
}

int main(int argc, char** argv) {
    int width = std::stoi(std::string(argv[1]));
    int height = std::stoi(std::string(argv[2]));
    std::cout << walls(width,height,{}) << std::endl;
}


