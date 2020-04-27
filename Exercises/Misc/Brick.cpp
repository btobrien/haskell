#include <iostream>
#include <numeric>
#include <vector>
using std::vector;

using Layer = vector<int>;

vector<Layer> layersOf(int width) {
    if (width < 0) return {}; // failure
    if (width == 0) return {{}}; // success
    vector<Layer> layers;
    for (int brick : {2,3}) {
        for (auto& layer : layersOf(width - brick)) { 
            layer.push_back(brick);
            layers.push_back(std::move(layer));
        }
    }
    return layers;
}

bool overlap(const Layer& x, const Layer& y) {
    int i = x.size() - 1
    int j = y.size() - 1;
    int balance = x[i--];
    while (i >= 0 || j >= 0) {
        if (balance == 0) return true;
        if (balance > 0) balance -= y[j--];
        else balance += x[i--];
    }
    return false;
}

using Graph = vector<vector<int>>;

Graph toGraph(const vector<Layer>& layers) {
    // note: the overlap relation is symmetric and anti-reflexive
    // (i.e. a layer never overlap itself)
    // => add 2 at a time and start j at i+1
    Graph graph(layers.size());
    for (int i = 0; i < layers.size(); i++) {
        for (int j = i + 1; j < layers.size(); j++) {
           if (!overlap(layers[i],layers[j])) {
               graph[i].push_back(j);
               graph[j].push_back(i);
           }
        }
    }
    return graph;
}

int numWalks(int length, const Graph& graph) {
    vector<int> counts(graph.size(), 1);
    for (int n = 0; n < length - 1; n++) {
        auto previous_counts = counts;
        for (int i = 0; i < graph.size(); i++) {
            counts[i] = 0;
            for (int j : graph[i]) {
                counts[i] += previous_counts[j];
            }
        }
    }
    return std::accumulate(std::begin(counts), std::end(counts), 0);
}

int main(int argc, char** argv) {
    int width = std::stoi(std::string(argv[1]));
    int height = std::stoi(std::string(argv[2]));
    std::cout << numWalks(height, toGraph(layersOf(width))) << std::endl;
}


