#include <iostream>
#include <vector>

std::vector<int> cutAndMend(std::vector<int> lst, int a, int b) {
    std::vector<int> result;
    for (int i = 0; i < lst.size(); i++) {
        if (i < a || i > b) {
            result.push_back(lst[i]);
        }
    }
    return result;
}