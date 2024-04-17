#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <cmath>

int square(int x) {
    return x * x;
}

int sumSqErr(const std::vector<int>& x, const std::vector<int>& y, int begin) {
    int l = x.size();
    if (l != y.size()) {
        throw std::runtime_error("not same size");
    }
    int sum = 0;
    for (int i = begin; i < l; i++) {
        sum += square(x[i] - y[i]);
    }
    return sum;
}

int main() {
    std::cout << "!\n";
    const std::string fileNameTrain = "mnist_train.csv";
    const std::string fileNameTest = "mnist_test.csv";

    // train
    std::ifstream file(fileNameTrain);
    if (!file.is_open()) {
        throw std::runtime_error("Error opening file");
    }
    std::string line;
    std::vector<std::vector<int>> trainData;
    while (std::getline(file, line)) {
        std::vector<int> data;
        std::stringstream ss(line);
        std::string value;
        while (std::getline(ss, value, ',')) {
            data.push_back(std::stoi(value));
        }
        trainData.push_back(data);
    }
    file.close();
    std::cout << "train data loaded. rows: " << trainData.size() << '\n';

    // test
    file.open(fileNameTest);
    if (!file.is_open()) {
        throw std::runtime_error("Error opening file");
    }
    int nRows = 0;
    int nCorrect = 0;
    while (std::getline(file, line)) {
        nRows++;
        std::vector<int> data;
        std::stringstream ss(line);
        std::string value;
        while (std::getline(ss, value, ',')) {
            data.push_back(std::stoi(value));
        }

        // predict
        int best = 0;
        int minError = -1;
        for (const auto& d : trainData) {
            int error = sumSqErr(data, d, 1);
            if (minError < 0 || error < minError) {
                minError = error;
                best = d[0];
            }
        }
        if (best == data[0]) {
            nCorrect++;
        }
        std::cout << "predicted: " << best << ", answer: " << data[0] << ", accuracy: " << static_cast<double>(nCorrect) / static_cast<double>(nRows) << '\n';
    }
    file.close();
    std::cout << "test data rows: " << nRows << ", accuracy: " << static_cast<double>(nCorrect) / static_cast<double>(nRows) << '\n';

    return 0;
}
