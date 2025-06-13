#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <cmath>
#include <cstdlib>
#include <ctime>
#include <algorithm>
#include <random>
#include <map>

using namespace std;

double square(double x) {
    return x * x;
}

double sumSqErr(const vector<double>& xs, const vector<double>& ys, int start) {
    double s = 0.0;
    for (size_t i = start; i < xs.size(); ++i) {
        s += square(xs[i] - ys[i]);
    }
    return s;
}

double sumSqErrSample(const vector<double>& xs, const vector<double>& ys, int start, const vector<int>& sampleIndexes) {
    double s = 0.0;
    for (int i : sampleIndexes) {
        s += square(xs[i] - ys[i]);
    }
    return s;
}

double predict(const vector<vector<double>>& trainData, const vector<double>& test, int nensemble, int nsample) {
    map<double, int> votes;

    for (int sampleNum = 0; sampleNum < nensemble; ++sampleNum) {
        double predicted = 0.0;
        double minError = -1.0;

        // Create a shuffled vector of indices from 1 to test.size()-1
        vector<int> indices(test.size() - 1);
        iota(indices.begin(), indices.end(), 1);
        shuffle(indices.begin(), indices.end(), std::mt19937{std::random_device{}()});
        indices.resize(nsample);

        for (const auto& train : trainData) {
            double error = sumSqErrSample(test, train, 1, indices);
            if (error < minError || minError < 0) {
                predicted = train[0];
                minError = error;
            }
        }
        votes[predicted]++;
    }

    // Find max vote
    double maxk = 0.0;
    int maxv = 0;
    for (const auto& pair : votes) {
        if (pair.second > maxv) {
            maxk = pair.first;
            maxv = pair.second;
        }
    }
    return maxk;
}

int main() {
    string fileNameTrain = "mnist_train.csv";
    string fileNameTest = "mnist_test.csv";

    // train
    ifstream trainFile(fileNameTrain);
    if (!trainFile.is_open()) {
        cerr << "Error opening train file: " << fileNameTrain << endl;
        return 1;
    }

    vector<vector<double>> trainData;
    string line;
    while (getline(trainFile, line)) {
        stringstream ss(line);
        string field;
        vector<double> row;
        while (getline(ss, field, ',')) {
            try {
                row.push_back(stod(field));
            } catch (const std::invalid_argument& e) {
                cerr << "Error parsing float in train data: " << e.what() << endl;
                trainFile.close();
                return 1;
            } catch (const std::out_of_range& e) {
                cerr << "Error parsing float (out of range) in train data: " << e.what() << endl;
                trainFile.close();
                return 1;
            }
        }
        trainData.push_back(row);
    }
    trainFile.close();
    cout << "train data loaded. rows: " << trainData.size() << endl;

    // test
    ifstream testFile(fileNameTest);
    if (!testFile.is_open()) {
        cerr << "Error opening test file: " << fileNameTest << endl;
        return 1;
    }

    int nRows = 0;
    int nCorrect = 0;
    while (getline(testFile, line)) {
        stringstream ss(line);
        string field;
        vector<double> test;
        while (getline(ss, field, ',')) {
            try {
                test.push_back(stod(field));
            } catch (const std::invalid_argument& e) {
                cerr << "Error parsing float in test data: " << e.what() << endl;
                testFile.close();
                return 1;
            } catch (const std::out_of_range& e) {
                cerr << "Error parsing float (out of range) in test data: " << e.what() << endl;
                testFile.close();
                return 1;
            }
        }
        nRows++;
        double predicted = predict(trainData, test, 10, 600);
        double answer = test[0];
        if (predicted == answer) {
            nCorrect++;
        }
        double accuracy = static_cast<double>(nCorrect) / nRows;
        cout << "row: " << nRows << ", predicted: " << predicted << ", answer: " << answer << ", accuracy: " << fixed << accuracy << endl;
    }
    testFile.close();

    return 0;
}
